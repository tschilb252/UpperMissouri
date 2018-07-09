#################################################
#' @title Upper Missouri Basin Study - Musselshell
#' Figures
#' @author Marketa McGuire
#' @description Summary figures for the Upper Missouri
#' Basin Study, Musselshell Strategy
#' Last Modified June 13 2018
#################################################
library(tidyverse)
library(data.table)
library(RWDataPlyr)
library(RColorBrewer)
setwd('C:/Users/MMcguire/Documents/GitHub/UpperMissouri/AdaptationStrategies/')
source('src/fncnLib.r')

#################################################
#' User Inputs
# Data Directories
dirInp = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableMusselshell.csv')
MeasTbl = fread('lib/MeasureTableMusselshell.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data
# Musselshell outputs for scenario runs are different from baseline
# code is modified to look for the right file depending on the run
# New reservoir users not found in baseline model
# code is modified to pull out correct set of users for each run
fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Shortages
fileTmp = fileList[3]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas3 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  if(Strategy != 'Baseline'){
    datTmpDT = Rdf2dt(datTmp, slotListTmp)
  } else {
    slotListTmp2 = slotListTmp[-1]
    datTmpDT = Rdf2dt(datTmp, slotListTmp2)
  }
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas3 = bind_rows(datMeas3, datTmpDT)
}

datMeas3 = datMeas3 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas3Agg = datMeas3 %>%
      mutate(Value = Value * 1.98347) %>%     # convert cfs to ac-ft
      mutate(WYear = wyear(Date)) %>%         # add water year column
      dplyr::rename(Slot = RiverWareSlot) %>%
      left_join(MeasTbl) %>%
      group_by(Measure, Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
      summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
      ungroup()


datMeasPlot = datMeas3Agg %>% filter(Scenario == 'Historical', WYear >= 1985, WYear <= 1990)
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

ggplot(data = datMeasPlot) +
  geom_line(aes(x = WYear, y = Value, colour = StrategyLab)) +
#  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, scales = 'free', ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491')) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    legend.text=element_text(size = 10),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

ggsave(paste0(dirOup, 'MusselshellDryYearsShortage.png'), height = 8, width = 10)

# storage in new reservoir
#------------------------------
fileTmp = fileList[1]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas1 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  if(Strategy != 'Baseline'){
    datTmpDT = Rdf2dt(datTmp, slotListTmp)
  } else {
    slotListTmp2 = slotListTmp[-1]
    datTmpDT = Rdf2dt(datTmp, slotListTmp2)
  }
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas1 = bind_rows(datMeas1, datTmpDT)
}

datMeas1 = datMeas1 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas1Plot = datMeas1 %>%
    mutate(WYear = wyear(Date), Month = month(Date), Day = day(Date)) %>%         # add water year column
    filter(Month == 9, Day == 30) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    group_by(Measure, Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    ungroup()


    datMeasPlot = datMeas1Plot %>% filter(Scenario == 'Historical', WYear >= 1985, WYear <= 1990)
    datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
    datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

    datMeasPlot$Measure = factor(datMeasPlot$Measure ,
      levels = unique(MeasTbl$Measure))

    ggplot(data = datMeasPlot) +
      geom_line(aes(x = WYear, y = Value, colour = StrategyLab)) +
    #  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab)) +
      facet_wrap(~Measure, scales = 'free', ncol = 1) +
      scale_colour_manual(values = c('#25499F', '#23A491')) +
      scale_linetype_manual(values = c("solid", "dashed")) +
      scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
      xlab('') +
      ylab('') +
      theme(
        axis.line.x=element_line(size=0.5, colour = 'gray60'),
        axis.line.y=element_line(size=0.5, colour = 'gray60'),
        axis.line=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
        axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size = 10),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.background = element_blank(),
        strip.text.x=element_text(size = 10),
        strip.text.y=element_text(size = 10)
      )

    ggsave(paste0(dirOup, 'MusselshellDryYearsNewResStg.png'), height = 8, width = 10)


#Instream Flows - WHAT TO SHOW EXACTLY, TBD
#-------------------------
fileTmp = fileList[2]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas2 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas2 = bind_rows(datMeas2, datTmpDT)
}

datMeas2 = datMeas2 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas2Agg = datMeas2 %>%
    mutate(WYear = wyear(Date), Month = month(Date)) %>%         # add water year and month columns
    filter(Month %in% 7:10) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    group_by(Scenario, Period, Strategy, Slot, WYear, Month) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = mean(Value)) %>%       # mean flow by month and year
    left_join(MeasTbl)


    datMeasPlot = datMeas2Agg %>% filter(Scenario == 'Historical', WYear >= 1985, WYear <= 1990)
    datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
    datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

    datMeasPlot$Measure = factor(datMeasPlot$Measure ,
      levels = unique(MeasTbl$Measure))

    ggplot(data = datMeasPlot) +
      geom_line(aes(x = Date, y = Value, colour = StrategyLab)) +
    #  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab)) +
      facet_wrap(~Measure, scales = 'free', ncol = 1) +
      scale_colour_manual(values = c('black', '#25499F', '#23A491')) +
      scale_linetype_manual(values = c("solid", "solid", "dashed")) +
      scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
      xlab('') +
      ylab('') +
      theme(
        axis.line.x=element_line(size=0.5, colour = 'gray60'),
        axis.line.y=element_line(size=0.5, colour = 'gray60'),
        axis.line=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
        axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size = 10),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.background = element_blank(),
        strip.text.x=element_text(size = 10),
        strip.text.y=element_text(size = 10)
      )

    ggsave(paste0(dirOup, 'MusselshellDryYearsFlow.png'), height = 8, width = 10)
