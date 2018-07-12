#################################################
#' @title Upper Missouri Basin Study - Sun
#' Figures
#' @author Dan Broman, MM
#' @description Summary figures for the Upper Missouri
#' Basin Study, Sun Strategy, Offstream Storage
#' Last Modified July 6 2018
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
dirInp = 'T:/WaterResources/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'T:/WaterResources/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableSunPishkunAdd.csv')
MeasTbl = fread('lib/MeasureTableSunPishkunAdd.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Reservoir fill
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

datMeas2Avg = datMeas2 %>%
    mutate(WYear = wyear(Date), Month = month(Date), Day = day(Date)) %>%         # add water year and month columns
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    filter(Slot == 'Pishkun Res.Storage') %>%
    filter(Month == 06, Day == 01) %>%
    group_by(Measure, Scenario, Period, Strategy) %>%   # group by scenario, period, and strategy
    summarise(Value = mean(Value)) %>%       # count of max storage
    ungroup()

datMeas2Full = datMeas2Avg %>%
    dplyr::mutate(Full = (Value-48000)/30000*100) %>%
    dplyr::mutate(Full = ifelse(Full>0,Full,0))

datMeasPlot = datMeas2Full %>%
    filter(is.na(Period) | Period == '2050s' | Period == 'Historical')

datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
      levels = unique(MeasTbl$Measure))

datMeasPlot$Scenario = factor(datMeasPlot$Scenario,
      levels = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'MID', 'LDP', 'MIP', 'LPP'))

ggplot(data = datMeasPlot, aes(x=Scenario, y=Full, fill=StrategyLab)) +
      geom_bar(stat="identity", position="dodge") +
#      facet_wrap(~Period, scales = 'free', ncol = 1) +
      scale_fill_manual(values = c('#25499F', '#23A491')) +
      scale_y_continuous(limit=c(0,100)) +
#      scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
      xlab('') +
      ylab('Avg Percent Full on June 1') +
      theme(
        axis.line.x=element_line(size=0.5, colour = 'gray60'),
        axis.line.y=element_line(size=0.5, colour = 'gray60'),
        axis.line=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
        axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_text(),
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

ggsave(paste0(dirOup, 'SunPishkunAdd30kAcFtFull.png'), height = 8, width = 10)


StgyTbl = fread('lib/StrategyTableSunPishkunAdd_wBase.csv')
dateStrt = as.Date('1986-10-01')
dateEnd = as.Date('1995-09-30')
#################################################
#'  Read in Data

# Shortages
fileTmp = fileList[1]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas = bind_rows(datMeas, datTmpDT)
}

datMeas = datMeas %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasAgg = datMeas %>%
    mutate(Value = Value * 1.98347) %>%     # convert cfs to ac-ft
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    group_by(Measure, Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
    ungroup()

tmp2 = datMeasAgg %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasPlot = datMeasAgg %>% filter(Scenario == 'Historical', WYear >= 1985, WYear <= 1990) %>%
  filter(Measure == 'GID Shortages')
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

ggplot(data = datMeasPlot) +
  geom_line(aes(x = WYear, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = WYear, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, scales = 'free', ncol = 1) +
  scale_colour_manual(values = c('#25499F', '#23A491')) +
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

ggsave(paste0(dirOup, 'SunPiskunAddStorage_GIDShortage.png'), height = 8, width = 10)
