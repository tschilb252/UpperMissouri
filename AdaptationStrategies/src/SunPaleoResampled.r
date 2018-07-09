#################################################
#' @title Upper Missouri Basin Study - Sun
#'        Basin Figures
#' @author MM
#' @description Figures for the Upper Missouri
#' Basin Study, Beaverhead River Basin
#' Sun Strategies - working
#' Last Modified June 15 2018
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
StgyTbl = fread('lib/StrategyTableSunPaleoResampled.csv')
MeasTbl = fread('lib/MeasureTableSun.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')

dateStrt = as.Date('1986-10-01')
dateEnd = as.Date('1995-09-30')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# WC storage
fileTmp = fileList[2]
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

datMeasAgg = datMeas %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    filter(!is.na(Value)) %>%
    left_join(StgyTbl)

datMeasAgg$StrategyLab = factor(datMeasAgg$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

#datMeasAggDist = datMeasAgg %>%
#  filter(Measure == 'Willow Creek EOWY Storage', Strategy == 'Baseline', Date %like% '09-30')
datMeasAggDist = datMeasAgg %>%
    filter(Measure == 'Willow Creek EOWY Storage') %>%
    mutate(WYear = wyear(Date), Month = month(Date), Day = day(Date)) %>%         # add water year and month columns
    filter(Month == 9, Day == 30)

ggplot() +
  geom_line(data = datMeasAggDist, aes(x = WYear, y = Value, colour = factor(Trace)))

test = datMeasAggDist %>% group_by(Trace) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  arrange(Value)


test2 = datMeasAggDist %>% filter(Trace == 68)

ggplot() +
  geom_line(data = test2, aes(x = WYear, y = Value, colour = factor(Trace)))


datMeasAggDist$StrategyLab = factor(datMeasAggDist$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot = datMeasAggDist %>%
  filter(Trace == 68) %>%
  mutate(Year = year(Date))

datMeasPlot1 = datMeasPlot %>% filter(Measure == 'Willow Creek EOWY Storage')

ggplot(data = datMeasPlot1) +
  # geom_ribbon(aes(x = Year, ymin = 0.75, ymax = 0.95), fill = '#FDEE91', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0.5, ymax = 0.75), fill = '#FCCA8B', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0, ymax = 0.5), fill = '#DC8C6B', alpha = 0.3) +
#  geom_hline(yintercept = c(0.77, 0.5, 0.25), alpha = 0.6, linetype = 3, size = 0.2) +
  geom_line(aes(x = Year, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491', '#7A5BA9')) +
#  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
#    labels = paste0(c(0, 25, 50, 75, 100), '%')) +
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
ggsave(paste0(dirOup, 'WillowCreekStorageLongestDrought_V2.png'), height = 8, width = 15)

#################################################################################################

#'  Read in Data
# FSID storage account

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

fileTmp = fileList[4]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas = data.table()
for(iterFile in 2:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas = bind_rows(datMeas, datTmpDT)
}

# get account release from a different rdf file
fileTmp = fileList[5]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
for(iterFile in 2:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas = bind_rows(datMeas, datTmpDT)
}


datMeasAgg = datMeas %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    filter(!is.na(Value)) %>%
    left_join(StgyTbl)

datMeasAgg$StrategyLab = factor(datMeasAgg$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasAggDist = datMeasAgg %>%
    filter(Measure == 'FSID WC Contract Volume')

test = datMeasAggDist %>%
    group_by(Trace) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    arrange(Value)

test2 = datMeasAggDist %>% filter(Trace == 68 | Trace == 115 | Trace == 4)

ggplot() +
  geom_line(data = test2, aes(x = Date, y = Value, colour = factor(Trace)))

##########################################################################################################


# WC reservoir inflow
fileTmp = fileList[2]
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

datMeasAgg = datMeas %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    filter(!is.na(Value)) %>%
    left_join(StgyTbl)

datMeasAgg$StrategyLab = factor(datMeasAgg$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

#datMeasAggDist = datMeasAgg %>%
#  filter(Measure == 'Willow Creek EOWY Storage', Strategy == 'Baseline', Date %like% '09-30')
datMeasAggDist = datMeasAgg %>%
    filter(Measure == 'Willow Creek EOWY Storage', Date %like% '09-30')

ggplot() +
  geom_line(data = datMeasAggDist, aes(x = WYear, y = Value, colour = factor(Trace)))

test = datMeasAggDist %>% group_by(Trace) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  arrange(Value)


test2 = datMeasAggDist %>% filter(Trace == 68)

ggplot() +
  geom_line(data = test2, aes(x = WYear, y = Value, colour = factor(Trace)))


datMeasAggDist$StrategyLab = factor(datMeasAggDist$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot = datMeasAggDist %>%
  filter(Trace == 68) %>%
  mutate(Year = year(Date))

datMeasPlot1 = datMeasPlot %>% filter(Measure == 'Willow Creek EOWY Storage')

ggplot(data = datMeasPlot1) +
  # geom_ribbon(aes(x = Year, ymin = 0.75, ymax = 0.95), fill = '#FDEE91', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0.5, ymax = 0.75), fill = '#FCCA8B', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0, ymax = 0.5), fill = '#DC8C6B', alpha = 0.3) +
#  geom_hline(yintercept = c(0.77, 0.5, 0.25), alpha = 0.6, linetype = 3, size = 0.2) +
  geom_line(aes(x = Year, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491', '#7A5BA9')) +
#  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
#    labels = paste0(c(0, 25, 50, 75, 100), '%')) +
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
ggsave(paste0(dirOup, 'WillowCreekStorageLongestDrought.png'), height = 8, width = 15)
