#################################################
#' @title Upper Missouri Basin Study - Marias
#'        Basin Figures
#' @author Dan Broman
#' @description Summary figures for the Upper Missouri
#' Basin Study, Marias River Basin
#' Last Modified June 12 2018
#################################################
library(tidyverse)
library(data.table)
library(RWDataPlyr)
library(RColorBrewer)
library(httr)
setwd('C:/Projects/git/UpperMissouri/AdaptationStrategies/')
source('src/fncnLib.r')

#################################################
#' User Inputs
# Data Directories
dirInp = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableMarias.csv')
MeasTbl = fread('lib/MeasureTableMarias.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Irrigation Diversions
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
    group_by(Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%       # sum up diversions
    ungroup()

datMeasAvg = datMeasAgg %>%
  group_by(Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeasAvgHist = datMeasAvg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  dplyr::select(-Scenario, -Period)

datMeasAvgFut = datMeasAvg %>%
  mutate(ValueHist = datMeasAvgHist$Value) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeasAvgFut$Measure = 'Irrigation Diversion'
datMeasAvgFut = datMeasAvgFut %>% mutate(ValueColScle = ValueChange)

# Reservoir EOWY Storage
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
    filter(Month == 9, Day == 30) %>%
    group_by(Scenario, Period, Strategy) %>%   # group by scenario, period, and strategy
    summarise(Value = mean(Value)) %>%       # mean eowy storage
    ungroup()

datMeas2AvgHist = datMeas2Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  dplyr::select(-Scenario, -Period)

datMeas2AvgFut = datMeas2Avg %>%
  mutate(ValueHist = datMeas2AvgHist$Value) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas2AvgFut$Measure = 'EOWY Storage'
datMeas2AvgFut = datMeas2AvgFut %>% mutate(ValueColScle = ValueChange)

# Recreation (Reservoir Elevation)
fileTmp = fileList[3]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas3 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas3 = bind_rows(datMeas3, datTmpDT)
}

ValueThresh = 2977 # includes most Tiber boat ramps

datMeas3 = datMeas3 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas3Agg = datMeas3 %>%
    mutate(WYear = wyear(Date), Month = month(Date)) %>%         # add water year and month columns
    filter(Month >= 4, Month <= 10, WYear >= 1951) %>%
    mutate(ValueExc = ifelse(Value >= ValueThresh, 1, 0)) %>%
    group_by(Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(ValueExc) / n()) %>%       # sum up shortages by above groups
    ungroup()

datMeas3Avg = datMeas3Agg %>%
  group_by(Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas3AvgHist = datMeas3Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  dplyr::select(-Scenario, -Period)

datMeas3AvgFut = datMeas3Avg %>%
  mutate(ValueHist = datMeas3AvgHist$Value) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas3AvgFut$Measure = 'Days above 2,977ft'
datMeas3AvgFut = datMeas3AvgFut %>% mutate(ValueColScle = ValueChange)

fileTmp = fileList[4]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas4 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas4 = bind_rows(datMeas4, datTmpDT)
}

datMeas4 = datMeas4 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas4Agg = datMeas4 %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    group_by(Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%
    ungroup()

datMeas4Avg = datMeas4Agg %>%
  group_by(Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas4AvgHist = datMeas4Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  dplyr::select(-Scenario, -Period)

datMeas4AvgFut = datMeas4Avg %>%
  mutate(ValueHist = datMeas4AvgHist$Value) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas4AvgFut$Measure = 'Hydropower Production'
datMeas4AvgFut = datMeas4AvgFut %>% mutate(ValueColScle = ValueChange)

fileTmp = fileList[5]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas5 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas5 = bind_rows(datMeas5, datTmpDT)
}

datMeas5 = datMeas5 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas5Agg = datMeas5 %>%
    mutate(WYear = wyear(Date), Month = month(Date)) %>%         # add water year and month columns
    filter(Month %in% 7:10) %>%
    group_by(Scenario, Period, Strategy, WYear, Month) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = mean(Value)) %>%       # mean flow by month and year
    ungroup()

datMeas5Avg = datMeas5Agg %>%
  group_by(Scenario, Period, Strategy, Month) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas5AvgHist = datMeas5Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas5AvgFut = datMeas5Avg %>%
  left_join(datMeas5AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

measLabTbl = data.table(Month = 1:12, Measure = paste0(month.abb, ' In-Stream Flow'))

datMeas5AvgFut = datMeas5AvgFut %>%
  left_join(measLabTbl)

datMeas5AvgFut = datMeas5AvgFut %>% mutate(ValueColScle = ValueChange)

# Combine measures and plot
datMeasPlot = bind_rows(datMeasAvgFut, datMeas2AvgFut, datMeas3AvgFut,
  datMeas4AvgFut, datMeas5AvgFut)

datMeasPlot$Scenario = factor(datMeasPlot$Scenario,
  levels = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
    'MID', 'LDP', 'MIP', 'LPP')))
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(datMeasPlot$Measure))

# Plot defs
pctLow = 5
pctHigh = 100
colPal = c('#DA4325', '#ECA14E', '#F4F3EB', '#5CC3AF', '#0A6265')

datMeasPlot = datMeasPlot %>%
  mutate(ValueTxt = ifelse(abs(ValueColScle ) > pctHigh, '•', '')) %>%
  mutate(ValueColScle = ifelse(abs(ValueColScle) < pctLow, 0,
  ifelse(ValueColScle > pctHigh, pctHigh,
    ifelse(ValueColScle < -1 * pctHigh, -1 * pctHigh, ValueColScle))))

# Plot 2050s
datMeasPlotFl = datMeasPlot %>%
  filter(Period %in% c('2050s', 'Historical') | is.na(Period))

ggplot(data = datMeasPlotFl, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = ValueTxt)) +
  geom_tile(colour = 'white', size = 1) +
  geom_text(size = 4, colour = 'white') +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left", labeller = label_wrap_gen(width=20)) +
  scale_fill_gradientn(colors = colPal, limits = c(-pctHigh, pctHigh)) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0), position="top") +
  scale_y_discrete(expand=c(0,0), position="right") +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  ) +
    coord_equal()

ggsave(paste0(dirOup, 'MariasGrid2050s.png'), height = 10, width = 8)
write.csv(datMeasPlot, paste0(dirOup, 'MariasGrid2050s.csv'), row.names = F, quote = F)

# Plot 2080s
datMeasPlotFl = datMeasPlot %>%
  filter(Period %in% c('2080s', 'Historical') | is.na(Period))

ggplot(data = datMeasPlotFl, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = ValueTxt)) +
  geom_tile(colour = 'white', size = 1) +
  geom_text(size = 4, colour = 'white') +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left", labeller = label_wrap_gen(width=20)) +
  scale_fill_gradientn(colors = colPal, limits = c(-pctHigh, pctHigh)) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0), position="top") +
  scale_y_discrete(expand=c(0,0), position="right") +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  ) +
    coord_equal()

ggsave(paste0(dirOup, 'MariasGrid2080s.png'), height = 10, width = 8)
write.csv(datMeasPlot, paste0(dirOup, 'MariasGrid2080s.csv'), row.names = F, quote = F)
