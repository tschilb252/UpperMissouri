#################################################
#' @title Upper Missouri Basin Study - Big Hole
#'        Basin Figures
#' @author Dan Broman
#' @description Summary figures for the Upper Missouri
# Basin Study, Big Hole River Basin
#' Last Modified April 27 2018
#################################################
library(tidyverse)
library(data.table)
library(RWDataPlyr)
library(RColorBrewer)
setwd('C:/Projects/git/UpperMissouri/AdaptationStrategies/')
source('src/fncnLib.r')

#################################################
#' User Inputs
# Data Directories
dirInp = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableBigHole.csv')
MeasTbl = fread('lib/MeasureTableBigHole.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

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
    summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
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

datMeasAvgFutFl = datMeasAvgFut %>%
  filter(Period == '2050s' | is.na(Period))
datMeasAvgFutFl$Measure = 'Shortage'

datMeasAvgFutFl$Scenario = factor(datMeasAvgFutFl$Scenario, levels = rev(c('HD', 'HW', 'CT', 'WD', 'WW', 'MID', 'LDP', 'MIP', 'LPP')))
datMeasAvgFutFl$Strategy = factor(datMeasAvgFutFl$Strategy, levels = c('Baseline', 'SummerMax40cfs', 'SummerMax20cfs', 'NoSummerMax'))


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
    group_by(Scenario, Period, Strategy, WYear, Month) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = mean(Value)) %>%       # mean flow by month and year
    ungroup()

datMeas2Avg = datMeas2Agg %>%
  group_by(Scenario, Period, Strategy, Month) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas2AvgHist = datMeas2Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas2AvgFut = datMeas2Avg %>%
  left_join(datMeas2AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas2AvgFutFl = datMeas2AvgFut %>%
  filter(Period == '2050s' | is.na(Period))

measLabTbl = data.table(Month = 1:12, Measure = paste0(month.abb, ' In-Stream Flow'))

datMeas2AvgFutFl = datMeas2AvgFutFl %>%
  left_join(measLabTbl)

datMeas2AvgFutFl$Scenario = factor(datMeas2AvgFutFl$Scenario, levels = rev(c('HD', 'HW', 'CT', 'WD', 'WW', 'MID', 'LDP', 'MIP', 'LPP')))
datMeas2AvgFutFl$Strategy = factor(datMeas2AvgFutFl$Strategy, levels = c('Baseline', 'SummerMax40cfs', 'SummerMax20cfs', 'NoSummerMax'))

m1 = ggplot(data = datMeasAvgFutFl, aes(x = Measure, y = Scenario, fill = ValueChange, label = round(ValueChange))) +
  geom_tile() +
  geom_text(size = 3) +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, 'RdBu')), limits = c(-100, 100)) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
    coord_equal()

m2 = ggplot(data = datMeas2AvgFutFl, aes(x = Measure, y = Scenario, fill = ValueChange, label = round(ValueChange))) +
  geom_tile() +
  geom_text(size = 3) +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_gradientn(colors = brewer.pal(9, 'RdBu'), limits = c(-100, 100)) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
    coord_equal()
ggsave(paste0(dirOup, 'BigHoleISFGrid2.png'), height = 14, width = 3)

p = grid.arrange(arrangeGrob(m1, m2), ncol = 1)
plot_grid(m1, m2, align = "h", ncol = 2, rel_width = c(1/8, 7/8))


ggsave(paste0(dirOup, 'BigHoleISFGrid3.png'), height = 14, width = 3)
