#################################################
#' @title Upper Missouri Basin Study - Big Hole
#'        Basin Figures
#' @author Dan Broman
#' @description Summary figures for the Upper Missouri
# Basin Study, Increased Irrigation Efficiency Strategy
#' Last Modified May 18 2018
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
StgyTbl = fread('lib/StrategyTableIncIrrigEffic.csv')
MeasTbl = fread('lib/MeasureTableIncIrrigEffic.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)
ctFiles = 2 #remive when PaleoEvent runs are done

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
# summarise data to reduce the size
datMeasAgg = datMeas %>%
  mutate(Wyear = wyear(Date)) %>%
  dplyr::rename(Slot = RiverWareSlot) %>%
  group_by(Wyear, Trace, ScenarioSet, Strategy, Slot) %>%
  dplyr::summarise(Value = sum(Value))

datMeasAgg = datMeasAgg %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasAgg2 = datMeasAgg %>%
  group_by(Measure, Scenario, Period, Strategy, Wyear) %>%
  summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
  ungroup()

datMeasAvg = datMeasAgg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeasAvgHist = datMeasAvg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeasAvgFut = datMeasAvg %>%
  left_join(datMeasAvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeasAvgFutFl = datMeasAvgFut %>%
  filter(Period %in% c('2050s', 'Historical') | is.na(Period))

datMeasAvgFutFl$Scenario = factor(datMeasAvgFutFl$Scenario, levels = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'MID', 'LDP', 'MIP', 'LPP')))
datMeasAvgFutFl = datMeasAvgFutFl %>% mutate(ValueColScle = ValueChange * -1)

datMeasPlot = datMeasAvgFutFl

datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab, levels = c('Baseline', 'Increase Irrigation Efficiency'))


ggplot(data = datMeasPlot, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = round(ValueChange))) +
  geom_tile(colour = 'black') +
  # geom_text(size = 3) +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left") +
  scale_fill_gradientn(colors = brewer.pal(9, 'RdBu'), limits = c(-175, 175)) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0), position="top") +
  scale_y_discrete(expand=c(0,0), position="right") +
  theme(axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank(),
    strip.background = element_blank()) +
    coord_equal()

ggsave(paste0(dirOup, 'IncIrrigEfficISFGrid.png'), height = 10, width = 8)
write.csv(datMeasPlot, paste0(dirOup, 'IncIrrigEfficISFGrid.csv'), row.names = F, quote = F)
