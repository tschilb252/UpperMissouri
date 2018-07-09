#################################################
#' @title Upper Missouri Basin Study - Increase
#' Irrigation Efficiency Figures
#' @author Dan Broman
#' @description Summary figures for the Upper Missouri
#' Basin Study, Increased Irrigation Efficiency Strategy
#' Last Modified June 14 2018
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
StgyTbl = fread('lib/StrategyTableIncIrrigEffic.csv')
MeasTbl = fread('lib/MeasureTableIncIrrigEfficThreeForksShort.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Depletion shortages
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
  # summarise data to reduce the size
  datTmpDTAgg = datTmpDT %>%
    mutate(Wyear = wyear(Date)) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    group_by(Wyear, Trace, ScenarioSet, Strategy, Slot) %>%
    dplyr::summarise(Value = sum(Value))
  datMeas = bind_rows(datMeas, datTmpDTAgg)
}


datMeasAgg = datMeas %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasAgg$Measure = 'Irrigators Above Three Forks'

datMeasAgg2 = datMeasAgg %>%
  group_by(Measure, Scenario, Period, Strategy, Wyear) %>%
  summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
  ungroup()

datMeasPlot = datMeasAgg2 %>% filter(Scenario == 'Historical', Wyear >= 1985, Wyear <= 1990)
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

#datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

ggplot(data = datMeasPlot) +
  geom_line(aes(x = Wyear, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = Wyear, y = Value, colour = StrategyLab, shape = StrategyLab)) +
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
ggsave(paste0(dirOup, 'IncIrrigEfficDryYearsShortage_V2.png'), height = 8, width = 10)


# Clark Canyon allocation fraction (shortages)
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

datMeas2Agg = datMeas2 %>%
  mutate(Wyear = wyear(Date)) %>%
  dplyr::rename(Slot = RiverWareSlot) %>%
  dplyr::filter(!is.na(Value))

datMeas2Agg = datMeas2Agg %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))


datMeas2Avg = datMeas2Agg %>%
  group_by(Measure,Scenario, Period, Strategy, Wyear) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()


datMeasPlot = datMeas2Avg %>% filter(Scenario == 'Historical', Wyear >= 1985, Wyear <= 1995)
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

datMeasPlot$Measure = "Clark Canyon Allocation Fraction"

ggplot(data = datMeasPlot) +
  geom_line(aes(x = Wyear, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = Wyear, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, scales = 'free', ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491')) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = paste0(c(0, 25, 50, 75, 100), '%')) +
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
  ggsave(paste0(dirOup, 'IncIrrigEfficDryYearsCCAlocFrac.png'), height = 8, width = 10)






# Combine measures and plot
datMeasPlot = bind_rows(datMeasAvgFut, datMeas2AvgFut, datMeas3AvgFut, datMeas4AvgFut, datMeas5AvgFut)
datMeasPlot$Scenario = factor(datMeasPlot$Scenario,
  levels = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
    'MID', 'LDP', 'MIP', 'LPP')))
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

# Plot defs
pctLow = 5
pctHigh = 100
colPal = c('#DA4325', '#ECA14E', '#F4F3EB', '#5CC3AF', '#0A6265')

datMeasPlot = datMeasPlot %>%
  mutate(ValueTxt = ifelse(abs(ValueColScle ) > pctHigh, '•', '')) %>%
  mutate(ValueColScle = ifelse(abs(ValueColScle) < pctLow, 0,
  ifelse(ValueColScle > pctHigh, pctHigh,
    ifelse(ValueColScle < -1 * pctHigh, -1 * pctHigh, ValueColScle))))

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

ggsave(paste0(dirOup, 'IncIrrigEfficGrid.png'), height = 10, width = 8)
write.csv(datMeasPlot, paste0(dirOup, 'IncIrrigEfficGrid.csv'),
  row.names = F, quote = F)
