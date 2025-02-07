#################################################
#' @title Upper Missouri Basin Study - Beaverhead
#'        Basin Figures
#' @author Dan Broman
#' @description Figures for the Upper Missouri
#' Basin Study, Beaverhead River Basin
#' In-Stream Flows
#' Last Modified July 11 2018
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
StgyTbl = fread('lib/StrategyTableBeaverheadPaleoResampled.csv')
MeasTbl = fread('lib/MeasureTableBeaverhead.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')

dateStrt = as.Date('1955-10-01')
dateEnd = as.Date('1968-09-30')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# In-stream flows
fileTmp = fileList[4]
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
  mutate(Wyear = wyear(Date), Month = month(Date)) %>%
  dplyr::rename(Slot = RiverWareSlot) %>%
  filter(Month %in% c(10:12, 1:4)) %>%
  group_by(Wyear, Trace, ScenarioSet, Scenario, Period, Strategy, Slot) %>%
  dplyr::summarise(Value = mean(Value, na.rm = T)) %>%
  left_join(MeasTbl) %>%
  left_join(StgyTbl)

datMeasAgg$StrategyLab = factor(datMeasAgg$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot = datMeasAgg %>%
  dplyr::rename(Year = Wyear) %>%
  filter(Trace == 139, Year >= year(dateStrt), Year <= year(dateEnd))

datMeasPlot = datMeasPlot %>%
  mutate(Measure =
    ifelse(Measure == 'Lower Beaverhead August Low Flows', 'Beaverhead nr Twin Bridges Winter Flows',
    ifelse(Measure == 'Upper Beaverhead August Flows', 'Beaverhead at Barretts Winter Flows', Measure)))

ggplot(data = datMeasPlot) +
  geom_line(aes(x = Year, y = Value, colour = StrategyLab, linetype = StrategyLab), alpha = 0.8, size = 1) +
  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab), size = 2, alpha = 0.8) +
  facet_wrap(~Measure, ncol = 1) +
  scale_colour_manual(values = c('black', '#B0170F', '#24449B', '#119B8B')) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('Mean Winter Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    legend.text=element_text(size = 10),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

ggsave(paste0(dirOup, 'BeaverheadFlowsLongestDrought.png'), height = 8, width = 15)
