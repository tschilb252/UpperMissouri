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
dirInp = 'Z:/DO/Team/WaterResources/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'C:/Users/MMcguire/Documents/GitHub/UpperMissouri/AdaptationStrategies/Figures/'

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableSunPaleoResampled_Pishkun.csv')
MeasTbl = fread('lib/MeasureTableSun_PishkunAdd.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')

dateStrt = as.Date('1986-10-01')
dateEnd = as.Date('1995-09-30')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

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

datMeasAgg = datMeas %>%
    mutate(Value = Value * 1.98347) %>%     # convert cfs to ac-ft
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    group_by(Trace, Measure, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
    ungroup() %>%
	left_join(StgyTbl)

datMeasAggDist = datMeasAgg %>%
  filter(Measure == 'GID Shortages')


test = datMeasAggDist %>% group_by(Trace) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  arrange(Value)


test2 = datMeasAggDist %>% filter(Trace == 68)

ggplot() +
  geom_line(data = test2, aes(x = WYear, y = Value, colour = factor(Trace)))


datMeasAggDist$StrategyLab = factor(datMeasAggDist$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot = datMeasAggDist %>%
  filter(Trace == 68)

ggplot(data = datMeasPlot) +
  # geom_ribbon(aes(x = Year, ymin = 0.75, ymax = 0.95), fill = '#FDEE91', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0.5, ymax = 0.75), fill = '#FCCA8B', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0, ymax = 0.5), fill = '#DC8C6B', alpha = 0.3) +
#  geom_hline(yintercept = c(0.77, 0.5, 0.25), alpha = 0.6, linetype = 3, size = 0.2) +
  geom_line(aes(x = WYear, y = Value, colour = StrategyLab)) +
  geom_point(aes(x = WYear, y = Value, colour = StrategyLab, shape = StrategyLab)) +
  facet_wrap(~Measure, ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491', '#7A5BA9')) +
#  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
#    labels = paste0(c(0, 25, 50, 75, 100), '%')) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  scale_y_continuous(labels = comma) +
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
ggsave(paste0(dirOup, 'Sun_GIDshortages_PishkunAdd_PaleoResq_V2.png'), height = 4, width = 6.5, dpi=350)
