#################################################
#' @title Upper Missouri Basin Study - DNRC
#' Figures
#' @author Dan Broman
#' @description Upper Missouri Basin Study
#' Figures from DNRC
#' Last Modified June 26 2018
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
dirInp = 'data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

#################################################
# Holter Hydropower
HdroClimHolt = fread(paste0(dirInp, 'HolterHydroClim.csv'))
HdroClimHolt = HdroClimHolt %>%
  mutate(Date = as.Date(Date))
HdroCapHolt = 7100 # Holter hydropower capacity in MwH

ggplot(data = HdroClimHolt) +
  geom_ribbon(aes(x = Date, ymin = Value10, ymax = Value90),
    fill = '#A3ABD3', color = '#25499F', alpha = 0.4, size = 1) +
  geom_line(aes(x = Date, y = Value50), color = '#152C5F', size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  geom_hline(yintercept = HdroCapHolt, color = '#B70B01')  +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="none",
  ) +
  ggtitle('Missouri River below Holter Dam \n
    Normal Range of Flow')
ggsave(paste0(dirOup, 'HolterHydro.png'), height = 8, width = 10)

#################################################
# Irrigated Acres by System Type
IrrigSystDat = fread(paste0(dirInp, 'IrrigSyst.csv'))

ggplot(data = IrrigSystDat) +
  geom_line(aes(x = Year, y = Acres, color = `System Type`, linetype = `System Type`), alpha = 0.8, size = 1) +
  geom_point(aes(x = Year, y = Acres, color = `System Type`, shape = `System Type`), size = 2, alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = unique(IrrigSystDat$Year), labels = unique(IrrigSystDat$Year)) +
  scale_color_manual(values = c('#B0170F', '#24449B', '#119B8B')) +
  xlab('') +
  ylab('Acres') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.border=element_blank(),
    ) +
  ggtitle('Montana Irrigated Acres by System Type')
ggsave(paste0(dirOup, 'IrrigSyst.png'), height = 8, width = 10)

#################################################
# Canyon Ferry and Tiber Releases
RlseDat = fread(paste0(dirInp, 'CFTiberReleases.csv'))
RlseDat = RlseDat %>%
  mutate(Date = as.Date(Date))
RlseDat$Site = factor(RlseDat$Site, levels = c('Canyon Ferry Release', 'Tiber Release',
  'Combined Release'))
ggplot(data = RlseDat) +
  geom_line(aes(x = Date, y = Flow, color = Site, linetype = Site), size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  scale_color_manual(values = c('#24449B', '#B0170F', 'black')) +
  scale_linetype_manual(values = c(2,3,1)) +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    axis.ticks=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  )
ggsave(paste0(dirOup, 'CFTiberRelease.png'), height = 8, width = 10)

#################################################
# Canyon Ferry and Tiber Releases (Eco Flow Strategy)
RlseEcoDat = fread(paste0(dirInp, 'CFTiberReleasesEcoFlow.csv'))
RlseEcoDat = RlseEcoDat %>%
  mutate(Date = as.Date(Date))
RlseEcoDat$Site = factor(RlseEcoDat$Site, levels = c('Canyon Ferry Release', 'Tiber Release',
  'Combined Release', 'Missouri River at Landusky'))
ggplot(data = RlseEcoDat) +
  geom_line(aes(x = Date, y = Flow, color = Site, linetype = Site, size = Site)) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  scale_color_manual(values = c('#24449B', '#B0170F', 'black', '#152C5F')) +
  scale_linetype_manual(values = c(2,3,1,1)) +
  scale_size_manual(values = c(1,1,1,1.5)) +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
  )
ggsave(paste0(dirOup, 'CFTiberReleaseEcoFlow.png'), height = 8, width = 10)

#################################################
# Gallatin ISF
HdroClimGall = fread(paste0(dirInp, 'GallatinFlows.csv'))
HdroClimGall = HdroClimGall %>%
  mutate(Date = as.Date(Date))

ggplot(data = HdroClimGall) +
  geom_ribbon(aes(x = Date, ymin = Value10, ymax = Value90),
    fill = '#A3ABD3', color = '#25499F', alpha = 0.4, size = 1) +
  geom_line(aes(x = Date, y = Value50), color = '#152C5F', size = 1) +
  geom_line(aes(x = Date, y = ValueMurphyRight), color = '#B0170F', size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="none",
    panel.background=element_blank(),
  ) +
  ggtitle('Gallatin River at Logan \n
FWP Murphy Claim Compared to Normal Range of Flow')
ggsave(paste0(dirOup, 'GallatinHydro.png'), height = 8, width = 10)

#################################################
# Gallatin Groundwater Recharge
GWRchgDat = fread(paste0(dirInp, 'GallatinGWRecharge.csv'))
GWRchgDat = GWRchgDat %>%
  mutate(Date = as.Date(Date))
GWRchgDat$Label = factor(GWRchgDat$Label, levels = c('Aquifer Recharge',
  'Recharge Return to Surface Water', 'Net Depletions to Surface Water'))
ggplot(data = GWRchgDat) +
  geom_line(aes(x = Date, y = Value, color = Label), size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  scale_color_manual(values = c('#24449B', '#B0170F', 'black')) +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
  ) +
  ggtitle('Modeled Ground Water Pumping with Aquifer Recharge \n
    Depiction of City of Bozeman Component')
ggsave(paste0(dirOup, 'GallatinGWRecharge.png'), height = 8, width = 10)

#################################################
# Gallatin Groundwater Recharge
PplnDat = fread(paste0(dirInp, 'GallatinCFPipelineDiv.csv'))
PplnDat = PplnDat %>%
  mutate(Date = as.Date(Date))
ggplot(data = PplnDat) +
  geom_line(aes(x = Date, y = Value), color = 'black', size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Flow (cfs)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
  ) +
  ggtitle('Sample of Modeled Pipeline Diversion Rates')
ggsave(paste0(dirOup, 'GallatinCFPipeline.png'), height = 8, width = 10)

#################################################
# Horse Creek Reservoir Storage
HrseCkResStg = fread(paste0(dirInp, 'HorseCreekResStorage.csv'))
HrseCkResStg = HrseCkResStg %>%
  mutate(Date = as.Date(Date))
ggplot(data = HrseCkResStg) +
  geom_line(aes(x = Date, y = Value), color = 'black', size = 1) +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Storage (ac-ft)') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
  ) +
  ggtitle('Horse Creek Reservoir Storage')
ggsave(paste0(dirOup, 'HorseCreekResStorage.png'), height = 8, width = 10)

#################################################
# Big Hole ISF
BigHoleISF = fread(paste0(dirInp, 'BigHoleISF.csv'))
BigHoleISF = BigHoleISF %>%
  mutate(Date = as.Date(Date))
BigHoleISF$Label = factor(BigHoleISF$Label, levels = c('Historical Gauged Flow',
  'Modeled Flow: 160/20 CFS Target', 'Modeled Flow: 160/40 CFS Target',
  'Modeled Flow: 160/60 CFS Target'))
ggplot(data = BigHoleISF) +
  geom_line(aes(x = Date, y = Value, color = Label, linetype = Label), size = 1) +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  scale_color_manual(values = c('#B0170F','#A3ABD2','#24449B','#152C5F')) +
  scale_linetype_manual(values = c(1,2,4,1)) +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.text.x=element_text(angle = 0, hjust = 0, vjust = 0.5, size = 12),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 12),
    legend.position="bottom",
    legend.title=element_blank(),
  ) +
  ggtitle('1988 Summer Flow Comparison for Big Hole River at Wisdom')
ggsave(paste0(dirOup, 'BigHoleISF.png'), height = 8, width = 10)
