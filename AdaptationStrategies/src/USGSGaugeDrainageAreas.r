#################################################
#' @title Upper Missouri Basin Study - Beaverhead
#'        Basin Figures
#' @author Dan Broman
#' @description Figures for the Upper Missouri
#' Basin Study, Beaverhead River Basin
#' Clark Canyon Water Users Allocation Fraction
#' Last Modified June 11 2018
#################################################
library(tidyverse)
library(data.table)
setwd('C:/Projects/git/UpperMissouri/AdaptationStrategies/')
source('src/fncnLib.r')

#################################################
#' User Inputs

gaugeMeta = fread('USGSGaugesMetadata.txt')
gaugeTbl = fread('lib/USGSGaugeTable.csv')

gaugeList = gaugeTbl$USGS_ID

gaugeAreaTbl = gaugeMeta %>%
  filter(SITE_NO %in% gaugeList) %>%
  dplyr::select(SITE_NO, STATION_NM, DA_SQ_MILE)

write.csv(gaugeAreaTbl, 'USGSGaugeAreaTable.csv', row.names = F, quote = F)
