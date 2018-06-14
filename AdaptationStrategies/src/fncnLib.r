#############################################################
# Impacts Assessment / Basin Studies Measures Calculations
#############################################################

require(data.table)
require(lubridate)
require(tools)
require(zoo)
require(RWDataPlyr)
require(gridExtra)
require(grid)
require(gtable)
library(scales)
library(cowplot)
library(tidyverse)

read_rwcsv = function(file, dat_name_list){
	require(data.table)
	dat = fread(file)
	dat = dat %>% setNames(dat_name_list) %>% mutate(Time = as.POSIXct(Time, format = '%m-%d-%Y %H:%M:%S')) %>% mutate(Date = as.Date(floor_date(Time, unit = 'day')))
	return(dat)
}

Rdf2dt = function(rdf_ls, name_list = NULL) {
	require(data.table)
	require(RWDataPlyr)
	if(is.null(name_list) == T){
		name_list = rdf_slot_names(rdf_ls)
	}
	nslots = length(name_list)
	rdf_dt = data.table()
	date_raw = rdf_get_timespan(rdf_ls)
	date_start = as.Date(date_raw[1])
	date_end = as.Date(date_raw[2])
	date_vec = seq(from = date_start, to = date_end, by = 'day')
	for(i in 1:nslots){
		name_sel = name_list[i]
		rdf_temp = data.table(rdf_get_slot(rdf_ls, name_sel)) %>% mutate(Date = date_vec) %>% gather(Trace, Value, -Date) %>% mutate(Trace = as.numeric(substring(Trace, 2)), RiverWareSlot = name_sel) %>% dplyr::select(Trace, RiverWareSlot, Date, Value)
		rdf_dt = bind_rows(rdf_dt, rdf_temp)
	}
	return(rdf_dt)
}

wyear = function(x, start_month=10) {
  x.wyr = ifelse(lubridate::month(x)>=start_month, lubridate::year(x)+1, lubridate::year(x))
  return(x.wyr)
}

dowy = function(date){
	# Returns day-of-water-year (day-of-year with Oct. 1 as base)
	# date - [Date] - vector of dates
	# date_dowy - [day-of-water-year] - vector of day-of-water-year
	date_dt = data.table(date = date) %>% mutate(wyear = wyear(date), month = month(date), doy = as.numeric(format(date, '%j'))) %>% mutate(dowy = ifelse(month >= 10, doy - (as.numeric(format(as.Date(paste0((wyear - 1), '-10-01')), '%j'))) + 1, doy + 92))
	date_dowy = date_dt$dowy
	return(date_dowy)
}

doy = function(date){
	# Returns day-of-year
	# date - [Date] - vector of dates
	# date_doy - [day-of-year] - vector of day-of-year
	date_doy = as.numeric(format(date, '%j'))
	return(date_doy)
}

calc_flowcentroid = function(flow, date){
	# Returns flow centroid of timing using approach described in
	# Stewart et al. (2004) Changes in Snowmelt Runoff Timing
	# flow - [cfs] - timeseries of flows
	# date - [Date] - vector of dates corresponding to flow timeseries
	# ct - [day-of-water-year] - vector of day-of-water-year
	flowcentroid_dt = data.table(flow = flow, date = date) %>% mutate(flow_af = flow * 1.9835, wyear = wyear(date), dowy = dowy(date)) %>% mutate(tq = dowy * flow_af) %>% group_by(wyear) %>% summarise(tq_sum = sum(tq), q_sum = sum(flow_af)) %>% mutate(ct = round(tq_sum / q_sum))
	ct = flowcentroid_dt$ct
	return(ct)
}

calc_storageday = function(storage, date, month_sel = NULL, day_sel = NULL, doy_sel = NULL, dowy_sel = NULL){
	# Returns storage for a selected day
	# storage - [volume or elevation; any units] - storage timeseries
	# date - [Date] - vector of dates corresponding to flow timeseries
	# year_sel - year
	# month_sel - month
	# day_sel - day
	# doy_sel - day of year
	# dowy_sel - day of water year
	# NOTE : month and day OR doy OR dowy must be provided
	# storage_sel - [volume or elevation; same units as storage] - storage timeseries from selected days
	# sets format of day to use
	m_monthday = 0
	m_doy = 0
	m_dowy = 0
	if(is.null(month_sel) == F & is.null(day_sel) == F){
		m_monthday = 1
	}
	if(is.null(doy_sel) == F){
		m_doy = 1
	}
	if(is.null(dowy_sel) == F){
		m_dowy = 1
	}

	# error checking
	if(m_monthday + m_doy + m_dowy > 1){
		stop('Too many day inputs provided')
	}
	if(m_monthday + m_doy + m_dowy == 0){
		stop('Day inputs not provided')
	}

	storage_dt = data.table(storage = storage, date = date) %>% mutate(year = wyear(date), month = month(date), day = day(date), doy = doy(date), dowy = dowy(date))

	if(m_monthday == 1){
		storage_dt = storage_dt %>% filter(month == month_sel & day == day_sel)
		storage_sel = storage_dt$storage
	}

	if(m_doy == 1){
		storage_dt = storage_dt %>% filter(doy == doy_sel)
		storage_sel = storage_dt$storage
	}

	if(m_dowy == 1){
		storage_dt = storage_dt %>% filter(dowy == dowy_sel)
		storage_sel = storage_dt$storage
	}
	return(storage_sel)
}

calc_storagefilllevel = function(storage, date, level_sel){
	# Returns day-of-water-year storage exceeds a selected level (volume or elevation)
	# storage - [volume or elevation; any units] - storage timeseries
	# date - [Date] - vector of dates corresponding to flow timeseries
	# level_sel - [volume or elevation; any units; needs to match storage] - storage level
	# storagedowy_sel - [day-of-water-year] - day-of-water-year for each water year exceeding level_sel
	storage_dt = data.table(storage = storage, date = date) %>% mutate(year = wyear(date), month = month(date), day = day(date), doy = doy(date), dowy = dowy(date)) %>% filter(storage >= level_sel) %>% group_by(year) %>% slice(which.min(dowy))
	storagedowy_sel = storage_dt$dowy
	return(storagedowy_sel)
}

calc_instreamflowexceed = function(flow, date, flow_sel){
	# Returns flow exceedance over selected threshold
	# flow - [cfs] - timeseries of flows
	# date - [Date] - vector of dates corresponding to flow timeseries
	flow_dt = data.table(flow = flow, date = date) %>% mutate(year = wyear(date)) %>% mutate(flow_exceed = ifelse(flow >= flow_sel, 1, 0)) %>% group_by(year) %>% summarise(flow_exceed = sum(flow_exceed), ndays = n()) %>% mutate(flow_exceed_pct = flow_exceed / ndays)
	flow_exceed = flow_dt$flow_exceed_pct
	return(flow_exceed)
}

# boat ramp elev nonexceedance

calc_hydropowersum = function(hydropower, date){
	# hydropower - [power] - timeseries of hydropower
	# date - [Date] - vector of dates corresponding to hydropower timeseries
	hydropower_dt = data.table(hydropower = hydropower, date = date) %>% mutate(year = wyear(date)) %>% group_by(year) %>% summarise(hydropower = sum(hydropower))
	hydropower_sum = hydropower_dt$hydropower
	return(hydropower_sum)
}

g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)
}

TostonEnergy = function(Flow){
  # Calculates power production at DNRC's Broadwater
  # facility (Toston)
  # Power production (in MW) is related to streanflow (in cfs)
  # Energy (in MWh) is calculated from power

  # Flow 1110 to 6800 power coefficients
  Clo1 = -0.000102927678765074
  Clo2 = 2.23638033061626
  Clo3 = -855.659845886428
# Flow 6800 to 24230 power coefficients
  Chi1 = -5.99619692894544e-06
  Chi2 = -0.278548746085341
  Chi3 = 11770.5396744282
	TostonPower = function(Flow){
	  if(Flow < 1110 | Flow >= 24230){
	    Power = 0
	  } else if(Flow >= 1110 & Flow < 6800){
	    Power = Clo1*Flow^2 + Clo2*Flow + Clo3
	  } else if(Flow >= 6800 & Flow < 24230){
	    Power = Chi1*Flow^2 + Chi2*Flow + Chi3
	  }
		return(Power)
	}
	Power = unlist(lapply(Flow, TostonPower))
  Energy = Power / 24
  return(Energy)
}

unlist(lapply(1:10, TostonEnergy))
