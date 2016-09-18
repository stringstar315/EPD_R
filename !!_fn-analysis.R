##############################################################################################################
####################                    List of Functions for Analysis                    ####################
##############################################################################################################



##   Function  1. RoadvGen.Ox_SQL = function
##   Function  2. Analysis.EpisodeSummary = function(YearStart = 2014, mainDB = 'db_as3')
##   Function  3. 
##   Function  4. 
##   Function  5.
##   Function  6.
##   Function  7.
##   Function  8.
##   Function  9.
##   Function 10.
##   Function 11.
##   Function 12.






##############################################################################################################
####################                    Function 1. RoadvGen.Ox_GenSQL                    ####################
##############################################################################################################
##############################################################################################################

RoadvGen.Ox_GenSQL = function() {
  
  pollutant.pick = c("o3_ug", "no2_ug", "nox_ug") # pollutants to pick
  airYear = 2010:2016 # year to be looked at
  air.hr = LoadAirHour(yearVec = airYear, pollutant.pick = pollutant.pick)
  rollHours = 8760 #hours of rolling
  
  source('!!_parameters.R')
  air.hr = air.hr %>% mutate(Ox = o3_ug/p_O3 + no2_ug/p_NO2)
  
  air.hr$siteType = "General"
  air.hr$siteType[which(air.hr$site == 'MB')] = NA
  air.hr$siteType[which(air.hr$site == 'CB')] = 'Roadside'
  air.hr$siteType[which(air.hr$site == 'CL')] = 'Roadside'
  air.hr$siteType[which(air.hr$site == 'MK')] = 'Roadside'
  
  air.hr.Cast = cast(air.hr, formula = date~siteType, fun.aggregate = function(x) mean(x, na.rm = T), value = 'Ox')
  
  a1 = air.hr.Cast %>% mutate(diff.Ox = Roadside - General) %>% select(date, General, Roadside, diff.Ox)
  
  airhr.Group.Rolling = rollingMean(a1, pollutant = "diff.Ox", 
      align = "right", width = rollHours, data.thresh = 66.66, type = 'siteType')
  airhr.Group.Rolling = rollingMean(airhr.Group.Rolling, pollutant = "General", 
      align = "right", width = rollHours, data.thresh = 66.66, type = 'siteType')
  airhr.Group.Rolling = rollingMean(airhr.Group.Rolling, pollutant = "Roadside", 
      align = "right", width = rollHours, data.thresh = 66.66, type = 'siteType')
  
  airhr.Group.Rolling = airhr.Group.Rolling[c(1,5,6,7)]
  
  airhr.Group.Rolling = selectByDate(airhr.Group.Rolling, year = 2012:2019)
  names(airhr.Group.Rolling) = c('date', 'diffroll', 'roadroll', 'genroll')
  
  ## Setup DB connection
  library(DBI)
  library(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="ckk%i9==", dbname="airdata", host="localhost")
  
  myQuery = 'DROP TABLE IF EXISTS ox8760;'
  dbSendQuery(con, myQuery)

  dbWriteTable(con, name = 'ox8760', airhr.Group.Rolling, row.names = F, overwrite = T,
               field.types=list(date="datetime", diffroll="float(5,1)", roadroll="float(5,1)", genroll="float(5,1)"))
  
  
}


##############################################################################################################
####################                 Function 2. Analysis.EpisodeSummary                  ####################
##############################################################################################################
##############################################################################################################

Analysis.EpisodeSummary = function(YearStart = 2014, mainDB = 'db_as3'){
  ### Acquire AQHI from FULL-List
  
  
  library(DBI)
  library(RMySQL)

  con_main <- dbConnect(MySQL(), user="user", password="as3", dbname = mainDB, host="localhost")
  
  ### 1. Hourly Counts
  myQuery = paste(
    'SELECT * FROM aqhi_hr_fulllist
     WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
  aqhi.hr = dbGetQuery(con_main, myQuery)
  
  aqhi.hr.cast = cast(aqhi.hr, date~site, fun.aggregate = 'mean', value = 'AQHI')
  aqhi.hr.cast = aqhi.hr.cast[c('date', RoadsideCluster, UrbanCluster, WesternCluster, EasternCluster)]
 
  seriousSite.OverALL = apply(aqhi.hr.cast %>% select(-date), 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)})
  vHighSite.OverALL = apply(aqhi.hr.cast %>% select(-date), 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.OverALL = apply(aqhi.hr.cast %>% select(-date), 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  seriousSite.Roadside = apply(aqhi.hr.cast[RoadsideCluster], 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)}) 
  vHighSite.Roadside = apply(aqhi.hr.cast[RoadsideCluster], 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.Roadside = apply(aqhi.hr.cast[RoadsideCluster], 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  seriousSite.Gen = apply(aqhi.hr.cast[GeneralCluster], 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)})
  vHighSite.Gen = apply(aqhi.hr.cast[GeneralCluster], 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.Gen = apply(aqhi.hr.cast[GeneralCluster], 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  episode.hrCount = cbind.data.frame(aqhi.hr.cast$date, seriousSite.OverALL, vHighSite.OverALL, highSite.OverALL,
                                     seriousSite.Roadside, vHighSite.Roadside, highSite.Roadside,
                                     seriousSite.Gen, vHighSite.Gen, highSite.Gen)
  names(episode.hrCount) = c('date', 'over_Ser', 'over_vHigh', 'over_High', 
                             'road_Ser', 'road_vHigh', 'road_High', 
                             'gene_Ser', 'gene_vHigh', 'gene_High')
  
  episode.hrCount.melt = as.data.frame(melt(episode.hrCount, id = 'date'))
  episode.hrCount.melt$AQHI = substr(episode.hrCount.melt$variable, 6, 15)
  episode.hrCount.melt$siteType = substr(episode.hrCount.melt$variable, 1, 4)
  episode.hrCount.melt = episode.hrCount.melt[c('date', 'siteType', 'AQHI', 'value')]
  
  myQuery = 'DROP TABLE IF EXISTS a_episode_hr_count;'
  dbSendQuery(con_main, myQuery)
  
  dbWriteTable(con_main, name = 'a_episode_hr_count', episode.hrCount.melt, row.names = F, overwrite = T)
  myQuery = 'ALTER TABLE a_episode_hr_count MODIFY date datetime NOT NULL; '
  dbSendQuery(con_main, myQuery)
  
  print(paste('Analysis.EpisodeSummary: DB updated -- \'a_episode_hr_count\'', sep = ''))
  
  ## *********
  
  
  ### 1. Daily Counts
  
  myQuery = paste(
    'SELECT * FROM aqhi_day_fullcast
     WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
  aqhi.dayMax = dbGetQuery(con_main, myQuery)
  aqhi.dayMax = aqhi.dayMax[c('date', RoadsideCluster, UrbanCluster, WesternCluster, EasternCluster)]
  
  seriousSite.OverALL = apply(aqhi.dayMax %>% select(-date), 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)})
  vHighSite.OverALL = apply(aqhi.dayMax %>% select(-date), 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.OverALL = apply(aqhi.dayMax %>% select(-date), 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  seriousSite.Roadside = apply(aqhi.dayMax[RoadsideCluster], 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)}) 
  vHighSite.Roadside = apply(aqhi.dayMax[RoadsideCluster], 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.Roadside = apply(aqhi.dayMax[RoadsideCluster], 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  seriousSite.Gen = apply(aqhi.dayMax[GeneralCluster], 1, 
    function(x) if(sum(x==11, na.rm = T)>=1){return (1)} else {return (NA)})
  vHighSite.Gen = apply(aqhi.dayMax[GeneralCluster], 1, 
    function(x) if(sum(x>=8, na.rm = T)>=1){return (1)} else {return (NA)})
  highSite.Gen = apply(aqhi.dayMax[GeneralCluster], 1, 
    function(x) if(sum(x>=7, na.rm = T)>=3){return (1)} else {return (NA)})
  
  episode.dayCount = cbind.data.frame(aqhi.dayMax$date, seriousSite.OverALL, vHighSite.OverALL, highSite.OverALL,
                                     seriousSite.Roadside, vHighSite.Roadside, highSite.Roadside,
                                     seriousSite.Gen, vHighSite.Gen, highSite.Gen)
  names(episode.dayCount) = c('date', 'over_Ser', 'over_vHigh', 'over_High', 
                             'road_Ser', 'road_vHigh', 'road_High', 
                             'gene_Ser', 'gene_vHigh', 'gene_High')
  
  episode.dayCount.melt = as.data.frame(melt(episode.dayCount, id = 'date'))
  episode.dayCount.melt$AQHI = substr(episode.dayCount.melt$variable, 6, 10)
  episode.dayCount.melt$siteType = substr(episode.dayCount.melt$variable, 1, 4)
  episode.dayCount.melt = episode.dayCount.melt[c('date', 'siteType', 'AQHI', 'value')]
  
  myQuery = 'DROP TABLE IF EXISTS a_episode_day_count;'
  dbSendQuery(con_main, myQuery)
  
  dbWriteTable(con_main, name = 'a_episode_day_count', episode.dayCount.melt, row.names = F, overwrite = T)
  myQuery = 'ALTER TABLE a_episode_day_count MODIFY date date NOT NULL; '
  dbSendQuery(con_main, myQuery)

  print(paste('Analysis.EpisodeSummary: DB updated -- \'a_episode_day_count\'', sep = ''))
  
  dbDisconnect(con_main)
  
}



##############################################################################################################
####################      Function 3. FULL Episode Conditions - Conc. AQHI Met. Etc.      ####################
##############################################################################################################
##############################################################################################################


Analysis.EpisodeFULL = function(YearStart = 2014, mainDB = 'db_as3'){
  
  library(DBI)
  library(RMySQL)

  con_main <- dbConnect(MySQL(), user="user", password="as3", dbname = mainDB, host="localhost")
  
  myQuery = paste(
    'SELECT * FROM aqhi_day_fullcast
     WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
  df1 = dbGetQuery(con_main, myQuery)
  
  myQuery = paste(
    'SELECT * FROM air_hkdaymax_group
     WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
  df2 = dbGetQuery(con_main, myQuery)
  
  myQuery = paste(
    'SELECT * FROM air_prddaymax_group
     WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
  df3 = dbGetQuery(con_main, myQuery)
  
  df = merge.data.frame(df1, df2, by = 'date', all.x = T)
  df = merge.data.frame(df, df3, by = 'date', all.x = T)
  
  myQuery = 'DROP TABLE IF EXISTS a_episode_FULL;'
  dbSendQuery(con_main, myQuery)
  
  dbWriteTable(con_main, name = 'a_episode_FULL', df, row.names = F, overwrite = T)
  myQuery = 'ALTER TABLE a_episode_FULL MODIFY date date NOT NULL; '
  dbSendQuery(con_main, myQuery)
  
  print(paste('Analysis.EpisodeFULL: DB updated -- \'a_episode_FULL\'', sep = ''))
  
}



##############################################################################################################
####################                   Function 4. AQHI cum Pollutants                    ####################
##############################################################################################################
##############################################################################################################


episode.DayMax = function(yearVec = 2014:2016) {
    
    pollutant.pick = c('o3_ug', 'no2_ug', 'rsp', 'nox_ug','AQHI')
    air.hr = LoadAirHour(yearVec = yearVec, pollutant.pick = pollutant.pick)
    source('!!_parameters.R')
    air.hr = air.hr %>% mutate(ox = o3_ug/p_O3 + no2_ug/p_NO2, no2_nox = no2_ug/nox_ug)
    no2_nox = timeAverage(selectByDate(air.hr, hour = 11:14) %>% select(date, site, no2_nox), avg.time = 'day', 
      data.thresh = 0, statistic = 'max', percentile = NA, type = 'site')
    air.DayMax = timeAverage(air.hr, avg.time = 'day', data.thresh = 0, statistic = 'max', type = 'site')
    air.DayMax$no2_nox = NULL 
    air.DayMax = merge.data.frame(air.DayMax, no2_nox, by = c('date', 'site'))
    
    library(DBI)
    library(RMySQL)
    
    con <- dbConnect(MySQL(), user="user", password="as3", dbname='db_as3', host="localhost")
    
    myQuery = 'SELECT * FROM aqhi02_daysummary;'
    aqhi.daySummary = dbGetQuery(con, myQuery)
    
    myQuery = 'SELECT * FROM health01_hour;'
    hRisk.hr = dbGetQuery(con, myQuery) %>% select(date, site, contri_O3, contri_NO2, contri_RSP, AQHI)
    hRisk.hr$date = hRisk.hr$date %>% ymd_hms()
    
    hRisk.VH.Max = as.data.frame(timeAverage(hRisk.hr %>% filter(AQHI >= 8), avg.time = 'day', 
                                             data.thresh = 0, statistic = 'max', type = 'site'))
    hRisk.H.Max = as.data.frame(timeAverage(hRisk.hr %>% filter(AQHI >= 7), avg.time = 'day', 
                                            data.thresh = 0, statistic = 'max', type = 'site'))
    names(hRisk.VH.Max) = c('site', 'date', 'contri_O3_VH', 'contri_NO2_VH', 'contri_RSP_VH', 'AQHI_VH')
    names(hRisk.H.Max) = c('site', 'date', 'contri_O3_High', 'contri_NO2_High', 'contri_RSP_High', 'AQHI_High')
    
    
    air.DayMax$date = air.DayMax$date %>% ymd()
    aqhi.daySummary$date = aqhi.daySummary$date %>% ymd()
    hRisk.VH.Max$date = hRisk.VH.Max$date %>% ymd()
    hRisk.H.Max$date = hRisk.H.Max$date %>% ymd()
    
    merge1 = merge.data.frame(air.DayMax, aqhi.daySummary, by = c('date', 'site'), all.x = T, all.y = F)
    merge2 = merge.data.frame(merge1, hRisk.VH.Max, by = c('date', 'site'), all.x = T, all.y = F)
    merge3 = merge.data.frame(merge2, hRisk.H.Max, by = c('date', 'site'), all.x = T, all.y = F)
    
    
    ## 1. Write to DB
    myQuery = 'DROP TABLE IF EXISTS a_concxaqhi;'
    dbSendQuery(con, myQuery)
    dbWriteTable(con, name = 'a_concxaqhi', merge3, row.names = F, overwrite = T)
    myQuery = 'ALTER TABLE a_concxaqhi MODIFY date date NOT NULL; '
    dbSendQuery(con, myQuery)
    
    print('ConcAQHI.DayMax:  \'a_concxaqhi\' MySQL updated')
    

} ## END function




##############################################################################################################
####################                   Function 5. Met during Episode                    ####################
##############################################################################################################


MetxAQHI = function(startYear = 2014){
  
  library(DBI)
  library(RMySQL)

  con <- dbConnect(MySQL(), user="user", password="as3", dbname = 'db_as3', host="localhost")

  myQuery = paste('SELECT * FROM aqhi_hr_melt_full WHERE date >= \'', 
                  startYear, '-01-01 00:00:00\' ' , sep = '')
  aqhi.hr = dbGetQuery(con, myQuery)
  
  myQuery = paste('SELECT * FROM met_hr_full WHERE date >= \'', 
                  startYear, '-01-01 00:00:00\' ' , sep = '')
  met.hr = dbGetQuery(con, myQuery)
  
  aqhi.hr.Cast = cast(aqhi.hr, date~site, value = 'AQHI')
    
  seriousSite = apply(aqhi.hr.Cast %>% select(-date), 1, function(x) if(sum(x==11, na.rm = T)>=1){return(1)} else{return(0)}) 
  vHighSite = apply(aqhi.hr.Cast %>% select(-date), 1, function(x) if(sum(x>=8, na.rm = T)>=1){return(1)} else{return(0)})
  highSite = apply(aqhi.hr.Cast %>% select(-date), 1, function(x) if(sum(x>=7, na.rm = T)>=3){return(1)} else{return(0)})
    
  aqhi.cast.ALL = cbind.data.frame(aqhi.hr.Cast, seriousSite, vHighSite, highSite)[c('date', 'seriousSite', 'vHighSite', 'highSite')]

  metxAQHI = merge.data.frame(met.hr, aqhi.cast.ALL, by = 'date')
 
  
  
}

































