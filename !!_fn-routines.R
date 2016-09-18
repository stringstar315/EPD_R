##############################################################################################################
####################                List of Functions for Rountine Process                ####################
##############################################################################################################

##   0. View_airhk = function ()
##   1. UpdateDB_AQHI_FULL = function (GenSQL = T, GenCSV = F, file1 = 'aqhi_archive.csv', file2 = 'aqhi_current.csv')
##   2. UpdateDB_Met_FULL = function(GenCIS = T, GenHour = T)
##   3. UpdateDB_airPRD = function()
##   4. UpdateDB_airHK_FULL = function()
##   5. UpdateDB_HealthRisk = function(yearVec = 2014:2016)
##   6.
##   7.
##   8.
##   9. 
##  10. Format_EPIC_ENVISTA = function (fmt_EPIC = T, fmt_EVIS = T, bindFiles = T, bindAQHI = T)
##  11. Format_HKO_hour = function()
##  12. 
##  13. 
##  14.
##  15. View_airhk = function ()
##  16. 
##  17.
##  18.
##  19. 




##  20. GenAAQI = function(date.start = '1/5/2015', date.end = '30/4/2016', pollutant.pick = c("no2_ug", "rsp", 'fsp'))


##  40. MultiFileBind = function(prefix = 'airhour', array.i = 2015:2016, extension = '.csv', skipLine = 0)
##  41. WriteCSV_ToFolder = function(file_path, master_path, varName, fileName, NA.char = '\\N')
##  42. GenHeadingPattern = function(txtInput = 'default')
##  43. DayMax8h.Cal = function(yearVec = cur_Year, cutOFF = 0, data.thresh = 66.66, species = 'o3_ug')







##############################################################################################################
####################                        Function 0. View_airhk                        ####################
##############################################################################################################
##############################################################################################################

View_airhk = function (){
  
  library(DBI)
  library(RMySQL)
  
  con <- dbConnect(MySQL(), user="user", password="as3", dbname = 'db_as3', host="localhost")
  
  myQuery = 'SELECT * FROM air01_hour WHERE date >= \'2016-07-01\';'
  air.hr = dbGetQuery(con, myQuery)
  
  
}








##############################################################################################################
####################                    Function 1. UpdateDB_AQHI_FULL                    ####################
##############################################################################################################
##############################################################################################################

##Usage: Update DB of AQHI -- 1. List of hourly AQHI col: date, site, AQHI
##                         -- 2. List of Daily AQHI summary: date [site array] [Max. Nu. of sites] [Episode Duration]

UpdateDB_AQHI_FULL = function (GenSQL = T, GenCSV = F, file1 = 'aqhi_archive.csv', file2 = 'aqhi_current.csv'){
  
  yearOut = 2013:2100
  setwd(paste(dir_airHK, '/00_formating', sep =''))
  
  r1 = read.csv(file1, na.strings = '\\N')
  r2 = read.csv(file2, na.strings = '\\N')
  bindFile = selectByDate(as.data.frame(rbind.data.frame(r1, r2)), year = yearOut)[c('date', ALLCluster)]
  
  setwd(dir_airHK)
  
  melt.df = as.data.frame(selectByDate(melt(bindFile, id = 'date'), year = yearOut))
  names(melt.df) = c('date', 'site', 'AQHI')
  
  ## FInd Duration of High VHigh Serious
  melt.df$hr = as.numeric(format.Date(melt.df$date, '%H')) + 1
  duration.Ser = apply(melt.df %>% select(AQHI), 1, function(x) if(sum(x==11, na.rm = T)>=1){return(1)} else{return(NA)}) 
  duration.Vhigh = apply(melt.df %>% select(AQHI), 1, function(x) if(sum(x>=8, na.rm = T)>=1){return(1)} else{return(NA)}) 
  duration.High = apply(melt.df %>% select(AQHI), 1, function(x) if(sum(x>=7, na.rm = T)>=1){return(1)} else{return(NA)}) 
  melt.df = cbind.data.frame(melt.df, duration.Ser, duration.Vhigh, duration.High)
  RiskClass.DayDuration = as.data.frame(timeAverage(melt.df %>% select(-AQHI,-hr), avg.time = 'day', type = 'site',
    data.thresh = 0, statistic = 'frequency'))
  
  ## FInd Start Time of High VHigh Serious
  episode.start.VH = as.data.frame(timeAverage(melt.df %>% select(date, site, hr, duration.Vhigh) %>% 
    filter(duration.Vhigh == 1) %>% select(-duration.Vhigh), avg.time = 'day', 
    type = 'site', data.thresh = 0, statistic = 'min'))
  episode.start.High = as.data.frame(timeAverage(melt.df %>% select(date, site, hr, duration.High) %>% 
    filter(duration.High == 1) %>% select(-duration.High), avg.time = 'day', 
    type = 'site', data.thresh = 0, statistic = 'min'))
  episode.start.Ser = as.data.frame(timeAverage(melt.df %>% select(date, site, hr, duration.Ser) %>% 
    filter(duration.Ser == 1) %>% select(-duration.Ser), avg.time = 'day', 
    type = 'site', data.thresh = 0, statistic = 'min'))
  
  ## Create fiiter for High VHigh Serious
  dayMax.Cast = as.data.frame(timeAverage(bindFile, avg.time = 'day', 
    data.thresh = 0, statistic = 'max'))
  
  filter.Serious = apply(dayMax.Cast %>% select(-date), 1, function(x) if(sum(x==11, na.rm = T)>=1){return(1)} else{return(0)}) 
  filter.vHigh = apply(dayMax.Cast %>% select(-date), 1, function(x) if(sum(x>=8, na.rm = T)>=1){return(1)} else{return(0)})
  filter.High = apply(dayMax.Cast %>% select(-date), 1, function(x) if(sum(x>=7, na.rm = T)>=3){return(1)} else{return(0)})
  dayMax.Cast = cbind.data.frame(dayMax.Cast, filter.Serious, filter.vHigh, filter.High)
  episode.dayfilter = melt(dayMax.Cast, id = c('date', 'filter.Serious', 'filter.vHigh', 'filter.High'))
  names(episode.dayfilter) = c('date', 'filter_ser', 'filter_vh', 'filter_high', 'site', 'aqhi_daymax')
  
  ## Merging above
  m1 = merge.data.frame(episode.dayfilter, RiskClass.DayDuration, by = c('date', 'site'), all.x = T)
  m2 = merge.data.frame(m1, episode.start.VH, by = c('date', 'site'), all.x = T)
  m3 = merge.data.frame(m2, episode.start.High, by = c('date', 'site'), all.x = T)
  m4 = merge.data.frame(m3, episode.start.Ser, by = c('date', 'site'), all.x = T)
  names(m4) = c('date', 'site', 'filter_ser', 'filter_vh', 'filter_high', 'aqhi_daymax', 'duration_ser',
                'duration_vh', 'duration_high', 'start_vh', 'start_high', 'start_ser')
  
  if(GenCSV) {
    
    outName = 'aqhi_hrList.csv'
    WriteCSV_ToFolder(dir_airHK, dir_Master, melt.df[1:3], outName, NA.char = '\\N')
    print(paste('UpdateDB_AQHI.FULL: \'', outName, '\' generated', sep =''))
    
    outName2 = 'aqhi_daySummary.csv'
    WriteCSV_ToFolder(dir_airHK, dir_Master, m4, outName2, NA.char = '\\N')
    print(paste('UpdateDB_AQHI.FULL: \'', outName2, '\' generated', sep =''))
    
  }
  
  setwd(dir_Master)
  
  
  ## TO copy the updated aqhi to SQL
  if (GenSQL) {
    
    library(DBI)
    library(RMySQL)
    
    main = 'db_as3'
    con <- dbConnect(MySQL(), user="user", password="as3", dbname=main, host="localhost")
    
    ## 1. Write the FULL Melt List to DB
    
    myQuery = 'DROP TABLE IF EXISTS aqhi01_hrlist;'
    dbSendQuery(con, myQuery)
    dbWriteTable(con, name = 'aqhi01_hrlist', melt.df[1:3], row.names = F, overwrite = T,
                 field.types=list(date="datetime", site='varchar(3)', AQHI='int(3)'))
    
    myQuery = 'ALTER TABLE aqhi01_hrlist MODIFY site varchar(3) NOT NULL; '
    dbSendQuery(con, myQuery)
    myQuery = 'ALTER TABLE aqhi01_hrlist MODIFY date datetime NOT NULL; '
    dbSendQuery(con, myQuery)
    print('UpdateDB_AQHI.FULL:  \'aqhi01_hrlist\' MySQL Generated')
  
    
    #### list of day summary
    myQuery = 'DROP TABLE IF EXISTS aqhi02_daysummary;'
    dbSendQuery(con, myQuery)
    dbWriteTable(con, name = 'aqhi02_daysummary', m4, row.names = F, overwrite = T)
    
    myQuery = 'ALTER TABLE aqhi02_daysummary MODIFY date date NOT NULL; '
    dbSendQuery(con, myQuery)

    print('UpdateDB_AQHI.FULL:  \'aqhi02_daysummary with stats\' MySQL updated')
    
    
    #### cast dayMAx
    myQuery = 'DROP TABLE IF EXISTS aqhi03_daymax_cast;'
    dbSendQuery(con, myQuery)
    
    
    dayCast = as.data.frame(timeAverage(bindFile, avg.time = 'day', data.thresh = 0, statistic = 'max'))
    
    dbWriteTable(con, name = 'aqhi03_daymax_cast', dayCast, row.names = F, overwrite = T)
    
    myQuery = 'ALTER TABLE aqhi03_daymax_cast MODIFY date date NOT NULL; '
    dbSendQuery(con, myQuery)
    
    print('UpdateDB_AQHI.FULL:  \'aqhi03_daymax_cast\' MySQL updated')
    
    
    dbDisconnect(con)
  } #END if (GenSQL)
  
} # END function
#------------------------------------------------------------------------------------------------------------#

  
##############################################################################################################
####################                    Function 2. UpdateDB_Met_FULL                    #####################
##############################################################################################################
##############################################################################################################

UpdateDB_Met_FULL = function(GenCIS = T, GenHour = T){
  
  yearVec = 1999:2100
  master_path = dir_Master
  file_path1 = dir_windHK
  
  library(DBI)
  library(RMySQL)
  con <- dbConnect(MySQL(), user="user", password="as3", dbname = 'db_as3', host="localhost")
      
  if (GenCIS) {
    
      setwd(paste(file_path1, '/HKO_CIS', sep =''))
      met.Day = read.csv('hko_cis_Daily.csv', na.strings = '\\N')
      met.Mon = read.csv('hko_cis_Monthly.csv', na.strings = '\\N')
      setwd(master_path)

      
      myQuery = 'SELECT * FROM aqhi02_daysummary'
      episode.filter = dbGetQuery(con, myQuery)[c('date', 'filter_ser', 'filter_vh', 'filter_high')]
      episode.filter$date = as.Date(episode.filter$date, '%Y-%m-%d')
      met.Day$date = as.Date(met.Day$date, '%Y-%m-%d')
      
      met.Day = merge.data.frame(met.Day, episode.filter %>% distinct(), by = 'date', all.x = T, all.y = F)
      
      ## 1. Write to DB
      myQuery = 'DROP TABLE IF EXISTS met01_daily;'
      dbSendQuery(con, myQuery)
      dbWriteTable(con, name = 'met01_daily', met.Day, row.names = F, overwrite = T)
      myQuery = 'ALTER TABLE met01_daily MODIFY date date NOT NULL; '
      dbSendQuery(con, myQuery)
      
      print('UpdateDB_Met.FULL:  \'met01_daily\' MySQL updated')
      
      myQuery = 'DROP TABLE IF EXISTS met02_month;'
      dbSendQuery(con, myQuery)
      dbWriteTable(con, name = 'met02_month', met.Mon, row.names = F, overwrite = T)
      myQuery = 'ALTER TABLE met02_month MODIFY date date NOT NULL; '
      dbSendQuery(con, myQuery)
      
      print('UpdateDB_Met.FULL:  \'met02_month\' MySQL updated')
    
  } ## END IF (GenCIS)
  
  if (GenHour) {
    
    w_stations = c('WGL', 'TAP', 'SHA', 'R2C', 'LFS', 'TUN', 'SF', 'KP', 'SE', 'SHL', 'GI')
    wData = LoadWind(2014:2016, load_stations = w_stations) %>% mutate(day = as.character(format.Date(date, '%Y-%m-%d')),
      hour = 1 + as.numeric(format.Date(date, '%H')))
    
    write.csv(wData, 'temp1.csv', na = '\\N', row.names = F)
    wData = read.csv('temp1.csv', na.strings = '\\N')
    file.remove('temp1.csv')
      
    for (i in 1:length(w_stations)){
      
      expr = paste('wData$ws_', w_stations[i], ' = wData$ws_', w_stations[i], ' * 3.6', sep = '')
      #print(expr)
      eval(parse(text=expr))
      
    }

    
    myQuery = 'SELECT * FROM aqhi02_daysummary'
      episode.filter = dbGetQuery(con, myQuery)[c('date', 'filter_ser', 'filter_vh', 'filter_high')]
      episode.filter$date = as.character(as.Date(episode.filter$date, '%Y-%m-%d'))
    wData2 = merge.data.frame(wData, episode.filter %>% distinct(), by.x = 'day', by.y = 'date', all.x = T, all.y = F)
    
    
    myQuery = 'DROP TABLE IF EXISTS met03_hourly;'
      dbSendQuery(con, myQuery)
      dbWriteTable(con, name = 'met03_hourly', wData2 %>% select(-day), row.names = F, overwrite = T)
      myQuery = 'ALTER TABLE met03_hourly MODIFY date datetime NOT NULL; '
      dbSendQuery(con, myQuery)
      
    print('UpdateDB_Met.FULL:  \'met03_hourly\' MySQL updated')
    
  } ## END IF (GenHour)
  
  dbDisconnect(con)
  
} ## END function
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                     Function 3. UpdateDB_airPRD                      ####################
##############################################################################################################
##############################################################################################################

UpdateDB_airPRD = function(){
  
  yearVec = 2015:2016
  df = LoadPRD.DayMax(yearVec = yearVec)
  df.melt = as.data.frame(melt(df, id = c('date', 'site')))
  df.cast = as.data.frame(cast(df.melt, date~variable+site, value = 'value', 
             fun.aggregate = function(x) max(x, na.rm = T)))
  
  library(DBI)
  library(RMySQL)
  
  main = 'db_as3'
  con <- dbConnect(MySQL(), user="user", password="as3", dbname=main, host="localhost")
  
  myQuery = 'SELECT * FROM aqhi02_daysummary'
  episode.filter = dbGetQuery(con, myQuery)[c('date', 'filter_ser', 'filter_vh', 'filter_high')]
  episode.filter$date = as.Date(episode.filter$date, '%Y-%m-%d')
  df.cast$date = as.Date(df.cast$date, '%Y-%m-%d')
  
  df.cast = merge.data.frame(df.cast, episode.filter %>% distinct(), by = 'date', all.x = T, all.y = F)
  
  
  ## Write the List to DB
  
  myQuery = 'DROP TABLE IF EXISTS air_prddaymax_group;'
  dbSendQuery(con, myQuery)
  
  dbWriteTable(con, name = 'air_prddaymax_group', df.cast, row.names = F, overwrite = T)

  myQuery = 'ALTER TABLE air_prddaymax_group MODIFY date date NOT NULL; '
  dbSendQuery(con, myQuery)
  
  print('UpdateDB_air_dayMaxGrouped:  \'air_prddaymax_group\' Generated')
  
  dbDisconnect(con)
  
} ## END function
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                     Function 4. GenAirDB_FULL                       ####################
##############################################################################################################
##############################################################################################################

UpdateDB_airHK.FULL = function(yearVec = 2010:2016, GenHrData = T, yrTbl.NewYr = F, newYear = cur_Year){
  
  ## This function Generate DB of (1)hr List and (2)yr List from yearVec using the CSVs
  
  
  library(DBI)
  library(RMySQL)
  
  mainDB = 'db_as3'
  con <- dbConnect(MySQL(), user="user", password="as3", dbname = mainDB, host="localhost")
  pollutant.pick = c('o3_ug', 'no2_ug', 'rsp', 'nox_ug', 'so2_ug', 'fsp', 'co_ug')
  
    
  ### Gen Hourly DB
  if(GenHrData){
    
    a1 = LoadAirHour(yearVec = yearVec, pollutant.pick = pollutant.pick)

    myQuery = 'DROP TABLE IF EXISTS air01_hour;'
    dbSendQuery(con, myQuery)
    dbWriteTable(con, name = 'air01_hour', a1, row.names = F, overwrite = T)
    
    myQuery = 'ALTER TABLE air01_hour MODIFY date datetime NOT NULL; '
    dbSendQuery(con, myQuery)
    
    print(paste('GenAirDB_FULL: \'', 'air01_hour', '\' generated', sep =''))
    
  }
  
  ### Gen Yearly DB 
  setwd(dir_airHK)
  
  air.yr = read.csv('airyear.csv', na.strings = '\\N')
  air.yr$date = air.yr$date %>% ymd()
  air.yr$remark = NA
  
  myQuery = 'DROP TABLE IF EXISTS air02_year;'
  dbSendQuery(con, myQuery)
  dbWriteTable(con, name = 'air02_year', air.yr[c('date', 'siteType', pollutant.pick)], row.names = F, overwrite = T)

    if(yrTbl.NewYr){
      
      df1 = LoadAirHour(yearVec = newYear, pollutant.pick = pollutant.pick)
      df1$siteType = 'General'
      df1$siteType[which(df1$site == 'CB' | df1$site == 'CL' | df1$site == 'MK')] = 'Roadside'
      df1.yr = selectByDate(timeAverage(df1, avg.time = 'year', data.thresh = 0, statistic = 'mean', 
                                        type = 'siteType'), year = 2016:2050)
      
      df1.yr$remark = format(max(df1$date), '%Y-%m-%d')
      df1.yr = df1.yr[c('date', 'siteType', pollutant.pick, 'remark')]
      
      df.final = rbind.data.frame(air.yr, df1.yr)
      
      myQuery = 'DROP TABLE IF EXISTS air02_year;'
      dbSendQuery(con, myQuery)
      dbWriteTable(con, name = 'air02_year', df.final, row.names = F, overwrite = T, append = F)
      
    }
    
  myQuery = 'ALTER TABLE air02_year MODIFY date date NOT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY siteType varchar(15) NOT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY co_ug float(20,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY fsp float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY no2_ug float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY nox_ug float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY o3_ug float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY rsp float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
  
  myQuery = 'ALTER TABLE air02_year MODIFY so2_ug float(12,2) DEFAULT NULL; '
  dbSendQuery(con, myQuery)
    
  dir_Master %>% setwd()
    
  print(paste('GenAirDB_FULL: \'', 'air02_year', '\' generated', sep =''))

} ## END function
#------------------------------------------------------------------------------------------------------------#



##############################################################################################################
####################                   Function 5. UpdateDB_HealthRisk                    ####################
##############################################################################################################
##############################################################################################################

UpdateDB_HealthRisk = function(yearVec = 2014:2016){
  
  pollutant.pick = c('o3_ug', 'no2_ug', 'rsp', 'so2_ug', 'AQHI')
  air.hr = LoadAirHour(yearVec = yearVec, pollutant.pick = pollutant.pick)
  
  roll3hr = rollingMean(air.hr, pollutant = c("o3_ug"), align = "right", width = 3,
                        data.thresh = 66.6, type = "site", new.name = "r3_O3")
  roll3hr = rollingMean(roll3hr, pollutant = c("no2_ug"), align = "right", width = 3,
                        data.thresh = 66.6, type = "site", new.name = "r3_NO2")
  roll3hr = rollingMean(roll3hr, pollutant = c("so2_ug"), align = "right", width = 3,
                        data.thresh = 66.6, type = "site", new.name = "r3_SO2")
  roll3hr = rollingMean(roll3hr, pollutant = c("rsp"), align = "right", width = 3,
                        data.thresh = 66.6, type = "site", new.name = "r3_RSP")
  roll3hr = as.data.frame(selectByDate(roll3hr, year = yearVec))
  
  source('!!_parameters.R')
  m3 = roll3hr %>% 
    mutate(rO3 = (exp(f_o3*r3_O3)-1)*100, rNO2 = (exp(f_no2*r3_NO2)-1)*100, rSO2 = (exp(f_so2*r3_SO2)-1)*100, 
           rRSP = (exp(f_rsp*r3_RSP)-1)*100, contri_O3 = round(rO3/(rO3+rNO2+rSO2+rRSP), 3), 
           contri_NO2 = round(rNO2/(rO3+rNO2+rSO2+rRSP),3), contri_SO2 = round(rSO2/(rO3+rNO2+rSO2+rRSP),3), 
           contri_RSP = round(rRSP/(rO3+rNO2+rSO2+rRSP),3))
  
  m3 = m3[c('date', 'site', 'AQHI', 'contri_O3', 'contri_NO2', 'contri_RSP', 'contri_SO2', 'o3_ug', 'no2_ug', 'rsp', 'so2_ug')]
  
  library(DBI)
  library(RMySQL)
  main = 'db_as3'
  con <- dbConnect(MySQL(), user="user", password="as3", dbname = main, host="localhost")
  
  m3$dayTag =  m3$date %>% substr(1,10) %>% ymd()
  
  myQuery = 'SELECT * FROM aqhi02_daysummary'
  episode.filter = dbGetQuery(con, myQuery)[c('date', 'filter_ser', 'filter_vh', 'filter_high')] %>% distinct()
  episode.filter$date = as.Date(episode.filter$date, '%Y-%m-%d')

  m4 = merge.data.frame(m3, episode.filter, by.x = 'dayTag', by.y = 'date', all.x = T, all.y = F)
  m4$dayTag = NULL
  
  myQuery = 'DROP TABLE IF EXISTS health01_hour;'
  dbSendQuery(con, myQuery)
  dbWriteTable(con, name = 'health01_hour', m4, row.names = F, overwrite = T)
  myQuery = 'ALTER TABLE health01_hour MODIFY date datetime NOT NULL; '
  dbSendQuery(con, myQuery)
  
  print('UpdateDB_HealthRisk.FULL:  \'health01_hour\' MySQL updated')
  
} ## END Function
#------------------------------------------------------------------------------------------------------------#






##############################################################################################################
####################                      Function 10. Format_hkair                       ####################
##############################################################################################################
##############################################################################################################

Format_EPIC_ENVISTA = function (fmt_EPIC = T, fmt_EVIS = T, bindFiles = T, bindAQHI = T){
  
  setwd(paste(dir_airHK, '/00_formating', sep = ''))
  
  if(fmt_EPIC){
    
    ep1 = selectByDate(read.csv('00_!EPICformat_CSV.csv', na.strings = '\\N'), year = 2016:2100)
    ep1$co = ep1$co*10
    names(ep1) = c("date", "site", "co_ug", "fsp",  "no2_ug",  "nox_ug",  "o3_ug", "rsp",  "so2_ug")
    ep1 = ep1[c("date", "site", "o3_ug", "no2_ug", "rsp", "nox_ug", "so2_ug", "fsp", "co_ug")]
    ep1$src = 'EPIC'
    ep1$remark = '\\N'
    
    maxDay = format.Date(max(ep1$date), '%Y-%m-%d')
    minDay = format.Date(min(ep1$date), '%Y-%m-%d')
    
    outName = paste('00-2_EPIC-Output_', minDay, '_to_', maxDay, '.csv', sep = '')
    write.csv(ep1, outName, na = '\\N', row.names = F)
    
    paste('Format_EPIC_ENVISTA: ', outName, sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', 'written to', sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', getwd() %>% as.character(), sep = '') %>% print()
    
  } #END EPIC
  
  if(fmt_EVIS){
    
    ev1 = read.csv('00_!EnvistaFormat_CSV.csv')
    ev1.melt = melt(ev1, id = 'date')
    ev1.melt$site = substr(ev1.melt$variable, 1, 2)
    ev1.melt$pollutant = substr(ev1.melt$variable, 7,15)
    ev1.melt$variable = NULL
    
    cData = selectByDate(cast(ev1.melt, formula = date*site~pollutant, fill = NA), year = 2016:2100)

    ## 2. convert to double and NAs
    cData$co = as.double(as.character(cData$co)) * 1000   #ppm to ppb
    cData$fsp = as.double(as.character(cData$fsp))
    cData$no = as.double(as.character(cData$no))
    cData$no2 = as.double(as.character(cData$no2))
    cData$nox = as.double(as.character(cData$nox))
    cData$o3 = as.double(as.character(cData$o3))
    cData$rsp = as.double(as.character(cData$rsp))
    cData$so2 = as.double(as.character(cData$so2))
    
    cData$co[cData$co < -70] = NA
    cData$fsp[cData$fsp < -5] = NA
    cData$no[cData$no < -7] = NA
    cData$no2[cData$no2 < -7] = NA
    cData$nox[cData$nox < -7] = NA
    cData$o3[cData$o3 < -7] = NA
    cData$rsp[cData$rsp < -5] = NA
    cData$so2[cData$so2 < -7] = NA
    
    cData$co[cData$co < 0] = 0
    cData$fsp[cData$fsp < 0] = 0
    cData$no[cData$no < 0] = 0
    cData$no2[cData$no2 < 0] = 0
    cData$nox[cData$nox < 0] = 0
    cData$o3[cData$o3 < 0] = 0
    cData$rsp[cData$rsp < 0] = 0
    cData$so2[cData$so2 < 0] = 0

    rd_Digit = 2
    cData$co = round(cData$co*1.164604,rd_Digit)
    cData$no2 = round(cData$no2*1.913278,rd_Digit)
    cData$nox = round(cData$nox*1.913278,rd_Digit)
    cData$o3 = round(cData$o3*1.996464,rd_Digit)
    cData$so2 = round(cData$so2*2.666111,rd_Digit)
    
    ###Remove NO
    cData$no = NULL
    
    names(cData) = c("date", "site", "co_ug", "fsp",  "no2_ug",  "nox_ug",  "o3_ug", "rsp",  "so2_ug")
    cData = cData[c("date", "site", "o3_ug", "no2_ug", "rsp", "nox_ug", "so2_ug", "fsp", "co_ug")]
    cData$src = 'EVIS'
    cData$remark = '\\N'
    
    maxDay = format.Date(max(cData$date), '%Y-%m-%d')
    minDay = format.Date(min(cData$date), '%Y-%m-%d')
    
    outName = paste('00-1_Envista-Output_', minDay, '_to_', maxDay, '.csv', sep = '')
    write.csv(cData, outName, na = '\\N', row.names = F)
    
    paste('Format_EPIC_ENVISTA: ', outName, sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', 'written to', sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', getwd() %>% as.character(), sep = '') %>% print()

  }  #END EVIS
  
  if(bindFiles){
    epic.name = tail(list.files('.', pattern = '00-2_EPIC-Output_'), n = 1)
    evis.name = tail(list.files('.', pattern = '00-1_Envista-Output_'), n = 1)
    
    epic.month = as.numeric(substr(epic.name, 37, 38))

    cumFile = rbind.data.frame(selectByDate(read.csv(epic.name, na.strings = '\\N'), month = 1:epic.month), 
               selectByDate(read.csv(evis.name, na.strings = '\\N'), month = (epic.month+1):12))
    
    maxDay = format.Date(max(cumFile$date), '%Y-%m-%d')
    minDay = format.Date(min(cumFile$date), '%Y-%m-%d')
    
    outName = paste('00-3_Envista-Cum-EPIC-Output_', minDay, '_to_', maxDay, '.csv', sep = '')
    
    if(bindAQHI){
      
      setwd(paste(dir_airHK, sep = ''))
      
      aqhiList = selectByDate(read.csv('aqhi_hrList.csv', na.strings = '\\N'), year = 2013:2100)
      cumFile2 = merge.data.frame(aqhiList, cumFile, by = c('date', 'site'), all.x = F, all.y = T)
      
      setwd(paste(dir_airHK, '/00_formating', sep = ''))
      
    } ##ENd bind AQHI
    
    
    write.csv(cumFile2, outName, na = '\\N', row.names = F)
    
    paste('Format_EPIC_ENVISTA: ', outName, sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', 'written to', sep = '') %>% print()
    paste('Format_EPIC_ENVISTA: ', getwd() %>% as.character(), sep = '') %>% print()
    
  }
  
  setwd(dir_Master)
  
} ##END Function
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                     Function 11. Format_HKO_hour                     ####################
##############################################################################################################
##############################################################################################################

Format_HKO_hour = function(){
  
  ###Need to adjust the skip line for .txt
  
  setwd(paste(dir_windHK, '/HKO_Valid', sep = ''))
  
  listFile = list.files(path = ".", pattern = "*.txt")

  for (i in 1:length(listFile)){
  
    outname = str_replace(listFile[i], "wind60", "")
    outname_1 = str_replace(outname, ".txt", "")
    outname = paste("!!_WindData_New", outname_1, ".csv", sep = "")
  
    r1 = read.csv(listFile[i], skip = 4, sep = "")
    r1$date = paste(substr(as.character(r1$Date), 1, 4), "-", substr(as.character(r1$Date), 5, 6), "-", 
          substr(as.character(r1$Date), 7, 8), " ", as.character(r1$hour -1), ":00",
          sep = "")
  
    r1$spd[which(r1$spd == "n/a")] = NA
    r1$dir[which(r1$dir == "n/a")] = NA
    r1$dir[which(r1$dir == "var")] = NA
    r1$dir[which(r1$dir == 0)] = NA
  
    r1 = r1[, c(5, 4, 3)]
    names(r1) = c("date", "ws", "wd")
  
    write.csv(r1, outname, row.names = F)
  
  }
  
  
  ## to merge wind
  listFile = list.files(path = ".", pattern = "*.csv")
  loops = length(listFile)/2
  
  for (i in 1:loops){
  
    r1 = selectByDate(read.csv(listFile[i]), year = 2011:2020)
    r2 = selectByDate(read.csv(listFile[i+17]), year = 2011:2020)
    print(paste(listFile[i], " v ", listFile[i+17]))
    
    r3 = rbind.data.frame(r2, r1)
    write.csv(r3, listFile[i+17], row.names = F, na = '\\N')
  
  }
  
  setwd(dir_Master)
  
} ##END Function
#------------------------------------------------------------------------------------------------------------#







##############################################################################################################
####################                        Function 20. Gen_AAQI                         ####################
##############################################################################################################
##############################################################################################################

GenAAQI = function(date.start = '1/5/2015', date.end = '30/4/2016', pollutant.pick = c("no2_ug", "rsp", 'fsp')){ 
  
  
  ## FInd the start and end Year based on Function Arg
  start.year = as.numeric(str_split(date.start, '/')[[1]][3])
  end.year = as.numeric(str_split(date.end, '/')[[1]][3])
  year.Select = start.year:end.year
  
  site.order = c('CW', 'EN', 'KT', 'SP', 'KC', 'TW', 'TK', 'YL', 
                 'TM', 'TC', 'TP', 'ST', 'MB', 'CB', 'CL', 'MK')
  
  ## Sample Name "AAQI_04-2016.csv"
  output.Name = paste('AAQI_', format.Date(as.Date(date.end, '%d/%m/%Y'), "%m-%Y"),'.csv', sep='')
  
  
  m1 = LoadAirHour(yearVec = year.Select, pollutant.pick = pollutant.pick)
  a = timeAverage(selectByDate(m1, start = date.start, end = date.end), avg.time = '2 year', 
                  type = 'site', statistic = 'mean', data.thresh = 66.66)
  a2 = timeAverage(selectByDate(m1, start = date.start, end = date.end), avg.time = '2 year', 
                   type = 'site', statistic = 'frequency', data.thresh = 66.66)
  
  a$order = NA
  for (i in 1:length(site.order)){
    
    j = which(site.order[i] == a$site)
    a$order[j] = i
    
  }
  
  a$no2_aaqi = round(round(a$no2_ug,0)/40,2)
  a$rsp_aaqi = round(round(a$rsp,0)/20,2)
  a$fsp_aaqi = round(round(a$fsp,0)/10,2)
  
  a$no2_rd = round(a$no2_ug,0)
  a$rsp_rd = round(a$rsp,0)
  a$fsp_rd = round(a$fsp,0)
  b = a[order(a$order), c('site', 'no2_rd', 'rsp_rd', 'fsp_rd', 'no2_aaqi', 'rsp_aaqi', 'fsp_aaqi')]
  d = a2[order(a$order),]
  d$no2_ug.percent = d$no2_ug/8760
  d$rsp.percent = d$rsp/8760
  d$fsp.percent = d$fsp/8760
  
  b = cbind.data.frame(b,d)
  b$date = paste(date.start, ' to ', date.end, sep ='')
  
  bindStr = ''
  for (ii in 1:length(site.order)){bindStr = paste(bindStr, site.order[ii], sep = ' ')}
  
  print(paste('Gen_AAQI: Year Selected -- ', min(year.Select), 'to', max(year.Select), sep = ''))
  print(paste('Gen_AAQI: site.order/length -- ', bindStr, '  [', length(site.order), ']',sep = ''))
  
  WriteCSV_ToFolder(file_path = dir_AAQI, master_path = dir_Master, varName = b, fileName = output.Name)
  print(dir_AAQI)
  
}##END Function
#------------------------------------------------------------------------------------------------------------#




##############################################################################################################
####################                     Function 40. MultiFileBind                       ####################
##############################################################################################################
##############################################################################################################

MultiFileBind = function (prefix = 'airhour', array.i = 2015:2016, extension = '.csv', skipLine = 0) {
  ## example usage -- the actual filename of the csv airhour2016.csv
  ## prefix = '01_airHour_'
  ## array.i = 2014:2015
  ## extension = '.csv'
  
  
  ## Need to navigate by a run function before execute
  bindStr = ""
  for (i in array.i){
    
    bindStr = paste(bindStr, 'p', i, ", ", sep = "")
    expr1 = paste('p', i, " = read.csv('", prefix, i, extension, "', skip = ", skipLine, 
                  ", na.strings = '\\\\N')", sep="") ##the readLine
    eval(parse(text=expr1))
    
  }
  
  cat_txt = paste("mergeData = rbind.data.frame(", bindStr, "deparse.level = 0)", sep='')
  eval(parse(text=cat_txt))
  
  outname = paste(prefix, "_", min(array.i), "-", max(array.i), sep = "")
  print(paste('MultiFileBind: ', outname, "is Generated!"))
  
  return(mergeData)
  
}
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                    Function 41. WriteCSV_ToFolder                    ####################
##############################################################################################################
##############################################################################################################

WriteCSV_ToFolder = function(file_path, master_path, varName, fileName, NA.char = '\\N') {
  
  setwd(file_path)
  
  write.csv(varName, fileName, row.names = F, na = NA.char)
  print(paste("WriteCSV_ToFolder: File - ", fileName, " is written in ", sep = ''))
  print(paste('WriteCSV_ToFolder: ', file_path, sep = ''))
  print(paste('WriteCSV_ToFolder: NA.Char = ', NA.char, sep = ''))
  
  setwd(master_path)
  
}
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                    Function 42. GenHeadingPattern                    ####################
##############################################################################################################
##############################################################################################################

GenHeadingPattern = function(txtInput = 'default'){
  
  str_length(txtInput)
  heading = paste('####################', str_pad(txtInput, width = 70, side = 'both', pad = ' '), '####################', sep = '')  
  print('##############################################################################################################')
  print(heading)
  print('##############################################################################################################')
  print('##############################################################################################################')
  
}
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                      Function 43. DayMax8h.Cal                       ####################
##############################################################################################################
##############################################################################################################

DayMax8h.Cal = function(yearVec = cur_Year, cutOFF = 0, data.thresh = 66.66, species = 'o3_ug'){
  
  txt1 = paste('Generate DayMax 8hr ', species, ' - from ', min(yearVec), " to ", max(yearVec), sep = '')
  print(txt1)
  txt2 = paste('cutoff conc.: ', cutOFF, ' data.thresh: ', data.thresh, sep = '')
  print(txt2)
  
  air.hr = LoadAirHour(yearVec = yearVec, pollutant.pick = species)
  
  air.hr = rollingMean(air.hr, pollutant = species, width = 8, 
                       data.thresh = data.thresh, align = "right", type = "site") 
  names(air.hr)[4] = 'rolling8'
  
  air.hr = timeAverage(air.hr, type = "site", data.thresh = data.thresh, statistic = "max") %>%
    filter(rolling8 >= cutOFF)
  
  #cast.Value = paste('rolling8', species, sep = '')
  air.hr.cast = cast(air.hr, date~site, value = 'rolling8', fun.aggregate = 'mean', fill = NA)
  
  return(air.hr.cast)
  
}
#------------------------------------------------------------------------------------------------------------#





















##############################################################################################################
####################               Function 13. UpdateDB_air_dayMaxGrouped                ####################
##############################################################################################################
##############################################################################################################

UpdateDB_air_dayMaxGrouped = function(yearVec = 2014:2016, main = 'db_as3'){
  
  pollutant.pick = c('o3_ug', 'rsp', 'no2_ug', 'nox_ug')
  a1 = LoadAirHour(yearVec, pollutant.pick)
  
  Road.DayMax = timeAverage(a1 %>% filter(site == 'CB' | site == 'MK' | site == 'CL'), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(Road.DayMax) = c('date', 'Road_O3DMax', 'Road_RSPDMax', 'Road_NO2DMax', 'Road_NOxDMax')
  
  NTEst.DayMax = timeAverage(a1 %>% filter(site == 'MB' | site == 'TP' | site == 'ST'), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(NTEst.DayMax) = c('date', 'NTEst_O3DMax', 'NTEst_RSPDMax', 'NTEst_NO2DMax', 'NTEst_NOxDMax')
  
  NTWst.DayMax = timeAverage(a1 %>% filter(site == 'YL' | site == 'TM' | site == 'TC'), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(NTWst.DayMax) = c('date', 'NTWst_O3DMax', 'NTWst_RSPDMax', 'NTWst_NO2DMax', 'NTWst_NOxDMax')
  
  UrbEst.DayMax = timeAverage(a1 %>% filter(site == 'TK' | site == 'EN' | site == 'KT'), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(UrbEst.DayMax) = c('date', 'UrbEst_O3DMax', 'UrbEst_RSPDMax', 'UrbEst_NO2DMax', 'UrbEst_NOxDMax')
  
  UrbWst.DayMax = timeAverage(a1 %>% filter(site == 'TW' | site == 'KC' | site == 'SP' | site == 'CW'), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(UrbWst.DayMax) = c('date', 'UrbWst_O3DMax', 'UrbWst_RSPDMax', 'UrbWst_NO2DMax', 'UrbWst_NOxDMax')
  
  MB.DayMax = timeAverage(a1 %>% filter(site == 'MB') %>% select(date, o3_ug, rsp), avg.time = 'day',
                            statistic = 'max', data.thresh = 0)
  names(MB.DayMax) = c('date', 'MB_O3DMax', 'MB_RSPDMax')
  
  Road.Day75p = timeAverage(a1 %>% filter(site == 'CB' | site == 'MK' | site == 'CL') %>% select(date, o3_ug), avg.time = 'day',
                            statistic = 'percentile', percentile = 75, data.thresh = 0)
  names(Road.Day75p) = c('date', 'Road_O3D75p')
  
    Road.Day50p = timeAverage(a1 %>% filter(site == 'CB' | site == 'MK' | site == 'CL') %>% select(date, o3_ug), avg.time = 'day',
                            statistic = 'percentile', percentile = 50, data.thresh = 0)
  names(Road.Day50p) = c('date', 'Road_O3D50p')
  
    Road.Day25p = timeAverage(a1 %>% filter(site == 'CB' | site == 'MK' | site == 'CL') %>% select(date, o3_ug), avg.time = 'day',
                            statistic = 'percentile', percentile = 25, data.thresh = 0)
  names(Road.Day25p) = c('date', 'Road_O3D25p')
  
  
  bindFile = cbind.data.frame(Road.DayMax, NTEst.DayMax[-1], UrbEst.DayMax[-1], 
                              NTWst.DayMax[-1], UrbWst.DayMax[-1], MB.DayMax[-1], 
                              Road.Day75p[-1], Road.Day50p[-1], Road.Day25p[-1])
  
  bindFile = bindFile[c('date', 'MB_O3DMax', 'NTEst_O3DMax', 'UrbEst_O3DMax', 'NTWst_O3DMax', 'UrbWst_O3DMax',
    'Road_O3DMax', 'Road_O3D75p', 'Road_O3D50p', 'Road_O3D25p',
    'MB_RSPDMax', 'NTEst_RSPDMax', 'UrbEst_RSPDMax', 'NTWst_RSPDMax', 'UrbWst_RSPDMax', 'Road_RSPDMax',
    'Road_NO2DMax', 'UrbWst_NO2DMax', 'UrbEst_NO2DMax', 'NTWst_NO2DMax', 'NTEst_NO2DMax',
    'Road_NOxDMax', 'UrbWst_NOxDMax', 'UrbEst_NOxDMax', 'NTWst_NOxDMax', 'NTEst_NOxDMax')]
  
  library(DBI)
  library(RMySQL)
  
  
  con <- dbConnect(MySQL(), user="user", password="as3", dbname=main, host="localhost")
  
  ## Write the List to DB
  
  myQuery = 'DROP TABLE IF EXISTS air_hkdaymax_group;'
  dbSendQuery(con, myQuery)
  
  dbWriteTable(con, name = 'air_hkdaymax_group', bindFile, row.names = F, overwrite = T)

  myQuery = 'ALTER TABLE air_hkdaymax_group MODIFY date date NOT NULL; '
  dbSendQuery(con, myQuery)
  
  print('UpdateDB_air_dayMaxGrouped:  \'air_hkdaymax_group\' Generated')
  
  dbDisconnect(con)
  
}







##############################################################################################################
####################                    Function 15. GenDB_PressBrief                     ####################
##############################################################################################################
##############################################################################################################

GenDB_PressBrief = function(AQHI_Gen = F, HKO.Met.Gen = F, HKAir.Gen = F, YearStart = 2014){
  
  library(DBI)
  library(RMySQL)

  mainDB = 'db_as3'
  con_main <- dbConnect(MySQL(), user="user", password="as3", dbname = mainDB, host="localhost")
  
  if(AQHI_Gen){
    
    #### Gen AQHI Database -- hr and day
    
    ## 1.1 -- Gen Hourly Table
    myQuery = paste(
      'SELECT * FROM aqhi_hr_fulllist
       WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
    aqhi.hr = dbGetQuery(con_main, myQuery)
    
    aqhi.hr$date = as.POSIXct(strptime(aqhi.hr$date, '%Y-%m-%d %H:%M:%S'))
  
    dbWriteTable(con_main, name = 'pb_aqhi_hr_list', aqhi.hr, row.names = F, overwrite = T,
       field.types=list(date="datetime", site='varchar(3)', AQHI='int(3)'))
    myQuery = 'ALTER TABLE pb_aqhi_hr_list MODIFY site varchar(3) NOT NULL; '
    dbSendQuery(con_main, myQuery)
    myQuery = 'ALTER TABLE pb_aqhi_hr_list MODIFY date datetime NOT NULL; '
    dbSendQuery(con_main, myQuery)
    ### ********************************************************
    
    ###***** (OPTIONAL) Delete Maintenance Periods
    myQuery = 
      'DELETE FROM aqhi_hrlist
       WHERE AND (site=\'MB\') AND (date BETWEEN \'2015-12-01 00:00:00\' AND \'2015-12-31 23:00:00\'); '
    #dbSendQuery(con_brief, myQuery)
    
    myQuery = 
      'DELETE FROM aqhi_hrlist
       WHERE AND (site=\'MB\') AND (date BETWEEN \'2016-01-01 00:00:00\' AND \'2016-02-29 23:00:00\'); '
    #dbSendQuery(con_brief, myQuery)
    myQuery = ''
    ### ********************************************************
    
    ## 1.2 Gen DayMax Table with Stats
    myQuery = paste(
      'SELECT * FROM aqhi_day_fullcast
       WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
    aqhi.dayM = dbGetQuery(con_main, myQuery)
    
    aqhi.dayM$date = as.POSIXct(strptime(aqhi.dayM$date, '%Y-%m-%d'))
    
    myQuery = 'DROP TABLE IF EXISTS pb_aqhi_day_cast;'
    dbSendQuery(con_main, myQuery)
    dbWriteTable(con_main, name = 'pb_aqhi_day_cast', aqhi.dayM, row.names = F, overwrite = T)

    myQuery = 'ALTER TABLE pb_aqhi_day_cast MODIFY date date NOT NULL; '
    dbSendQuery(con_main, myQuery)
    ### ********************************************************
    
    
    
    ## 2.1- Hourly Max in Group -- General, Roadside, Overall
    aqhihr.Cast = as.data.frame(cast(aqhi.hr, date~site, value = 'AQHI'))[c('date', ALLCluster)]
    gen.hrMax = apply(aqhihr.Cast[GeneralCluster], 1, function(x) max(x, na.rm = T))
    road.hrMax = apply(aqhihr.Cast[RoadsideCluster], 1, function(x) max(x, na.rm = T))
    overall.hrMax = apply(aqhihr.Cast[ALLCluster], 1, function(x) max(x, na.rm = T))
    aqhihr.Group = as.data.frame(melt(cbind.data.frame(aqhihr.Cast[1], gen.hrMax, road.hrMax, overall.hrMax), 
                                      id = 'date'))
    names(aqhihr.Group) = c('date', 'site', 'AQHI_hrMax')
  
    myQuery = 'DROP TABLE IF EXISTS pb_aqhi_hrmaxgroup_list;'
    dbSendQuery(con_main, myQuery)
    dbWriteTable(con_main, name = 'pb_aqhi_hrgroup_list', aqhihr.Group, row.names = F, overwrite = T)
    myQuery = 'ALTER TABLE pb_aqhi_hrgroup_list MODIFY date datetime NOT NULL; '
    dbSendQuery(con_main, myQuery)
    ### ********************************************************
    
    ## 2.2- Daily Max in Group -- General, Roadside, Overall
    aqhi.day = as.data.frame(timeAverage(aqhi.hr, avg.time = 'day', statistic = 'max', 
                           data.thresh = 0, type = 'site'))[c('date', 'site', 'AQHI')]
    
    aqhiday.Cast = as.data.frame(cast(aqhi.day, date~site, value = 'AQHI'))[c('date', ALLCluster)]
    gen.dayMax = apply(aqhiday.Cast[GeneralCluster], 1, function(x) max(x, na.rm = T))
    road.dayMax = apply(aqhiday.Cast[RoadsideCluster], 1, function(x) max(x, na.rm = T))
    overall.dayMax = apply(aqhiday.Cast[ALLCluster], 1, function(x) max(x, na.rm = T))
    aqhiday.Group = as.data.frame(melt(cbind.data.frame(aqhiday.Cast[1], gen.dayMax, road.dayMax, overall.dayMax), 
                                      id = 'date'))
    names(aqhiday.Group) = c('date', 'site', 'AQHI_dayMax')
    
    myQuery = 'DROP TABLE IF EXISTS pb_aqhi_daymaxgroup_cast;'
    dbSendQuery(con_main, myQuery)
    dbWriteTable(con_main, name = 'pb_aqhi_daymaxgroup_cast', aqhiday.Group, row.names = F, overwrite = T)
    myQuery = 'ALTER TABLE pb_aqhi_daymaxgroup_cast MODIFY date date NOT NULL; '
    dbSendQuery(con_main, myQuery)
    ### ********************************************************

    ##END AQHI
    
  }
  
  if(HKO.Met.Gen){
    
    setwd(dir_HKO.Met)
    met.df = read.csv('hko_cis_Monthly.csv', na.strings = '\\N')
    setwd(dir_Master)
    
    myQuery = 'DROP TABLE IF EXISTS pb_met_monthly;'
    dbSendQuery(con_main, myQuery)
    dbWriteTable(con_main, name = 'pb_met_monthly', met.df, row.names = F, overwrite = T)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY date date NOT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY cloudiness float DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY rainfall float DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY sunshine float DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY wDir int(3) DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY wSpd float DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
    myQuery = 'ALTER TABLE pb_met_monthly MODIFY low_vis int DEFAULT NULL; '
    dbSendQuery(con_main, myQuery)
    
  }
  
  if(HKAir.Gen){
    
    ## 1. Gen Yearly Table
    myQuery = paste(
      'SELECT * FROM air_yrhk_full
       WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
    air.yr = dbGetQuery(con_main, myQuery)
    
    myQuery = 'DROP TABLE IF EXISTS pb_air_yrhk;'
    dbWriteTable(con_main, name = 'pb_air_yrhk', air.yr, row.names = F, overwrite = T)
    myQuery = 'ALTER TABLE pb_air_yrhk MODIFY date date NOT NULL; '
    dbSendQuery(con_main, myQuery)
    myQuery = 'ALTER TABLE pb_air_yrhk MODIFY sitetype varchar(10) NOT NULL; '
    dbSendQuery(con_main, myQuery)
    
    
    ## 2. Gen Hourly Table
    myQuery = paste(
      'SELECT * FROM air_hrhk_full
       WHERE date >= \'', YearStart, '-01-01 00:00:00\' ;', sep='')
    airdata.hr = dbGetQuery(con_main, myQuery)
    
    airdata.hr$date = as.POSIXct(strptime(airdata.hr$date, '%Y-%m-%d %H:%M:%S'))
    
    myQuery = 'DROP TABLE IF EXISTS pb_air_hrhk;'
    dbSendQuery(con_main, myQuery)
    dbWriteTable(con_main, name = 'pb_air_hrhk', airdata.hr, row.names = F, overwrite = T)

    myQuery = 'ALTER TABLE pb_air_hrhk MODIFY date datetime NOT NULL; '
    dbSendQuery(con_main, myQuery)
    myQuery = 'ALTER TABLE pb_air_hrhk MODIFY site varchar(3) NOT NULL; '
    dbSendQuery(con_main, myQuery)

  }
  
  dbDisconnect(con_main)

}











HKO_mSummary.Read = function (mSummary = F, d_UV = F){
  
  if (mSummary){
    yearVec = 2016:2016
    monthVec = 1 :8
    counter = 1
    
    bindStr = ""
    bindStr2 = ""
    
    for (yr in yearVec){
      
      for (mon in monthVec){
        
        url = paste('http://www.weather.gov.hk/wxinfo/pastwx/metob', yr, str_pad(mon, 2, pad = '0'), '.htm', sep = '')
         doc = htmlParse(url)
         tableNodes = getNodeSet(doc, "//table")
         tbl = readHTMLTable(tableNodes[[2]])
         startRows = which(tbl[1] == 13) - 12
         
          if(is.na(which(tbl[1] == 31)[1] > 0)){
            if(is.na(which(tbl[1] == 30)[1] > 0)){
              if(is.na(which(tbl[1] == 29)[1] > 0)){
                endRows = which(tbl[1] == 28)
              } else {endRows = which(tbl[1] == 29)}  
            } else {endRows = which(tbl[1] == 30)}
          } else {endRows = which(tbl[1] == 31)}
         
         expr1 = paste('p', counter,' = as.data.frame(tbl[startRows[1]:endRows[1], 1:9])', sep = "")
         eval(parse(text=expr1))
         expr1 = paste('p', counter,'$year = yr', sep = "")
         eval(parse(text=expr1))
         expr1 = paste('p', counter,'$month = mon', sep = "")
         eval(parse(text=expr1))
         
         expr1 = paste('q', counter,' = as.data.frame(tbl[startRows[2]:endRows[2], 1:7])', sep = "")
         eval(parse(text=expr1))
         expr1 = paste('q', counter,'$year = yr', sep = "")
         eval(parse(text=expr1))
         expr1 = paste('q', counter,'$month = mon', sep = "")
         eval(parse(text=expr1))
        
         bindStr = paste(bindStr, 'p', counter, ", ", sep = "")
         bindStr2 = paste(bindStr2, 'q', counter, ", ", sep = "")
         
        counter = counter + 1
        
      }
      
      print(yr)
      
    }
    
    cat_txt = paste("mergeData = rbind.data.frame(", bindStr, "deparse.level = 0)", sep='')
      eval(parse(text=cat_txt))
      
    cat_txt2 = paste("mergeData2 = rbind.data.frame(", bindStr2, "deparse.level = 0)", sep='')
      eval(parse(text=cat_txt2))
    
    mergeData_1 = mergeData[c(10,11,1:9)]
    names(mergeData_1) = c('y', 'm', 'd', 'p_avg', 't_max', 't_avg', 't_min', 'dewpt', 'rh', 'cloud', 'rainfall')
    mergeData_2 = mergeData2[c(8,9,1:7)]
    names(mergeData_2) = c('y', 'm', 'd', 'low_vis', 'sun_hrs', 'daily_solar', 'evap', 'wd_wgl', 'ws_wgl')
    
    mergedFile = cbind.data.frame(mergeData_1, mergeData_2)
    WriteCSV_ToFolder(file_path = dir_HKO.Met, master_path = dir_Master, varName = mergedFile, 
                      fileName = 'hko_cis_Daily_TEMP.csv', NA.char = '\\N')
  
  }
  
  if (d_UV){
    
    
    df <- data.frame(matrix(ncol = 5, nrow = 10000))
    names(df) = c('year', 'month', 'day', 'uv_max', 'uv_avg')
    counter = 1
    
    for (yr in 2014:2016){
      
      for (m in 1:12){
        
        for (d in 1 : 28){
          
          url = paste('http://www.weather.gov.hk/cgi-bin/hko/yes.pl?year=', yr, '&month=', str_pad(m, 2, pad = "0"), '&day=', str_pad(m, 2, pad = '0'), '&language=english&B1=Confirm', sep = '')
          
          thepage = readLines(url)
          
          myPattern1 = 'The maximum UV index was'
          myPattern2 = 'The mean UV index recorded at King'
          
          datalines1 = grep(myPattern1, thepage, value = T)
          datalines2 = grep(myPattern2, thepage, value = T)
          
          uv.max = as.numeric(substr(datalines1, str_length(datalines1)-2, str_length(datalines1)-1))
          uv.avg = as.numeric(substr(datalines2, str_length(datalines2)-2, str_length(datalines2)-1))
          
          df[counter, 1] = yr
          df[counter, 2] = m
          df[counter, 3] = d
          df[counter, 4] = uv.max
          df[counter, 5] = uv.avg
            
          
          counter = counter + 1
          
          WriteCSV_ToFolder(file_path = dir_HKO.Met, master_path = dir_Master, varName = df, 
                      fileName = 'hko_cis_D_UV_TEMP.csv', NA.char = '\\N')
          
        }
        
      }
      
      
    }
    
  }
}  









##

























