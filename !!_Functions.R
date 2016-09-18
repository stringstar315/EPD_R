##############################################################################################################
####################                          List of Functions                           ####################
##############################################################################################################

##  1. DayMax8h.Cal = function(yearVec = cur_Year, cutOFF = 0, data.thresh = 66.66, species = 'o3_ug')
##  2. MultiFileBind = function (prefix = '01_airHour', array.i = 2015:2016, extension = '.csv', skipLine = 0)
##  3. LoadAirHour = function (yearVec = cur_Year, columns = c('o3_ug', 'no2_ug', 'rsp'), file_path = dir_AirHour, master_path = dir_Master)
##  4. LoadAQHI = function (yearVec = cur_Year, file_path = dir_AQHI, master_path = dir_Master)
##  5. LoadWind = function (yearVec = cur_Year, load_stations = c('WGL', 'R2C'), file_path = dir_Wind, master_path = dir_Master )
##  6. WriteCSV_ToFolder = function(file_path, master_path, varName, fileName)
##  7. Gen_AAQI = function(date.start = '1/5/2015', date.end = '30/4/2016', dir.AAQI = dir_AAQI, dir.Master = dir_Master)
##  8. MergeByDate = function(data1, data2)
##  9. LoadPRDHour = function (yearVec = cur_Year, columns = c('date', 'site', 'o3_ug', 'no2_ug', 'rsp'), file_path = dir_PRD, master_path = dir_Master)
## 10. EpisodeDays.ListbyAQHI = function(yearVec = cur_Year, episodeAQHI = 11)
## 11. EpisodeDays.summaryAQHI = function(yearVec = cur_Year, list.pollutants = c('o3_ug', 'no2_ug', 'rsp', 'nox_ug', 'so2_ug'))
## 12. PlotTrajectory = function(yearVec = cur_Year)
## 13. UpdateDataFrame = function(old.df, new.df)



## 80. HKair.HourToYear = function(csvFile = '')





## 30. GenHeadingPattern = function(txtInput = 'default')

## 0. LogFile = function(elapseTime, fun.Run, type.Run, remarks = NA, logEnable = F)

#####################################################################################################
######################################### List of Functions ######################################### 
#####################################################################################################





MultiFileBind = function (prefix = '01_airHour_', array.i = 2015:2016, extension = '.csv', skipLine = 0) {
  ## example usage -- the actual filename of the csv airhour2016.csv
  ## prefix = '01_airHour_'
  ## array.i = 2014:2015
  ## extension = '.csv'
  
  bindStr = ""
  for (i in array.i){
    
    bindStr = paste(bindStr, 'p', i, ", ", sep = "")
    expr1 = paste('p', i, " = read.csv('", prefix, i, extension, "', skip = ", skipLine, 
                  "", sep="") ##the readLine
    eval(parse(text=expr1))
    
  }
  
  cat_txt = paste("mergeData = rbind.data.frame(", bindStr, "deparse.level = 0)", sep='')
  eval(parse(text=cat_txt))
  
  outname = paste(prefix, "_", min(array.i), "-", max(array.i), sep = "")
  print(paste('MultiFileBind: ', outname, "is Generated!"))
  
  return(mergeData)
  
}







##############################################################################################################
####################                    Function 80 - HKair.HourToYear                    ####################
##############################################################################################################
## Need a input of .csv
# 
# 
# HKair.HourToYear = function(yearVec = cur_Year){
#   
#   csvFile = paste('airHour', yearVec, '.csv', sep = '')
#   setwd(dir_airHK)
#   r1 = read.csv(csvFile, na.strings = '\\N')
#   
#   r1.month = timeAverage(r1, avg.time = 'month', data.thresh = 66.66, statistic = 'mean', type = 'site')
#   r1.month.Year = timeAverage(r1.month, avg.time = 'year', data.thresh = 66.66, statistic = 'mean', type = 'site')
#   r1.year = timeAverage(r1, avg.time = 'year', data.thresh = 66.66, statistic = 'mean', type = 'site')
#   
#   
#   
#   
#   
#   
#   
#   r2 = r1
#   
#   uniq.Site = as.character(unique(r1$site))
#   len.Site = length(uniq.Site)
#   
#   r1.month = timeAverage(r1, avg.time = 'month', data.thresh = 66.66, statistic = 'mean', type = 'site')
#   
#   for (site.j in 1:len.Site){
#     
#     s1 = r1.month %>% filter(site == uniq.Site[site.j])
#     
#     for (poll.k in 3:10){
#       
#       tempMat = matrix(data = NA, nrow = 12, ncol = 1)
#       for (LL in 1:12){tempMat[LL] = any(which(s1[poll.k] >= 0), LL)}
#       (sum(tempMat[1:3]) >= 2 & sum(tempMat[4:6]) >= 2 & sum(tempMat[7:9]) >= 2 & sum(tempMat[10:12]) >= 2)
#     }
#   
#   }
#   
#   
#   
#   
#   
#   
#   
#   avg.Year = matrix(data = NA, nrow = len.Site, ncol = 8)
#   avg.Year[,1] = uniq.Site
# 
#   
#   
#   
#     tempMat = matrix(data = NA, nrow = 12, ncol = 1)
#     
#     for (poll.k in 3:9){
#       
#       for (LL in 1:12){tempMat[LL] = any(which(s1[poll.k] >= 0), LL)}
#       if (sum(tempMat[1:3]) >= 2 & sum(tempMat[4:6]) >= 2 & sum(tempMat[7:9]) >= 2 & sum(tempMat[10:12]) >= 2){
#         avg.Year[site.j, poll.k-1] = mean(s1[poll.k], na.rm = T)  
#       } else {avg.Year[site.j, poll.k-1] = NA}
#       
#     }
#     
#   }
#   
#   
# 
#   
# 
#   
#   
#   
#   o3_AAvg = timeAverage(selectByDate(r1 %>% filter(site == uniq.Site[7]), month = which(m1$o3_ug >= 0)), avg.time = 'year', data.thresh = 66.66,
#                         statistic = 'mean')
#   
#   r1.year = timeAverage(r1, avg.time = 'year', data.thresh = 66.66, statistic = 'mean', type = 'site')
#   setwd(dir_Master)
#   
# 
# 
# mean((r1 %>% filter(site == 'CB'))$o3_ug, na.rm = T)


#------------------------------------------------------------------------------------------------------------#



## -- Function 13 -- ##
UpdateDataFrame = function(old.df, new.df, merge.Col = c('date', 'site'), source.Col = 'source', new.Txt = "NEW"){
  
  ind = which(merge.data.frame(old.df[c(merge.Col)], new.df[c(merge.Col, source.Col)], all = T)[3] == new.Txt)
  old.retain = merge.data.frame(old.df, new.df[c(merge.Col)], all = T)[-ind,]
  final.df = rbind.data.frame(old.retain, new.df)
  return(final.df)
  
  
}





## -- Function 30 -- ##
GenHeadingPattern = function(txtInput = 'default'){
  
  str_length(txtInput)
  heading = paste('####################', str_pad(txtInput, width = 70, side = 'both', pad = ' '), '####################', sep = '')  
  print('##############################################################################################################')
  print(heading)
  print('##############################################################################################################')
 
}







## -- Function 12 -- ##
aaa = function() {
  
  hk <- importTraj("hk", 2015)
  
  hk$day <- as.Date(hk$date)
    hk$hour <- substr(hk$date, start = 12, stop = 13)
  
  
  p1 = trajPlot(selectByDate(hk, start = "1/10/2015", end = "9/10/2015") %>% filter(hour.inc >= -72 & hour == '00'), 
                origin = T, type = 'day', group = 'hour', col = 'jet',
                projection = "lambert", parameters = c(22.3, 114.17),
                orientation = c(90, 0, 0))
  
  p1$plot$x.limits = c(113, 125)
  p1$plot$y.limits = c(19, 40)
  
  p1$plot
  
  
}





## -- Function 11 -- ##

ListofPollutantsMax = function(yearVec = cur_Year, list.pollutants = c('o3_ug')){
  
  a1 = LoadAirHour(yearVec = yearVec, columns = c(list.pollutants))
  a1$site = substr(a1$site, 1, 2)
  a1.dayMax = as.data.frame(timeAverage(a1, avg.time = 'day', data.thresh = 0, statistic = 'max', type = 'site'))
  
  m1 = melt(a1.dayMax, id = c('date', 'site'))
  a2 = cast(m1, formula = date~variable+site, fun.aggregate = 'mean', value = 'value', fill = NA)
  
#   a2$road.Max = apply(a2[c(roadStation)], 1, function(x) max(x, na.rm = T))
#   a2$gen.Max = apply(a2[c(genStation)], 1, function(x) max(x, na.rm = T))
#   a2$overall.Max = apply(a2[c(roadStation, genStation)], 1, function(x) max(x, na.rm = T))
#  
#   c1 = a2[c('date', roadStation, genStation)] %>% 
#     filter(overall.Max >= episodeAQHI)
  
  return(a2)
  
}



## -- Function 10 -- ##
## List out 1.Date, 2.Site DayMax AQHI, 3.No.of Gen&Road site involved, 
## 4. Max Number of Site at one time, 5. Duration of Episode
## Work with AQHI 7 or Above
EpisodeDays.ListbyAQHI = function(yearVec = cur_Year, episodeAQHI = 11){
  
  a1 = LoadAQHI(yearVec = yearVec)
  a1$site = substr(a1$site, 1, 2)
  a1.dayMax = timeAverage(a1, avg.time = 'day', data.thresh = 0, statistic = 'max', type = 'site')
  
  ## may need to update road and gen Station column names in Setup
  ## Reshape as "date~site"
  a2 = cast(a1.dayMax, formula = date~site, fun.aggregate = 'mean', value = 'AQHI', fill = NA)
  a2$road.Max = apply(a2[c(roadStation)], 1, function(x) max(x, na.rm = T))
  a2$gen.Max = apply(a2[c(genStation)], 1, function(x) max(x, na.rm = T))
  a2$overall.Max = apply(a2[c(roadStation, genStation)], 1, function(x) max(x, na.rm = T))
  
  ##count site of Serious
  a2$road.Serious.Count = apply(a2[c(roadStation)], 1, function(x) sum(x==11, na.rm = T))
  a2$gen.Serious.Count = apply(a2[c(genStation)], 1, function(x) sum(x==11, na.rm = T))
  a2$overall.Serious.Count = apply(a2[c(roadStation, genStation)], 1, function(x) sum(x==11, na.rm = T))
  
  ##count site of v.High or Above
  a2$road.vHigh.Count = apply(a2[c(roadStation)], 1, function(x) sum(x>=8, na.rm = T))
  a2$gen.vHigh.Count = apply(a2[c(genStation)], 1, function(x) sum(x>=8, na.rm = T))
  a2$overall.vHigh.Count = apply(a2[c(roadStation, genStation)], 1, function(x) sum(x>=8, na.rm = T))
  
  ##count site of High or Above
  a2$road.High.Count = apply(a2[c(roadStation)], 1, function(x) sum(x>=7, na.rm = T))
  a2$gen.High.Count = apply(a2[c(genStation)], 1, function(x) sum(x>=7, na.rm = T))
  a2$overall.High.Count = apply(a2[c(roadStation, genStation)], 1, function(x) sum(x>=7, na.rm = T))

  #### Find the Max Duration of Episode and Max Number of site at one time
  cast.AQHI = cast(a1 %>% select(date, site, AQHI), date~site, value = 'AQHI', fun.aggregate = 'mean')
  column.Last = dim(cast.AQHI)[2]
  cast.AQHI$Ser.Hour = apply(cast.AQHI[2:column.Last], 1, function(x) if(any(x==11, na.rm = T)){return(1)} else{return(0)})
  cast.AQHI$VH.Hour = apply(cast.AQHI[2:column.Last], 1, function(x) if(any(x>=8, na.rm = T)){return(1)} else{return(0)})
  cast.AQHI$Hi.Hour = apply(cast.AQHI[2:column.Last], 1, function(x) if(any(x>=7, na.rm = T)){return(1)} else{return(0)})
  
  serious.Hours = timeAverage(cast.AQHI %>% select(date, Ser.Hour) %>% filter(Ser.Hour == 1),
    avg.time = 'day', statistic = 'frequency', data.thresh = 0) %>% filter(Ser.Hour >= 1)
  vHigh.Hours = timeAverage(cast.AQHI %>% select(date, VH.Hour) %>% filter(VH.Hour == 1),
    avg.time = 'day', statistic = 'frequency', data.thresh = 0) %>% filter(VH.Hour >= 1)
  high.Hours = timeAverage(cast.AQHI %>% select(date, Hi.Hour) %>% filter(Hi.Hour == 1),
    avg.time = 'day', statistic = 'frequency', data.thresh = 0) %>% filter(Hi.Hour >= 1)
  
  cast.AQHI$Ser.Site = apply(cast.AQHI[2:column.Last], 1, function(x) sum(x==11, na.rm = T))
  cast.AQHI$VH.Site = apply(cast.AQHI[2:column.Last], 1, function(x) sum(x>=8, na.rm = T))
  cast.AQHI$Hi.Site = apply(cast.AQHI[2:column.Last], 1, function(x) sum(x>=7, na.rm = T))
  
  serious.Sites = timeAverage(cast.AQHI %>% select(date, Ser.Site) %>% filter(Ser.Site >= 1),
    avg.time = 'day', statistic = 'max', data.thresh = 0) %>% filter(Ser.Site >= 1)
  vHigh.Sites = timeAverage(cast.AQHI %>% select(date, VH.Site) %>% filter(VH.Site >= 1),
    avg.time = 'day', statistic = 'max', data.thresh = 0) %>% filter(VH.Site >= 1)
  high.Sites = timeAverage(cast.AQHI %>% select(date, Hi.Site) %>% filter(Hi.Site >= 1),
    avg.time = 'day', statistic = 'max', data.thresh = 0) %>% filter(Hi.Site >= 1)

  appendNames = c('road.Max', 'gen.Max', 'overall.Max', 'road.Serious.Count', 'gen.Serious.Count', 
    'overall.Serious.Count', 'road.vHigh.Count', 'gen.vHigh.Count', 'overall.vHigh.Count', 'road.High.Count',
    'gen.High.Count', 'overall.High.Count')
  
  c1 = a2[c('date', roadStation, genStation, appendNames)] %>% 
    filter(overall.Max >= episodeAQHI)
  
  c2 = MergeByDate(c1, serious.Hours)
  c2 = MergeByDate(c2, vHigh.Hours)
  c2 = MergeByDate(c2, high.Hours)
  c2 = MergeByDate(c2, serious.Sites)
  c2 = MergeByDate(c2, vHigh.Sites)
  c2 = MergeByDate(c2, high.Sites)
  
  appendNames2 = c('gen.vHigh.Count', 'road.vHigh.Count', 'VH.Site', 'VH.Hour',
                   'gen.Serious.Count', 'road.Serious.Count', 'Ser.Site', 'Ser.Hour',
  'road.Max', 'gen.Max', 'overall.Max', 'overall.Serious.Count', 'overall.vHigh.Count', 'road.High.Count',
  'gen.High.Count', 'overall.High.Count', 'Hi.Hour', 'Hi.Site')
  
  
  c3 = c2[c('date', roadStation, genStation, appendNames2)]
  
  return(c3)
  
}












## -- Function 9 -- ##
LoadPRDHour = function (yearVec = cur_Year, columns = c('date', 'site', 'o3_ug', 'rsp'),
                        file_path = dir_PRD, master_path = dir_Master) {
  
  start_Time = Sys.time()  
  
  setwd(file_path)
  
  prefix = "PRDhour_"
  extension = '.csv'
  array.i = yearVec
  mergeData = MultiFileBind(prefix, array.i, extension)
  
  setwd(master_path)
  
  ### file Log   ### 
  end_Time = Sys.time()  
  elapseTime = as.difftime(end_Time - start_Time, units = "sec")
  LogFile(elapseTime, "LoadAirHour", type.Run = class(LoadAirHour), paste("Loaded", min(yearVec), "-", 
                                                                          max(yearVec), sep = ""))
  ### file Log   ###  
  
  
  ## to format dates
  if (any(columns == 'date')){
    return(selectByDate(mergeData[columns], year = 1900:2100))  
  } else {
    return(mergeData[columns])
  }
  
}




## -- Function 8 -- ##
MergeByDate = function(data1, data2) {
  
  m2 = merge.data.frame(data1, data2, by = 'date', all.x = T)
  return(m2)
  
}





## ------------ END 2 ------------ ##
#-----------------------------------#


## -- Function 7 -- ##
Gen_AAQI = function(date.start = '1/5/2015', date.end = '30/4/2016', dir.AAQI = dir_AAQI, dir.Master = dir_Master){

  year.Select = 2015:2016
  pollutant.pick = c("date", "site", "no2_ug", "rsp", 'fsp')
  site.order = c('CW', 'EN', 'KT', 'SP', 'KC', 'TW', 'TK', 'YL', 
                 'TM', 'TC', 'TP', 'ST', 'MB', 'CB', 'CL', 'MK')
  
  ## Sample Name "AAQI_04-2016.csv"
  output.Name = paste('AAQI_', format.Date(as.Date(date.end, '%d/%m/%Y'), "%m-%Y"),'.csv', sep='')
  
  
  m1 = LoadAirHour(yearVec = year.Select, columns = pollutant.pick)
  a = timeAverage(selectByDate(m1, start = date.start, end = date.end), avg.time = '2 year', 
                  type = 'site', statistic = 'mean', data.thresh = 66.66)
  a2 = timeAverage(selectByDate(m1, start = date.start, end = date.end), avg.time = '2 year', 
                   type = 'site', statistic = 'frequency', data.thresh = 66.66)
  
  a$site = substr(a$site, 1, 2)
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
  
  WriteCSV_ToFolder(file_path = dir.AAQI, master_path = dir.Master, varName = b, fileName = output.Name)
 
}

## -- Function 1 -- ##
DayMax8h.Cal = function(yearVec = cur_Year, cutOFF = 0, data.thresh = 66.66, species = 'o3_ug'){
  
  pollutant.pick = c(species)
  
  txt1 = paste('Generate DayMax 8hr ', species, ' - from ', min(yearVec), " to ", max(yearVec), sep = '')
  print(txt1)
  txt2 = paste('cutoff conc.: ', cutOFF, ' data.thresh: ', data.thresh, sep = '')
  print(txt2)
  
  air.hr = LoadAirHour(yearVec, pollutant.pick)
  
  air.hr = rollingMean(air.hr, pollutant = species, width = 8, 
    data.thresh = data.thresh, align = "right", type = "site") 
  names(air.hr)[4] = 'rolling8'
  
  air.hr = timeAverage(air.hr, type = "site", data.thresh = data.thresh, statistic = "max") %>%
    filter(rolling8 >= cutOFF)
  
  #cast.Value = paste('rolling8', species, sep = '')
  air.hr.cast = cast(air.hr, date~site, value = 'rolling8', fun.aggregate = 'mean', fill = NA)
  
  return(air.hr.cast)
  
}




## -- Function 3 -- ##
LoadAirHour = function (yearVec = cur_Year, pollutant.pick = c('o3_ug', 'no2_ug', 'rsp'),
  file_path = dir_AirHour, master_path = dir_Master) {

start_Time = Sys.time()  

  setwd(file_path)
  
    prefix = "01_airHour"
    extension = '.csv'
    array.i = yearVec
    mergeData = MultiFileBind(prefix, array.i, extension)
  
  setwd(master_path)
  
### file Log   ### 
end_Time = Sys.time()  
elapseTime = as.difftime(end_Time - start_Time, units = "sec")
LogFile(elapseTime, "LoadAirHour", type.Run = class(LoadAirHour), paste("Loaded", min(yearVec), "-", 
  max(yearVec), sep = ""))
### file Log   ###  
  
  columns = c('date', 'site', pollutant.pick)
  ## to format dates
  if (any(columns == 'date')){
    return(selectByDate(mergeData[columns], year = 1900:2100))  
  } else {
    return(mergeData[columns])
  }
  
}


## -- Function 4 -- ##
LoadAQHI = function (yearVec = cur_Year, file_path = dir_AQHI, master_path = dir_Master) {

start_Time = Sys.time()  
  
  setwd(file_path)
  #print(file_path)
  
    listFile = list.files(path = ".", pattern = "*.csv")
    fileName = listFile[length(listFile)]
    print(paste("Read: ", fileName, sep = ""))
  
    a1 = read.csv(fileName)
  
  setwd(master_path)
  
### file Log   ### 
end_Time = Sys.time()  
elapseTime = as.difftime(end_Time - start_Time, units = "sec")
LogFile(elapseTime, "LoadAQHI", type.Run = class(LoadAQHI), paste("Loaded", min(yearVec), "-", 
  max(yearVec), sep = ""))
### file Log   ###

  return(selectByDate(a1, year = yearVec))
  
}


## -- Function 5 -- ##
LoadWind = function (yearVec = cur_Year, load_stations = c('WGL', 'R2C'), 
  file_path = dir_Wind, master_path = dir_Master ) {
  
  setwd(file_path)
  
  wlen = length(load_stations)
  w = list(wlen)
  preFIX = "!_WindData_"
  
  for (i in 1:wlen){
    
    wStation = paste(preFIX, load_stations[i], sep = "")
    w[[i]] = selectByDate(read.csv(paste(wStation, ".csv", sep = "")), year = yearVec)
    
    names(w[[i]])[2] = paste(names(w[[i]])[2], load_stations[i], sep = ".")
    names(w[[i]])[3] = paste(names(w[[i]])[3], load_stations[i], sep = ".")
    glimpse(w[[i]])
    
  }

  ## Match airData and AQHIdata and windData
  m1 = w[[1]][1]
  m2 = merge.data.frame(m1, w[[1]], by = "date", all.x = T)
  
  for (j in 2:length(load_stations)){
    m2 = merge.data.frame(m2, w[[j]], by = "date", all.x = T)
  }
  glimpse(m2)
  
  
  setwd(master_path)
  
  return(m2)
  
}

## -- Function 6 -- ##

## -- Function 0 -- ##
LogFile = function(elapseTime, fun.Run, type.Run, remarks = NA, logEnable = F) {
  
  if(!logEnable){
    return()
  }
  
  curPath = getwd()
  setwd(dir_Master)
  
  logFile <- as.data.frame(x = matrix(nrow = 1, ncol = 6))
  names(logFile) = c("date", "user", "type", "executed", "runTime", "remarks")
  logFile$user = Sys.info()["nodename"]
  logFile$date = format.Date(Sys.time(), "%Y-%m-%d %H:%M")
  logFile$runTime = elapseTime
  logFile$executed = fun.Run
  logFile$type = type.Run
  logFile$remarks = remarks
  
  if(file.exists('!log.csv')) {
    rlogFile = read.csv("!log.csv")
    logFile = rbind.data.frame(rlogFile, logFile)
  } 
  
  write.csv(logFile, "!log.csv", row.names = F)
  
  setwd(curPath)
  
}


#######################################################################################
################################### END of Reviewed ################################### 
#######################################################################################



















### Function 3.2 for load wind Data CIS
LoadWind.cis = function (file_path, master_path, fileName, yearVec = yearVec) {
  
  setwd(file_path)

  w1 = read.csv(fileName)
  w1$ws.WGL = w1$wd.WGL/3.6
  
  setwd(master_path)
  return(selectByDate(w1, year = yearVec))
  
}






### Function 5 Calculate AQHI

Cal_AQHI.ug = function(o3_r3, no2_r3, rsp_r3, so2_r3){
  
  AR = (exp(round(o3_r3,0)*f_o3)-1 + exp(round(no2_r3,0)*f_no2)-1 + exp(round(rsp_r3,0)*f_rsp)-1 + exp(round(so2_r3,0)*f_so2)-1)*100
  
  
  AQHI_Class = AR
  AQHI_Class[which(AR > aqhi_02)] = NA
  AQHI_Class[which(AR > aqhi_01)] = "aqhi_01"
  AQHI_Class[which(AR > aqhi_02)] = "aqhi_02"
  AQHI_Class[which(AR > aqhi_03)] = "aqhi_03"
  AQHI_Class[which(AR > aqhi_04)] = "aqhi_04"
  AQHI_Class[which(AR > aqhi_05)] = "aqhi_05"
  AQHI_Class[which(AR > aqhi_06)] = "aqhi_06"
  AQHI_Class[which(AR > aqhi_07)] = "aqhi_07"
  AQHI_Class[which(AR > aqhi_08)] = "aqhi_08"
  AQHI_Class[which(AR > aqhi_09)] = "aqhi_09"
  AQHI_Class[which(AR > aqhi_10)] = "aqhi_10"
  AQHI_Class[which(AR > aqhi_11)] = "aqhi_11"

  return(AR)
  
}



### Function 6 normalize to range from 0 to 1
Normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}







### Function Replace 'n/a', 'variable' in wind Data


Replace.NA.wd = function(file_path, master_path){
  
  setwd(file_path)
  
  listFile = list.files(path = ".", pattern = "*.csv")
  
  
  for (i in 1:length(listFile)){
    
    wfile = read.csv(listFile[i])
    
    wfile$ws[which(wfile$wd == 'n/a' | wfile$wd == 'Variable')] = NA
    wfile$wd[which(wfile$wd == 'n/a' | wfile$wd == 'Variable')] = NA
    
    wfile$ws[which(wfile$ws == 'n/a' | wfile$ws == 'Variable')] = NA
    wfile$wd[which(wfile$ws == 'n/a' | wfile$ws == 'Variable')] = NA
    
    
    write.csv(wfile, paste(listFile[i], sep=''), row.names = F)
    
  }
  
  setwd(master_path)
  
}




### Function  Plot PollutionRose as polar frequenzy
Rose_Pollution = function(mData, pollutant_input){
  
  fontsizePlot = 28
  colPlot = c("ivory2", "palegreen", "orange", "red", "black")
  
  
  
  ########### switch case of pollutants
  switch(pollutant_input,
    o3 = list("o3_ug", "o3", c(seq(0,180,20)), "o3 (ug/m3)"),
    no2 = list("no2_ug", "NO2", c(seq(0,250,25)), "NO2 (ug/m3)"),
    rsp = list("rsp", "RSP", c(seq(0,250,25)), "RSP (ug/m3)"),
    nox = list("nox_ug", "NOx", c(seq(0,400,40)), "NOx (ug/m3)"),
    so2 = list("so2_ug", "SO2", c(seq(0,100,10)), "SO2 (ug/m3)"),
    fsp = list("fsp", "FSP", c(seq(0,150,15))), "FSP (ug/m3)")

  
  pollutantPlot = "nox_ug"
  pollutantName = "NOx"
  brkVec = c(seq(0,400,40))
  legTxt = "NOx (ug/m3)"
  
  pollutantPlot = "so2_ug"
  pollutantName = "SO2"
  brkVec = c(seq(0,100,10))
  legTxt = "SO2 (ug/m3)"
  
  pollutantPlot = "fsp"
  pollutantName = "FSP"
  brkVec = c(seq(0,150,15))
  legTxt = "FSP (ug/m3)"
  
  ########### switch case of pollutants
  
  
  
  p1 = polarFreq(selectByDate(mData[which(mData$site == siteName),], month = c(1:12), year = 2011:2015,
    hour = c(0:23)), pollutant = pollutantPlot, type = "default", grid = 5, annotate = F, offset = 0,
    angle = 10, statistic = "mean", cols = colPlot, min.bin = 1, 
    breaks = brkVec, par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent",
    ws.int = 0.7)
  
  wdLIM = 25
  
  p1$plot$y.limits = c(-wdLIM, wdLIM)
  p1$plot$x.limits = c(-wdLIM, wdLIM)
  p1$plot$aspect.ratio = 1
  
  
  p1$plot$legend$right$args$key$footer = legTxt
  p1$plot$legend$right$args$key$header = ""
  p1$plot$xlab = expression(Wind ~ Speed ~ "("~m~s^-1~")" )
  
  p1$plot
  
  xpos = wdLIM*0.75
  ypos = xpos*0.8
  
  trellis.last.object() + layer(ltext(xpos, ypos, srt = 45,
  expression(Wind ~ Speed ~ "("~m~s^-1~")" ), cex = 0.65))
  
  
  print(pollutantPlot)
  print(pollutantName)

  
}

































