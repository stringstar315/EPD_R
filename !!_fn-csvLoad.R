##############################################################################################################
####################                List of Functions for Loading CSV Data                ####################
##############################################################################################################

##   1. LoadAirHour = function (yearVec = cur_Year, pollutant.pick = c('o3_ug', 'no2_ug', 'rsp'))
##   2. 
##   3. LoadWind = function (yearVec = cur_Year, load_stations = c('WGL', 'R2C'))
##   4. LoadPRD.DayMax = function (yearVec = cur_Year, pollutant.pick = c('o3_ug', 'no2_ug', 'rsp'))
##   5.
##   6.
##   7.
##   8.
##   9.
##  10.
##  11.
##  12.






##############################################################################################################
####################                       Function 1. LoadAirHour                        ####################
##############################################################################################################

LoadAirHour = function (yearVec = cur_Year, pollutant.pick = c('o3_ug', 'no2_ug', 'rsp')){
                        
  setwd(dir_airHK)
  
  prefix = "airhour"
  extension = '.csv'
  array.i = yearVec
  mergeData = MultiFileBind(prefix, array.i, extension)
  
  setwd(dir_Master)

  return(selectByDate(mergeData[c('date', 'site', pollutant.pick)], year = 1990:2100))

}


##############################################################################################################
####################                         Function 2. LoadAQHI                         ####################
##############################################################################################################

LoadAQHI = function (yearVec = cur_Year, fileName = 'aqhi.csv') {
  
  setwd(dir_airHK)

  a1 = read.csv(fileName, na.strings = '\\N')
  
  setwd(dir_Master)

  return(selectByDate(a1, year = yearVec))
  
}


##############################################################################################################
####################                         Function 3. LoadWind                         ####################
##############################################################################################################

LoadWind = function (yearVec = cur_Year, load_stations = c('WGL', 'R2C')) { 

  setwd(dir_windHK)
  
  wlen = length(load_stations)
  w = list(wlen)
  preFIX = "!_WindData_"
  
  for (i in 1:wlen){
    
    wStation = paste(preFIX, load_stations[i], sep = "")
    w[[i]] = selectByDate(read.csv(paste(wStation, ".csv", sep = "")), year = yearVec)
    
    names(w[[i]])[2] = paste(names(w[[i]])[2], load_stations[i], sep = "_")
    names(w[[i]])[3] = paste(names(w[[i]])[3], load_stations[i], sep = "_")
    glimpse(w[[i]])
    
  }
  
  ## Match airData and AQHIdata and windData
  m1 = w[[1]][1]
  m2 = merge.data.frame(m1, w[[1]], by = "date", all.x = T)
  
  for (j in 2:length(load_stations)){
    m2 = merge.data.frame(m2, w[[j]], by = "date", all.x = T)
  }
  glimpse(m2)
  
  
  setwd(dir_Master)
  
  return(m2)
  
}


##############################################################################################################
####################                       Function 4. LoadPRDHour                        ####################
##############################################################################################################

LoadPRD.DayMax = function (yearVec = cur_Year, pollutant.pick = c('o3_ug', 'no2_ug', 'rsp')) {

  setwd(dir_airPRD)
  
  prefix = "airPRD"
  extension = '.csv'
  array.i = yearVec
  mergeData = MultiFileBind(prefix, array.i, extension)
  
  setwd(dir_Master)
  
  return(selectByDate(mergeData[c('date', 'site', pollutant.pick)], year = 1900:2100))

}







































