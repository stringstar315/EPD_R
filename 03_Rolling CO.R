###### ---- Rolling NOx for roadside ---- ######

source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')
logEnable = T

### -- Need to FIll inside #############################################
rollHours = 8760 #hours of rolling
pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", 'co_ug') # pollutants to pick
airYear = 2010:2016 # year to be looked at
### -- Need to FIll inside #############################################


air.hr = LoadAirHour(dir_AirHour, dir_Master, airYear, pollutant.pick)
m2 = air.hr

m2$site = substr(m2$site, 1, 2)

## TK is excluded
m2$site2 = 'Roadside'
m2$site2[which(m2$site == 'CW')] = 'General'
m2$site2[which(m2$site == 'EN')] = 'General'
m2$site2[which(m2$site == 'KT')] = 'General'
m2$site2[which(m2$site == 'KC')] = 'General'
m2$site2[which(m2$site == 'TW')] = 'General'
m2$site2[which(m2$site == 'SP')] = 'General'
m2$site2[which(m2$site == 'MB')] = 'General'
m2$site2[which(m2$site == 'TP')] = 'General'
m2$site2[which(m2$site == 'ST')] = 'General'
m2$site2[which(m2$site == 'YL')] = 'General'
m2$site2[which(m2$site == 'TM')] = 'General'
m2$site2[which(m2$site == 'TC')] = 'General'

m2 = timeAverage(m2 %>% mutate(no_ug = (nox_ug - no2_ug)/p_NO2*p_NO)
  , avg.time = 'hour', statistic = 'mean', type = 'site2') 


m3 = rollingMean(m2, pollutant = "o3_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site2')
m3 = rollingMean(m3, pollutant = "nox_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site2')
m3 = rollingMean(m3, pollutant = "no2_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site2')
m3 = rollingMean(m3, pollutant = "no_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site2')
m3 = rollingMean(m3, pollutant = "co_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site2')

m3$year = format.Date(m3$date, '%Y')
airYear2 = airYear
airYear2[1] = airYear2[1]+1
m3 = selectByDate(m3, year = airYear2)
rm(airYear2)

m4a = cast(m3, formula = date~site2, value = paste('rolling', rollHours, 'o3_ug', sep = ''))
names(m4a) = c('date', 'Gen_O3', "Road_O3")

m4b = cast(m3, formula = date~site2, value = paste('rolling', rollHours, 'nox_ug', sep = ''))
names(m4b) = c('date', 'Gen_NOX', "Road_NOX")
m4b = m4b %>% select(-date)

m4c = cast(m3, formula = date~site2, value = paste('rolling', rollHours, 'no2_ug', sep = ''))
names(m4c) = c('date', 'Gen_NO2', "Road_NO2")
m4c = m4c %>% select(-date)

m4d = cast(m3, formula = date~site2, value = paste('rolling', rollHours, 'no_ug', sep = ''))
names(m4d) = c('date', 'Gen_NO', "Road_NO")
m4d = m4d %>% select(-date)

m4e = cast(m3, formula = date~site2, value = paste('rolling', rollHours, 'co_ug', sep = ''))
names(m4e) = c('date', 'Gen_CO', "Road_CO")

m4 = cbind.data.frame(m4a, m4b, m4c, m4d, m4e)

m5 = m4[c('date',	'Gen_O3', "Road_O3", 'Gen_NOX', "Road_NOX", 'Gen_NO2', "Road_NO2",
  'Gen_NO', "Road_NO", 'Gen_CO', "Road_CO")]
names(m5) = c('date',	'General_O3', "Roadside_O3", 'General_NOX', "Roadside_NOX", 'General_NO2', 
              "Roadside_NO2", 'General_NO', "Roadside_NO", 'General_CO', "Roadside_CO")

WriteCSV_ToFolder(dir_RollingCO, dir_Master, m5, "rolling.csv")



airYear2 = airYear
airYear2[1] = airYear2[1]+1
m3 = selectByDate(m3, year = airYear2)
rm(airYear2)

m4a = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'o3_ug', sep = ''))
names(m4a) = c('date', 'CB_O3', "CL_O3", "MB_O3", "MK_O3", "TC_O3")

m4b = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'nox_ug', sep = ''))
names(m4b) = c('date', 'CB_NOX', "CL_NOX", "MB_NOX", "MK_NOX", "TC_NOX")
m4b = m4b %>% select(-date)

m4c = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'no2_ug', sep = ''))
names(m4c) = c('date', 'CB_NO2', "CL_NO2", "MB_NO2", "MK_NO2", "TC_NO2")
m4c = m4c %>% select(-date)

m4d = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'no_ug', sep = ''))
names(m4d) = c('date', 'CB_NO', "CL_NO", "MB_NO", "MK_NO", "TC_NO")
m4d = m4d %>% select(-date)

m4e = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'co_ug', sep = ''))
names(m4e) = c('date', 'CB_CO', "CL_CO", "MB_CO", "MK_CO", "TC_CO")
m4e = m4e %>% select(-date)

m4 = cbind.data.frame(m4a, m4b, m4c, m4d, m4e)

m5 = m4[c('date',	'MB_O3',	'TC_O3',	'CB_NOX',	'CL_NOX',	'MK_NOX',	'CB_NO2',	'CL_NO2',
  'MK_NO2',	'CB_NO',	'CL_NO',	'MK_NO', 'CB_CO',	'CL_CO',	'MK_CO')]


WriteCSV_ToFolder(dir_RollingNO2, dir_Master, m5, "rolling.csv")


