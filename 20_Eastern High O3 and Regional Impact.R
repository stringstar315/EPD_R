logEnable = T
source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')


#### ---------- Input ------------- ###
year.Select = 2015:2016

pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", 
  "so2_ug", "rsp", "fsp")


#### ---------- Input ------------- ###
#### ---------- Input ------------- ###

## Sample Name "AAQI_04-2016.csv"
output.Name = paste('AAQI_', format.Date(as.Date(date.end, '%d/%m/%Y'), "%m-%Y"),'.csv', sep='')


m1 = LoadAirHour(dir_AirHour, dir_Master, year.Select, pollutant.pick)


wStation.pick = c("GI", "SF", "KP", "R2C", "SE", "WGL", "CL", "MK", 'NP')
w1 = LoadWind(dir_Wind, dir_Master, 2011:2016, wStation.pick)
























