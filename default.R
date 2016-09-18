start_Time = Sys.time()

source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')

setwd(dir_Master)

## 1.1. Load and combine hourly air data


#pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp", "fsp", 'co')

air.hr = LoadAirHour(dir_AirHour, dir_Master, airYear, pollutant.pick)
air.dayAvg = as.data.frame(timeAverage(air.hr, avg.time = "day", statistic = "mean", type = "site"))
#air.dayMax = as.data.frame(timeAverage(air.hr, avg.time = "day", statistic = "max", type = "site"))



## 1.2. Load AQHI data
AQHI.hr = LoadAQHI(dir_AQHI, dir_Master, 2013:2020)
AQHI.dayMax = as.data.frame(timeAverage(AQHI.hr %>% select(date, site, siteMax), avg.time = "day", 
                                        statistic = "max", type = "site"))

## 1.3 Match airData and AQHIdata
m1 = merge.data.frame(air.hr %>% mutate(tag = paste(date, site)), 
  AQHI.hr %>% mutate(tag = paste(date, site)) %>% select(-c(date, site)), by = "tag", all.x = T) %>%
  select(-c(tag))

### print time used and log
end_Time = Sys.time()
elapseTime = as.difftime(end_Time - start_Time, units = "sec")
print(round(elapseTime, 1))
LogFile(elapseTime, "default.R", type.Run = 'source')
## end logging

# ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
