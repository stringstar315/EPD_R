
source('!!_Setup.R')
source('!!_Functions.R')
source('!!_parameters.R')

## -- Setup Input -- ##
airYear = 2015:2016
pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp", "fsp", 'co_ug')

m1 = LoadAirHour(yearVec = airYear, columns = pollutant.pick)

m2 = m1 %>% mutate(no_ug = (nox_ug - no2_ug)/p_NO2*p_NO)
m2$site = substr(m2$site, 1, 2)
m2$site2 = 'General'
m2$site2[which(m2$site == 'CB' | m2$site == 'CL' | m2$site == 'MK')] = 'Roadside'

m2$year = format.Date(m2$date, "%Y")
m2$month = as.numeric(format.Date(m2$date, "%m"))
m2$quarter = ceiling(m2$month/3)

m3 = cast(m2 %>% filter(site != 'MB'), quarter~site+year, value = 'o3_ug', fun.aggregate = 'mean', na.rm= T)









WriteCSV_ToFolder(dir_StatSummary, dir_Master, m2, "m2.csv")
#### -------------------------------------------------------------



m3 = LoadAQHI(dir_AQHI, dir_Master, 2015:2016)
m3$site = substr(m3$site, 1, 2)
m4 = cast(m3, date~site, value = 'AQHI')
m5 = cast(timeAverage(m3, avg.time = 'day', type = 'site'), date~site, value = 'siteMax')


WriteCSV_ToFolder(dir_StatSummary, dir_Master, m4, "m4_AQHI_hr.csv")
WriteCSV_ToFolder(dir_StatSummary, dir_Master, m5, "m5_AQHI_day.csv")
