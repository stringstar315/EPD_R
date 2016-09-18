###### ---- NO2 CL v CB ---- ######
# \Dropbox\01_Documents\05_EPD\21_Project Studies\!_Analysis\2016_Compare Roadside NO2

source('!!_Setup.R')
source('!!_Functions.R')

## -- Setup Input -- ##
pollutant.pick = c("date", "site", "o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp")
airYear = 2012:2016
wStation.pick = c("R2C", "WGL", 'GI')

## -- Read Data In -- ##
m1 = LoadAirHour(yearVec = airYear, columns = pollutant.pick)
a1 = LoadAQHI(yearVec = airYear)

# Merge AQHI and AirHour
m1 = merge.data.frame(m1 %>% mutate(tag = paste(date, site)), 
  a1 %>% mutate(tag = paste(date, site)) %>% select(-c(date, site)), by = "tag", all.x = T) %>%
  select(-c(tag))

w1 = LoadWind(yearVec = airYear, load_stations = wStation.pick)



##########################################################################################################

m2 = m1 %>% filter(site == 'CL_HK' | site == 'CB_HK')
m2$site = substr(m2$site, 1, 2)

no2.Cast = cast(m2, date~site, value = 'no2_ug', fun.aggregate = 'mean', na.rm = T)
no2.Cast$diff = no2.Cast$CL - no2.Cast$CB
no2.50 = no2.Cast %>% filter(diff > 50) %>% select(date, diff)
no2.0 = no2.Cast %>% filter(diff > 0) %>% select(date, diff)
m2 = merge(m2, no2.50, by = 'date', all.x = T)
m2 = merge(m2, no2.0, by = 'date', all.x = T)
temp1 = names(m2)
temp1[length(temp1)] = 'diff.0'
temp1[length(temp1)-1] = 'diff.50'
names(m2) = temp1


m2 = merge.data.frame(m2, w1, by = 'date', all = T)

WriteCSV_ToFolder(dir_RoadNO2, dir_Master, m2, "p1-NO2 Differences.csv")



## 1. Select the hours with CL > CB by 40 and 50
m2$site = substr(m2$site, 1, 2)
m2 = m2 %>% filter(site == "CB" | site == "CL" | site == "MK") %>% 
  select(-so2_ug,-fsp,-siteMax,-AQHI)

no2_cast = cast(m2, date~site, value = 'no2_ug', fun.aggregate = 'mean', na.rm = T)
no2_cast$diff = no2_cast$CL - no2_cast$CB
no2.50 = no2_cast %>% filter(diff > 50) %>% select(date, diff)
no2.40 = no2_cast %>% filter(diff > 40) %>% select(date, diff)
m2 = merge(m2, no2.50, by = 'date', all.x = T)
m2 = merge(m2, no2.40, by = 'date', all.x = T)
temp1 = names(m2)
temp1[length(temp1)] = 'diff.40'
temp1[length(temp1)-1] = 'diff.50'
names(m2) = temp1
WriteCSV_ToFolder(dir_RoadNO2, dir_Master, m2, "p3_diff_CBvCL_hr.csv")






no2_cast.day = as.data.frame(timeAverage(no2_cast %>% select(date, diff), avg.time = 'day', 
  statistic = 'max'))
no2.day.50 = no2_cast.day %>% filter(diff > 50)
no2.day.50$date = format.Date(no2.day.50$date, '%Y-%m-%d')
no2.day.40 = no2_cast.day %>% filter(diff > 40)
no2.day.40$date = format.Date(no2.day.40$date, '%Y-%m-%d')






m2$date2 = format.Date(m2$date, '%Y-%m-%d')
m2 = merge(m2, no2.day.50, by.x = 'date2', by.y = 'date', all.x = T)

m2 = merge(m2, no2.50, by = 'date', all.x = T)

WriteCSV_ToFolder(dir_RoadNO2, dir_Master, m2, "diff50.csv")



which(m2$diff > 0)








plot(no2_cast.day)

mean(no2_cast.day$diff, na.rm = T)

mean(no2_cast$diff[which(format.Date(no2_cast$date, '%Y') == 2016)], na.rm = T)































