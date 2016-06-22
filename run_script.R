
source('setup.R')
source('functions.R')
source('parameters.R')

setwd(dir_Master)

# ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
# ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
# 1. Load and Combine Data


####  GI = Green Island
####  HKO = HKO
####  KP = King's Park
####  R2C = Airport
####  SE = Kai Tak
####  SHA = ShaTin
####  SHL = TsingYi Shell
####  TAP = Tap Mun
####  TUN = Tuen Mun
####  WGL = Wagland
####  JKB = TKO
####  SKG = Sai Kung
####  NP = North Point
####  SF = Star Ferry
####  TC = Tate's Carin
####  LFS = Lau Fau Shan
####  TMS = Tai Mo Shan


## Load and combine hourly air data
air_Vec = c("date", "site","o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp", "fsp")
r1 = LoadAirHour(dir_AirHour, dir_Master, 2013:2016, air_Vec)

## Load AQHI data
a1 = LoadAQHI(dir_AQHI, dir_Master, 2013:2016)


## Match airData and AQHIdata
m1 = merge.data.frame(r1 %>% mutate(tag = paste(date, site)), 
  a1 %>% mutate(tag = paste(date, site)) %>% select(-c(date, site)), by = "tag", all.x = T) %>%
    select(-c(tag))


# Load Wind Data
wind_Vec = c("R2C", "TUN", "LFS", "SF", "KP", "HKO", "SE", "SHA", "WGL")
w1 = LoadWind(dir_Wind, dir_Master, 2014:2016, wind_Vec)

## Match airData and AQHIdata and windData
m2 = merge.data.frame(m1, w1[[1]], by = "date", all.x = T)

for (j in 2:length(wind_Vec)){
  m2 = merge.data.frame(m2, w1[[j]], by = "date", all.x = T)
}

# Load Data End
# ********************************************************************************
# ******************************************************************************** 



p1 = timePlot(selectByDate(m1, year = 2014, month = 2) %>% filter(AQHI >= 7), 
        pollutant = "rsp", avg.time = "hour", type = "site")



# Wind Roses

p1 = windRose(m2 %>% filter(AQHI >= 7, site == "TC_HK"), type = "month", angle = 45, paddle = F, 
  breaks = ws.Class)





# ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
# ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
# 2. Compare East and West Seriousness by AQHI

## 2a) AQHI day Max Summary
cutAQHI = m1 %>% select(date, site, siteMax) %>% filter(siteMax >= 7) %>% 
  mutate(Year = as.numeric(format(date, "%Y")), Month = as.numeric(format(date, "%m")))
daySummary = timeAverage(cutAQHI, avg.time = "day", statistic = "max", type = "site") %>% filter(siteMax > 0)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, daySummary, "dayMaxAQHI.csv")

cutAQHI = m1 %>% select(date, site, AQHI) %>% filter(AQHI >= 7) %>% 
  mutate(Year = as.numeric(format(date, "%Y")), Month = as.numeric(format(date, "%m")))
hourSummary = timeAverage(cutAQHI, avg.time = "hour", statistic = "max", type = "site") %>% filter(AQHI > 0)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, hourSummary, "hrMaxAQHI.csv")
## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------ 


# 2 Compare East and West Seriousness by 8h-dayMax O3
cutO3_AQO = m1 %>% select(date, site, o3_ug)
cutO3_AQO = rollingMean(cutO3_AQO, pollutant = "o3_ug", width = 8, 
                        data.thresh = 66.66, align = "right", type = "site")  
cutO3_AQO = timeAverage(cutO3_AQO, type = "site", data.thresh = 66.66, statistic = "max") %>%
  filter(rolling8o3_ug >= 80)
View(cutO3_AQO)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, cutO3_AQO, "dayMax8hO3.csv")
## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------



## 3 Risk Contribution to %AR
## comments:
  #1. AQHI is calculated by raw three hour average
  #2. Calcualtions here used validated data and no surrogate is applied
  #3. 3-hr average is only used for calcualtion of %Risk but not for the actual conc. calculation of that hour 

roll3hr = m1
roll3hr = rollingMean(roll3hr, pollutant = c("o3_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_O3")
roll3hr = rollingMean(roll3hr, pollutant = c("no2_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_NO2")
roll3hr = rollingMean(roll3hr, pollutant = c("so2_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_SO2")
roll3hr = rollingMean(roll3hr, pollutant = c("rsp"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_RSP")
roll3hr = rollingMean(roll3hr, pollutant = c("nox_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_NOx")
roll3hr = selectByDate(roll3hr, year = 2014:2016)


risk_hourly = roll3hr %>% 
  mutate(rO3 = (exp(f_o3*r3_O3)-1)*100, rNO2 = (exp(f_no2*r3_NO2)-1)*100, rSO2 = (exp(f_so2*r3_SO2)-1)*100, 
  rRSP = (exp(f_rsp*r3_RSP)-1)*100, contri_O3 = round(rO3/(rO3+rNO2+rSO2+rRSP)*100, 1), 
  contri_NO2 = round(rNO2/(rO3+rNO2+rSO2+rRSP)*100,1), contri_SO2 = round(rSO2/(rO3+rNO2+rSO2+rRSP)*100,1), 
  contri_RSP = round(rRSP/(rO3+rNO2+rSO2+rRSP)*100,1), Ox_ppb = round(r3_O3/1.996464+r3_NO2/1.913278,1))

vec1 = c("O3", "NO2", "SO2", "RSP")
risk_hourly$dom = apply(risk_hourly[,c("contri_O3", "contri_NO2", "contri_SO2", "contri_RSP")], 1, 
  function(x) vec1[match(max(x), x, incomparables = NA, nomatch = NA)])

risk_hourly_Dup_11 = risk_hourly %>% filter(AQHI == 11)
risk_hourly_Dup_11$AQHI = "Dup_11"
risk_hourly_Dup_11$siteMax = "Dup_11"

risk_hourly = rbind.data.frame(risk_hourly, risk_hourly_Dup_11)
risk_hourly[c("fsp", "r3_O3", "r3_NO2", "r3_SO2", "r3_RSP", "r3_NOx", "rO3", "rNO2", "rSO2", "rRSP")] = NULL

WriteCSV_ToFolder(dir_West_v_East, dir_Master, risk_hourly[c("date", "site", "AQHI", "siteMax", 
  "contri_O3", "contri_NO2", "contri_SO2", "contri_RSP", "o3_ug", "no2_ug", "so2_ug","rsp", "nox_ug", "Ox_ppb", "dom")], "risk_hourly.csv")
## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   




## 2b2) Risk Contribution to %AR
roll3hr = m2
roll3hr = rollingMean(roll3hr, pollutant = c("o3_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_O3")
roll3hr = rollingMean(roll3hr, pollutant = c("no2_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_NO2")
roll3hr = rollingMean(roll3hr, pollutant = c("so2_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_SO2")
roll3hr = rollingMean(roll3hr, pollutant = c("rsp"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_RSP")
roll3hr = rollingMean(roll3hr, pollutant = c("nox_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_NOx")

risk_hourly = roll3hr %>%  
  mutate(rO3 = (exp(f_o3*r3_O3)-1)*100, rNO2 = (exp(f_no2*r3_NO2)-1)*100, rSO2 = (exp(f_so2*r3_SO2)-1)*100, 
  rRSP = (exp(f_rsp*r3_RSP)-1)*100, contri_O3 = round(rO3/(rO3+rNO2+rSO2+rRSP)*100, 1), 
  contri_NO2 = round(rNO2/(rO3+rNO2+rSO2+rRSP)*100,1), contri_SO2 = round(rSO2/(rO3+rNO2+rSO2+rRSP)*100,1), 
  contri_RSP = round(rRSP/(rO3+rNO2+rSO2+rRSP)*100,1), Ox_ppb = round(r3_O3/1.996464+r3_NO2/1.913278,1))

names(risk_hourly[c("r3_O3", "r3_NO2", "r3_SO2", "r3_RSP", "r3_NOx")]) = 
  c("o3_ug", "no2_ug", "so2_ug", "rsp", "nox_ug")

vec1 = c("O3", "NO2", "SO2", "RSP")
risk_hourly$dom = apply(risk_hourly[,c(38:41)], 1, 
  function(x) vec1[match(max(x), x, incomparables = NA, nomatch = NA)])

risk_hourly$siteDom = paste(risk_hourly$site, risk_hourly$dom, sep = "_")
risk_WindSpeed = timeAverage(risk_hourly %>% filter(AQHI >= 7), avg.time = "month", type = "siteDom")
WriteCSV_ToFolder(dir_West_v_East, dir_Master, risk_WindSpeed, "risk_speed.csv")


p1 = windRose(risk_hourly %>% filter(AQHI >= 7, site == "TM_HK"), ws = "ws_R2C", wd = "wd_R2C", 
  type = "dom", 
  breaks = ws.Class, paddle = F)




WriteCSV_ToFolder(dir_West_v_East, dir_Master, risk_hourly[c("date", "site", "AQHI", "siteMax", "contri_O3",
  "contri_NO2", "contri_SO2", "contri_RSP", "o3_ug", "no2_ug", "so2_ug", "rsp", "nox_ug", "Ox_ppb", 
  "dom")], "risk_hourly.csv")





aa = apply(risk_hourly[1:10,c(14:17)], 1, function(x) vec1[match(max(x), x, incomparables = NA, nomatch = NA)])
aa



bb = apply(risk_hourly[1:10,c(14:17)], 1, function(x) max(x))

bb


risk_hourly$Max = apply(risk_hourly %>% select(rO3, rNO2, rRSP, rSO2), 1, max)
risk_hourly$Rank = NA

for (i in 1:dim(risk_hourly)[1]){
  Rank1[i] = match(risk_hourly[i,c("Max")], risk_hourly[i,c(10:13)])
  print(i)
}





siteAvg = timeAverage(Hourly_RC %>% select(date, site, contri_O3, contri_NO2, contri_SO2, contri_RSP), 
  avg.time = "year", data.thresh = 0, type = "site", statistic = "mean")
WriteCSV_ToFolder(dir_West_v_East, dir_Master, siteAvg, "siteAvg.csv")


risk_DayAvg = timeAverage(Hourly_RC, avg.time = "day", data.thresh = 66.6, type = "site", statistic = "mean")

WriteCSV_ToFolder(dir_West_v_East, dir_Master, risk_DayAvg %>% select(date, site, AQHI, siteMax, contri_O3,
  contri_NO2, contri_SO2, contri_RSP, o3_ug, no2_ug, so2_ug, rsp, nox_ug, Ox_ppb), "risk_DayAvg.csv")




test_1 = pollutant_RC %>% filter(AQHI >=7)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, test_1, "test_1.csv")


test2 = m1 %>% select(date, site, o3_ug, AQHI) %>% filter(AQHI >= 7)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, test2, "test_2.csv")

setwd(dir_Master)











# West and East AQHI Counts
cutAQHI = m1 %>% select(date, site, AQHI, siteMax) %>% filter(siteMax >= 7) %>% 
  mutate(Year = as.numeric(format(date, "%Y")))
View(cutAQHI)

daySummary = timeAverage(cutAQHI %>% select(-c(AQHI)), 
  avg.time = "day", statistic = "max", type = "site")
c1 = cast(daySummary, formula = date ~ site, value = "siteMax")
WriteCSV_ToFolder(dir_West_v_East, dir_Master, daySummary, "dayMaxAQHI.csv")





# to find 8hO3 = xxx ug/m3, how many % aqhi 7+
cutO3_AQO = m1 %>% select(date, site, o3_ug, siteMax)
cutO3_AQO = rollingMean(cutO3_AQO, pollutant = "o3_ug", width = 8, 
  data.thresh = 66.66, align = "right", type = "site")  
cutO3_AQO = timeAverage(cutO3_AQO, type = "site", data.thresh = 66.66, statistic = "max", avg.time = "day")

O3_rolling_gped = cutO3_AQO %>% mutate(site2 = "East")
O3_rolling_gped$site2[which(O3_rolling_gped$site == "TC_HK")] = "West"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "TM_HK")] = "West"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "YL_HK")] = "West"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "CB_HK")] = "Road"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "CL_HK")] = "Road"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "MK_HK")] = "Road"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "MB_HK")] = "Rural"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "CW_HK")] = "Urban"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "SP_HK")] = "Urban"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "KC_HK")] = "Urban"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "TW_HK")] = "Urban"
O3_rolling_gped$site2[which(O3_rolling_gped$site == "TK_HK")] = "Urban2"

O3_rolling_gped = timeAverage(O3_rolling_gped, type = "site2", data.thresh = 0, statistic = "max", avg.time = "day")
O3_rolling_allGen = timeAverage(O3_rolling_gped %>% filter(site2 != "Road" & site2 != "Urban2" &
  site2 != "Rural"), data.thresh = 0, statistic = "max", avg.time = "day")

WriteCSV_ToFolder(dir_West_v_East, dir_Master, O3_rolling_allGen, "O3_rolling_allGen.csv")




O3_WestandEast = cutO3_AQO %>% filter(site == c("TC_HK") | site == c("TM_HK") | site == c("YL_HK") | 
  site == c("EN_HK") | site == c("KT_HK") | site == c("TP_HK") | site == c("ST_HK"))





O3_rolling_gped = timeAverage(O3_rolling_gped, type = "site2", data.thresh = 0, statistic = "max", avg.time = "day")







table(O3_WestandEast$site)
table(O3_WestandEast$site, O3_WestandEast$siteMax)

table(cutO3_AQO$site)



# to compute NO2
cutNO2 = m1 %>% select(date, site, no2_ug) %>% filter(no2_ug >= 200.5) %>% 
  mutate(Year = as.numeric(format(date, "%Y"))) 
View(cutNO2)

table(cutNO2$site, cutNO2$Year)

NO2Summary = timeAverage(cutAQHI %>% select(-c(AQHI)), 
  avg.time = "day", statistic = "max", type = "site")
c1 = cast(daySummary, formula = date ~ site, value = "siteMax")
WriteCSV_ToFolder(dir_West_v_East, dir_Master, daySummary, "dayMaxAQHI.csv")




# RSP

rsp_dist = m1 %>% select(date, site, AQHI, siteMax, rsp) %>% mutate(Year = as.numeric(format.Date(date, "%Y"))) %>% 
  filter(rsp >= 150 & rsp <330)
table(rsp_dist$site, rsp_dist$AQHI, rsp_dist$Year)



# Compare East and West End
# ********************************************************************************
# ******************************************************************************** 




setwd(dir_Master)


