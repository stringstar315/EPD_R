
source('!!_Setup.R')
source('!!_Functions.R')
source('!!_parameters.R')

## -- Setup Input -- ##
pollutant.pick = c("o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp") ##HK_Data

airYear = 2015:2016
wStation.pick = c("R2C", "WGL", "SF", "SE")

## -- Read Data In -- ##
m1 = LoadAirHour(yearVec = airYear, pollutant.pick = pollutant.pick)
a1 = LoadAQHI(yearVec = airYear)
p1 = LoadPRDHour(yearVec = 2015:2016, columns = c('date', 'site', 'o3_ug', 'rsp'))

m1 = merge.data.frame(m1 %>% mutate(tag = paste(date, site)), 
  a1 %>% mutate(tag = paste(date, site)) %>% select(-c(date, site)), by = "tag", all.x = T) %>%
  select(-c(tag))

w1 = LoadWind(yearVec = airYear, load_stations = wStation.pick)

m2 = MergeByDate(m1, w1)
##########################################################################################################
##########################################################################################################
##########################################################################################################



## 1.1 Analysis of High or Above Days in 2015-2016
## a) Calculation the 3hr rolling mean for pollutants --> Than find out the risk contribution to AQHI
## b) Mark X to the date with a specified AQHI, 7 in this case


roll3hr = selectByDate(m2, year = 2014:2016)
roll3hr$site = substr(roll3hr$site, 1, 2)

roll3hr = rollingMean(roll3hr, pollutant = c("o3_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_O3")
roll3hr = rollingMean(roll3hr, pollutant = c("no2_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_NO2")
roll3hr = rollingMean(roll3hr, pollutant = c("so2_ug"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_SO2")
roll3hr = rollingMean(roll3hr, pollutant = c("rsp"), align = "right", width = 3,
                      data.thresh = 66.6, type = "site", new.name = "r3_RSP")
roll3hr = as.data.frame(selectByDate(roll3hr, year = 2000:2030))

m3 = roll3hr %>% 
  mutate(rO3 = (exp(f_o3*r3_O3)-1)*100, rNO2 = (exp(f_no2*r3_NO2)-1)*100, rSO2 = (exp(f_so2*r3_SO2)-1)*100, 
         rRSP = (exp(f_rsp*r3_RSP)-1)*100, contri_O3 = round(rO3/(rO3+rNO2+rSO2+rRSP), 3), 
         contri_NO2 = round(rNO2/(rO3+rNO2+rSO2+rRSP),3), contri_SO2 = round(rSO2/(rO3+rNO2+rSO2+rRSP),3), 
         contri_RSP = round(rRSP/(rO3+rNO2+rSO2+rRSP),3))

aqhi.Select = 7
episodeDayHours = m3 %>% filter(AQHI >= aqhi.Select) %>% select(date) %>% distinct()
episodeDayHours$Mark = 'X'

m4 = MergeByDate(m3, episodeDayHours) %>% filter(Mark == "X")

## Then Pick the Day Max Values relevant for analysis

##Local ozone Picks -- NTEast Exclude MB
m3 = m4 ###use only filter data

O3DayMax.NTWest = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'TC' | site == 'TM' | site == 'YL'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.NTWest)[2] = 'O3DayMax.NTWest'

O3DayMax.NTEast = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'TP' | site == 'ST' | site == 'EN'| site == 'KT'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.NTEast)[2] = 'O3DayMax.NTEast'

O3DayMax.MB = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'MB'), statistic = 'max',
  data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.MB)[2] = 'O3DayMax.MB'

O3DayMax.Road = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'CB' | site == 'CL' | site == 'MK'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.Road)[2] = 'O3DayMax.Road'

O3_75p.Road = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'CB' | site == 'CL' | site == 'MK'), 
  statistic = 'percentile', percentile = 75, data.thresh = 0, avg.time = 'day', type = 'default')
names(O3_75p.Road)[2] = 'O3_75p.Road'
O3_50p.Road = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'CB' | site == 'CL' | site == 'MK'), 
  statistic = 'percentile', percentile = 50, data.thresh = 0, avg.time = 'day', type = 'default')
names(O3_50p.Road)[2] = 'O3_50p.Road'
O3_25p.Road = timeAverage(m3 %>% select(date, site, o3_ug) %>% filter(site == 'CB' | site == 'CL' | site == 'MK'), 
  statistic = 'percentile', percentile = 25, data.thresh = 0, avg.time = 'day', type = 'default')
names(O3_25p.Road)[2] = 'O3_25p.Road'

##PRD ozone/PM Picks
O3DayMax.PRDEast = timeAverage(p1 %>% select(date, site, o3_ug) %>% filter(site == 'HP_PR' | site == 'KK_PR'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.PRDEast)[2] = 'O3DayMax.PRDEast'
O3DayMax.PRDWest = timeAverage(p1 %>% select(date, site, o3_ug) %>% filter(site == 'MK_PR' | site == 'KT_PR' | site == 'TK_PR' | site == 'TM_PR'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(O3DayMax.PRDWest)[2] = 'O3DayMax.PRDWest'

PMDayMax.PRDEast = timeAverage(p1 %>% select(date, site, rsp) %>% filter(site == 'HP_PR' | site == 'KK_PR'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.PRDEast)[2] = 'PMDayMax.PRDEast'
PMDayMax.PRDWest = timeAverage(p1 %>% select(date, site, rsp) %>% filter(site == 'MK_PR' | site == 'KT_PR' | site == 'TK_PR' | site == 'TM_PR'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.PRDWest)[2] = 'PMDayMax.PRDWest'


##Local PM Picks
PMDayMax.All = timeAverage(m3 %>% select(date, rsp), statistic = 'max',
  data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.All)[2] = 'PMDayMax.All'

PMDayMax.NTWest = timeAverage(m3 %>% select(date, site, rsp) %>% filter(site == 'TC' | site == 'TM' | site == 'YL'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.NTWest)[2] = 'PMDayMax.NTWest'

PMDayMax.NTEast = timeAverage(m3 %>% select(date, site, rsp) %>% filter(site == 'TP' | site == 'ST' | site == 'EN'| site == 'KT'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.NTEast)[2] = 'PMDayMax.NTEast'

PMDayMax.Road = timeAverage(m3 %>% select(date, site, rsp) %>% filter(site == 'CL' | site == 'CB' | site == 'MK'), 
  statistic = 'max', data.thresh = 0, avg.time = 'day', type = 'default')
names(PMDayMax.Road)[2] = 'PMDayMax.Road'


m5 = merge.data.frame(m4 %>% mutate(date2 = format.Date(date, '%Y-%m-%d')), 
  O3DayMax.NTWest %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3DayMax.NTEast %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)

m5 = merge.data.frame(m5, 
  O3DayMax.MB %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3DayMax.Road %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3_75p.Road %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3_50p.Road %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3_25p.Road %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3DayMax.PRDEast %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  O3DayMax.PRDWest %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.PRDEast %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.PRDWest %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.All %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.NTWest %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.NTEast %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)
m5 = merge.data.frame(m5, 
  PMDayMax.Road %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)


AQHI.cast = cast(m5 %>% select(date, site, AQHI), date~site, value = 'AQHI')
AQHI.dayMax.cast = as.data.frame(timeAverage(AQHI.cast, avg.time = 'day', data.thresh = 0,
  statistic = 'max', type = 'default'))
lastCol = dim(AQHI.dayMax.cast)[2]
site.AQHI7.Count = apply(AQHI.dayMax.cast[2:lastCol], 1, function(x) sum(x>=7, na.rm = T))
site.AQHI8.Count = apply(AQHI.dayMax.cast[2:lastCol], 1, function(x) sum(x>=8, na.rm = T))
site.AQHI11.Count = apply(AQHI.dayMax.cast[2:lastCol], 1, function(x) sum(x>=11, na.rm = T))
site.Count = cbind.data.frame(AQHI.dayMax.cast[1], site.AQHI7.Count, site.AQHI8.Count, site.AQHI11.Count)


m5 = merge.data.frame(m5, 
  site.Count %>% mutate(date2 = format.Date(date, '%Y-%m-%d')) %>% select(-date), by = 'date2', all.x = T)


m5$date2 = NULL

m6 = m5 %>% select(-c(rO3, rNO2, rSO2, rRSP, r3_O3, r3_NO2, r3_SO2, r3_RSP))

WriteCSV_ToFolder(dir_EpisodeDays, dir_Master, m6, 'HighOrAboveDays.csv')


### End of 1.1 ###################
## 1.1 Analysis of High or Above Days in 2015-2016
################### ################### ################### ###################







## 1.2 Analysis of the Episode Days -- including those with dayMax AQHI >= 8
m2 = timeAverage(m1 %>% select(date, site, siteMax), avg.time = 'day', data.thresh = 0, 
  statistic = 'max', type = 'site')
m2$site = substr(m2$site, 1, 2)

episode.List = cast(m2 %>% filter(siteMax >= 8), date~site, fun.aggregate = 'max', 
  value = 'siteMax', fill = NA)
col.Order = c("date", "CB", "CL", "MK", "CW", "EN", "KC", "KT", "MB", "SP", "ST", "TC", "TK", "TM",  
  "TP", "TW", "YL")
episode.List = episode.List[,col.Order]

#### 1.1.1 Find the Max Duration of Episode
serious.Select = timeAverage(m1 %>% select(date, site, AQHI) %>% filter(AQHI == 11), 
  avg.time = 'day', data.thresh = 0, statistic = 'frequency', type = 'site')
serious.Select$site = substr(serious.Select$site, 1, 2)
serious.Duration = cast(serious.Select %>% filter(AQHI >= 0), date~site, fun.aggregate = 'max', 
  value = 'AQHI', fill = NA)
serious.Duration.Max = apply(serious.Duration[2:dim(serious.Duration)[2]], 1, 
  function(x) max(x, na.rm = T))
serious.Bind = cbind.data.frame(serious.Duration[1], serious.Duration.Max)

vHigh.Select = timeAverage(m1 %>% select(date, site, AQHI) %>% filter(AQHI >= 8 & AQHI <= 10), 
  avg.time = 'day', data.thresh = 0, statistic = 'frequency', type = 'site')
vHigh.Select$site = substr(vHigh.Select$site, 1, 2)
vHigh.Duration = cast(vHigh.Select %>% filter(AQHI >= 0), date~site, fun.aggregate = 'max', 
  value = 'AQHI', fill = NA)
vHigh.Duration.Max = apply(vHigh.Duration[2:dim(vHigh.Duration)[2]], 1, 
  function(x) max(x, na.rm = T))
vHigh.Bind = cbind.data.frame(vHigh.Duration[1], vHigh.Duration.Max)

episode.Duration = merge.data.frame(vHigh.Bind, serious.Bind, by = 'date', all = T)
names(episode.Duration) = c('date', 'vHigh.Dur', 'serious.Dur')

episode.List = merge.data.frame(episode.List, episode.Duration, by = 'date', all = T)

#### 1.1.2 Find the Max Station of Episode at any time
cast.AQHI = cast(m1 %>% select(date, site, AQHI), date~site, value = 'AQHI', fun.aggregate = 'mean')
serious.SiteNum = apply(cast.AQHI[2:dim(cast.AQHI)[2]], 1, function(x) sum(x==11 ,na.rm = T))
serious.Site.Max = timeAverage(cbind.data.frame(cast.AQHI[1],serious.SiteNum), avg.time = 'day',
  data.thresh = 0, statistic = 'max')
names(serious.Site.Max) = c('date', 'serious.Max')

vHigh.SiteNum = apply(cast.AQHI[2:dim(cast.AQHI)[2]], 1, function(x) sum(x>=8 & x<=10 ,na.rm = T))
vHigh.Site.Max = timeAverage(cbind.data.frame(cast.AQHI[1],vHigh.SiteNum), avg.time = 'day',
  data.thresh = 0, statistic = 'max')
names(vHigh.Site.Max) = c('date', 'vHigh.Max')

episode.Site.Max = merge.data.frame(vHigh.Site.Max, serious.Site.Max, by = 'date', all = T)
episode.Site.Max$filterCol = episode.Site.Max$vHigh.Max + episode.Site.Max$serious.Max
episode.Site.Max = episode.Site.Max %>% filter(filterCol != 0)
episode.Site.Max$filterCol = NULL

episode.List = merge.data.frame(episode.List, episode.Site.Max, by = 'date', all = T)


WriteCSV_ToFolder(dir_EpisodeDays, dir_Master, episode.List, 'episode_List.csv')






































