

start_Time = Sys.time()

source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')
logEnable = T



AQHI.hr = LoadAQHI(dir_AQHI, dir_Master, 2013:2020)
AQHI.dayMax = as.data.frame(timeAverage(AQHI.hr %>% select(date, site, siteMax), avg.time = "day", 
  statistic = "max", type = "site"))

AQHI.dayMax$site = substr(AQHI.dayMax$site, 1, 2)
cAQHI.SiteMax = cast(AQHI.dayMax, date~site, value = 'siteMax', fun.aggregate = 'mean')

a = as.data.frame(apply(cAQHI.SiteMax[2:17], 1, function(x) max(x, na.rm = T)))
names(a) = 'dayMax'
b = as.data.frame(apply(cAQHI.SiteMax[c(2,3,8)], 1, function(x) sum(x>=8, na.rm = T)))
names(b) = 'road.VH'
c = as.data.frame(apply(cAQHI.SiteMax[c(2:17)[-c(2,3,8)]], 1, function(x) sum(x>=8, na.rm = T)))
names(c) = 'gen.VH'
d = as.data.frame(apply(cAQHI.SiteMax[c(2,3,8)], 1, function(x) sum(x==11, na.rm = T)))
names(d) = 'road.Ser'
e = as.data.frame(apply(cAQHI.SiteMax[c(2:17)[-c(2,3,8)]], 1, function(x) sum(x==11, na.rm = T)))
names(e) = 'gen.Ser'


cAQHI.SiteMax = cbind.data.frame(cAQHI.SiteMax, c)
cAQHI.SiteMax = cbind.data.frame(cAQHI.SiteMax, b) 
cAQHI.SiteMax = cbind.data.frame(cAQHI.SiteMax, d)
cAQHI.SiteMax = cbind.data.frame(cAQHI.SiteMax, e)
cAQHI.SiteMax = cbind.data.frame(cAQHI.SiteMax, a)

days.select =  cAQHI.SiteMax[which(cAQHI.SiteMax$dayMax >= 8 ), -c(2:17)]

AQHI.hr$site = substr(AQHI.hr$site, 1, 2)
cAQHI.hr = cast(AQHI.hr, date~site, value = 'AQHI', fun.aggregate = 'mean')

a2 = as.data.frame(apply(cAQHI.hr[2:17], 1, function(x) sum(x>=8, na.rm = T)))
names(a2) = 'siteNumMax.VH'
cAQHI.hr = cbind.data.frame(cAQHI.hr, a2)
siteNum.Max.VH = as.data.frame(timeAverage(cAQHI.hr[c('date', 'siteNumMax.VH')], 
  avg.time = 'day', statistic = 'max'))

b2 = as.data.frame(apply(cAQHI.hr[2:17], 1, function(x) sum(x==11, na.rm = T)))
names(b2) = 'siteNumMax.Ser'
cAQHI.hr = cbind.data.frame(cAQHI.hr, b2)
siteNum.Max.Ser = as.data.frame(timeAverage(cAQHI.hr[c('date', 'siteNumMax.Ser')], 
  avg.time = 'day', statistic = 'max'))



days.select = merge.data.frame(days.select, siteNum.Max.VH, by = 'date', all.x = T)
days.select = merge.data.frame(days.select, siteNum.Max.Ser, by = 'date', all.x = T)

AQHI.hr.VH = AQHI.hr
AQHI.hr.VH[which(AQHI.hr.VH$AQHI < 8), c('AQHI')] = NA
AQHI.hr.VH[which(AQHI.hr.VH$AQHI >= 8), c('AQHI')] = 1
AQHI.hr.VH = timeAverage(AQHI.hr.VH, avg.time = 'day', statistic = 'sum', type = 'site')
c.AQHI.hr.VH = cast(AQHI.hr.VH, date~site, fun.aggregate = 'mean', value = 'AQHI')
a3 = as.data.frame(apply(c.AQHI.hr.VH[2:17], 1, function(x) max(x, na.rm = T)))
names(a) = 'durMax.VH'


AQHI.hr.Ser = AQHI.hr
AQHI.hr.Ser[which(AQHI.hr.Ser$AQHI < 11), c('AQHI')] = NA
AQHI.hr.Ser[which(AQHI.hr.Ser$AQHI == 11), c('AQHI')] = 1
AQHI.hr.Ser = timeAverage(AQHI.hr.Ser, avg.time = 'day', statistic = 'sum', type = 'site')
c.AQHI.hr.Ser = cast(AQHI.hr.Ser, date~site, fun.aggregate = 'mean', value = 'AQHI')
b3 = as.data.frame(apply(c.AQHI.hr.Ser[2:17], 1, function(x) max(x, na.rm = T)))
names(a) = 'durMax.Ser'


days.select = cbind.data.frame(days.select, a3)
days.select = cbind.data.frame(days.select, b3)

WriteCSV_ToFolder(dir_HighAQHI, dir_Master, days.select, "highDays.csv")


### print time used and log
  end_Time = Sys.time()
  elapseTime = as.difftime(end_Time - start_Time, units = "sec")
  print(round(elapseTime, 1))
  LogFile(elapseTime, "default.R", type.Run = 'source')
## end logging
  