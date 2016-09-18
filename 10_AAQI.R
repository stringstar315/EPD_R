source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')
logEnable = T

#### ---------- Input ------------- ###
year.Select = 2015:2016
date.start = '1/5/2015'
date.end = '30/4/2016'
pollutant.pick = c("date", "site", "no2_ug", "rsp", 'fsp')
site.order = c('CW', 'EN', 'KT', 'SP', 'KC', 'TW', 'TK', 'YL', 
               'TM', 'TC', 'TP', 'ST', 'MB', 'CB', 'CL', 'MK')

#### ---------- Input ------------- ###
#### ---------- Input ------------- ###

## Sample Name "AAQI_04-2016.csv"
output.Name = paste('AAQI_', format.Date(as.Date(date.end, '%d/%m/%Y'), "%m-%Y"),'.csv', sep='')


m1 = LoadAirHour(dir_AirHour, dir_Master, year.Select, pollutant.pick)
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

WriteCSV_ToFolder(dir_AAQI, dir_Master, b, output.Name)





