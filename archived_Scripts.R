

######################################################################################################
###### ---- Comparison of the East and West Polution of Hong Kong ---- ######
######################################################################################################
######################################################################################################
### C:\Users\poas31\Dropbox\01_Documents\05_EPD\21_Project Studies\!_Analysis\West_V_East

# Excel: 'p1_Conc_Diff.xlsx'
# Generate csv - 'm2_conc.csv'

m1$ox = m1$o3_ug/p_O3 + m1$no2_ug/p_NO2
m2 = timeAverage(selectByDate(m1, year = 2011:2016) %>% select(-AQHI, -siteMax), 
  avg.time = 'month', data.thresh = 66.66, statistic = 'mean', type = 'site')
m2$site = substr(m2$site, 1, 2)

WriteCSV_ToFolder(dir_West_v_East, dir_Master, m2, "m2_conc.csv")
######## ---------------------------------------------------------------------------------------


# Excel: 'p2_AQHI_analysis.xlsx'
# Generate csv - 'm2_aqhi.csv'

m2 = AQHI.hr
m2$site = substr(m2$site, 1, 2)

m2 = timeAverage(cast(m2, date~site, fun.aggregate = mean, value = 'siteMax'), avg.time = 'day',
  statistic = 'mean')

WriteCSV_ToFolder(dir_West_v_East, dir_Master, m2, "m2_aqhi.csv")













######################################################################################################
## -- END END END END END -- ######-- END END END END END --#######-- END END END END END --############
## -- END END END END END -- ######-- END END END END END --#######-- END END END END END --#############
######################################################################################################














######################################################################################################
###### ---- Comparison of NO2 at CB v CL ---- ######
######################################################################################################
######################################################################################################


## C:\Users\poas31\Dropbox\01_Documents\05_EPD\21_Project Studies\!_Analysis\2016_Compare Roadside NO2
## 2016_Compare Roadside NO2
# Table 1. The prevailing wind directions with the highest and 2nd highest NO2 concentrations
# Excel --> 'p1_Wind Dir v High NO2.xlsx'
# Generated csv 'm5.csv'

m2 = m2 %>% filter(site == "CB_HK" | site == "CL_HK" | site == "MK_HK") %>% 
  select(-so2_ug,-fsp,-siteMax,-AQHI)
m2$site = substr(m2$site, 1, 2)

#WriteCSV_ToFolder(dir_RoadNO2, dir_Master, selectByDate(m2, year = 2012:2016), "m2.csv")
#setwd(dir_RoadNO2)
#m2 = read.csv(file = 'm2.csv')
#setwd(dir_Master)

m3 = melt(m2, id = c("date", "site", "o3_ug", "no2_ug", 'nox_ug', 'rsp'))
m3$type = substr(m3$variable, 1, 2)
m3$ws = m3$value
m3$wSite = m3$variable
m3$value = NULL
m3$variable = NULL
m3$wSite = substr(m3$wSite, 4, 9)

temp1 = m3[m3$type == "wd",] %>% select(date, site, ws, wSite) %>% 
  mutate(Tag = paste(date,site, wSite), wd = ws) %>% select(Tag, wd)
m3 = m3 %>% mutate(Tag = paste(date,site, wSite))
m3 = m3[-which(m3$type == "wd"),]

m4 = merge.data.frame(m3, temp1, by = "Tag", all.x = T)
m4$Tag = NULL
m4$type = NULL

m4 = selectByDate(m4, year = 2012:2016)
m4$yr = substr(m4$date, 1, 4)

m5 = m4[-which(m4$wSite == 'CL' & m4$yr == 2012),]
m5 = m5[-which(m5$wSite == 'MK' & m5$yr == 2012),]

WriteCSV_ToFolder(dir_RoadNO2, dir_Master, m5, "m5.csv")
######## ---------------------------------------------------------------------------------------

#Table 2. Detailed breakdown of NO2 by months and wind directions at Waglan Island
# p2_detailed breakdowns_m2.xlsx
# generate 'm2.csv'

m2 = m2 %>% filter(site == "CB_HK" | site == "CL_HK" | site == "MK_HK") %>% 
  select(-so2_ug,-fsp,-siteMax,-AQHI)
m2$site = substr(m2$site, 1, 2)

WriteCSV_ToFolder(dir_RoadNO2, dir_Master, selectByDate(m2, year = 2012:2016), "m2.csv")

######## ---------------------------------------------------------------------------------------

######################################################################################################
## -- END END END END END -- ######-- END END END END END --#######-- END END END END END --############
## -- END END END END END -- ######-- END END END END END --#######-- END END END END END --#############
######################################################################################################




### EN and KT pollution





