


airYear = 2015:2016
pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp", "fsp")
logEnable = T
source('default.R')

# Load Wind Data - Hourly (2011-2015 only)
#wStation.pick = c("R2C", "TUN", "LFS", "SF", "KP", "HKO", "SE", "SHA", "WGL")
#wStation.pick = c("GI", "SF", "KP", "HKO", "SE", "WGL", "R2C", "SHL", "MK", "CL")
#wStation.pick = c("SF", "SE")

wStation.pick = c("GI", "SF", "KP", "R2C", "SE", "WGL", "CL", "MK", 'NP')
w1 = LoadWind(dir_Wind, dir_Master, 2011:2016, wStation.pick)


# Load Wind Data - Daily (online from HKO)
w.cis = LoadWind.cis(dir_Wind.cis, dir_Master, "dayWind.csv",2014:2016)



#### ------- ------- ------- ------- End ------- ------- ------- -------
#### *******************************************************************
#### *******************************************************************



#####################################################################################################################
#####################################################################################################################
# Rose plot for difference of NO2 at CB and CL
m2 = m2 %>% filter(site == "CB_HK" | site == "CL_HK" | site == "MK_HK") %>% 
  select(-so2_ug,-fsp,-siteMax,-AQHI)
m2$site = substr(m2$site, 1, 2)

CL_NO2 = m2 %>% filter(site == "CL") %>% select(date, no2_ug) 

m3 = merge.data.frame(m2, CL_NO2, by = 'date')
m3$no2_Diff. = m3$no2_ug.x - m3$no2_ug.y


siteName = 'CB'
pollutantPlot = 'no2_Diff.'
colPlot = c(  "blue", 'white', 'red')
fontsizePlot = 24
yearPlot = 2012:2014
brkVec = seq(-50, 50, 10)

names(m3)[17] = 'ws'
names(m3)[18] = 'wd'

names(m3)[7] = 'GI_ws'
names(m3)[8] = 'GI_wd'

names(m3)[13] = 'R2C_ws'
names(m3)[14] = 'R2C_wd'




p1 = polarFreq(selectByDate(m3 %>% filter(site == siteName), month = c(1:12), 
  year = yearPlot, hour = c(0:23)), type = "month", grid = 10, annotate = F, 
  pollutant = pollutantPlot, offset = 5, breaks = brkVec,
  angle = 10, statistic = "mean", cols = colPlot, min.bin = 1, 
  par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent", 
  ws.int = 3)

wdLIM = 22

p1$plot$y.limits = c(-wdLIM, wdLIM)
p1$plot$x.limits = c(-wdLIM, wdLIM)
p1$plot$aspect.ratio = 1


p1$plot$legend$right$args$key$footer = 'Diff. NO2'
p1$plot$legend$right$args$key$header = "ug/m3"


p1$plot

xpos = wdLIM*0.7
ypos = xpos*0.75

trellis.last.object() + layer(ltext(xpos, ypos, srt = 45,
  expression(Wind ~ Spd. ~ "("~m~s^-1~")" ), cex = 0.55))










WriteCSV_ToFolder(dir_RoadNO2, dir_Master, selectByDate(m4, year = 2013:2016) %>% filter(site == "MK" | site == 'CB'), "m4_MKCB.csv")


setwd(dir_RoadNO2)
m4 = read.csv(file = 'm4_temp.csv')
setwd(dir_Master)




write.csv(selectByDate(m4, year = 2013:2016), "temp_m4.csv", row.names = F)
m4 = read.csv("temp_m4.csv")





write.csv(selectByDate(m4, year = 2013), "roadNO2_2013.csv", row.names = F)




pollutantPlot = "no2_ug"
pollutantName = "NO2"
brkVec = c(seq(80,140,10))
legTxt = "NO2 (ug/m3)"

colPlot = c(  "ivory2", "palegreen", "orange",   "red",   "brown")

colPlot = c(  "ivory2", "red",   "black")

fontsizePlot = 24
yearPlot = 2016

siteName = "CL"


p1 = polarFreq(selectByDate(m4 %>% filter(site == siteName & wSite == "CL"), month = c(1:12), 
  year = yearPlot, hour = c(6:23)),
  pollutant = pollutantPlot, type = "default", grid = 1, annotate = F, 
  offset = 5,
  angle = 22.5, statistic = "mean", cols = colPlot, min.bin = 1, breaks = brkVec,
  par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent", 
  ws.int = 50)


aa = p1$data
bb = p1$data
cc = p1$data

View(cc)

write.csv(selectByDate(m4, year =2015), 'temp_M.csv', row.names = F)



p1 = pollutionRose(selectByDate(m4 %>% filter(site == siteName & wSite == "CL"), month = c(1:12), 
  year = yearPlot, hour = c(0:23)), pollutant = pollutantPlot, type = "month", 
  annotate = F, offset = 15, grid = 0.5,
  angle = 30, normalise = T, paddle = F, statistic = "abs.count", 
  breaks = c(0, 150, 200), par.settings=list(fontsize=list(text=fontsizePlot)),
  col = colPlot, seg = 0.95)

graphName = paste(siteName, yearPlot, "-month.png", sep = "")
png(filename = graphName,
    width = 3000, height = 2500, units = "px", pointsize = 24,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))

p1$plot$par.settings$fontsize$text = 60

p1$plot

dev.off ()


p2 = windRose(selectByDate(m4 %>% filter(site == siteName & wSite == "CL"), month = c(1:12), 
  year = yearPlot, hour = c(6:23)), type = "month", 
  annotate = F, offset = 10, grid = 0.5,
  angle = 22.5, normalise = T, paddle = F, statistic = "abs.count", 
  par.settings=list(fontsize=list(text=fontsizePlot)),
  col = colPlot, seg = 0.95, breaks = c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6))

graphName = paste(siteName, yearPlot, "-wd.png", sep = "")
png(filename = graphName,
    width = 3000, height = 2500, units = "px", pointsize = 24,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))

p2$plot$par.settings$fontsize$text = 60

p2$plot

dev.off ()


write.csv(p1$data, "tem.csv")



p1 = pollutionRose(selectByDate(m4 %>% filter(site == siteName, wSite == "CL"), month = c(1:12), year = yearPlot,
  hour = c(6:21)), pollutant = pollutantPlot, type = "month", annotate = F, offset = 15,
  angle = 22.5, normalise = T, paddle = F, statistic = "prop.mean", 
  breaks = c(0, summary(selectByDate(m4, year = yearPlot)$no2_ug)[[5]], 200),
  col = colPlot, seg = 0.95)


summary(selectByDate(m4, year = yearPlot)$no2_ug)[[5]]

write.csv(p1$data, "tem.csv")


wind.station = "WGL"
cat_txt1 = paste('m2$ws = m2$ws_', wind.station, sep = "")
cat_txt2 = paste('m2$wd = m2$wd_', wind.station, sep = "")
eval(parse(text=cat_txt1))
eval(parse(text=cat_txt2))


## To Generate Wind Rose Plots

## can't make inside for loop. Problem with the plot handle. Set i=1 and i=i+1
## at the end and run 9 times

all.wSite = c("GI", "CL", "WGL", "KP", "SF",  "HKO", "SE", "MK", "SHL")
for (i in 1:length(all.wSite)){

  wSite = all.wSite[i]
  plotYear = 2015
  plotType = 'month'
  fontsizePlot = 24

  p1 = windRose(selectByDate(m2, year = plotYear), 
    ws = paste("ws_", wSite, sep =''), wd = paste("wd_", wSite, sep =''), 
    type = plotType, paddle = F, angle = 10, grid = 20, breaks = ws.Class, 
    par.settings=list(fontsize=list(text=fontsizePlot)), annotate = F, 
    normalise = T, seg = 0.99)

  graphName = paste('Normal_WindRose_', plotYear, '_by', plotType, '_wStation', wSite, ".png", sep = "")
  png(filename = graphName,
      width = 3000/2, height = 2500/2, units = "px", pointsize = 24,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  p1$plot$par.settings$fontsize$text = 30
  p1$plot$legend$bottom$args$key$labels = ws.Class.Name
  p1$plot$legend$bottom$args$key$footer = ''
  p1$plot
  
  dev.off ()
  
}

bb = m2 %>% filter(ws_WGL <= 3.5) %>% select(date, ws_WGL, wd_WGL)
windRose(bb, ws = 'ws_WGL', wd = 'wd_WGL', breaks = ws.Class )


View(bb)




#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# 2. Compare East and West Seriousness by AQHI    ###################################################################

## Select days(Max) and hours for which AQHI reached 7 or above. 
##Working excel: East_West_AQHI.xlsx

cutAQHI = m1 %>% select(date, site, siteMax) %>% filter(siteMax >= 7) %>% 
  mutate(Year = as.numeric(format(date, "%Y")), Month = as.numeric(format(date, "%m")))
daySummary = timeAverage(cutAQHI, avg.time = "day", statistic = "max", type = "site") %>% filter(siteMax > 0)
daySummary$site = substr(daySummary$site, 1, 2)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, daySummary, "dayMaxAQHI.csv")

cutAQHI = m1 %>% select(date, site, AQHI) %>% filter(AQHI >= 7) %>% 
  mutate(hourTag = 1+as.numeric(format(date, "%H")))
hourSummary = timeAverage(cutAQHI, avg.time = "hour", statistic = "max", type = "site") %>% filter(AQHI > 0)
hourSummary$site = substr(hourSummary$site, 1, 2)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, hourSummary, "hrMaxAQHI.csv")
## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------ 



#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# 2. Concentration Summary East and West     ################################################################

m1$site = substr(m1$site,1,2)
site.EW.Conc = m1 %>% filter(site == "TC" | site == "YL" | site == "TM" | site == "EN" | site == "KT" | 
  site == "ST" | site == "TP") %>% select(date, site, o3_ug, no2_ug, rsp, so2_ug, nox_ug) %>% 
  mutate(Ox = round(o3_ug/p_O3 + no2_ug/p_NO2, 1))
WriteCSV_ToFolder(dir_West_v_East, dir_Master, selectByDate(site.EW.Conc, year = 2014:2016), "EastvWest_Conc.csv")










#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# 3. Monthly Concentration of West East stations     ################################################################

m1$site = substr(m1$site,1,2)

## to find out the DayMax 8h O3 for West and East sites
site.EW = m1 %>% filter(site == "TC" | site == "YL" | site == "TM" | site == "EN" | site == "KT" | 
  site == "ST" | site == "TP") %>% select(date, site, o3_ug)
site.EW = rollingMean(site.EW, pollutant = "o3_ug", width = 8, data.thresh = 66.66, align = "right", type = "site")
site.EW = selectByDate(site.EW, year = 2014:2016)
site.EW = as.data.frame(timeAverage(site.EW %>% mutate(o3.8h = rolling8o3_ug), avg.time = "day", 
  type = "site", data.thresh = 66.66, statistic = "max")) 

site.EW = site.EW %>% select(date, site, o3.8h)

## cast Average of maxday 8h o3 for East and western sides
site.EW$site2 = "Eastern"
site.EW$site2[which(site.EW$site == "TC" | site.EW$site == "YL" |site.EW$site == "TM")] = "Western"

site.EW = cast(na.omit(site.EW), formula = date~site2, value = "o3.8h", fun.aggregate = mean)
site.EW$EastBig = 1
site.EW$EastBig = site.EW$Eastern > site.EW$Western



WriteCSV_ToFolder(dir_West_v_East, dir_Master, site.EW, "EastvWest_8hO3.csv")




site.EW = merge.data.frame(site.EW, w.cis, by = "date", all.x = T)
site.EW = merge.data.frame(site.EW, AQHI.dayMax, by = "date", all.x = T)

p1 = windRose(site.EW %>% filter(EastBig == FALSE), ws='ws.WGL', wd='wd.WGL', type = "siteMax", paddle = F, 
  breaks = ws.Class, angle = 22.5, annotate = F, statistic = 'abs.count', offset = 0)
p1$plot$legend$bottom$args$key$labels = ws.Class.Name
p1$plot

p1 = windRose(site.EW, ws='ws.WGL', wd='wd.WGL', breaks = ws.Class, 
  offset = 0, statistic = 'abs.count', paddle = F, angle = 22.5, type = "year")







site.we$site2 = "Eastern"
site.we$site2[which(site.we$site == "TC_HK" | site.we$site == "YL_HK" |site.we$site == "TM_HK")] = "Western"
o3.we = timeAverage(cast(site.we %>% select(date, o3_ug, site2), formula = date~site2, value = "o3_ug", mean))
scatterPlot(o3.we, x="Eastern", y = "Western", linear = T, ci = F, type = "year")






aa = site.we %>% select(date, o3_ug, site2)
o3.we = cast(data = aa, formula = date~site2)





p1 = ggplot(data = o3.we, aes(x=Western, y=Eastern))








site.we = m1 %>% filter(site == "TC_HK" | site == "YL_HK" | site == "TM_HK" | site == "EN_HK" | site == "KT_HK" | 
  site == "ST_HK" | site == "TP_HK") %>% select(-AQHI, -siteMax)

monAvg = timeAverage(site.we, avg.time = "hour", type = "site", data.thresh = 66.7)
monAvg$site = substr(monAvg$site, 1, 2)








# 2 Compare East and West Seriousness by 8h-dayMax O3
cutO3_AQO = m1 %>% select(date, site, o3_ug)
cutO3_AQO = rollingMean(cutO3_AQO, pollutant = "o3_ug", width = 8, 
  data.thresh = 66.66, align = "right", type = "site")  
cutO3_AQO = timeAverage(cutO3_AQO, type = "site", data.thresh = 66.66, statistic = "max") %>%
  filter(rolling8o3_ug >= 80)
View(cutO3_AQO)
WriteCSV_ToFolder(dir_West_v_East, dir_Master, cutO3_AQO, "dayMax8hO3.csv")
## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
### Analysis - #3 -                                                                             #####################
### Select ALL hours with General 7 or above. compare the difference of East and West Stations. #####################
### MB is excluded in the Analysis - since NOx is contributed to AQHI                           #####################






#3.1 Find out the 3 hour rolling mean and then Calculate the Risk Contributions
roll3hr = m1
roll3hr = rollingMean(roll3hr, pollutant = c("o3_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_O3")
roll3hr = rollingMean(roll3hr, pollutant = c("no2_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_NO2")
roll3hr = rollingMean(roll3hr, pollutant = c("so2_ug"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_SO2")
roll3hr = rollingMean(roll3hr, pollutant = c("rsp"), align = "right", width = 3,
  data.thresh = 66.6, type = "site", new.name = "r3_RSP")
roll3hr = selectByDate(roll3hr, year = 2014:2016)

riskContrib = roll3hr %>% 
  mutate(rO3 = (exp(f_o3*r3_O3)-1)*100, rNO2 = (exp(f_no2*r3_NO2)-1)*100, rSO2 = (exp(f_so2*r3_SO2)-1)*100, 
  rRSP = (exp(f_rsp*r3_RSP)-1)*100, contri_O3 = round(rO3/(rO3+rNO2+rSO2+rRSP), 3), 
  contri_NO2 = round(rNO2/(rO3+rNO2+rSO2+rRSP),3), contri_SO2 = round(rSO2/(rO3+rNO2+rSO2+rRSP),3), 
  contri_RSP = round(rRSP/(rO3+rNO2+rSO2+rRSP),3))

tmpVec1 = c("O3", "NO2", "SO2", "RSP")
riskContrib$dom = apply(riskContrib[,c("contri_O3", "contri_NO2", "contri_SO2", "contri_RSP")], 1, 
  function(x) tmpVec1[match(max(x), x, incomparables = NA, nomatch = NA)])

#3.2 Select the General Sites without MB, with AQHI at 1-11 - ALL Data
gSite = riskContrib %>% select(date, site, o3_ug, no2_ug, rsp, so2_ug, AQHI, siteMax, nox_ug, 
  dom, contri_O3, contri_NO2, contri_SO2, contri_RSP) %>%
  filter(site != "CB_HK", site != "CL_HK", site != "MK_HK", site != "TK_HK", site != "MB_HK")

gAQHI.max = timeAverage(gSite %>% select(date, AQHI), avg.time = "hour", statistic = "max", data.thresh = 0)
names(gAQHI.max)[2] = "gAQHI.max"

gSite = merge.data.frame(gSite, gAQHI.max, by = "date", all.x = T)
gSite$site = substr(gSite$site, 1, 2)


# 3.3 Day Avg the Risk Contrinution
gSite.day = timeAverage(gSite %>% select(-AQHI, -gAQHI.max), avg.time = "day", type = "site", data.thresh = 66.66)
tmpVec1 = c("O3", "NO2", "SO2", "RSP")
gSite.day$dom.day = apply(gSite.day[,c("contri_O3", "contri_NO2", "contri_SO2", "contri_RSP")], 1, 
  function(x) tmpVec1[match(max(x), x, incomparables = NA, nomatch = NA)])

WriteCSV_ToFolder(dir_West_v_East, dir_Master, gSite.day, "gSiteDay.csv")




#3.3 Duplicate gAQHI.max at 11
gSite_Dup_11.g = gSite %>% filter(gAQHI.max == 11)
gSite_Dup_11.g$AQHI = "Dup_11"
gSite_Dup_11.g$siteMax = "Dup_11"
gSite_Dup_11.g$gAQHI.max = "Dup_11"
gSite = as.data.frame(rbind.data.frame(gSite, gSite_Dup_11.g))

#3.4 Duplicate AQHI at 11 (site only)
gSite_Dup_11.s = gSite %>% filter(AQHI == 11)
gSite_Dup_11.s$AQHI = "Dup_11_site"
gSite_Dup_11.s$siteMax = "Dup_11_site"
gSite_Dup_11.s$gAQHI.max = "Dup_11_site"
gSite = as.data.frame(rbind.data.frame(gSite, gSite_Dup_11.s))


WriteCSV_ToFolder(dir_West_v_East, dir_Master, gSite, "an3_gSite.csv")




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


df1 = as.data.frame(risk_hourly)
df1$site2 = NA

df1$site2[which(df1$site == "TP_HK")] = "East"
df1$site2[which(df1$site == "ST_HK")] = "East"
df1$site2[which(df1$site == "EN_HK")] = "East"
df1$site2[which(df1$site == "KT_HK")] = "East"
df1$site2[which(df1$site == "TC_HK")] = "West"
df1$site2[which(df1$site == "TM_HK")] = "West"
df1$site2[which(df1$site == "YL_HK")] = "West"
df1$site2[which(df1$site == "CB_HK")] = "Road"
df1$site2[which(df1$site == "CL_HK")] = "Road"
df1$site2[which(df1$site == "MK_HK")] = "Road"
df1$site2[which(df1$site == "MB_HK")] = "Rural"
df1$site2[which(df1$site == "CW_HK")] = "Urban"
df1$site2[which(df1$site == "SP_HK")] = "Urban"
df1$site2[which(df1$site == "KC_HK")] = "Urban"
df1$site2[which(df1$site == "TW_HK")] = "Urban"
df1$site2[which(df1$site == "TK_HK")] = "Urban2"

df2 = df1 %>% select(date, site2, AQHI)



## ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   ------   





## 4. WS and NO2 Analysis
df1 = m2

df1$site2 = NA
df1$site2[which(df1$site == "TP_HK")] = "East"
df1$site2[which(df1$site == "ST_HK")] = "East"
df1$site2[which(df1$site == "EN_HK")] = "East"
df1$site2[which(df1$site == "KT_HK")] = "East"
df1$site2[which(df1$site == "TC_HK")] = "West"
df1$site2[which(df1$site == "TM_HK")] = "West"
df1$site2[which(df1$site == "YL_HK")] = "West"
df1$site2[which(df1$site == "CB_HK")] = "Road"
df1$site2[which(df1$site == "CL_HK")] = "Road"
df1$site2[which(df1$site == "MK_HK")] = "Road"
df1$site2[which(df1$site == "MB_HK")] = "Rural"
df1$site2[which(df1$site == "CW_HK")] = "Urban"
df1$site2[which(df1$site == "SP_HK")] = "Urban"
df1$site2[which(df1$site == "KC_HK")] = "Urban"
df1$site2[which(df1$site == "TW_HK")] = "Urban"
df1$site2[which(df1$site == "TK_HK")] = "Urban2"

df2 = df1 %>% filter(site2 == "East" | site2 == "West", ws_WGL <= 3.5 | ws_R2C <= 3.5)

df3 = selectByDate(df2, year = 2014:2015)
df3$Year = as.character(format.Date(df3$date, "%Y"))

table(df3$ws_WGL, df3$Year)
scatterPlot(df3, x= "no2_ug", y = "ws_R2C", type = "site2")


hist(df3$ws_WGL)







p1 = timePlot(selectByDate(m1, year = 2014, month = 2) %>% filter(AQHI >= 7), 
              pollutant = "rsp", avg.time = "hour", type = "site")



# Wind Roses

p1 = windRose(m2 %>% filter(AQHI >= 7, site == "TC_HK"), type = "month", angle = 45, paddle = F, 
              breaks = ws.Class)




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

p1 = ggplot(data = df, aes(x = nox_12m, y = Ox_12m, fill = site2)) 

p1 + geom_point()


bp <- ggplot(data=PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
bp




p1 = ggplot(data = df, aes(x = nox_12m, y = Ox_12m, fill = site))

p1 + theme_bw() + 
  ylim(0, 80) +
  xlim(0, 380) +
  labs(x = "NOx (ppb)", y = "Ox (ppb)") +
  
  
  
  ggtitle("") +
  theme(plot.title = element_text(colour="black",size=24,angle=0,hjust=.5,vjust=.5,face="bold",
                                  margin=margin(0,0,20,0))) +
  
  theme(axis.text.x = element_text(colour="black",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="plain",
                                    margin=margin(10,0,0,0)),
        axis.title.y = element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="plain", 
                                    margin=margin(0,20,0,0))) +
  
  #vjust adjust the vertical justification of the labels, which is often useful
  
  
  geom_point(data = df %>% filter(gp2 >= 2011, site2 == "roadside"), 
             colour = 'black', size = 4) +  
  geom_point(data = df %>% filter(gp2 >= 2011, site2 == "general"), 
             colour = 'red', size = 4) +
  
  geom_smooth(data = df %>% filter(gp2 >= 2011), method = lm, se = F, 
              colour = 'blue') 














# test Wind direction
wTest.CL = as.data.frame(m4$date[which(m4$wSite == "CL" & m4$site == "CL")])
names(wTest.CL) = "date"
wTest.CL$wd = NA
wTest.CL$ws = NA
wTest.CL$wd = as.data.frame(m4$wd[which(m4$wSite == "CL" & m4$site == "CL")])
wTest.CL$ws = as.data.frame(m4$ws[which(m4$wSite == "CL" & m4$site == "CL")])
names(wTest.CL) = c("date", "wd", "ws")


wTest.GI = as.data.frame(m4$wd[which(m4$wSite == "GI" & m4$site == "CL")])
names(wTest.GI) = "wd2"
wTest.GI$ws2 = NA
wTest.GI$ws2 = as.data.frame(m4$ws[which(m4$wSite == "GI" & m4$site == "CL")])
names(wTest.GI) = c("wd2", "ws2")


wTest = cbind.data.frame(wTest.CL, wTest.GI)
p3 = windRose(wTest, ws= "ws", wd = 'wd', wd2 = "wd2", ws2 = "ws2")
write.csv(wTest, "wTest.csv", row.names = F)
