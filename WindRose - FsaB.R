## plot and compared the windRose of SO2
## 1. 2014.7 to 2015.6
## 2. 2015.7 to 2016.6



airYear = 2014:2016
pollutant.pick = c("date", "site", "so2_ug")
logEnable = T
source('default.R')


wStation.pick = c("GI")
w1 = LoadWind(dir_Wind, dir_Master, 2011:2016, wStation.pick)


## Match airData and AQHIdata and windData
m2 = merge.data.frame(m1, w1[[1]], by = "date", all.x = T)

for (j in 2:length(wStation.pick)){
  m2 = merge.data.frame(m2, w1[[j]], by = "date", all.x = T)
}
glimpse(m2)


m3 = m2 %>% filter(site == 'KC_HK')

names(m3)[6] = 'ws'
names(m3)[7] = 'wd'


colPlot = c('darkgreen', 'palegreen','#FFFEE0', 'orange','red')
fontsizePlot = 24
yearPlot = 2012:2014

brkVec = c(seq(0,60,5))
legTxt = "SO2 (ug/m3)"
fontsizePlot = 24




#plotData = selectByDate(m3, year = 2010:2014, month = 7:9)
plotData = selectByDate(m3, start = '1/7/2014', end = '30/6/2015')
write.csv(plotData, '2014')
p1 = polarFreq(plotData, grid = 5, annotate = F, 
  pollutant = 'so2_ug', offset = 0, breaks = brkVec,
  angle = 10, statistic = "mean", cols = colPlot, min.bin = 1, 
  par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent", 
  ws.int = 2)

wdLIM = 24

p1$plot$y.limits = c(-wdLIM, wdLIM)
p1$plot$x.limits = c(-wdLIM, wdLIM)
p1$plot$aspect.ratio = 1



p1$plot$legend$right$args$key$footer = legTxt
p1$plot$legend$right$args$key$header = ""


p1$plot
xpos = wdLIM*0.7
ypos = xpos*0.75

trellis.last.object() + layer(ltext(xpos, ypos, srt = 45,
  expression(Wind ~ Speed ~ "("~m~s^-1~")" ), cex = 0.65))


#####

plotData = selectByDate(m3, start = '1/7/2015', end = '30/6/2016')
p1 = polarFreq(plotData, grid = 5, annotate = F, 
               pollutant = 'so2_ug', offset = 0, breaks = brkVec,
               angle = 10, statistic = "mean", cols = colPlot, min.bin = 1, 
               par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent", 
               ws.int = 2)

wdLIM = 24

p1$plot$y.limits = c(-wdLIM, wdLIM)
p1$plot$x.limits = c(-wdLIM, wdLIM)
p1$plot$aspect.ratio = 1



p1$plot$legend$right$args$key$footer = legTxt
p1$plot$legend$right$args$key$header = ""


p1$plot
xpos = wdLIM*0.7
ypos = xpos*0.75

trellis.last.object() + layer(ltext(xpos, ypos, srt = 45,
  expression(Wind ~ Speed ~ "("~m~s^-1~")" ), cex = 0.65))


#########end######





