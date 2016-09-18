source('!!_Setup.R')
source('!!_fun-dataLoad.R')
source('!!_fun-routines.R')
source('!!_fun-analysis.R')



source('!!_Functions.R')
source('!!_parameters.R')


airYear = 2014:2016
pollutant.pick = c("o3_ug", "no2_ug", "nox_ug", "so2_ug", "rsp") ##HK_Data
wStation.pick = c("R2C", "WGL", "SF", "SE")


w1 = LoadWind(yearVec = airYear, load_stations = wStation.pick)

i = 2014
while (i <= 2016) {
  
  fontsizePlot = 40
  yearPlot = i
  p1 = windRose(selectByDate(w1, year = 2014, month = 1:6), ws = 'ws.WGL', wd = 'wd.WGL', 
    grid.line = 10,
    angle = 10, paddle = F, type = 'month', breaks = ws.Class, offset = 10, annotate = F, 
    par.settings=list(fontsize=list(text=fontsizePlot)))
  p1$plot$legend$bottom$args$key$labels = ws.Class.Name
  p1$plot$legend$bottom$args$key$footer = ""
  
  graphName = paste('WGL_wind', yearPlot, '.png', sep = "")
  
  png(filename = graphName,
      width = 3000/1.5, height = 2500/1.5, units = "px", pointsize = 24,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  p1$plot
  
  dev.off ()
  
  i = i + 1
  
  break
}


















