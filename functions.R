### Function 1 for load and combine hourly data
LoadAirHour = function (file_path, master_path, yearVec = yearVec, columns = columns) {
  
  setwd(file_path)
  
  prefix = "01"
  avgType = "Hour"
  
  bindStr = ""
  for (i in yearVec){
    
    bindStr = paste(bindStr, 'yr', i, ", ", sep = "")
    expr1 = paste("yr", i, " = read.csv('", prefix, "_air", avgType, i, ".csv')" , sep="")
    eval(parse(text=expr1))
    
  }
  
  cat_txt = paste("mergeData = rbind.data.frame(", bindStr, "deparse.level = 0)", sep='')
  eval(parse(text=cat_txt))
  
  outname = paste(prefix, "_air", avgType, "!", min(yearVec), "-", max(yearVec), ".csv", sep = "")
  print(outname)
  
  
  setwd(master_path)
  return(selectByDate(mergeData[columns], year = 1900:2100))
  
}

### Function 2 for load AQHI Data
LoadAQHI = function (file_path, master_path, yearVec = yearVec) {
  
  setwd(file_path)
  listFile = list.files(path = ".", pattern = "*.csv")
  fileName = listFile[length(listFile)]
  print(paste("Read: ", fileName, sep = ""))
  
  a1 = read.csv(fileName)
  
  setwd(master_path)
  return(selectByDate(a1, year = yearVec))
  
}


### Function 3 for load wind Data
LoadWind = function (file_path, master_path, yearVec = yearVec, load_stations) {
  
  setwd(file_path)
  
  wlen = length(load_stations)
  w = list(wlen)
  preFIX = "!_WindData_"
  
  for (i in 1:wlen){
    wStation = paste(preFIX, load_stations[i], sep = "")
    w[[i]] = selectByDate(read.csv(paste(wStation, ".csv", sep = "")), year = yearVec)
    names(w[[i]])[2] = paste(names(w[[i]])[2], load_stations[i], sep = "_")
    names(w[[i]])[3] = paste(names(w[[i]])[3], load_stations[i], sep = "_")
  }

  setwd(master_path)
  
  return(w)
  
}


### Function 4 write Files
WriteCSV_ToFolder = function(file_path, master_path, varName, fileName) {
  
  setwd(file_path)
  
  write.csv(varName, fileName, row.names = F)
  
  setwd(master_path)
  
}


### Function 5 Calculate AQHI

Cal_AQHI.ug = function(o3_r3, no2_r3, rsp_r3, so2_r3){
  
  AR = (exp(round(o3_r3,0)*f_o3)-1 + exp(round(no2_r3,0)*f_no2)-1 + exp(round(rsp_r3,0)*f_rsp)-1 + exp(round(so2_r3,0)*f_so2)-1)*100
  
  
  AQHI_Class = AR
  AQHI_Class[which(AR > aqhi_02)] = NA
  AQHI_Class[which(AR > aqhi_01)] = "aqhi_01"
  AQHI_Class[which(AR > aqhi_02)] = "aqhi_02"
  AQHI_Class[which(AR > aqhi_03)] = "aqhi_03"
  AQHI_Class[which(AR > aqhi_04)] = "aqhi_04"
  AQHI_Class[which(AR > aqhi_05)] = "aqhi_05"
  AQHI_Class[which(AR > aqhi_06)] = "aqhi_06"
  AQHI_Class[which(AR > aqhi_07)] = "aqhi_07"
  AQHI_Class[which(AR > aqhi_08)] = "aqhi_08"
  AQHI_Class[which(AR > aqhi_09)] = "aqhi_09"
  AQHI_Class[which(AR > aqhi_10)] = "aqhi_10"
  AQHI_Class[which(AR > aqhi_11)] = "aqhi_11"

  return(AR)
  
}











### Function 6 Plot PollutionRose as polar frequenzy
Rose_Pollution = function(mData, pollutant_input){
  
  fontsizePlot = 28
  colPlot = c("ivory2", "palegreen", "orange", "red", "black")
  
  
  
  ########### switch case of pollutants
  switch(pollutant_input,
    o3 = list("o3_ug", "o3", c(seq(0,180,20)), "o3 (ug/m3)"),
    no2 = list("no2_ug", "NO2", c(seq(0,250,25)), "NO2 (ug/m3)"),
    rsp = list("rsp", "RSP", c(seq(0,250,25)), "RSP (ug/m3)"),
    nox = list("nox_ug", "NOx", c(seq(0,400,40)), "NOx (ug/m3)"),
    so2 = list("so2_ug", "SO2", c(seq(0,100,10)), "SO2 (ug/m3)"),
    fsp = list("fsp", "FSP", c(seq(0,150,15))), "FSP (ug/m3)")

  
  pollutantPlot = "nox_ug"
  pollutantName = "NOx"
  brkVec = c(seq(0,400,40))
  legTxt = "NOx (ug/m3)"
  
  pollutantPlot = "so2_ug"
  pollutantName = "SO2"
  brkVec = c(seq(0,100,10))
  legTxt = "SO2 (ug/m3)"
  
  pollutantPlot = "fsp"
  pollutantName = "FSP"
  brkVec = c(seq(0,150,15))
  legTxt = "FSP (ug/m3)"
  
  ########### switch case of pollutants
  
  
  
  p1 = polarFreq(selectByDate(mData[which(mData$site == siteName),], month = c(1:12), year = 2011:2015,
    hour = c(0:23)), pollutant = pollutantPlot, type = "default", grid = 5, annotate = F, offset = 0,
    angle = 10, statistic = "mean", cols = colPlot, min.bin = 1, 
    breaks = brkVec, par.settings=list(fontsize=list(text=fontsizePlot)), mis.col = "transparent",
    ws.int = 0.7)
  
  wdLIM = 25
  
  p1$plot$y.limits = c(-wdLIM, wdLIM)
  p1$plot$x.limits = c(-wdLIM, wdLIM)
  p1$plot$aspect.ratio = 1
  
  
  p1$plot$legend$right$args$key$footer = legTxt
  p1$plot$legend$right$args$key$header = ""
  p1$plot$xlab = expression(Wind ~ Speed ~ "("~m~s^-1~")" )
  
  p1$plot
  
  xpos = wdLIM*0.75
  ypos = xpos*0.8
  
  trellis.last.object() + layer(ltext(xpos, ypos, srt = 45,
  expression(Wind ~ Speed ~ "("~m~s^-1~")" ), cex = 0.65))
  
  
  print(pollutantPlot)
  print(pollutantName)

  
}

































