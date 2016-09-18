

yearVec = 1999:2015
monthVec = 1 :12
counter = 1

bindStr = ""
bindStr2 = ""

for (yr in yearVec){
  
  for (mon in monthVec){
    
    url = paste('http://www.weather.gov.hk/wxinfo/pastwx/metob', yr, str_pad(mon, 2, pad = '0'), '.htm', sep = '')
     doc = htmlParse(url)
     tableNodes = getNodeSet(doc, "//table")
     tbl = readHTMLTable(tableNodes[[2]])
     startRows = which(tbl[1] == 13) - 12
     
      if(is.na(which(tbl[1] == 31)[1] > 0)){
        if(is.na(which(tbl[1] == 30)[1] > 0)){
          if(is.na(which(tbl[1] == 29)[1] > 0)){
            endRows = which(tbl[1] == 28)
          } else {endRows = which(tbl[1] == 29)}  
        } else {endRows = which(tbl[1] == 30)}
      } else {endRows = which(tbl[1] == 31)}
     
     expr1 = paste('p', counter,' = as.data.frame(tbl[startRows[1]:endRows[1], 1:9])', sep = "")
     eval(parse(text=expr1))
     expr1 = paste('p', counter,'$year = yr', sep = "")
     eval(parse(text=expr1))
     expr1 = paste('p', counter,'$month = mon', sep = "")
     eval(parse(text=expr1))
     
     expr1 = paste('q', counter,' = as.data.frame(tbl[startRows[2]:endRows[2], 1:7])', sep = "")
     eval(parse(text=expr1))
     expr1 = paste('q', counter,'$year = yr', sep = "")
     eval(parse(text=expr1))
     expr1 = paste('q', counter,'$month = mon', sep = "")
     eval(parse(text=expr1))
    
     bindStr = paste(bindStr, 'p', counter, ", ", sep = "")
     bindStr2 = paste(bindStr2, 'q', counter, ", ", sep = "")
     
    counter = counter + 1
    
  }
  
  print(yr)
  
}

cat_txt = paste("mergeData = rbind.data.frame(", bindStr, "deparse.level = 0)", sep='')
  eval(parse(text=cat_txt))
  
cat_txt2 = paste("mergeData2 = rbind.data.frame(", bindStr2, "deparse.level = 0)", sep='')
  eval(parse(text=cat_txt2))

mergeData = mergeData[c(8,9,1:7)]
mergeData2 = mergeData[c(6,7,1:5)]

 