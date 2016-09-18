##############################################################################################################
####################                        List of SQL Functions                         ####################
####################               Aprior connection to Desired DB required               ####################
##############################################################################################################

##  1. Load.csvToMySQL = function()


##############################################################################################################
####################                          1. Load.csvToMySQL                          ####################
##############################################################################################################

Load.csvToMySQL = function(CSVfileName = 'airData.csv', SQLtableName = 'hkair', filePath = dir_SQL.csv){
  
  pathAndFile = paste(filePath, '/', CSVfileName, sep = '')
  print(paste('File Path and File:  ', pathAndFile, sep = ""))
  
  myQuery = paste(
    'LOAD DATA INFILE', '"', pathAndFile, '"   ',
    'INTO TABLE ', SQLtableName, '  ',
    'FIELDS TERMINATED BY ","  ',
    'ENCLOSED BY \'"\'  ',
    'LINES TERMINATED BY "\r\n"  ',
    'IGNORE 1 LINES;  ',
    sep = '')
  print(myQuery)
  dbSendQuery(con, myQuery)
  
}

#------------------------------------------------------------------------------------------------------------#


