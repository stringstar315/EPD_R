
source('!!_Setup.R')
source('!!_Functions.R')
source('!!_parameters.R')

##############################################################################################################
####################                         Generate CSV for SQL                         ####################
##############################################################################################################

year.Pick = 2004:2016
pollutant.Pick = c('o3_ug', 'no2_ug', 'rsp', 'nox_ug', 'so2_ug', 'fsp', 'co_ug')

r1 = LoadAirHour(year.Pick, pollutant.pick = pollutant.Pick) %>% mutate(Tag = paste(date, site))
r1$site = substr(r1$site, 1, 2)
r2 = LoadAQHI(year.Pick) %>% mutate(Tag = paste(date, site)) %>% select(Tag, AQHI)
r3 = merge.data.frame(r1, r2, by = 'Tag', all.x = T)
r3 = r3[c('date', 'site', 'AQHI', pollutant.Pick)]
outName = paste('airData', min(year.Pick),'-', max(year.Pick),'.csv', sep = '')
print(outName)
WriteCSV_ToFolder(dir_SQL.csv, dir_Master, r3, 'airData.csv', NAs = '\\N')
remove(r1,r2,r3)


##############################################################################################################
####################                         Generate CSV for SQL                         ####################
##############################################################################################################

#### ------------------------------------------------------------------------------------- ####


##############################################################################################################
####################                            SQL OPERATIONs                            ####################
####################                          List of Operations                          ####################
##############################################################################################################
##    1. Create New Table with a specified Name










library(DBI)
library(RMySQL)
library(psych)

## Examples
dbListFields(con, 'hkair')

## Setup DB connection
con <- dbConnect(MySQL(), user="root", password="ckk%i9==", dbname="airdata", host="localhost")
dbListTables(con)

## Delete Table
myQuery = 'DROP TABLE work_hm; '
dbSendQuery(con, myQuery)

## Clear Table Content
myQuery = "TRUNCATE TABLE hkair"
dbSendQuery(con, myQuery)

##############################################################################################################
####################                   Load Data from File in Directory                   ####################
##############################################################################################################

fileName = 'airData.csv'
filePath = dir_SQL.csv
pathAndFile = paste(filePath, '/', fileName, sep = '')
print(paste('File Path and File:  ', pathAndFile, sep = ""))

myQuery = paste(
  'LOAD DATA INFILE', '"', pathAndFile, '"   ',
  'INTO TABLE hkair  ',
  'FIELDS TERMINATED BY ","  ',
  'ENCLOSED BY \'"\'  ',
  'LINES TERMINATED BY "\r\n"  ',
  'IGNORE 1 LINES;  ',
sep = '')
print(myQuery)
dbSendQuery(con, myQuery)

#------------------------------------------------------------------------------------------------------------#


##############################################################################################################
####################                         Create NEW SQL Table                         ####################
##############################################################################################################

myQuery = 'CREATE TABLE airdata.work_hm (
  date datetime NOT NULL, 
  site varchar( 8 ) NOT NULL, 
  AQHI float default NULL, 
  o3_ug float default NULL, 
  no2_ug float default NULL, 
  rsp float default NULL, 
  nox_ug float default NULL, 
  so2_ug float default NULL, 
  fsp float default NULL, 
  co_ug float default NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8; '

dbSendQuery(con, myQuery)


myQuery = 'CREATE TABLE airdata.work_hm (
  date datetime NOT NULL, 
  co_ug float default NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8; '

dbSendQuery(con, myQuery)





#------------------------------------------------------------------------------------------------------------#

##############################################################################################################
####################                         Create NEW SQL Table                         ####################
##############################################################################################################

myQuery = 'CREATE TABLE airdata.old_Table2 (
date datetime NOT NULL, 
site varchar( 8 ) NOT NULL, 
AQHI float default NULL, 
o3_ug float default NULL, 
no2_ug float default NULL, 
rsp float default NULL, 
nox_ug float default NULL, 
so2_ug float default NULL, 
fsp float default NULL, 
co_ug float default NULL,
source varchar(8) NOT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8; '

dbSendQuery(con, myQuery)

#------------------------------------------------------------------------------------------------------------#

Load.csvToMySQL('Old_Data.csv', SQLtableName = 'old_Table2')
Load.csvToMySQL('New_Data.csv', SQLtableName = 'new_Table')





a1 = read.csv('New_Data.csv', na.strings = '\\N')
a2 = read.csv('Old_Data.csv', na.strings = '\\N')


a3 = UpdateDataFrame(old.df = a2, new.df = a1)
WriteCSV_ToFolder(file_path = dir_SQL.csv, master_path = dir_Master, a3, 'fional.csv', NAs = '\\N')


a3 = merge.data.frame(a1[c('date', 'site', 'source')], a2[c('date', 'site')], by = c('date', 'site'), all = T )
a3[-(which(a3$source == 'NEW')),]


dbWriteTable(con, 'old_table2', a1, overwrite = T, append = F, row.names = F)






mer
install.packages('data.table')
library(data.table)


?all.equal



myQuery = "TRUNCATE TABLE working"
dbSendQuery(con, myQuery)

myQuery = '
  INSERT INTO working
  SELECT * FROM hkair WHERE ((date BETWEEN "2015-01-01 00:00:00" AND "2016-12-31 23:00:00") OR 
  (date BETWEEN "2005-01-01 00:00:00" AND "2005-12-31 23:00:00"));
'
dbSendQuery(con, myQuery)






dbDisconnect(con)

myQuery = "SELECT * FROM working"
df = dbGetQuery(con, myQuery)
str(df)

dbWriteTable(con, 'df_1', df, overwrite = T, append = F)

on.exit(dbDisconnect(con))














