

year.Pick = 2007:2016
pollutant.Pick = c('o3_ug', 'no2_ug', 'rsp', 'nox_ug', 'so2_ug', 'fsp', 'co_ug')

r1 = LoadAirHour(year.Pick, pollutant.pick = pollutant.Pick) %>% mutate(Tag = paste(date, site))
r2 = LoadAQHI(year.Pick) %>% mutate(Tag = paste(date, site)) %>% select(Tag, AQHI)


r3 = merge.data.frame(r1, r2, by = 'Tag', all.x = T)
r3 = r3[c('date', 'site', 'AQHI', pollutant.Pick)]

write.csv(r3, 'test1.csv', row.names = F, na = '\\N')

library(DBI)
library(RMySQL)
library(psych)

con <- dbConnect(MySQL(), user="root", password="ckk%i9==",
                 dbname="airdata", host='localhost')

dbListTables(con)
myQuery = "SELECT * FROM working"
df = dbGetQuery(con, myQuery)
str(df)

dbWriteTable(con, 'df_1', df, overwrite = T, append = F)

on.exit(dbDisconnect(con))
dbDisconnect(con)
