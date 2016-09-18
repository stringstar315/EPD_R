



c.co = melt(co, id=c('month', 'day', 'hour'))
names(c.co)[4] = 'site'
names(c.co)[5] = 'CO'
c.co$Tag = paste(c.co$month,c.co$day,c.co$hour,c.co$site)


c.fsp = melt(fsp, id=c('month', 'day', 'hour'))
names(c.fsp)[4] = 'site'
names(c.fsp)[5] = 'fsp'
c.fsp$Tag = paste(c.fsp$month,c.fsp$day,c.fsp$hour,c.fsp$site)

c.no2 = melt(no2, id=c('month', 'day', 'hour'))
names(c.no2)[4] = 'site'
names(c.no2)[5] = 'NO2'
c.no2$Tag = paste(c.no2$month,c.no2$day,c.no2$hour,c.no2$site)

c.o3 = melt(o3, id=c('month', 'day', 'hour'))
names(c.o3)[4] = 'site'
names(c.o3)[5] = 'O3'
c.o3$Tag = paste(c.o3$month,c.o3$day,c.o3$hour,c.o3$site)

c.rsp = melt(rsp, id=c('month', 'day', 'hour'))
names(c.rsp)[4] = 'site'
names(c.rsp)[5] = 'rsp'
c.rsp$Tag = paste(c.rsp$month,c.rsp$day,c.rsp$hour,c.rsp$site)

c.so2 = melt(so2, id=c('month', 'day', 'hour'))
names(c.so2)[4] = 'site'
names(c.so2)[5] = 'SO2'
c.so2$Tag = paste(c.so2$month,c.so2$day,c.so2$hour,c.so2$site)











