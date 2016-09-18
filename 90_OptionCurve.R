source('!!_Setup.R')


setwd(dir_Options)

## --- Input -- 
start_Point = 22200
end_Point = 23400
step = 10
multiplier = 10


## --- Input END --- 



r1 = read.csv('options.csv') %>% filter(month >= cur_Month & year >= cur_Year)
r1$call.Index =NA
r1$long.Index =NA
r1$call.Index[which(r1$call.put == 'call')] = 1
r1$call.Index[which(r1$call.put == 'put')] = -1

plot.data = as.data.frame(seq(start_Point, end_Point, step))
names(plot.data) = 'Base'
plot.data$Return = NA

for (j in 1:length(plot.data$Base)){
  
  sum = 0
  
  for (i in 1:length(r1$strike)){
    
    value = (plot.data$Base[j] - r1$strike[i]) * r1$call.Index[i]

    if (r1$long.short[i] == 'long') {
      
      if (value > 0){
        sum = sum + value - r1$price[i]  
      } else {
        sum = sum - r1$price[i]
      }
      
    } else if (r1$long.short[i] == 'short'){
      
      if (value > 0){
        sum = sum - value + r1$price[i]  
      } else {
        sum = sum + r1$price[i]
      }

    }
      
  }
  
  plot.data$Return[j] = sum
  
}

plot.data$Return = plot.data$Return * multiplier
plot(plot.data)
abline(h=0)
abline(v=22950, col = 'red')
abline(v=22800)
abline(v=22600)
abline(v=23000)
abline(h=-3000)
abline(h=3000, col = 'blue')
abline(h=2000, col = 'blue')

setwd(dir_Master) 
