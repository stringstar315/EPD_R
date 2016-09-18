

hist(unlist(selectByDate(w1[[1]], year = 2014)[2]), na.rm = T)
hist(unlist(selectByDate(w1[[1]], year = 2015)[2]), na.rm = T)
mean(unlist(selectByDate(w1[[2]], year = 2014)[2]), na.rm = T)
mean(unlist(selectByDate(w1[[2]], year = 2015)[2]), na.rm = T)