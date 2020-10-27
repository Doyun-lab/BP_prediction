library(forecast)
library(tidyverse)
library(caret)

fls = list.files()

wr =c('1.csv','2.csv','3.csv','4.csv','5.csv','6.csv','7.csv','8.csv','9.csv')


fls = fls[fls%in% wr]



final <- list()
z <- 1
for (i in fls) {
  final[[z]] <- read.csv(i)
  z <- z + 1
}


j <- 1
pred_data_list <- list()
for (i in final){
  train_data = ts(i$IBP1)
  pred_data <- c()
  
  x <- 1
  y <- 6000
  
  for (i in (1:10)){
    
    ts.data = train_data[x:y]
    
    model = nnetar(ts.data, P=1, size=24, repeats = 50, decay=0.5, maxit=150)
    
    fcast = forecast(model, h = 100)
    
    pred = fcast[['mean']]
    
    pred_data = c(pred_data, pred)
    
    x <- x + 100
    y <- y + 100
    
    print(x)
    
  }
  pred_data_list[[j]] <- pred_data
  j <- j + 1
}


z <- 1
result_rmse <- list()
result_mape <- list()
for(i in final){
  result_rmse[[z]] = round(rmse(final[[z]]$IBP1[6001:7000], pred_data_list[[z]]), 3)
  result_mape[[z]] = round(MAPE(pred_data_list[[z]], final[[z]]$IBP1[6001:7000]), 3)
  z <- z + 1
}

rmse_mean <- c()
for(i in result_rmse){
  rmse_mean <- c(rmse_mean, i)
}
sum(rmse_mean)

mape_mean <- c()
for(i in result_mape){
  mape_mean <- c(mape_mean, i)
}
sum(mape_mean)

j <- 1
z <- 1
for(i in final){
  png(filename = paste0("E:\\data_1013_ma\\",z,".png"))
    
   plot(seq(1:1000), i$IBP1[6001:7000], type = 'l',
         main = paste0("NNETAR ", z, " data"), sub = paste0("RMSE: ",result_rmse[[j]] , " MAPE:", result_mape[[j]]))
   lines(seq(1:1000), pred_data_list[[j]], col = "red")
    
   dev.off()
    
   j <- j + 1
   z <- z + 1
}
