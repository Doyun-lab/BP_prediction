library(changepoint)
library(forecast)
library(zoo)
library(imputeTS)

fls = ls()[1:36]

x <- 1
data_1013 <- list()
for (file_list in fls){
  
  data = get(file_list)
  
  # 3 sigma
  data$IBP1 = ifelse(data$IBP1 < boxplot(data$IBP1)$stats[1,] | data$IBP1 > boxplot(data$IBP1)$stats[5,], NA, data$IBP1)
  
  # NA 10% 이상 구간 제거
  i <- 1
  while(i < nrow(data)-1000){
    na_sum <- sum(is.na(data$IBP1[i:(i+999)]))
    
    if (na_sum > 100){
      data$IBP1[i:(i+999)] <- 0
    } else{
      data$IBP1[i:(i+999)] <- data$IBP1[i:(i+999)]}
    
    i <- i + 999
  }
  
  data = subset(data, data$IBP1 != 0)
  
  # cpt var
  v_start_end <- round((length(data$IBP1))*0.1)
  data1 = data$IBP1[1:v_start_end]
  v_start.PELT = cpt.var(data1, method = "PELT", penalty = "Hannan-Quinn")
  
  start = round(length(data$IBP1)*0.85)
  end = length(data$IBP1)
  
  v_end.AMOC = cpt.var(data$IBP1[start:end], method = "AMOC")
  
  start_cut = cpts(v_start.PELT)[1]
  end_cut = cpts(v_end.AMOC)
  
  prep_data <- data[start_cut:round(start+end_cut),]
  
  # 20 ~ 170
  prep_data$IBP1 <- ifelse(prep_data$IBP1 < 20 | prep_data$IBP1 > 170, NA, prep_data$IBP1)
  prep_data$IBP1 <- na_interpolation(prep_data$IBP1, option = "linear")
  
  # 결과 저장
  data_1013[[x]] <- prep_data
  
  x <- x + 1
  print(x)
}

data_1013_copy <- data_1013
data_1013_ma <- list()
for(i in 1:36){
  png(filename=paste0('final/data_1013_ma/',i,".png"))
  
  data_1013_copy[[i]]$IBP1 <- ma(data_1013_copy[[i]]$IBP1, 200)
  data_1013_ma[[i]] <- na.omit(data_1013_copy[[i]])
  
  par(mfrow = c(2, 1))
  plot(ts(file_3sigma[[i]]))
  plot(ts(data_1013_ma[[i]]$IBP1))
  dev.off()
}
