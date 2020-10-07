
setwd('E:\\preprocessing')

load('share_data.RData')

fls = ls()[1:36]

fls 


library(changepoint)
library(imputeTS)

fls = ls()[1:36]

i <- 1

file_3sigma <- list()
file_com <- list()

for (file_list in fls[1:10]){
  
  data = get(file_list)  # 2902990
  
  # 2902990
  data_na = ifelse(data$IBP1 < boxplot(data$IBP1)$stats[1,] | data$IBP1 > boxplot(data$IBP1)$stats[5,], NA, data$IBP1)
  
  # file_3sigma[[i]] <- data_na
  
  # data$IBP1 = na_interpolation(data_na, option = "linear")
  data$IBP1 = na_interpolation(data_na, option = "stine")
  
  png(filename=paste0('check/diff/20_',file_list,".png"))
  
  # data_na <- as.numeric(data_na$na.fill.na.approx.data_na....extend..)
  
  v_start_end <- round((length(data$IBP1))*0.15)
  data1 = data$IBP1[1:v_start_end]
  v_start.PELT = cpt.var(data1, method = "PELT")
  
  start = round(length(data$IBP1)*0.85)
  end = length(data$IBP1)
  
  v_end.AMOC = cpt.var(data$IBP1[start:end], method = "AMOC")
  
  start_cut = cpts(v_start.PELT)[length(cpts(v_start.PELT))]
  end_cut = cpts(v_end.AMOC)
  
  par(mfrow=c(3,1))
  
  plot(ts(data$IBP1))
  abline(v=start_cut, col="deeppink")
  abline(v=round(start+end_cut), col="blue")
  
  plot(ts(data$IBP1[start_cut:round(start+end_cut)]))
  
  # file_com[[i]] <- data$IBP1[start_cut:round(start+end_cut)]
  
  new_data = data[start_cut:round(start+end_cut),]
  
  new_data$Time2 = as.POSIXct(new_data$Time)
  
  t_min = min(new_data$Time2)
  t_max = max(new_data$Time2)
  t_cur = t_min
  
  avg_1 = c()
  avg_2 = c()
  
  while (t_cur <= t_max) {
    
    temp <- new_data[t_cur <= new_data$Time2 & new_data$Time2 < t_cur + 2,]
    
    avg_1 <- c(avg_1,as.character(t_cur))
    
    avg_1
    
    avg_2 <- c(avg_2, mean(temp$IBP1))
    
    t_cur <- as.POSIXct(t_cur) + 2
    
    print(t_cur)
    
    print(paste0(t_cur, ' / ', t_max))
    
  }
  
  t_avg <- data.frame(Time=avg_1, avg=avg_2)
  
  t_avg2 = na.omit(t_avg)
  
  avg_diff = diff(t_avg2$avg)
  
  data_diff = cbind(t_avg2[2:nrow(t_avg2),],avg_diff)
  
  data_diff_sub = subset(data_diff, data_diff$avg_diff>20)
  
  avg_diff_time = data_diff_sub$Time
  
  t_min = min(avg_diff_time)
  t_max = max(avg_diff_time)
  t_cur = t_min
  
  index_ = c()
  
  for(diff_time in avg_diff_time){
    
    temp_index <- which(as.POSIXct(diff_time) -2 < new_data$Time2 & new_data$Time2 <= diff_time)
    
    index_ <- c(index_,temp_index)
    
    print(paste0(diff_time, ' / ', length(index_)))
    
    
  }
  
  new_data[index_,]$IBP1 = NA
  
  new_data$IBP1 = na_interpolation(new_data$IBP1,'stine')
  
  plot(ts(new_data$IBP1))
  
  i <- i + 1
  
  dev.off() 
}
