setwd("/Users/doyun/Downloads/")
load("/Users/doyun/Downloads/share_data.RData")

library(changepoint)

fls = ls()[1:36]

for (file_list in fls){
  
  png(filename=paste0('Hannan/0.15/',file_list,".png"))
  
  data = get(file_list)$IBP1[1:round(nrow(get(file_list))*0.2)]
  v_start.PELT = cpt.var(data,method = "PELT", penalty = "Hannan-Quinn")
  
  start = round(nrow(get(file_list))*0.85)
  end = nrow(get(file_list))
  
  v_end.AMOC = cpt.var(get(file_list)$IBP1[start:end],method = "AMOC")
  
  start_cut = cpts(v_start.PELT)[length(cpts(v_start.PELT))]
  end_cut = cpts(v_end.AMOC)
  
  par(mfrow = c(2,1))
  
  plot(ts(get(file_list)$IBP1))
  abline(v=start_cut, col="deeppink")
  abline(v=round(start+end_cut), col="blue")
  
  plot(ts(get(file_list)[start_cut:round(start+end_cut),]$IBP1))
  
  dev.off() 
}
