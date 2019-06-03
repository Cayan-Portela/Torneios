rm(list=ls());gc()

library(tidyverse)
library(xtable)

setwd("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/RESULTS FINAL")

mega_lista <- c(indicadores1,indicadores2,indicadores3)

# indicadores1[[1]]$y_teste.1

acc <- c()
precision <- c()
recall <- c()
f_score <- c()

for (i in 1:length(mega_lista)){

  p1 <- ((mega_lista[[i]]$y_teste.1*2+mega_lista[[i]]$y_teste.2)-2)*(-1)
  
  p2 <- mega_lista[[i]]$preditos
  
  t1 <- table(p1,p2)
  
  if (ncol(t1)==2){
    acc <- c(acc, sum(diag(t1))/sum(t1))
    pp1 <- t1[2,2]/(t1[2,2]+t1[1,2])
    precision <- c(precision, pp1)
    pp2 <- t1[2,2]/(t1[2,2]+t1[2,1])
    recall <- c(recall, pp2)
    f_score <- c(f_score, (((1/pp1)+(1/pp2))/2)^(-1))
  }
  
  else{
    if (unique(p2)==0){
      acc <- c(acc, sum(diag(t1))/sum(t1))
      precision <- c(precision, 0)
      recall <- c(recall, 0)
      f_score <- c(f_score, 0)
    }
    if (unique(p2)==1){
      acc <- c(acc, sum(diag(t1))/sum(t1))
      pp1 <- t1[2,1]/(t1[2,1]+t1[1,1])
      precision <- c(precision, pp1)
      pp2 <- 1
      recall <- c(recall, 1)
      f_score <- c(f_score, (((1/pp1)+(1/pp2))/2)^(-1))
    }
  }
  
}

result_all_metrics <- cbind(result_base1,acc,precision,recall,f_score)

# result_all_metrics <- result_all_metrics[c(2,1,4,3,6,5,8,7,10,9,12,11,14,13,16,15,18,17,20,19,
#                      22,21,24,23,26,25,28,27,30,29,32,31,34,33,36,35,38,37,40,39,
#                      42,41,44,43,46,45,48,47),]

print(result_all_metrics)
# View(result_all_metrics)

write.csv2(result_all_metrics,"metrics_BRA.csv")

xtable(result_all_metrics[,7:10],digits = c(0,7,7,7,7))

