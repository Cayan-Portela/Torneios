rm(list=ls());gc()

library(tidyverse)
library(xtable)

setwd("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/RESULTS FINAL")


# ESTRATEGIA, DO GREGO..

mega_lista <- c(indicadores1,indicadores2,indicadores3)

transactions <- c()
ganho <- c()
TC_zero <- c()
TC_BH <- c()

BandH <- as.numeric(BandH)

for (ind in 1:length(mega_lista)){

teste <- mega_lista[[ind]]

# Considerando preditos 1 = valorizar

acao <- c(ifelse(teste$preditos[1]==1, "compra","mantem"))

for (i in 1: (nrow(teste)-2) ) {
  
  if ( teste[["preditos"]][i+1] - teste[["preditos"]][i]  == 1 ) {
    acao[i+1] <- "compra"
  } else if ( teste[["preditos"]][i+1] - teste[["preditos"]][i]  == -1 ) {
    acao[i+1] <- "vende"
  } else {
    acao[i+1] <- "mantem"
  }  
}

# Numero de operacoes
transactions <- c(transactions, sum(acao %in% c('vende','compra')))

# Ganhos usando o modelo
mod_op <- data.frame(Acao  = acao,
                     Fator = ifelse(acao == "compra" , -1 ,
                                    ifelse(acao == "vende" , 1 , 0)),
                     Preco = teste[["preco_close"]][1: length(teste[["preco_close"]])-1] )

ganho <- c(ganho, sum(mod_op[["Preco"]] * mod_op[["Fator"]]))

TC_zero <- c(TC_zero, (sum(mod_op[["Preco"]] * mod_op[["Fator"]]))/(sum(acao %in% c('vende','compra'))))
TC_BH <- c(TC_BH, (sum(mod_op[["Preco"]] * mod_op[["Fator"]]) - BandH)/(sum(acao %in% c('vende','compra'))))

}



result_strategies <- cbind(result_base1[,1:3],ganho,transactions,TC_zero,TC_BH)

# result_strategies <- result_strategies[c(2,1,4,3,6,5,8,7,10,9,12,11,14,13,16,15,18,17,20,19,
#                                            22,21,24,23,26,25,28,27,30,29,32,31,34,33,36,35,38,37,40,39,
#                                            42,41,44,43,46,45,48,47),]

result_strategies$TC_zero[which(is.nan(result_strategies$TC_zero))] <- 0
result_strategies$TC_BH[which(is.infinite(result_strategies$TC_BH))] <- 0

print(result_strategies)
# View(result_all_metrics)

write.csv2(result_strategies,"strategies_ALE.csv")


xtable(result_strategies[,4:7],digits = c(0,4,0,7,7))










