# ESTRATEGIA, DO GREGO..

teste <- indicadores[[1]]
 
# Considerando preditos 1 = valorizar
acao <- c()

    for (i in 1: (nrow(teste)-1) ) {
      
      if ( teste[["preditos"]][i+1] - teste[["preditos"]][i]  == 1 ) {
            acao[i] <- "compra"
        } else if ( teste[["preditos"]][i+1] - teste[["preditos"]][i]  == -1 ) {
            acao[i] <- "vende"
        } else {
            acao[i] <- "mantem"
        }  
    }


# Numero de operacoes
sum(acao %in% c('vende','compra'))

# Ganhos usando o modelo
mod_op <- data.frame(Acao  = acao,
                     Fator = ifelse(acao == "compra" , 1 ,
                                    ifelse(acao == "vende" , -1 , 0)),
                     Preco = teste[["preco_close"]][2: length(teste[["preco_close"]])] )

ganho <- mod_op[["Preco"]] * mod_op[["Fator"]]
sum(ganho)
