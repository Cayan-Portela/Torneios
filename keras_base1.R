library(keras)
library(dplyr)

# install_keras()

# Script inicial

x_train <- base_completa_tab1_treino[, - 52] %>% scale()   # YYY tab1 = 52   # which(colnames(base_completa_tudo_treino) == "YYY") --- 126
x_test  <- base_completa_tab1_teste[, - 52] %>% scale()

y_train <- ifelse( base_completa_tab1_treino[,52] == 1 , 1 , 0) %>%
  as.matrix() %>%
  to_categorical(y_train, num_classes = 2)

y_test  <- ifelse( base_completa_tab1_teste[,52] == 1 , 1 , 0) %>%
  as.matrix() %>%
  to_categorical(y_train, num_classes = 2)


resultados <- list()

##### -------------------- Cenario 1: --------------------- ###### 
##### Tabela 1+2, No feature selection, Sigmoid, Dropout 0.3 #####

colunas <- list()

colunas$todas <- colnames(x_train)

colunas$lasso <- c("SMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW","RSI","MACD",
                   "WILL_R","ADO","CCI","ROC","DISP","OSCP","PSY","DIU","DID","BIAS",
                   "VOLR","ARATIO","BRATIO","BB_LOW","DMI","TRIMA","REX","NVI","VAMA",
                   "HPR","LPR","VMOM","MPP","OBV","VOLAT","MFI","VARR","MQO_BETA","OPEN",
                   "CLOSE","VOLUME") 

colunas$torneios <- c("PSY","STOCH_K","RSI","BIAS","ROC","KC_U","MACD","CLOSE","WILL_R","CCI",
                      "MPP","VOLR","STOCH_D_SLOW","MAXX","KC_L","DISP","MAE_UP","MOM","SMA","WMA")

colunas$stepwise <- c("BIAS","STOCH_D","STOCH_K","ROC","DID","OSCP","MACD","MQO_BETA","NVI",
                    "VOLUME","MPP","PPO","VOLAT","DMI","VMOM","ATR","PVI","VARR","OBV","OPEN",
                    "DIU","CLOSE","PSY","MAXX","STOCH_D_SLOW","BRATIO","EMA","TRIMA","VOLR","KC_U",
                    "MOM","VAMA","ARATIO","MFI","CCI","LPR","WMA","SMA","BB_UP","BB_LOW","MINN","RSI",
                    "DISP","SAR","MAE_UP","MAE_LOW","HPR","KC_L","REX","WILL_R")

# i = 1
# k = 'sigmoid'
# j = 0


#### ======== CENARIO 1 ========== #####

for (i in 1:4){
  
  colunas_sel <- which( colnames(base_completa_tab1_treino) %in% colunas[[i]] )
  
  for (k in c('sigmoid','relu') ){
    
    for(j in c(0.3,0) ){
      
      model <- keras_model_sequential() 
      
      # Definindo arquitetura da rede
      model %>% 
        layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
        layer_dropout(rate = j) %>% 
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 2, activation = 'sigmoid')
      
      # Funcao de perda, metodo de otimizacao e metrica 
      model %>% 
        compile(
          loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy')
        )
      
      # Treina o modelo. (Definir outro epochs e batch_size ?)
      history <- model %>% fit(
        x_train[ , colunas_sel ], y_train, 
        epochs = 400, batch_size = 128, 
        validation_split = 0.2
      )
      
      #plot(history)
      
      # Accuracy = Avaliacao na base teste. Esse valor de acuracia que utilizaremos no paper.
      resultados[[ length(resultados) + 1 ]] <- data.frame(Variables  = names(colunas[i]),
                                                           Hidden_layers = 3,
                                                           Dropout    = j,
                                                           Activation = k,
                                                           Accuracy_in = history$metrics$val_acc[[400]],
                                                           Accuracy_out   = model %>% 
                                                             evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                             .$acc
      )
      
      
    }
  }
}


tabela_1_base1 <- do.call( rbind, resultados )

write.csv(tabela_1_base1, file = "tabela_1_base1.csv", row.names = F)
save(tabela_1_base1, file = 'tabela_1_base1.RData')
##### -------------------- Fim Cenario 1: --------------------- ###### 



#### ----- Cenario 2: 5 camadas ------------ #####
system.time(
  
  for (i in 1:4){
    
    colunas_sel <- which( colnames(base_completa_tab1_treino) %in% colunas[[i]] )
    
    for (k in c('sigmoid','relu') ){
      
      for(j in c(0.3,0) ){
        
        model <- keras_model_sequential() 
        
        # Definindo arquitetura da rede
        model %>% 
          layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
          layer_dropout(rate = j) %>% 
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 2, activation = 'sigmoid')
        
        # Funcao de perda, metodo de otimizacao e metrica 
        model %>% 
          compile(
            loss = 'categorical_crossentropy',
            optimizer = optimizer_rmsprop(),
            metrics = c('accuracy')
          )
        
        # Treina o modelo. (Definir outro epochs e batch_size ?)
        history <- model %>% fit(
          x_train[ , colunas_sel ], y_train, 
          epochs = 400, batch_size = 128, 
          validation_split = 0.2
        )
        
        plot(history)
        
        # Accuracy = Avaliacao na base teste. Esse valor de acuracia que utilizaremos no paper.
        resultados[[ length(resultados) + 1 ]] <- data.frame(Variables  = names(colunas[i]),
                                                             Hidden_layers = 5,
                                                             Dropout    = j,
                                                             Activation = k,
                                                             Accuracy_in = history$metrics$val_acc[[400]],
                                                             Accuracy_out   = model %>% 
                                                               evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                               .$acc
        )
        
        
      }
    }
  }
  
  
)

tabela_2_base1 <- do.call( rbind, resultados )

write.csv(tabela_2_base1, file = "tabela_2_base1.csv", row.names = F)
save(tabela_2_base1, file = 'tabela_2_base1.RData')

##### ====== Fim ===== Cenario 2 ====== ######




##### -------------------- Cenario 3: 7 camadas --------------------- ###### 

system.time(
  
  for (i in 1:4){
    
    colunas_sel <- which( colnames(base_completa_tab1_treino) %in% colunas[[i]] )
    
    for (k in c('sigmoid','relu') ){
      
      for(j in c(0.3,0) ){
        
        model <- keras_model_sequential() 
        
        # Definindo arquitetura da rede
        model %>% 
          layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
          layer_dropout(rate = j) %>% 
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 15, activation = k) %>%
          layer_dropout(rate = j) %>%
          layer_dense(units = 2, activation = 'sigmoid')
        
        # Funcao de perda, metodo de otimizacao e metrica 
        model %>% 
          compile(
            loss = 'categorical_crossentropy',
            optimizer = optimizer_rmsprop(),
            metrics = c('accuracy')
          )
        
        # Treina o modelo. (Definir outro epochs e batch_size ?)
        history <- model %>% fit(
          x_train[ , colunas_sel ], y_train, 
          epochs = 400, batch_size = 128, 
          validation_split = 0.2
        )
        
        plot(history)
        
        # Accuracy = Avaliacao na base teste. Esse valor de acuracia que utilizaremos no paper.
        resultados[[ length(resultados) + 1 ]] <- data.frame(Variables  = names(colunas[i]),
                                                             Hidden_layers = 7,
                                                             Dropout    = j,
                                                             Activation = k,
                                                             Accuracy_in = history$metrics$val_acc[[400]],
                                                             Accuracy_out   = model %>% 
                                                               evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                               .$acc
        )
        
        
      }
    }
  }
  
  
)

tabela_3_base1 <- do.call( rbind, resultados )

write.csv(tabela_3_base1, file = "tabela_3_base1.csv", row.names = F)
save(tabela_3_base1, file = 'tabela_3_base1.RData')


###### rbind de resultados #######

result_base1 <- rbind( tabela_1_base1,
                       tabela_2_base1,
                       tabela_3_base1)


write.csv(result_base1, file = "result_base1.csv", row.names = F)
save(result_base1, file = 'result_base1.RData')
