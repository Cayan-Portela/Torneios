#library(mlbench)
library(keras)
library(dplyr)

# install_keras()

# Script inicial

#base_completa_tudo_treino <- base_completa_tab1_treino
#base_completa_tudo_teste  <- base_completa_tab1_teste

x_train <- base_completa_tudo_treino[, - 126] %>% scale()   # YYY tab1 = 52   # which(colnames(base_completa_tudo_treino) == "YYY") --- 126
x_test  <- base_completa_tudo_teste[, - 126] %>% scale()

y_train <- ifelse( base_completa_tudo_treino[,126] == 1 , 1 , 0) %>%
  as.matrix() %>%
  to_categorical(y_train, num_classes = 2)

y_test  <- ifelse( base_completa_tudo_teste[,126] == 1 , 1 , 0) %>%
  as.matrix() %>%
  to_categorical(y_train, num_classes = 2)


resultados <- list()

##### -------------------- Cenario 1: --------------------- ###### 
##### Tabela 1+2, No feature selection, Sigmoid, Dropout 0.3 #####

# Criando objeto sequential model (MLP)

colunas <- list()

colunas$todas <- colnames(x_train)

colunas$lasso <- c("MFM","ADX","CHOSC","ADO","APO","AR_POS","AR_OSC","ATR","ATRP","AVOL","BB_BW","BWW","PERC_B",
               "CCI","CMF","CVOL","ROC","COPP","DPO","DMI","DEMA","DSS","EMV","FORCE","HULL","MQO_ALPHA","MQO_STD",
               "MACD","VOLUME","MASS","RMF","MFI","MIDPOINT","MOM","NVI","NATR","OBV","DS1","CHOPPINESS","PPO","PPOH",
               "PVOH","PVI","KST","PSK","RSI","RVI","STRSI","STOCH_D","STOCH_K","STOCH_D_SLOW","TSI","ULCER","ULTOSC",
               "VOOSC","PVOI","WILL_R","DISP","OSCP","PSY","DIU","BIAS","VOLR","ARATIO","BRATIO","REX","HPR",
               "LPR","VMOM","VARR") 

colunas$torneios <- c("KC_U","MAE_UP","RSI","CLOSE","KC_M","KC_L","OPEN","COPP","TRIX","TEMA","STRSI",
                  "OSCP","FS1","MQO_BETA","MFM","PSY","SR1","FR1","DONCHIAN","STOCH_D_SLOW","WWS","BIAS",
                  "DISP","AB_UP","WMA","FS2","SAR","WILL_R","SS1","APO","MQO_PRED","PD1","MOM","MACD","TRIMA",
                  "TP","DR1","SR2","FR2","HPR","PPOH","MAE_LOW","MQO_ALPHA","EMA","STOCH_K","DID","MACDH",
                  "PPO","DEMA","DS1")



colunas$stepwise <- c("MFM", "BIAS","PSY","STOCH_D","STOCH_K","STOCH_D_SLOW","ULTOSC","PPOH","CCI","VOLR",
                     "DIU", "MFI","BRATIO","DISP","NVI","BWW","PERC_B","EMV","COPP","MACD","ROC","OSCP","MASS",
                     "VOLUME", "ADX","CHOSC","PVOH","CHOPPINESS","PVO","DMI","RVI","MACDH","PPO","REX","MOM",
                     "VOOSC", "APO","DSS","ADL","HULL","OBV","VMOM","MPP","VARR","ATR","VOLAT","AD","MIDPRICE",
                     "ARATIO", "MQO_STD")

i = 1
k = 'sigmoid'
j = 0
    for (i in 1:4){
      
      colunas_sel <- which( colnames(base_completa_tudo_treino) %in% colunas[[i]] )
     
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
    
              plot(history)
              
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

##### -------------------- Fim Cenario 1: --------------------- ###### 



#### ----- Cenario 2: 5 camadas ------------ #####
system.time(

for (i in 1:4){
  
  colunas_sel <- which( colnames(base_completa_tudo_treino) %in% colunas[[i]] )
  
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

tabela_2 <- do.call( rbind, resultados )

write.csv(tabela_2, file = "tabela_2.csv", row.names = F)
save(tabela_2, file = 'tabela_2.RData')

##### ====== Fim ===== Cenario 2 ====== ######




##### -------------------- Cenario 3: 7 camadas --------------------- ###### 

system.time(
  
  for (i in 1:4){
    
    colunas_sel <- which( colnames(base_completa_tudo_treino) %in% colunas[[i]] )
    
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

tabela_3 <- do.call( rbind, resultados )

write.csv(tabela_3, file = "tabela_3.csv", row.names = F)
save(tabela_3, file = 'tabela_3.RData')


###### result base completa ######

result_completa <- rbind( tabela_1,
                          tabela_2,
                          tabela_3)


write.csv(result_completa, file = "result_completa.csv", row.names = F)
save(result_completa, file = 'result_completa.RData')

