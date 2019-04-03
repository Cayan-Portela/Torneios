library(keras)
library(dplyr)

# install_keras()

# Script inicial

set.seed(12345)

treino_id <- sample(x       = seq_len( nrow(base_completa2) ), 
                    size    = floor(0.75*nrow(base_completa2)),
                    replace = FALSE)
  
x_train <- base_completa2[treino_id, - 126] %>% scale()
x_test  <- base_completa2[-treino_id, - 126] %>% scale()

y_train <- ifelse( base_completa2[treino_id,126] == 1 , 1 , 0) %>%
           as.matrix()
y_train <- to_categorical(y_train, num_classes = 2)

y_test  <- ifelse( base_completa2[-treino_id,126] == 1 , 1 , 0) %>%
           as.matrix()
y_test  <- to_categorical(y_test, num_classes = 2)


resultados <- list()

# Criando objeto sequential model (MLP)

colunas <- list()

colunas$todas <- colnames(x_train)

colunas$lasso <- c("MFM","ADL","ADX","ADO","APO","AR_POS","AR_OSC","ATRP","AVOL","BB_BW",
                   "BWW","VOLAT","PERC_B","CCI","CVOL","CMO","ROC","COPP","DPO","DEMA","FORCE",
                   "MQO_ALPHA","MQO_BETA","MACDH","MASS","RMF","MOM","NVI","NATR","SAR",
                   "PVOH","RVI","STRSI","STOCH_K","STOCH_D_SLOW","TRIX","VOOSC","DISP",
                   "OSCP","DIU","VOLR","BRATIO","REX","VARR")

colunas$torneios <- c("KC_U","MQO_STD","FORCE","TP","EMA","LPR","PVOI","FR2","AVOL","SS2",
                    "CLOSE","NATR","BB_BW","FS1","ATRP","PERC_B","STOCH_D_SLOW","ADX",
                    "MASS","STRSI","WWS","TEMA","ARATIO","VOLAT","AR_OSC","MIDPOINT",
                    "MACDH","TRIX","MACD","VWAP","APO","MOM","BIAS","ULTOSC","DSS","PPOH",
                    "RMF","VOLR","DISP","MFM","WILL_R","CMF","TSI","VOLUME","SR2","VARR","FS2",
                    "DPO","DEMA","DS1")



colunas$stepwise <- c("DPO","DISP","VOLAT","MIDPOINT","PPOH","DSS","PVI","AD","RVI",
                      "HULL","NVI","DMI","STOCH_D_SLOW","BRATIO","BWW","EMV","ADX",
                      "ATRP","VOOSC","NATR","VARR","HPR","PSK","CHOSC","AR_OSC","RSI",
                      "MASS","KAMA","MQO_STD","VWAP","AR_POS","AB_DOWN","MQO_BETA",
                      "AR_NEG","APO","CHAND_SHORT","VAMA","BB_UP","BB_LOW","ROC","MFM",
                      "FORCE","PC_UP","MQO_ALPHA","PC_DOWN","DIU","MQO_PRED","ATR", "AVOL","KST")


# i = 4
# k = 'sigmoid'
# j = 0.3

system.time(
    for (i in 1:4) {
      
      colunas_sel <- which( colnames(base_completa2) %in% colunas[[i]] )
          
          for(j in c(0.3,0) ) {
            
              model <- keras_model_sequential() 
              
              # Definindo arquitetura da rede
              model %>% 
                    layer_dense(units = 15, activation = 'sigmoid', 
                                input_shape = length(colunas[[i]]) ) %>% 
                    layer_dropout(rate = j) %>% 
                    layer_dense(units = 15, activation = 'sigmoid') %>%
                    layer_dropout(rate = j) %>%
                    layer_dense(units = 15, activation = 'sigmoid') %>%
                    layer_dropout(rate = j) %>%
                    layer_dense(units = 15, activation = 'sigmoid') %>%
                    layer_dropout(rate = j) %>%
                    layer_dense(units = 2, activation = 'sigmoid')
    
              # Funcao de perda, metodo de otimizacao e metrica 
              model %>% 
                    compile(
                      loss = 'categorical_crossentropy',
                      optimizer = optimizer_rmsprop(),
                      metrics = c('accuracy',
                                  'binary_accuracy',
                                  'categorical_accuracy')
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
                                                                   Activation = 'sigmoid',
                                                                   Accuracy_in = history$metrics$val_acc[[400]],
                                                                   Accuracy_out   = model %>% 
                                                                                 evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                                                 .$acc,
                                                                   Binary_acc_out = model %>% 
                                                                                    evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                                                    .$binary_accuracy,
                                                                   Categ_acc_out  = model %>% 
                                                                                    evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                                                    .$categorical_accuracy
                                                                   )
          
          }
 
    }
    
)



##### -------------------- Fim Cenario 1: --------------------- ###### 

tabela_1 <- do.call( rbind, resultados )

write.csv(tabela_1, file = "tabela_1.csv", row.names = F)
save(tabela_1, file = 'tabela_1.RData')

#### ----- Cenario 2: 5 camadas ------------ #####
rm(resultados)
resultados <- list()

system.time(

for (i in 1:4){
  
  colunas_sel <- which( colnames(base_completa2) %in% colunas[[i]] )
    
    for(j in c(0.3,0) ){
      
      model <- keras_model_sequential() 
      
      # Definindo arquitetura da rede
      model %>% 
        layer_dense(units = 15, activation = 'sigmoid', input_shape = length(colunas[[i]]) ) %>% 
        layer_dropout(rate = j) %>% 
        layer_dense(units = 15, activation = 'sigmoid') %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = 'sigmoid') %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = 'sigmoid') %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = 'sigmoid') %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = 'sigmoid') %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 2, activation = 'sigmoid')
      
      # Funcao de perda, metodo de otimizacao e metrica 
      model %>% 
        compile(
          loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy',
                      'binary_accuracy',
                      'categorical_accuracy')
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
                                                           Hidden_layers = 5,
                                                           Dropout    = j,
                                                           Activation = 'sigmoid',
                                                           Accuracy_in = history$metrics$val_acc[[400]],
                                                           Accuracy_out   = model %>% 
                                                             evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                             .$acc,
                                                           Binary_acc_out = model %>% 
                                                             evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                             .$binary_accuracy,
                                                           Categ_acc_out  = model %>% 
                                                             evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                             .$categorical_accuracy
      )
    }
}


)

tabela_2 <- do.call( rbind, resultados )

write.csv(tabela_2, file = "tabela_2.csv", row.names = F)
save(tabela_2, file = 'tabela_2.RData')

##### ====== Fim ===== Cenario 2 ====== ######




##### -------------------- Cenario 3: 7 camadas --------------------- ###### 
rm(resultados)
resultados <- list()

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
            metrics = c('accuracy',
                        'binary_accuracy',
                        'categorical_accuracy')
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
                                                             Hidden_layers = 7,
                                                             Dropout    = j,
                                                             Activation = 'sigmoid',
                                                             Accuracy_in = history$metrics$val_acc[[400]],
                                                             Accuracy_out   = model %>% 
                                                               evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                               .$acc,
                                                             Binary_acc_out = model %>% 
                                                               evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                               .$binary_accuracy,
                                                             Categ_acc_out  = model %>% 
                                                               evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                               .$categorical_accuracy
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

