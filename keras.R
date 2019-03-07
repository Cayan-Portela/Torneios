library(mlbench)
library(keras)
library(dplyr)

# install_keras()

# Script inicial

x_train <- base_completa_tudo_treino[, - 126] %>% scale() # which(colnames(base_completa_tudo_treino) == "YYY") --- 126
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
model <- keras_model_sequential() 

# Definindo arquitetura da rede
model %>% 
  layer_dense(units = 20, activation = 'sigmoid', input_shape = c(125)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# Funcao de perda, metodo de otimizacao e metrica 
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Treina o modelo. (Definir outro epochs e batch_size ?)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

# Avaliacao na base teste. Esse valor de acuracia que utilizaremos no paper.
resultados[[1]] <- model %>% evaluate(x_test, y_test) %>% .$acc


##### -------------------- Fim Cenario 1: --------------------- ###### 





# ------------------------------------------------------------------- #





##### -------------------- Cenario 2: --------------------- ###### 
##### Tabela 1+2, LASSO selection, Sigmoid, Dropou 0.3. #####

lasso_sel <- c("MFM","ADX","CHOSC","ADO","APO","AR_POS","AR_OSC","ATR","ATRP","AVOL","BB_BW","BWW","PERC_B",
               "CCI","CMF","CVOL","ROC","COPP","DPO","DMI","DEMA","DSS","EMV","FORCE","HULL","MQO_ALPHA","MQO_STD",
               "MACD","VOLUME","MASS","RMF","MFI","MIDPOINT","MOM","NVI","NATR","OBV","DS1","CHOPPINESS","PPO","PPOH",
               "PVOH","PVI","KST","PSK","RSI","RVI","STRSI","STOCH_D","STOCH_K","STOCH_D_SLOW","TSI","ULCER","ULTOSC",
               "VOOSC","PVOI","WILL_R","DISP","OSCP","PSY","DIU","BIAS","VOLR","ARATIO","BRATIO","REX","HPR",
               "LPR","VMOM","VARR") 

colunas_lasso <- which( colnames(base_completa_tudo_treino) %in% lasso_sel )

model2 <- keras_model_sequential() 

# Definindo arquitetura da rede
model2 %>% 
  layer_dense(units = 20, activation = 'sigmoid', input_shape = c(70)) %>% # length(colunas_lasso) = 70 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# Funcao de perda, metodo de otimizacao e metrica 
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


history2 <- model2 %>% fit(
  x_train[ , colunas_lasso ], y_train,  # x_train apenas com colunas_lasso
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history2)

# Avaliacao na base teste
resultados[[2]] <- model2 %>% evaluate(x_test[, colunas_lasso], y_test) %>% .$acc

##### -------------------- Fim Cenario 2: --------------------- ###### 





# ------------------------------------------------------------------- #





##### -------------------- Cenario 3: --------------------- ###### 
##### Tabela 1+2, Torneio selection, Sigmoid, Dropout 0.3. #####

torneios_sel <- c("KC_U","MAE_UP","RSI","CLOSE","KC_M","KC_L","OPEN","COPP","TRIX","TEMA","STRSI",
"OSCP","FS1","MQO_BETA","MFM","PSY","SR1","FR1","DONCHIAN","STOCH_D_SLOW","WWS","BIAS",
"DISP","AB_UP","WMA","FS2","SAR","WILL_R","SS1","APO","MQO_PRED","PD1","MOM","MACD","TRIMA",
"TP","DR1","SR2","FR2","HPR","PPOH","MAE_LOW","MQO_ALPHA","EMA","STOCH_K","DID","MACDH",
"PPO","DEMA","DS1")

colunas_torneios <- which( colnames(base_completa_tudo_treino) %in% torneios_sel )

model3 <- keras_model_sequential() 

# Definindo arquitetura da rede
model3 %>% 
  layer_dense(units = 20, activation = 'sigmoid', input_shape = c(50)) %>% # length(colunas_torneios) = 70 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# Funcao de perda, metodo de otimizacao e metrica 
model3 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Treina o modelo. (Definir outro epochs e batch_size ?)
history3 <- model3 %>% fit(
  x_train[ , colunas_torneios ], y_train,  # x_train apenas com colunas_torneios
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history3)

# Avaliacao na base teste. Esse valor de acuracia que utilizaremos no paper.
resultados[[3]] <- model3 %>% evaluate(x_test[, colunas_torneios], y_test) %>% .$acc
