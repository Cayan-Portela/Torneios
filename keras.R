library(mlbench)
library(keras)

# install_keras()

# Script inicial

BASIS <- BASIS %>% na.omit()

x_train <- BASIS[,-122] %>% as.matrix()
y_train <- ifelse( BASIS[,122] == 1 , 1 , 0) %>%
           as.matrix() %>%
           to_categorical(y_train, num_classes = 2)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 20, activation = 'sigmoid', input_shape = c(121)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 15, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)


