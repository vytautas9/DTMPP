# https://www.kaggle.com/datasets/crowww/a-large-scale-fish-dataset
# https://www.kaggle.com/code/hammaadali/cnn-insects-recognition/notebook
# https://studymachinelearning.com/keras-imagedatagenerator-with-flow_from_directory/

library(keras)
library(tictoc)
library(imager)

set.seed(123)

# path to dataset
path <- paste0(getwd(), "/Data")

# We removed "GT" folders from the dataset

# We'll divide image pixels by 255 and fill border pixels by the nearest pixel
imageBatchGen <- image_data_generator(
   rescale = 1./255, validation_split = 0.2, fill_mode = "nearest"
)

# Train dataset
train_gen <- flow_images_from_directory(
   paste0(path, "/Fish_Dataset/Fish_Dataset"), target_size = c(150,150), 
   class_mode = "categorical", batch_size = 32, 
   color_mode = "rgb", shuffle = TRUE,
   generator = imageBatchGen, subset = "training",
   seed = 123
   )

# Validation dataset
val_gen <- flow_images_from_directory(
   paste0(path, "/Fish_Dataset/Fish_Dataset"), target_size = c(150,150), 
   class_mode = "categorical", batch_size = 32,
   color_mode = "rgb", shuffle = FALSE,
   generator = imageBatchGen, subset = "validation",
   seed = 123
   )














#--------------- feed forward  1 ---------------#


model <- keras_model_sequential() 

model %>% 
   layer_flatten(input_shape = c(150, 150, 3)) %>% 
   
   layer_dense(32, activation = "relu") %>% 
   
   layer_dense(16, activation = "relu") %>% 
   
   layer_dense(9, activation = 'softmax')

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

# 565.45 sec elapsed | 11%

#-----------------------------------------------#










#--------------- feed forward  2 ---------------#


model <- keras_model_sequential() 

model %>% 
   layer_flatten(input_shape = c(150, 150, 3)) %>% 
   
   layer_dense(256, activation = "relu") %>% 
   
   layer_dense(128, activation = "relu") %>% 
   
   layer_dense(9, activation = 'softmax')

# be drop out labai overfittina (pasiekia beveik 100%)

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

# 614.39 sec elapsed | 54%

#-----------------------------------------------#













#-------------------- CNN 1 --------------------#
# Building a model
model <- keras_model_sequential() 

model %>% 
   layer_conv_2d(32, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   layer_dropout(0.3) %>% 
   
   layer_conv_2d(16, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   layer_dropout(0.3) %>% 
   
   layer_flatten() %>% 
   
   layer_dense(16, activation = "relu") %>% 
   layer_dropout(0.5) %>% 
   
   layer_dense(9, activation = 'softmax')

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]

y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)
#---------------------------------------------#

















#-------------------- CNN 2 --------------------#
# Building a model
model <- keras_model_sequential() 

model %>% 
   layer_conv_2d(32, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   layer_dropout(rate = 0.3) %>% 
   
   layer_conv_2d(64, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   layer_dropout(0.3) %>% 
   
   layer_flatten() %>% 
   
   layer_dense(128, activation = "relu") %>% 
   layer_dropout(0.5) %>% 
   
   layer_dense(64, activation = "relu") %>% 
   layer_dropout(0.5) %>% 
   
   layer_dense(9, activation = 'softmax')

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]


y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

#---------------------------------------------#













#-------------------- CNN 3 --------------------#
# Building a model
model <- keras_model_sequential() 

model %>% 
   layer_conv_2d(64, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(64, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(32, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(32, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_flatten() %>% 
   
   layer_dense(512, activation = "relu") %>% 
   layer_dense(128, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(128, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(9, activation = 'softmax')

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]


y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

#---------------------------------------------#







#-------------------- CNN 4 --------------------#
# Building a model
model <- keras_model_sequential() 

model %>% 
   layer_conv_2d(64, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(64, c(3, 3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(128, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_conv_2d(128, c(3,3), activation = "relu", input_shape = c(150, 150, 3), padding = "same") %>% 
   layer_max_pooling_2d(2, 2) %>% 
   
   layer_flatten() %>% 
   
   layer_dense(1024, activation = "relu") %>% 
   layer_dense(512, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(512, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(9, activation = 'softmax')

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]


y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

#---------------------------------------------#








#----------- Transfer learning  1---------------#
base_model <- application_resnet50(weights="imagenet", include_top=FALSE, pooling="max", input_shape = c(150, 150, 3))

freeze_weights(base_model)

predictions <- base_model$output %>% 
   layer_dense(512, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(256, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(128, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(9, activation = 'softmax')

model <- keras_model(base_model$input, predictions)

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]


y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

#---------------------------------------------#











#----------- Transfer learning  2---------------#
base_model <- application_resnet50(weights="imagenet", include_top=FALSE, pooling="max", input_shape = c(150, 150, 3))

freeze_weights(base_model)

predictions <- base_model$output %>% 
   layer_dense(1024, activation = "relu") %>% 
   layer_dense(512, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(512, activation = "relu") %>% 
   layer_dropout(0.2) %>% 
   
   layer_dense(9, activation = 'softmax')

model <- keras_model(base_model$input, predictions)

summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_adam(),#optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

tic()
history <- model %>%
   fit(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
   )
toc()

history$metrics$accuracy[length(history$metrics$accuracy)]


y_true <- val_gen$classes %>% 
   as.factor()
y_pred <- model %>% 
   predict(val_gen, steps = val_steps + 1) %>% 
   max.col() %>% 
   -1 %>% 
   as.factor()

caret::confusionMatrix(y_pred, y_true)

#---------------------------------------------#