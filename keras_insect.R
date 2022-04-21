# https://www.kaggle.com/datasets/hammaadali/insects-recognition
# https://www.kaggle.com/code/hammaadali/cnn-insects-recognition/notebook

library(keras)


# path to dataset
path <- paste0(getwd(), "/Data")


imageBatchGen <- image_data_generator(
   rescale = 1/255, validation_split = 0.2,
   fill_mode = "nearest"
)

train_gen <- flow_images_from_directory(
   path, target_size = c(100,100), 
   class_mode = "categorical", batch_size = 100, 
   subset = "training", generator = imageBatchGen
   )

val_gen <- flow_images_from_directory(
   path, target_size = c(100,100), 
   class_mode = "categorical", batch_size = 100,
   subset = "validation", generator = imageBatchGen
   )


library(imager)
#Butterfly example
img <- load.image(paste0(path, "/Butterfly/google0.jpg"))
plot(img)

#Dragonfly example
img <- load.image(paste0(path, "/Dragonfly/google0.jpg"))
plot(img)

#Grasshopper example
img <- load.image(paste0(path, "/Grasshopper/google0.jpg"))
plot(img)

#Ladybird example
img <- load.image(paste0(path, "/Ladybird/google0.jpg"))
plot(img)

#Mosquito example
img <- load.image(paste0(path, "/Mosquito/google1.jpg"))
plot(img)


# Building a model
model <- keras_model_sequential() 

# exceeds free system memory
model %>% 
   layer_conv_2d(256, c(3, 3), activation = "relu", input_shape = c(100, 100, 3), padding = "same") %>% 
   layer_dropout(rate = 0.4) %>% 
   layer_conv_2d(128, c(3, 3), activation = "relu", padding = "same") %>% 
   layer_dropout(rate = 0.3) %>%
   layer_flatten() %>% 
   layer_dense(units = 1024, activation = "relu") %>% 
   layer_dense(units = 5, activation = 'softmax')
summary(model)

model %>% compile(
   loss = 'categorical_crossentropy',
   optimizer = optimizer_rmsprop(),
   metrics = c('accuracy')
)

steps <- train_gen$n / train_gen$batch_size
val_steps <- val_gen$n / val_gen$batch_size

library(tictoc)

tic()
model %>%
   fit_generator(
      train_gen, epochs = 10, 
      steps_per_epoch = steps,
      validation_data = val_gen, 
      validation_steps = val_steps
      )
toc()

