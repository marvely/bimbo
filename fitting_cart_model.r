library(rpart)
library(sqldf)
setwd("~/Documents/Kaggle Project/grupo_bimbo")
train_orgin <- read.csv("~/Documents/Kaggle Project/grupo_bimbo/train.csv", stringsAsFactors=FALSE)
str(train_orgin)
variable_to_drop <- c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", "Dev_porxima")
train_orgin <- train_orgin[, !names(train_orgin) %in% variable_to_drop]

# change categorical cols into factors.
categorical_to_factor <- c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID")
train_orgin[categorical_to_factor] = lapply(train_orgin[categorical_to_factor], as.factor)

# split into train and validation sets
set.seed(22)
total_row_num <- nrow(train_orgin)
sample_size <- floor(0.75*total_row_num)
training_set_int <- sample(seq_len(total_row_num), size = sample_size)
training_set <- train_orgin[training_set_int, ]
validation_set <- train_orgin[-training_set_int, ]

# apply the package rpart
model_CART <- rpart( Demanda_uni_equil ~ ., data = training_set)
