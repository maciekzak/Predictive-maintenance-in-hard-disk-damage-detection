#install.packages('xgboost')
library(xgboost)

#install.packages('caret')
library(caret)

#install.packages('mltools')
library(mltools)

#install.packages('data.table')
library(data.table)

#install.packages('forecast')
library(forecast)

#install.packages('ggplot2')
library(ggplot2)

#install.packages('rpart.plot')
library(rpart.plot)

#przy pierwszym odpaleniu nalezy  odkomentowac i zainstalowac pakiety

#dane test 1
#######################
data_test<-read.csv("D:/INNE/sieci/dane_do_testu_csv_ST12000NM0007.csv",sep = ";",dec = ".")
data_test<-data_test[,-c(1:2)]

#przy pierwszym wczytaniu należy zmienic scieżki plikow 

#standaryzuje dane poniewaz po wczytaniu maja typ "character"

failure<-data_test$failure
data_test<-data_test[,-1]
data_test<-cbind(data_test,failure)

data_test$failure<-as.factor(data_test$failure)

data_test$smart_5_normalized<-as.numeric(data_test$smart_5_normalized)
data_test$smart_5_raw<-as.numeric(data_test$smart_5_raw)
data_test$smart_187_normalized<-as.numeric(data_test$smart_187_normalized)
data_test$smart_187_raw<-as.numeric(data_test$smart_187_raw)
data_test$smart_188_normalized<-as.numeric(data_test$smart_188_normalized)
data_test$smart_188_raw<-as.numeric(data_test$smart_188_raw)
data_test$smart_197_normalized<-as.numeric(data_test$smart_197_normalized)
data_test$smart_197_raw<-as.numeric(data_test$smart_197_raw)
data_test$smart_198_normalized<-as.numeric(data_test$smart_198_normalized)
data_test$smart_198_raw<-as.numeric(data_test$smart_198_raw)
#jako ze w danych wystepowaly wartosci NA musialem je zlikwidowac
data_test<-na.omit(data_test)


#konkretny dysk nr1
########################
data_test2<-read.csv("D:/INNE/sieci/ZCH08034.csv",sep = ";",dec = ".")
data_test2<-data_test2[,-c(1:2)]

failure2<-data_test2$failure
data_test2<-data_test2[,-1]
data_test2<-cbind(data_test2,failure2)


data_test2$smart_5_normalized<-as.numeric(data_test2$smart_5_normalized)
data_test2$smart_5_raw<-as.numeric(data_test2$smart_5_raw)
data_test2$smart_187_normalized<-as.numeric(data_test2$smart_187_normalized)
data_test2$smart_187_raw<-as.numeric(data_test2$smart_187_raw)
data_test2$smart_188_normalized<-as.numeric(data_test2$smart_188_normalized)
data_test2$smart_188_raw<-as.numeric(data_test2$smart_188_raw)
data_test2$smart_197_normalized<-as.numeric(data_test2$smart_197_normalized)
data_test2$smart_197_raw<-as.numeric(data_test2$smart_197_raw)
data_test2$smart_198_normalized<-as.numeric(data_test2$smart_198_normalized)
data_test2$smart_198_raw<-as.numeric(data_test2$smart_198_raw)

data_test2$failure2<-as.factor(data_test2$failure2)

data_test2<-na.omit(data_test2)

#konkretny dysk nr2
#####################
data_test3<-read.csv("D:/INNE/sieci/ZCH0CJ7L.csv",sep = ";",dec = ".")
data_test3<-data_test3[,-c(1,2)]

failure3<-data_test3$failure
data_test3<-data_test3[,-1]
data_test3<-cbind(data_test3,failure3)

data_test3$smart_5_normalized<-as.numeric(data_test3$smart_5_normalized)
data_test3$smart_5_raw<-as.numeric(data_test3$smart_5_raw)
data_test3$smart_187_normalized<-as.numeric(data_test3$smart_187_normalized)
data_test3$smart_187_raw<-as.numeric(data_test3$smart_187_raw)
data_test3$smart_188_normalized<-as.numeric(data_test3$smart_188_normalized)
data_test3$smart_188_raw<-as.numeric(data_test3$smart_188_raw)
data_test3$smart_197_normalized<-as.numeric(data_test3$smart_197_normalized)
data_test3$smart_197_raw<-as.numeric(data_test3$smart_197_raw)
data_test3$smart_198_normalized<-as.numeric(data_test3$smart_198_normalized)
data_test3$smart_198_raw<-as.numeric(data_test3$smart_198_raw)

data_test3$failure3<-as.factor(data_test3$failure3)


################################################################################

data_train<-read.csv("D:/INNE/sieci/train.csv",sep = ";",dec = ".")
data_train$date<-as.POSIXct(data_train$date,format = "%d.%m.%Y")
data_train$date<-as.Date(data_train$date) 
data_train<-na.omit(data_train)


data_train$smart_5_normalized<-as.numeric(data_train$smart_5_normalized)
data_train$smart_5_raw<-as.numeric(data_train$smart_5_raw)
data_train$smart_187_normalized<-as.numeric(data_train$smart_187_normalized)
data_train$smart_187_raw<-as.numeric(data_train$smart_187_raw)
data_train$smart_188_normalized<-as.numeric(data_train$smart_188_normalized)
data_train$smart_188_raw<-as.numeric(data_train$smart_188_raw)
data_train$smart_197_normalized<-as.numeric(data_train$smart_197_normalized)
data_train$smart_197_raw<-as.numeric(data_train$smart_197_raw)
data_train$smart_198_normalized<-as.numeric(data_train$smart_198_normalized)
data_train$smart_198_raw<-as.numeric(data_train$smart_198_raw)
data_train<-na.omit(data_test5)
data_train$failure<-as.factor(data_train$failure)
data_train<-na.omit(data_train)


table(data_train$failure)

train_up_sample2<-upSample(x=data_train[,-3],y=data_train$failure)
train_up_sample2<-train_up_sample2[,-c(1,2)]
#sprawdzamy czy rekordow sie zgadza
table(train_up_sample2$Class)




#wykresy
plot(density(train_up_sample2$smart_5_raw), main = "smart5_raw", type="l", col="red",ylim=c(0,0.002))
plot(density(train_up_sample2$smart_197_raw), main = "smart197_raw", type="l", col="red",ylim=c(0,0.1))






#########################################################################
#trenujemy model

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), #liczba drzew
  max_depth = c(2,4,6), #zwiekszanie tego wplywa na zwiekszenie dokladnosci i zuzycia pamieci
  eta = 0.3, #c(0.025,0.05,0.1,0.3), #tempo nauki
  gamma = 0, # wplywa na to jak duzy wplyw ma pojedyncza probka treningowa
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) próbka kolumn dla drzewa
  min_child_weight = 1, # c(1,2,3) # odpawiada za minimalna liczbe wystapien w kazdym wezle
  subsample = 1 # c(0.5, 0.75, 1.0) # uzywa sie by nie przeszlo do przesadnego dopasowania modelu
)

xgb_tune2 <- train(x = train_up_sample2[,-11],
                  y = train_up_sample2[,11],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
xgb_tune2


xgb_tune2$bestTune

############
#wybieramy parametry dla ktorych algorytm mial najlepsza skotecznosc

train_control2<- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)

final_grid <- expand.grid(nrounds = xgb_tune2$bestTune$nrounds,
                          eta = xgb_tune2$bestTune$eta,
                          max_depth = xgb_tune2$bestTune$max_depth,
                          gamma = xgb_tune2$bestTune$gamma,
                          colsample_bytree = xgb_tune2$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune2$bestTune$min_child_weight,
                          subsample = xgb_tune2$bestTune$subsample)

xgb_model2 <- train(x = train_up_sample2[,-11],
                   y = train_up_sample2[,11],
                   trControl = train_control2,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)
xgb_model2


###########
#sprawdzamy dla danych testowych
xgb.pred1<- predict(xgb_model2, data_test)
predict(xgb_model2, data_test)



confusionMatrix(as.factor(as.numeric(xgb.pred1)),
                as.factor(as.numeric(data_test$failure)))



#dla konkretnego dysku
xgb.pred2<- predict(xgb_model2, data_test2)
predict(xgb_model2, data_test2)


confusionMatrix(as.factor(as.numeric(xgb.pred2)),
                as.factor(as.numeric(data_test2$failure2)))


#dla kolejnego dysku algorytm wykryl z duzym wyprzedzeniem ze zachodza jakies anomalie i ze moze ulec uszkodzeniu

xgb.pred3<- predict(xgb_model2, data_test3)
predict(xgb_model2, data_test3)


confusionMatrix(as.factor(as.numeric(xgb.pred3)),
                as.factor(as.numeric(data_test3$failure3)))




#rysujemy wykres dla drzewa klasyfikacji (to nie jest na podstawie naszego modelu xgBoost)

fit <- rpart(train_up_sample2$Class~., data = train_up_sample2, method = 'class')
rpart.plot(fit, extra = 110,fallen.leaves = FALSE)






