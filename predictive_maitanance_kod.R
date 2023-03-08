#install.packages('factoextra')
library(factoextra)
#install.packages("party") 
library(party)
#install.packages("caret)
library(caret)

set.seed(100)
dane<-read.csv("D:/szkoła/projekty/klasyfikacja/predictive-maintenance-dataset.csv",sep=",")
dane<-dane[2:9] 

vib<-dane[,3]
dane<-dane[,-3]
dane<-cbind(vib,dane)
dane<-na.omit(dane)
dane$vib<-round(dane$vib)
dane$vib<-as.factor(dane$vib)
#Bierzemy jedynie 1%danych do dalszej analizy 

ind<-sample(2,nrow(dane),replace = TRUE,prob = c(0.001,0.999))

data_1<-dane[ind==1,]

ind2<-sample(2,nrow(data_1),replace = TRUE,prob = c(0.7,0.3))

data_train<-data_1[ind2==1,]
data_test<-data_1[ind2==2,]

myf<- vib ~ revolutions+humidity+x1+x2+x3+x4+x5

windy_ctree<-ctree(myf,data=data_train)

temp<-table(predict(windy_ctree),data_train$vib)
temp

#wizualizacja drzewa dla jakiej wartosci ile razy wystepuje 
plot(windy_ctree)
#spróbujemy teraz przewidziec co algorytm drzewa zwroci dla danych testowych

test_pred<-predict(windy_ctree,newdata=data_test)
test_pred
table(test_pred,data_test$vib)

#jak możemy odczytać z tabeli tam gdzie wyniki się pokrywają program poprawnie przewidział jakie wibracje
#ma winda w porównaniu do innych parametrów


#sprobujemy innej metody przewidywania

#budujemy model treningowy
model<-train(vib ~.,data=data_train,
             method="svmPoly",
             na.action = na.omit,
             preProcess= c("scale","center"),
             trControl=trainControl(method="none"),
             tuneGrid=data.frame(degree=1,scale=1,C=1))


#budujemy model cv

model_cv<-train(vib ~.,data=data_train,
             method="svmPoly",
             na.action = na.omit,
             preProcess= c("scale","center"),
             trControl=trainControl(method="cv",number = 10),
                        tuneGrid=data.frame(degree=1,scale=1,C=1))

#przewidujemy
model.training<-predict(model,data_train) #dane traningowe
#model.training<-round(model.training)
model.training
#levels(as.factor(model.training))
#levels(as.factor(data_train$vib))
model.testing<-predict(model,data_test) #dane testowe
#model.testing<-round(model.testing)

model.cv<-predict(model_cv,data_train) #robimy spardzenie krzyżowe
#model.cv<-round(model.cv)


#macierz konfuzji
model.training.conf<-confusionMatrix(model.training,data_train$vib)
model.testing.conf<-confusionMatrix(model.testing,data_test$vib)
model.cv.conf<-confusionMatrix(model.cv,data_train$vib)


print(model.training.conf)
print(model.testing.conf)
print(model.cv.conf)



#wizualizacja które parametry mają wpływ na dany wynik
Importance<-varImp(model)
plot(Importance,col="red")





