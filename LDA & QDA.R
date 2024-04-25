#LDA 
library(rattle) #for wine dataset
data(wine , package="rattle")
str(wine)
attach(wine)
colSums(is.na(wine))

library(MASS)
b=boxplot(wine)
b$out

#LDA
model_lda = lda(Type~.,data=wine)
model_lda
model_lda_vals = predict(wine_lda)
model_lda_vals

# Plotting Histogram and representing classes graphically
ldahist(data = model_lda_vals$x[,1] , g=wine$Type)
plot(model_lda_vals$x[,1],model_lda_vals$x[,2])
text(model_lda_vals$x[,1],model_lda_vals$x[,2],Type,cex = 0.7,pos = 4,col ="blue")

#confusion matrix
library(caret)
library(lattice)
C=confusionMatrix(wine_lda_vals$class,wine$Type)
C

#QDA
names(wine)
model_qda = qda(Type~.,data=wine)
model_qda
model_qda_predictval = predict(model_qda)
model_qda_predictval

QDA_conf_matrix = confusionMatrix(model_qda_predictval$class , wine$Type)
QDA_conf_matrix

#roc Curve:
install.packages('ROCR')
library(ROCR)
#make prediction
wpre_lda = prediction(model_lda_vals$posterior[,2],wine$Type ==2)
wper_lda = performance(wpre_lda,"tpr","fpr")
plot(wper_lda,col = "blue",lwd = 2, main = "ROC Curve",xlim = c(0,1),ylim = c(0,1))
