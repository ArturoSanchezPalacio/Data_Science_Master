####PRÁCTICA 2. PREDICCIÓN

##Arturo Sánchez Palacio
#En esta versión se descartan los current:

#Las bibliotecas a utilizar son:
library(dplyr)
library(ggplot2)
library(MASS)
library(ROCR)
library(caret)
library(DescTools)
library(SDMTools)
library(rsample) #Para construir la muestra
library(glmnet) #Para las regresiones cresta y risso
library(ISLR) #Para modelos polinomiales
#En primer lugar se cargan los datos en R

borrador <- read.csv("/Users/arturosanchezpalacio/Documents/CUNEF/Predicción/Informes/Préstamos mejorados/LoanStats_2016Q3.csv", sep = ",", skip = 1, header = TRUE)

#Comprobamos que la carga de datos se haya realizado de manera correcta:

str(borrador)
dim(borrador)



#Elijo las variables que se consideran según la literatura del tema las más relevantes:
datos<-borrador[, c("loan_status","loan_amnt","funded_amnt","funded_amnt_inv","grade","sub_grade","dti","revol_util","home_ownership","verification_status")]

#Se eliminan las observaciones con huecos por no ser una cantidad significativa

limpios<-datos[!is.na(datos$grade),]
limpios<-limpios[!is.na(limpios$loan_amnt),]
limpios<-limpios[!is.na(limpios$sub_grade),]
limpios<-limpios[!is.na(limpios$funded_amnt),]
limpios<-limpios[!is.na(limpios$funded_amnt_inv),]
limpios<-limpios[!is.na(limpios$dti),]
limpios<-limpios[!is.na(limpios$revol_util),]
limpios<-limpios[!is.na(limpios$home_ownership),]
limpios<-limpios[!is.na(limpios$loan_status),]
limpios<-limpios[!is.na(limpios$verification_status),]
#Al observar los datos se aprecia que existen huecos no cubiertos por NA si no por espacios vacíos "". Estas observaciones se eliminan también.
limpios<-limpios[!(limpios$revol_util==""),]


dim(limpios)
#Quedan 99060 observaciones
str(limpios)

ggplot(limpios, aes(limpios$loan_amnt)) + 
  geom_histogram(breaks=seq(0, 35000, by=1000), 
                 col="black", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="blue", high="red")+
  labs(title="Distribución de préstamos", x="Cantidad prestada", y="Número de préstamos")

Desc(limpios$grade, main = "Loan grades", plotit = TRUE)
Desc(limpios$sub_grade, main = "Loan subgrades", plotit = TRUE)

#DEPURACIÓN

#Convierto los porcentajes en ratios entre 0 y 1:

limpios[,"revol_util"] <- as.numeric(sub("%", "",limpios$"revol_util", fixed =TRUE))/100


#Dicotomizo el estado:
limpios<-limpios[limpios$loan_status!="Current",] #Estoy prescindiendo de los current porque no se sabe que va a pasar con ellos
limpios$loan_status <- as.character(limpios$loan_status)
limpios$loan_status[limpios$loan_status == "Current"| limpios$loan_status == "Fully Paid" ] <- 1
limpios$loan_status[limpios$loan_status != 1] <- 0


limpios$loan_status <- as.factor(limpios$loan_status)


#limpios['dti']<-limpios['dti']/10000

head(limpios)
str(limpios)
summary(limpios)



log1<-glm(loan_status~., data=limpios, family = binomial)
summary(log1)
#Se observa que las subgrades no aportan más información que las grades luego se eliminan.
log2<-glm(loan_status~.-sub_grade, data=limpios, family = binomial)
summary(log2)

step <- stepAIC(log2, direction="both")
step$anova

#Este análisis sugiere como modelo final uno que incluya: grade + dti + verification_status

log3<-glm(loan_status~ grade + revol_util + dti + 
            verification_status, data=limpios, family = binomial) #Aunque revol_util no aparece la conservo por el artículo

summary(log3)



#A continuación se procede a entrenar el modelo:

set.seed(123)
muestra <- initial_split(limpios, prop = .7, strata = "loan_status")
loans_train <- training(muestra)
loans_test  <- testing(muestra)

#fit.log será el modelo entrenado
fit.log <-  glm(loan_status~grade + revol_util + dti + 
                  verification_status,
                data = loans_train, family = binomial(link= "logit"))

fitted.results <- predict(fit.log, newdata = loans_test, type = "response")
loans_test$prob <- fitted.results

pred <- prediction(loans_test$prob,loans_test$loan_status)

confusion.matrix(round(loans_test$prob), loans_test$loan_status)




#HASTA AQUÍ LA MEJORA DE LA PRÁCTICA ANTERIOR

#VAMOS CON LA CRESTA

x=model.matrix(loan_status~., limpios)[,-1]
y=limpios$loan_status

loans_ridge <- glmnet(
  x,
  y,
  alpha = 0,
  family="binomial"
)

plot(loans_ridge, xvar = "lambda")

#Se crea una nueva muestra de entrenamiento:

set.seed(123)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod<-glmnet(x[train, ], y[train], alpha=0, family="binomial")

cv.out<-cv.glmnet(x[train, ], y[train], alpha=0, family="binomial")
cv.out
plot(cv.out)
(mejorlambda<-cv.out$lambda.min)
#El mejor lambda es 0.0005641603


ridge.pred<-predict(ridge.mod, s=mejorlambda, newx=x[test, ], type="class")
(fallidos.ridge<-sum(ridge.pred!=y.test))
length(y.test)



predict(loans_ridge, type="coefficients", s=mejorlambda)

#############################HASTA AQUÍ CRESTA ############################

#Ahora Lasso:

lasso.mod<-glmnet(x,y, alpha=1, family="binomial")
#lasso.mod<-glmnet(x[train,], y[train], alpha=1, family="binomial")
plot(lasso.mod)

#Aplicamos de nuevo cross validation:

set.seed(123)

cv_lasso_out<-cv.glmnet(x[train, ], y[train], alpha=1, family="binomial")
cv_lasso_out
plot(cv_lasso_out)
(mejorlambda2<-cv_lasso_out$lambda.min)
#El mejor lambda es 0.0005511902
lasso.pred<-predict(lasso.mod, s=mejorlambda2, newx = x[test, ], type="class")
(fallidos.lasso<-sum(lasso.pred!=y.test))

length(y.test)
#A simple vista es mejor el modelo cresta.

(lasso.coef<-predict(lasso.mod, type="coefficients", s=mejorlambda2))
#Muchas variables son prescindibles.


fit.elnet <- glmnet(x[train, ], y[train], family="binomial", alpha=.5)
plot(fit.elnet, xvar="lambda")

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0

fit.elnet.cv <- cv.glmnet(x[train, ], y[train], type.measure="mse", alpha=.5,
                          family="binomial")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x[train, ], y[train], 
                                            alpha=i/10, family="binomial"))
}



yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x[test, ], type="class")
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x[test, ], type="class")
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x[test, ], type="class")
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x[test, ], type="class")
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x[test, ], type="class")
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x[test, ], type="class")
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x[test, ], type="class")
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x[test, ], type="class")
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x[test, ], type="class")
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x[test, ], type="class")
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x[test, ], type="class")

(error0<- sum(yhat0!=y.test))
(error1<- sum(yhat1!=y.test))
(error2<- sum(yhat2!=y.test))
(error3<- sum(yhat3!=y.test))
(error4<- sum(yhat4!=y.test))
(error5<- sum(yhat5!=y.test))
(error6<- sum(yhat6!=y.test))
(error7<- sum(yhat7!=y.test))
(error8<- sum(yhat8!=y.test))
(error9<- sum(yhat9!=y.test))
(error10<- sum(yhat10!=y.test))



#De aquí se deduce que el alpha óptimo se encuentra en torno a 0.2 pues presenta el menor error.

#Calculamos su mejor lambda:

fit.elnet <- glmnet(x[train, ], y[train], family="binomial", alpha=0.2)
plot(fit.elnet)
cv_elnet_out<-cv.glmnet(x[train, ], y[train], alpha=.2, family="binomial")
plot(cv_elnet_out)
(mejorlambda3<-cv_elnet_out$lambda.min)
#El mejor lambda es 0.03335388

elnet.pred<-predict(fit.elnet, s=mejorlambda3, newx=x[test, ], type="class")
(fallidos.elnet<-sum(elnet.pred!=y.test))
length(y.test)


###################### HASTA AQUÍ ELASTIC NET ###########################


########## MODELOS POLINOMIALES ############

# modpol1<-lm(loan_status~grade+home_ownership+verification_status+revol_util, data=limpios)
# modpol2<-lm(loan_status~grade+home_ownership+verification_status+poly(revol_util,2), data=limpios)
# modpol3<-lm(loan_status~grade+home_ownership+verification_status+poly(revol_util,3), data=limpios)
# modpol4<-lm(loan_status~grade+home_ownership+verification_status+poly(revol_util,4), data=limpios)
# modpol5<-lm(loan_status~grade+home_ownership+verification_status+poly(revol_util,5), data=limpios)

modpol1<-lm(loan_status~revol_util, data=limpios)
modpol2<-lm(loan_status~poly(revol_util,2), data=limpios)
modpol3<-lm(loan_status~poly(revol_util,3), data=limpios)
modpol4<-lm(loan_status~poly(revol_util,4), data=limpios)
modpol5<-lm(loan_status~poly(revol_util,5), data=limpios)
anova(modpol1, modpol2, modpol3, modpol4, modpol5)


#No tienen sentido por ser una apróximación a una variable discreta (?).







################ CURVAS ROC ########################  

#### MODELO RIDGE

ridge.model<-cv.glmnet(x[train, ], y[train], alpha=0, family="binomial", type.measure = 'auc')

prob <- predict(ridge.model,type="response", 
                newx = x[test, ], s = mejorlambda)
pred <- prediction(prob, y.test)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
#ggsave("Curva ROC cresta.png")




### MODELO LASSO
lasso.model<-cv.glmnet(x[train, ], y[train], alpha=1, family="binomial", type.measure = 'auc')

prob <- predict(lasso.model,type="response", 
                newx = x[test, ], s = mejorlambda2)
pred <- prediction(prob, y.test)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
#ggsave("Curva ROC Lasso.png")

#MODELO ELASTIC NET
elastic.model<-cv.glmnet(x[train, ], y[train], alpha=0.2, family="binomial", type.measure = 'auc')

prob <- predict(elastic.model,type="response", 
                newx = x[test, ], s = mejorlambda3)
pred <- prediction(prob, y.test)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
#ggsave("Curva ROC elastic.png")
