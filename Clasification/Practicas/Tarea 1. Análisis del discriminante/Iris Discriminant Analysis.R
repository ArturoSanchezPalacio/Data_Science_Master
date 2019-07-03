### PRÁCTICA 1. Discriminant Analysis for the Iris Dataset

# Author: Arturo Sánchez Palacio
# Date: 21/XI/18
# Role: Student of a Data Science for Finance Master (CUNEF)


#These are the libraries we are going to use in this study. Make sure you have them installed before going on.

library(tidyverse)
library(MASS)
library(klaR)
library(mvnormtest)

# Firstly, we have a glance over the dataframe and check its quality:

head(iris)
str(iris)

sum(is.na(iris$Sepal.Length))
sum(is.na(iris$Sepal.Width))
sum(is.na(iris$Petal.Length))
sum(is.na(iris$Petal.Width))
sum(is.na(iris$Species))

#The dataset is complete. It is composed by four quantitative variables about the size of the flower and a categoric variable
#specifying the species of each specimen.

#First we are going to check how the sample is distributed. In order to do that we are going to plot the dimension of the petal and the sepal
#in a dispersion graph where each colour corresponds the species of each flower:

data <- iris
extradata <- mutate(data, Petal.Area = Petal.Width * Petal.Length ) #Calculates the variable Petal.Area (we assume petals are close to the shape of a rectangle)
extradata <- mutate(extradata, Sepal.Area = Sepal.Width * Sepal.Length) #Calculates the variable Sepal.Area (we assume sepals are close to the shape of a rectangle)

ggplot(data = extradata, aes(x = Petal.Area, y = Sepal.Area, col = Species)) + geom_point() #Interpretation of the graph available on the report.

#To finish this exploration we are going to test if the variables follow a normal distribution. In order to do that we are going to use
# the Shapiro-Wilks test through the function mshapiro.test available in the library mvnormtest:

setosa <- data[1:50,1:4]
versicolor <- data[51:100, 1:4]
virginica <- data[101:150, 1:4]

setosa <- t(setosa)
versicolor <- t(versicolor)
virginica <- t(virginica)
mshapiro.test(setosa)
mshapiro.test(versicolor)
mshapiro.test(virginica)

#From the Shapiro test we deduca that the distribution doesn't necessarily follow a normal distribution so we present
# two choices:

#CHOICE 1
#We assume that the difference is not significative and that we can work with the data.

#CHOICE 2
#We scale the varibles in order to guarantee that the distribution are normal and the Discriminant Analysis is effitient.







### CHOICE 1 ####

#Once we have done a small exploration of the sample we are going to check whether there is or not a significal difference between
# each population's means.

fit.manova <- manova(data = data, cbind(data$Sepal.Length, data$Petal.Length, data$Sepal.Width, data$Petal.Width)~data$Species)
summary((fit.manova),test = 'Wilks')

# After this comprobation we proceed to the Linear Discriminant Analysis. In order to conduct this analysis we are going to employ the library MASS.

(linear_analysis <- lda(Species ~., data = data ))

#The sample is perfectly balanced so there is no need to add prior probabilities.


# We also conduct the Quadratic Discriminant Analysis:

(quadratic_analysis <- qda(Species ~., data = data))

#Now that we have both analysis we can check prediction for new elements. Imagin we have a new flower with measurements: Petal: 1.6 x 0.4 and Sepal: 5.5 x 3.6 (length x width):

predict(linear_analysis,newdata = data.frame(Sepal.Length = 5.5,Sepal.Width = 3.6, Petal.Length = 1.6, Petal.Width = 0.4))$class
predict(quadratic_analysis,newdata = data.frame(Sepal.Length = 5.5,Sepal.Width = 3.6, Petal.Length = 1.6, Petal.Width = 0.4))$class

# Both prediction classify this flower as setosa (which seems consistent since its petals and sepals are quite tiny)


#Once this is done it is time to check which model gives better predictions. In order to do that the sample is going to be
#splitted in a training sample and a testing sample. We are going to repeat this experiment several times estimating in each the error.
#After this we are going to compare the mean error of both models. (Obviously) the one witht the smallest error will be the best one.

sample_size <- dim(iris)[1]
training_size <- sample_size * 0.7 #We take 80% from the original sample to train the model and 20% to test it.
testing_size <- sample_size - training_size

iterations <- 100 #We are going to repeat the experiment 100 times.

set.seed(12345) #We set the seed in order to obtain consistent results in the experiment

# First, we conduct the experiment over the Linear Analysis

linear_error <- dim(iterations) #We initialize a vector where we are going to store the error

for (k in 1:iterations) {
  train <- sample(1:sample_size,training_size) #We choose the training sample.
  m1 <- lda(Species~.,data[train,]) #We apply the Linear Discriminant Analysis over the training sample.
  predict(m1,data[-train,])$class #We use this model to predict the testing set.
  tablin <- table(data$Species[-train],predict(m1,data[-train,])$class) #This gives a table with the results from the analysis
  linear_error[k] <- (testing_size - sum(diag(tablin)))/testing_size #We calculate the error
}
specie_train <- data[train, 5]
plot(m1, col = as.integer(specie_train))
plot(m1, dimen = 1, type = "b")
(linear_error_mean <- mean(linear_error))
tablin

# Secondly, we conduct the experiment over the Quadratic Analysis

#We are going to use the same sample as in the first experiment:

quadratic_error <- dim(iterations)

for (k in 1:iterations) {
  train <- sample(1:sample_size,training_size) #We choose the training sample.
  m2 <- qda(Species~.,data[train,]) #We apply the Quadratic Discriminant Analysis over the training sample.
  predict(m2,data[-train,])$class #We use this model to predict the testing set.
  tablin <- table(data$Species[-train],predict(m2,data[-train,])$class) #This gives a table with the results from the analysis
  quadratic_error[k] <- (testing_size - sum(diag(tablin)))/testing_size #We calculate the error
}
(quadratic_error_mean <- mean(quadratic_error))
tablin

# Finally we are going to plot the results of both classifications (in order to do this qe use the function partimat included
# in the library klaR):


partimat(Species~.,data = data,method = "lda") 

partimat(Species~.,data = data,method = "qda") 


### CHOICE 2 ####

#Reminder: in this choice we decide to scale the variable to ensure that the follow a normal distribution:

scaled_data <- as.data.frame(scale(data[,-5]))
scaled_data <- mutate(scaled_data, Species = iris$Species)

#Once we have done a small exploration of the sample we are going to check whether there is or not a significal difference between
# each population's means.

fit.manova <- manova(data = scaled_data, cbind(scaled_data$Sepal.Length, scaled_data$Petal.Length, scaled_data$Sepal.Width, scaled_data$Petal.Width)~scaled_data$Species)
summary((fit.manova),test = 'Wilks')

# After this comprobation we proceed to the Linear Discriminant Analysis. In order to conduct this analysis we are going to employ the library MASS.

(linear_analysis <- lda(Species ~., data = scaled_data ))

#The sample is perfectly balanced so there is no need to add prior probabilities.


# We also conduct the Quadratic Discriminant Analysis:

(quadratic_analysis <- qda(Species ~., data = scaled_data))


#Once this is done it is time to check which model gives better predictions. In order to do that the sample is going to be
#splitted in a training sample and a testing sample. We are going to repeat this experiment several times estimating in each the error.
#After this we are going to compare the mean error of both models. (Obviously) the one witht the smallest error will be the best one.

sample_size <- dim(iris)[1]
training_size <- sample_size * 0.7 #We take 70% from the original sample to train the model and 30% to test it.
testing_size <- sample_size - training_size

iterations <- 100 #We are going to repeat the experiment 100 times.

set.seed(12345) #We set the seed in order to obtain consistent results in the experiment

# First, we conduct the experiment over the Linear Analysis

linear_error <- dim(iterations) #We initialize a vector where we are going to store the error

for (k in 1:iterations) {
  train <- sample(1:sample_size,training_size) #We choose the training sample.
  m1 <- lda(Species~.,scaled_data[train,]) #We apply the Linear Discriminant Analysis over the training sample.
  predict(m1,scaled_data[-train,])$class #We use this model to predict the testing set.
  tablin <- table(scaled_data$Species[-train],predict(m1,scaled_data[-train,])$class) #This gives a table with the results from the analysis
  linear_error[k] <- (testing_size - sum(diag(tablin)))/testing_size #We calculate the error
}
specie_train <- scaled_data[train, 5]
plot(m1, col = as.integer(specie_train))
plot(m1, dimen = 1, type = "b")
(linear_error_mean <- mean(linear_error))
tablin

# Secondly, we conduct the experiment over the Quadratic Analysis

#We are going to use the same sample as in the first experiment:

quadratic_error <- dim(iterations)

for (k in 1:iterations) {
  train <- sample(1:sample_size,training_size) #We choose the training sample.
  m2 <- qda(Species~.,scaled_data[train,]) #We apply the Quadratic Discriminant Analysis over the training sample.
  predict(m2,scaled_data[-train,])$class #We use this model to predict the testing set.
  tablin <- table(scaled_data$Species[-train],predict(m2,scaled_data[-train,])$class) #This gives a table with the results from the analysis
  quadratic_error[k] <- (testing_size - sum(diag(tablin)))/testing_size #We calculate the error
}
(quadratic_error_mean <- mean(quadratic_error))
tablin

# Finally we are going to plot the results of both classifications (in order to do this qe use the function partimat included
# in the library klaR):


partimat(Species~.,data = scaled_data,method = "lda") 

partimat(Species~.,data = scaled_data,method = "qda") 


