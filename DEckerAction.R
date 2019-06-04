#################
#setwd("C:/Users/Henrik Decker/Documents/Henrik/Uni Kiel/6. Semester/Data Science/Datens?tze")
# Importing Function Packages
library(readr)
library(e1071)
library(Metrics)
library(dplyr)
library(readr)
library(haven)
library(ggplot2)
##70% of sample size
pisa <- read_sav("PISA-2015_GERMANY.sav")
pisa1 <- pisa[complete.cases(pisa),]
pisa_train <- floor(0.7 * nrow(pisa1))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(pisa1)), size = pisa_train)

train <- pisa1[train_ind, ]
test <- pisa1[-train_ind, ]

qplot(EAPMATH, PA042Q01TA, data=train)
mymodel <- svm(EAPMATH~ ST011Q03TA PA042Q01TA , data=train)
plot(mymodel, data= train)
#################