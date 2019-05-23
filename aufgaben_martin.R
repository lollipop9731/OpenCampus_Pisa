#### setting up working directory ##############################################
setwd("C:/Users/Martin/Desktop/data_science/PISA-2015-GERMANY/")

#### packages ##################################################################
library("haven")  # import of the SPSS dataset
library("ggplot2")
library("Rmisc")  # summarySE

#### import of SPSS dataset ####################################################
df <- read_sav("PISA-2015_GERMANY.sav")

#### visualizations ############################################################
# histogram
ggplot(data=df) +
  aes(x=EAPMATH) +
  geom_histogram(binwidth=20, color="darkblue", fill="lightblue")+
  xlab("Math score") + 
  ylab("Count")

dev.off()

# bar plot
ggplot(data=df) +
  aes(x=factor(IMMIG), y=EAPMATH) +
  geom_bar(stat="summary", fun.y="mean", width=0.7, fill="steelblue") +
  xlab("Immigration status") + 
  ylab("Mean math Score") +
  theme_minimal()

dev.off()

# scatterplot
ggplot(data=df) +
  aes(x=EAPMATH, y=EAPREAD) + 
  geom_point() +
  theme_minimal()

dev.off()


## bar plot
# summarize reading comprehension for different immigration statuses
summaryReading <- summarySE(df, 
                            measurevar="EAPREAD", 
                            groupvars="IMMIG", 
                            na.rm=TRUE)

# remove cases with missing values for immigration status
summaryReading <- summaryReading[!is.na(summaryReading$IMMIG), ]

# create bar plot with standard deviations as error bars
ggplot(data=summaryReading) +
  aes(x=factor(IMMIG), y=EAPREAD) +
  geom_bar(stat="summary", fun.y="mean", width=0.7, fill="steelblue") +
  geom_errorbar(aes(ymin=EAPREAD-sd, ymax=EAPREAD+sd), width=.2) +
  ggtitle("Plot PISA Germany") +
  scale_y_continuous(limits=c(0,800)) +
  scale_x_discrete(labels=c("Native", "Second-Generation", "First-Generation")) +
  xlab("Immigration status") + 
  ylab("Mean EAP in reading comprehension") +
  theme_minimal()

dev.off()

#### linear regression #########################################################
# model estimation
mod <- lm(formula=EAPREAD ~ ST001D01T +        # student international grade (derived)
            # ST011Q03TA +     # quiet place to study
            ST013Q01TA +       # how many books
            PA042Q01TA +       # annual household income
            # IMMIG +          # index immigration status
            # MISCED +         # mother's education
            # FISCED +         # father's education
            # HISCED +         # highest education parents
            # BMMJ1 +          # isei mother
            BFMJ2 +            # isei father
            # hisei +          # index highest parental occupational status
            as.factor(LANGN),  # language at home
          data=df)

summary(mod)

#### svm #######################################################################
# Importing Function Packages
library(readr)
library(e1071)
library(Metrics)
library(dplyr)
library(haven)

# Importing Training and Test Data
pisa_test <- read_sav("PISA-2015_GERMANY_test.sav")
pisa_train <- read_sav("PISA-2015_GERMANY_train.sav")

pisa_test <- pisa_test[!is.na(pisa_test$PA042Q01TA), ]
pisa_test <- pisa_test[!is.na(pisa_test$EAPREAD), ]

## Data Preparation
# Training the SVM
svm_tune <- tune(svm, EAPREAD ~ PA042Q01TA, data=pisa_train, ranges = list(epsilon = seq(0,1,0.2), cost = 2^(2:3)))

## Checking the prediction Quality
# Calculating the prediction for the test dataset using the best model according to the grid search
pred <- predict(svm_tune$best.model, pisa_test)

# Calculating the prediction quality using the RMSE
rmse <- rmse(pisa_test$PA042Q01TA, pred)
