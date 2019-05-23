## libraries
library("haven")
library("ggplot2")
library("Rmisc")  # summarySE
library("readr")  # for svm
library("e1071")  # for svm
library("Metrics")  # for svm
library("dplyr")  # for svm

## import dataframes
pisa <- read_sav("PISA-2015_GERMANY.sav")
pisa_test <- read_sav("PISA-2015_GERMANY_test.sav")
pisa_train <- read_sav("PISA-2015_GERMANY_train.sav")


#### Aufgabe 1: Balkendiagramm, das signifikante Unterschiede einer kategoriellen Variable auf eine Leistung bzw. den Umsatz zeigt. ###########
## t-test: IV - quiet place to study (yes vs. no), DV - Reading Score
t.test(pisa$EAPREAD[pisa$ST011Q03TA==1],
       pisa$EAPREAD[pisa$ST011Q03TA==2])

## Balkendiagramm
# summarize mean reading scores depending on the availability of a quiet place to study
summaryReading <- summarySE(pisa, 
                            measurevar="EAPREAD", 
                            groupvars="ST011Q03TA", 
                            na.rm=TRUE)

# remove cases with missing values for immigration status
summaryReading <- summaryReading[!is.na(summaryReading$ST011Q03TA), ]

# create bar plot with confidence intervals as error bars
ggplot(data=summaryReading) +
  aes(x=factor(ST011Q03TA), y=EAPREAD) +
  geom_bar(stat="summary", fun.y="mean", width=0.7, fill="steelblue") +
  geom_errorbar(aes(ymin=EAPREAD-ci, ymax=EAPREAD+ci), width=.2) +
  ggtitle("PISA Germany: Reading comprehension") +
  scale_y_continuous(limits=c(0,600)) +
  scale_x_discrete(labels=c("Yes", "No")) +
  xlab("Availability of a quiet place to study") + 
  ylab("Mean EAP in reading comprehension") +
  theme_minimal()


#### Aufgabe 2: Optimierung eines linearen Modells zur Prognose einer Leistung bzw. des Umsatzes (Training anhand von 70% der Daten, Testung anhand der restlichen 30%)


#### Aufgabe 3: Optimierung einer SVM oder eines neuronalen Netzes zur Vorhersage einer frei wählbaren Variable.