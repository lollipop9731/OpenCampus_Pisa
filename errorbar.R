library("ggplot2")

pisaData <- PISA_2015_GERMANY %>% select(HISCED,EAPMATH)

# Spalte hinzuf?gen als Charakter -> nicht continous
pisaData$HISCED_cat <- as.character(pisaData$HISCED)

# Calculate Mean and Standard deviation
data <- pisaData %>%
  group_by(HISCED_cat) %>%
  summarise(
    mean = mean(EAPMATH),
    sd = sd(EAPMATH)
  )

# Basic Histogram
ggplot(data) +
  geom_bar(aes(x=HISCED_cat,y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar(aes(x=HISCED_cat,ymin=mean-sd, ymax = mean + sd),width=0.4, colour="blue", alpha=0.9, size=1.5)+
  ggtitle("Standardabweichung HISCED und Mathescore") 

# T Test Vergleich Deutsche und Kurdische Sch?ler im Readscore
t.test(PISA_2015_GERMANY$EAPREAD[PISA_2015_GERMANY$LANGN==148],PISA_2015_GERMANY$EAPREAD[PISA_2015_GERMANY$LANGN==105])
