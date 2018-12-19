#############
### Setup ###
#############

setwd("/home/t_chen/travaux/5annee/BigData/BigData")
# setwd("D:\\Etudes\\INSA\\5ISS\\Big_Data\\BigData")

# recommended libraries by teacher
#install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)

df <- read.csv("athlete_events.csv", sep = ",")

###############
### Graph 1 ###
###############

# number of medal by country
df1 <- ddply(df, "NOC", summarise, somme = sum(count(Medal[!is.na(Medal)])$freq))
# ggplot(df1, aes(x = NOC, y = somme, color = NOC)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# top 10 countries by number of medals
topN<-function(df,col,n) {
  return (df[order(col,decreasing = TRUE),][1:n,])
}

df1_top10<-topN(df1,df1$somme,10)
# n'est pas utile
# x<-arrange(df1_top10,desc(df1_top10$somme))
df1_top10$NOC2<-factor(df1_top10$NOC, df1_top10$NOC)

ggplot(df1_top10, aes(NOC2,somme)) + 
  geom_bar(stat = "identity",aes(fill=NOC2)) + 
  geom_text(aes(label=somme),color="black",vjust=-0.3) +
  labs(title = "Top 10 countries with the most medals", x = "Country", y = "Nb of medal") + 
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

###############
### Graph 2 ###
###############

# pour plusieur graphes
# facet_wrap(~Sex)

Year <- df$Year
Sex <- df$Sex
Height <- df$Height
Weight <- df$Weight

df2 <- data.frame(Year, Sex, Height, Weight)
df2_BMI <- ddply(df2, c("Year", "Sex"), summarise, moyenne = round(mean(Weight/Height/Height*10000, na.rm = TRUE), 2))
ggplot(df2_BMI, aes(x = Year, y = moyenne, color = Sex)) +
  geom_line() +
  labs(title = "The average BMI of athelete of all countrise", x = "Year", y = "Average Height") +
  theme(plot.title = element_text(hjust = 0.5))


###############
### Graph 3 ###
###############

# select the most interesting data to show
df3 <- ddply(df, "NOC", summarise, NBAthelete = sum(count(ID)$freq))
df3_ordered <- df3[order(df1$somme, decreasing = TRUE),][1:10,]
df3_ordered$NOC <- factor(df3_ordered$NOC, df3_ordered$NOC)
ggplot(df3_ordered, aes(NOC, NBAthelete)) + 
  geom_bar(stat = "identity", aes(fill=NOC)) +
  labs(title = "Top 10 countries with the most atheletes", x = "Country", y = "Nb of athelete") + 
  geom_text(aes(label=NBAthelete),color="black",vjust=-0.3) +
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

###############
### Graph 4 ###
###############


df4 <- ddply(df, "Age", summarise, somme = sum(count(Medal[!is.na(Medal)])$freq))
df4 <- df4[!is.na(df4$Age),]
ggplot(df4, aes(x = Age, y = somme)) +
  geom_line()




