# setwd("/home/t_chen/travaux/5annee/BigData/")
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
ggplot(df1, aes(x = NOC, y = somme, color = NOC)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# top 10 countries by number of medals
topN<-function(df,col,n) {
  return (df[order(col,decreasing = TRUE),][1:n,])
}
df1_top10<-topN(df1,df1$somme,10)
x<-arrange(df1_top10,desc(df1_top10$somme))
df1_top10$NOC2<-factor(x$NOC,x$NOC)
ggplot(df1_top10, aes(NOC2,somme,color=NOC2)) + geom_bar(stat = "identity",aes(fill=NOC2)) + xlab("Country") + ylab("Nb of medal") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
### Graph 2 ###
###############

df2 <- ddply(df, c("Sport", "Sex"), summarise, somme = sum(count(Sex)$freq))
# facet_wrap(~Sex)
ggplot(df2, aes(x = Sport, y = somme, color = Sex)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
### Graph 3 ###
###############

# select the most interesting data to show
df3 <- ddply(df, c("NOC", "Year"), summarise, somme = sum(count(ID)$freq))
ggplot(df3, aes(x = NOC, y = somme, color = Year)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
### Graph 4 ###
###############

# attention! sometimes Age = N/A
df4 <- ddply(df, "Age", summarise, somme = sum(count(Medal)$freq, na.rm = TRUE))

###############
### Graph 5 ###
###############

# moyenne = N/A existe
df5 <- ddply(df, "NOC", summarise, moyenne = mean(Weight, trim = 0, na.rm = TRUE))



