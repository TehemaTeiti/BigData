# setwd("/home/t_chen/travaux/5annee/BigData/")
# setwd("D:\\Etudes\\INSA\\5ISS\\Big_Data\\BigData")

# recommended libraries by teacher
#install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)

df <- read.csv("athlete_events.csv", sep = ",")

# select the most interesting data to show

# number of medal by country
df1 <- ddply(df, "NOC", summarise, somme = sum(count(Medal)$freq, na.rm = TRUE))
ggplot(df1, aes(x = NOC, y = somme, color = NOC)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

df2 <- ddply(df, c("Sport", "Sex"), summarise, somme = sum(count(Sex)$freq))
# facet_wrap(~Sex)
ggplot(df2, aes(x = Sport, y = somme, color = Sex)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# select the most interesting data to show
df3 <- ddply(df, c("NOC", "Year"), summarise, somme = sum(count(ID)$freq))
ggplot(df3, aes(x = NOC, y = somme, color = Year)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# attention! sometimes Age = N/A
df4 <- ddply(df, "Age", summarise, somme = sum(count(Medal)$freq, na.rm = TRUE))

# moyenne = N/A existe
df5 <- ddply(df, "NOC", summarise, moyenne = mean(Weight, trim = 0, na.rm = TRUE))



