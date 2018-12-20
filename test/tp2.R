setwd("D:\\Etudes\\INSA\\5ISS\\Big_Data\\TP")

library(ggplot2)
library(plyr)
library(reshape2)

####################################
###   1. Plots and Given Names   ###
####################################

prenoms<-read.csv("prenoms.csv",encoding = "UTF-8",sep=";")

# number of birth by year
nbBirthByYear<-ddply(prenoms,"Année",summarise,nbBirth=sum(Nombre))
ggplot(nbBirthByYear,aes(Année,nbBirth))+geom_bar(stat="identity")

# number of male/female births by year
nbBirthByGenderByYear<-ddply(prenoms,c("Année","Sexe"),summarise,nbBirth=sum(Nombre))
ggplot(nbBirthByGenderByYear,aes(Année,nbBirth,color=Sexe))+geom_bar(stat="identity",position = "dodge",aes(fill=Sexe))

# find a name in the dataset
findName<-function(name){
  return (length(which(prenoms$Prénom == name)) > 0)
}

trueName<-"Safa"
fakeName<-"Tehema"
findName(trueName)
findName(fakeName)

# top 10 most given names
countNames<-ddply(prenoms,"Prénom",summarise,nbName=sum(Nombre))
topTenNames<-countNames[order(countNames$nbName,decreasing = TRUE),][1:10,]
ggplot(topTenNames,aes(Prénom,nbName))+geom_bar(stat="identity")+xlab("Prénom")+ylab("Nombre")

# top 5 given names by sex + (their evolution by year)
countNamesBySex<-ddply(prenoms,c("Prénom","Sexe"),summarise,nbName=sum(Nombre))

countMaleNames<-countNamesBySex[countNamesBySex$Sexe=="M",]
topFiveMaleNames<-countMaleNames[order(countMaleNames$nbName,decreasing = TRUE),][1:5,]

countFemaleNames<-countNamesBySex[countNamesBySex$Sexe=="F",]
topFiveFemaleNames<-countFemaleNames[order(countFemaleNames$nbName,decreasing = TRUE),][1:5,]

topAllNames <- rbind(topFiveFemaleNames,topFiveMaleNames)
x<-arrange(topAllNames,Sexe,nbName)
topAllNames$Prénom2<-factor(x$Prénom,x$Prénom)
ggplot(x,aes(topAllNames$Prénom2,nbName,color=Sexe))+geom_bar(stat="identity",aes(fill=Sexe))+xlab("Prénom")+ylab("Nombre")

