#############
### Setup ###
#############

# recommended libraries by teacher
# install.packages("ggplot2")
library(ggplot2)
library(plyr)
library(reshape2)

df <- read.csv("athlete_events.csv", sep = ",")

###############
### Graph 1 ###
###############

# number of medal by country
df1 <- ddply(df, "NOC", summarise, 
             nbMedals = sum(count(Medal[!is.na(Medal)])$freq), 
             nbAthletes = sum(count(unique(ID))$freq),
             avgMedalPerAthletes = nbMedals/nbAthletes*100)

# top 10 countries by number of medals
topN<-function(df,col,n) {
  return (df[order(col,decreasing = TRUE),][1:n,])
}

df1_top10<-topN(df1,df1$nbMedals,10)
df1_top10$NOC<-factor(df1_top10$NOC, df1_top10$NOC)

ggplot(df1_top10, aes(NOC,nbMedals)) + 
  geom_bar(stat = "identity",aes(fill=NOC)) + 
  geom_text(aes(label=nbMedals),color="black",vjust=-0.3) +
  labs(title = "Top 10 countries with the most medals", x = "Country", y = "Nb of medal") + 
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("top10countries_nbMedals.pdf",path="image")
ggsave("top10countries_nbMedals.png",path="image")

###############
### Graph 2 ###
###############

# number of athletes by countries over the years

df2 <- ddply(df, "NOC", summarise, nbAthletes = sum(count(unique(ID))$freq))
df2_ordered <- df2[order(df1$nbMedals, decreasing = TRUE),][1:10,]
df2_ordered$NOC <- factor(df2_ordered$NOC, df2_ordered$NOC)
ggplot(df2_ordered, aes(NOC, nbAthletes)) + 
  geom_bar(stat = "identity", aes(fill=NOC)) +
  labs(title = "Number of athletes in the top 10 countries", x = "Country", y = "Nb of athletes") + 
  geom_text(aes(label=nbAthletes),color="black",vjust=-0.3) +
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("top10countries_nbAthletes.pdf",path="image")
ggsave("top10countries_nbAthletes.png",path="image")

###############
### Graph 3 ###
###############

# histogram of number of medals compared to the age
df_medalPerAge<-na.omit(df[c("Age","Medal")])
ggplot(df_medalPerAge,aes(x=Age,y=..count..)) +
  geom_histogram(position="identity",
                 fill="lightblue",
                 binwidth = 4) +
  stat_bin(geom="text",
           binwidth = 4,
           aes(label=..count..),
           vjust=-0.3) +
  theme_bw() +
  labs(title = "Histogram of number of medals compared to the age", x = "Age", y = "Number of medals") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("hist_medalPerAge.png",path="image")
ggsave("hist_medalPerAge.pdf",path="image")



###############
### Graph 4 ###
###############

# nombre de médaille par BMI

df4 <- ddply(df, c("Year", "Sex"), summarise, 
             BMI_mean = round(mean(10000*Weight/Height/Height, na.rm = TRUE), 2), 
             nbMedals=sum(count(Medal[!is.na(Medal)])$freq))

ggplot(na.omit(df4), aes(x = BMI_mean, y = nbMedals, color = Sex, size=nbMedals,alpha=0.5)) +
  geom_jitter() +
  labs(title = "The number of medals over the average BMI of athletes of all countries", x = "BMI", y = "Number of medals") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("BMI_nbMedals.pdf",path="image")
ggsave("BMI_nbMedals.png",path="image")