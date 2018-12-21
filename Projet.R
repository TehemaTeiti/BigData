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

ggsave("p1.pdf",path="image")
ggsave("p1.png",path="image")

###############
### Graph 2 ###
###############

# evolution of the mean of BMI over the years

df2 <- ddply(df, c("Year", "Sex"), summarise, 
                 BMI_mean = round(mean(10000*Weight/Height/Height, na.rm = TRUE), 2), 
                 nbMedals=sum(count(Medal[!is.na(Medal)])$freq))
ggplot(df2, aes(x = Year, y = BMI_mean, color = Sex)) +
  geom_line() +
  labs(title = "The average BMI of athletes of all countries", x = "Year", y = "Average BMI") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("p2.pdf",path="image")
ggsave("p2.png",path="image")

###############
### Graph 3 ###
###############

# number of athletes by countries over the years

df3 <- ddply(df, "NOC", summarise, nbAthletes = sum(count(unique(ID))$freq))
df3_ordered <- df3[order(df1$nbMedals, decreasing = TRUE),][1:10,]
df3_ordered$NOC <- factor(df3_ordered$NOC, df3_ordered$NOC)
ggplot(df3_ordered, aes(NOC, nbAthletes)) + 
  geom_bar(stat = "identity", aes(fill=NOC)) +
  labs(title = "Number of athletes in the top 10 countries", x = "Country", y = "Nb of athletes") + 
  geom_text(aes(label=nbAthletes),color="black",vjust=-0.3) +
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("p3.pdf",path="image")
ggsave("p3.png",path="image")

###############
### Graph 4 ###
###############

# relation between the number of medals and the age

df4 <- ddply(df, "Age", summarise, nbMedals = sum(count(Medal[!is.na(Medal)])$freq))
df4 <- df4[!is.na(df4$Age),]
ggplot(df4, aes(x = Age, y = nbMedals)) +
  geom_line() +
  labs(title = "Top 10 countries with the most athletes", x = "Country", y = "Nb of athletes") + 
  # geom_text(aes(label=nbMedals),color="black",vjust=-0.3) +
  labs(fill='Country') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("p4.pdf",path="image")
ggsave("p4.png",path="image")

####################
### other graphs ###
####################

# evolution of number of medals over the years for USA
df1_overYears <- ddply(df,c("NOC","Year"),summarise,nbMedals=sum(count(Medal[!is.na(Medal)])$freq))
df1_overYears_USA <- subset(df1_overYears,NOC=="USA")
ggplot(df1_overYears_USA, aes(Year,nbMedals)) +
  geom_area(stat = "identity", position = "stack", fill="lightblue") +
  # geom_text(aes(label=nbMedals),color="black",vjust=-0.3) +
  labs(title = "Number of medals for USA over the years", x = "Years", y = "Nb of medals") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("nbMed_overYears_USA.pdf",path="image")
ggsave("nbMed_overYears_USA.png",path="image")

# correlation between number of athletes and number of medals
ggplot(df1,aes(nbAthletes,nbMedals)) +
  geom_smooth() +
  geom_point() +
  labs(title = "Correlation between number of athletes and number of medals", x = "Nb of athletes", y = "Nb of medals") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("cor_nbAth_nbMed.pdf",path="image")
ggsave("cor_nbAth_nbMed.png",path="image")

# correlation between BMI and number of medals
ggplot(na.omit(df2), aes(x=BMI_mean, y=nbMedals)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Correlation between BMI and number of medals", x = "BMI", y = "Nb of athletes") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("cor_BMI_nbMed.pdf",path="image")
ggsave("cor_BMI_nbMed.png",path="image")

# rapport nb médailles sur nb athletes par pays

# histogramme sur nb médailles par tranche d'âge
df_medalPerAge<-na.omit(df[c("Age","Medal","Sex")])
ggplot(df_medalPerAge,aes(Age,color=Sex)) +
  geom_histogram(position="identity",
                 fill="white",
                 binwidth = 2,
                 alpha=0.5) +
  theme_bw() +
  labs(title = "Histogram of number of medals compared to the age", x = "Age", y = "Number of medals") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("hist_medalPerAge.png",path="image")
ggsave("hist_medalPerAge.pdf",path="image")

cor(df1_top10$nbAthletes,df1_top10$nbMedals)

# nombre de médaille par BMI

ggplot(na.omit(df2), aes(x = BMI_mean, y = nbMedals, color = Sex, size=nbMedals,alpha=0.5)) +
  geom_jitter() +
  labs(title = "The number of medals over the average BMI of athletes of all countries", x = "BMI", y = "Number of medals") +
  theme(plot.title = element_text(hjust = 0.5))
