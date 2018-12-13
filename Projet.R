setwd("/home/t_chen/travaux/5annee/BigData/")

df <- read.csv("athlete_events.csv", sep = ",")
df1 <- ddply(df, "NOC", summarise, somme = sum(count(Medal)$freq, na.rm = TRUE))
ggplot(df1, aes(x = NOC, y = somme, color = NOC)) + geom_point()

