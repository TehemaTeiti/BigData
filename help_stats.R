### some stats functions ###

df <- read.csv("athlete_events.csv", sep = ",")
notes<-c(10,15,8,12,13,19,5)

### observation ###

head(df,10) # ten firsts
tail(df,5) # five lasts

### location ###

mean(notes)
median(notes)
max(notes)
min(notes)
summary(notes) 

### dispersion ###

range(notes) # min & max
sd(notes) # ecart-type (standard deviation) : the more values are dispersed, the more sd will be (if all values are equals, sd(x)=0)
f<-ecdf(notes) # empirical cumulative distribution function
data.frame(notes,f(notes))

hist(notes) # histogram
