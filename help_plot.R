df <- read.csv("athlete_events.csv", sep = ",")

#################
###   ddply   ###
#################

# takes a dataframe in input, returns a dataframe

## transform : add fields computed relatively to categories

m<-ddply(msleep,"vore",transform,rank=rank(-sleep_total))


## summarise : group by category, and return aggregate stats

ddply(msleep,"vore",summarise,med=median(sleep_total))
