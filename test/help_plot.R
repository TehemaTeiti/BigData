#################
###   ddply   ###
#################

# takes a dataframe in input, returns a dataframe

## transform : add fields computed relatively to categories

msleep

ddply(msleep,"vore",transform,rank=rank(-sleep_total))

## summarise : group by category, and return aggregate stats

ddply(msleep,"vore",summarise,med=median(sleep_total))

###################
###   reshape   ###
###################

## melt : transform large dataframe in long one

df <- read.csv("dataset_test.csv", sep = ";")
melt(df,id.vars = "Pays",measure.vars = c("Taille","Poids", "Age"))

################
###   Plot   ###
################

# simple plot on 3 variables with points
ggplot(diamonds,aes(x=carat,y=price,color=depth))+geom_point()
