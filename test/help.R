############################
#      Logical vectors     #
############################

b1<-c(T,F,T,F)
b2<-c(T,T,F,F)
b3<-b1&b2
b3<-b1|b2

# check NA
is.na(NA)

# check values in a vector
x<-c("Gold","Bronze",NA,"Bronze","Silver","Silver","Gold")
idxGold<-which(x=="Gold")
x[idxGold]

# select all element whithout NA
x[!is.na(x)]

# which : takes a logical vector and return index of TRUE elements
which(is.na(x))

######################
#      Dataframe     #
######################

# each column is a vector or a factor
# all vectors/factors must have the same length

# create a dataframe from vectors
Country<-c("France","Italy","Japan","Korea","Quebec")
Capital<-c("Paris","Rome","Tokyo","Seoul","Montreal")
Continent<-c("Europe","Europe","Asia","Asia","North America")
df<-data.frame(Country,Capital,Continent)

# select columns
df[1] # 1st column
df[1:2] # column 1 & 2
df[-1:-2] # all columns without 1 & 2

# select rows
df[1,] # 1st row
df[1:3,] # row 1 to 3
df[-(2:4),] # all rows wihtout 2 to 4

# splitting by a column (group by)
split(df,df$Continent)

# select countries from Europe
idxEurope<-which(df$Continent=="Europe")
df[idxEurope,]

# pasting
# cbind : combines columns of two datasets
# rbind : combines rows of two datasets

# ordering
arrange(df,df$Country) # order by country
arrange(df,df$Capital) # order by capital
# select,subset

##############################
#      Character vectors     #
##############################

# concatenate
c1<-"Hello"
c2<-"World"
paste(c1,c2)


##########################
#      Index vectors     #
##########################

lv<-c(5,2,NA,4,NA,-8,10,-5)
lvchar<-c("Gold","Bronze",NA,"Bronze","Silver","Silver","Gold")

# with logical vector
lv[!is.na(lv)]

# with positive integers
lv[3:6]

# with negative integers (values are excluded)
lv[-(1:4)]
