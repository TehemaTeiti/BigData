setwd("/Users/mac/Documents/5annee/BigData/")

df <- read.csv("BlackFriday.csv", sep = ",")

df1 <- ddply(df, c("Gender", "Age"), summarise, Somme = sum(Purchase))
ggplot(df1, aes(x = Age, y = Somme, color = Gender)) + geom_point()

df2 <- ddply(df, c("Marital_Status", "Gender"), summarise, Somme = sum(Purchase))
ggplot(df2, aes(x = Marital_Status, y = Somme, color = Gender)) + geom_point()

df3 <- ddply(df, c("City_Category", "Gender"), summarise, Somme = sum(Purchase))
ggplot(df3, aes(x = City_Category, y = Somme, color = Gender)) + geom_point()

df4 <- ddply(df, c("User_ID", "Gender"), summarise, Somme = sum(Product_Category_1, Product_Category_2, Product_Category_3, na.rm = TRUE))
ggplot(df4, aes(x = User_ID, y = Somme, color = Gender)) +geom_point()

df5 <- ddply(df, c("Stay_In_Current_City_Years", "Gender"), summarise, Somme = sum(Purchase))
ggplot(df5, aes(x = Stay_In_Current_City_Years, y = Somme, color = Gender)) +geom_point()
