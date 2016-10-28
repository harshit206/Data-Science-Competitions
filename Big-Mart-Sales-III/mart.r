#loading libraries
library(ggplot2)
library(dplyr)

#load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")


#treating different levels LF = low fat
train$Item_Fat_Content <- gsub("LF", "Low Fat", train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("low fat", "Low Fat", train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("reg","Regular", train$Item_Fat_Content)
test$Item_Fat_Content <- gsub("LF","Low Fat", test$Item_Fat_Content)
test$Item_Fat_Content <- gsub("low fat","Low Fat", test$Item_Fat_Content)
test$Item_Fat_Content <- gsub("reg","Regular", test$Item_Fat_Content)

#filling missing values
train$Item_Weight[is.na(train$Item_Weight)] <- median(train$Item_Weight, na.rm = TRUE)
test$Item_Weight[is.na(test$Item_Weight)] <- median(test$Item_Weight, na.rm = TRUE)

#converting into factors
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)



#Adding new level in fat content based on Item_Type
levels(train$Item_Fat_Content) <- c(levels(train$Item_Fat_Content), "None")
levels(test$Item_Fat_Content) <- c(levels(test$Item_Fat_Content), "None")

train[which(train$Item_Type=="Health and Hygiene"), ]$Item_Fat_Content = "None"
train[which(train$Item_Type=="Household"), ]$Item_Fat_Content = "None"
train[which(train$Item_Type=="Others"), ]$Item_Fat_Content = "None"
test[which(test$Item_Type=="Health and Hygiene"), ]$Item_Fat_Content = "None"
test[which(test$Item_Type=="Household"), ]$Item_Fat_Content = "None"
test[which(test$Item_Type=="Others"), ]$Item_Fat_Content = "None"

#Missing Outlet Size
levels(train$Outlet_Size)[1] <- "Other"
levels(test$Outlet_Size)[1] <- "Other"

#addign new variable, no. of years 

train$Year <- as.factor(2013 - train$Outlet_Establishment_Year)
test$Year <- as.factor(2013 - test$Outlet_Establishment_Year)

train <- select(train, -c(Outlet_Establishment_Year))
test <- select(test, -c(Outlet_Establishment_Year))

#vizualizing Item_Mrp
ggplot(train, aes(Item_MRP)) + geom_density(adjust = 1/5)

#Adding MRP_factor levels as high , med, etc.
train$MRP_factor <- as.factor(ifelse(train$Item_MRP < 70, "Low" ,
                        ifelse(train$Item_MRP <130, "Medium",
                        ifelse(train$Item_MRP < 201, "High", "Very High"))))


test$MRP_factor <- as.factor(ifelse(test$Item_MRP < 70, "Low" ,
                          ifelse(test$Item_MRP <130, "Medium",
                         ifelse(test$Item_MRP < 201, "High", "Very High"))))


#Reordering of columns
train <- select(train, c(Item_Identifier,
                          Item_Weight,
                          Item_Fat_Content,
                          Item_Visibility,
                          Item_Type,
                          Item_MRP,
                          Outlet_Identifier,
                          Outlet_Size,
                          Outlet_Location_Type,
                          Outlet_Type,
                          Year,
                          MRP_factor,
                          Item_Outlet_Sales))

#Outlet Type vs year graph
ggplot(train, aes(Outlet_Type, Year)) + geom_point()      #Supermarket 2 is new
ggplot(train, aes(Outlet_Type, Item_Outlet_Sales)) + geom_bar(stat = "identity") #less sales in grocery store

#Summary of "other" outlet type and outlet type
other<- as.data.frame( setNames(
       aggregate(
             train$Outlet_Size, 
           by=list(Category=train$Outlet_Identifier, 
                                      Category=train$Outlet_Type,
                                       Category=train$Outlet_Location_Type,
                                   Category=train$Outlet_Size), FUN= length), c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")))
other
# as per analysis
levels(train$Outlet_Size)[1] <- "Small"
levels(test$Outlet_Size)[1] <- "Small"

train[which(train$Outlet_Identifier=="OUT010"), ]$Outlet_Size <- "Small"
train[which(train$Outlet_Identifier=="OUT017"), ]$Outlet_Size <- "Small"
train[which(train$Outlet_Identifier=="OUT045"), ]$Outlet_Size <- "Small"
test[which(test$Outlet_Identifier=="OUT010"), ]$Outlet_Size <- "Small"
test[which(test$Outlet_Identifier=="OUT017"), ]$Outlet_Size <- "Small"
test[which(test$Outlet_Identifier=="OUT045"), ]$Outlet_Size <- "Small"


#Vizualisation
library(ggplot2)

ggplot(train, aes(x = Item_Weight, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Item_Fat_Content, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Item_Type, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Item_MRP, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Outlet_Identifier, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Outlet_Establishment_Year, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Outlet_Size, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Outlet_Type, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Outlet_Identifier, y = Item_Outlet_Sales)) + geom_point(size = 1.5 , color = "red")
ggplot(train, aes(x = Item_Type)) + geom_bar(stat = "count", fill = "red")

#outlet type
#outlet size
#outlet year
#outlet identifier
##MRP
#type
#visi
#fat

#model building
library(randomForest)
rf1 <- randomForest(train~. -Item_Identifier -Outlet_Identifier, ntrees = 2000, mtry = 3, do.trace = 100)
solution1 <- predict(rf1, test)
write.csv(solution1, file = "Solution.csv")