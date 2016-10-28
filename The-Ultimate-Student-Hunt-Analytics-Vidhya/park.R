#load data

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

#combine data sets
test$Footfall <- 'NA'
combi <- rbind(train,test)

str(combi)

#convert Date into 'date' format
combi$Date <- as.POSIXct(combi$Date, format = "%d-%m-%Y")
combi$Footfall <- as.numeric(combi$Footfall)
combi$Park_ID <- as.factor(combi$Park_ID)
combi$Location_Type <- as.factor(combi$Location_Type)

#create  new average ambient pollution variable
combi$Average_Ambient_Pollution <- (combi$Min_Ambient_Pollution + combi$Max_Ambient_Pollution)/2



#summary
summary(combi$Date)
summary(combi$Direction_Of_Wind)    #missing values
summary(combi$Average_Breeze_Speed)  #missing values
summary(combi$Max_Breeze_Speed)    #missing values
summary(combi$Min_Breeze_Speed)   #missing values
summary(combi$Var1)               #unknown variable, missing values
summary(combi$Average_Atmospheric_Pressure)   #missing values
summary(combi$Max_Atmospheric_Pressure)  #missing values
summary(combi$Min_Atmospheric_Pressure)   #missing values
summary(combi)

#Generate 'month' variable from date
library(lubridate)
combi$Month1 <- month(combi$Date)
combi$Month1 <- as.character(combi$Month1)
combi$Month <- 'NA'
combi[which(combi$Month1 == "1"), ]$Month = "January"
combi[which(combi$Month1 == "2"), ]$Month = "February"
combi[which(combi$Month1 == "3"), ]$Month = "March"
combi[which(combi$Month1 == "4"), ]$Month = "April"
combi[which(combi$Month1 == "5"), ]$Month = "May"
combi[which(combi$Month1 == "6"), ]$Month = "June"
combi[which(combi$Month1 == "7"), ]$Month = "July"
combi[which(combi$Month1 == "8"), ]$Month = "August"
combi[which(combi$Month1 == "9"), ]$Month = "September"
combi[which(combi$Month1 == "10"), ]$Month = "October"
combi[which(combi$Month1 == "11"), ]$Month = "November"
combi[which(combi$Month1 == "12"), ]$Month = "December"



#deleting month1 variable
combi <- combi[-c(20)]

#impute missing values
  library(mice)
  library(VIM)

md.pattern(combi)

mice_plot <- aggr(data_frame, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data_frame), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


#plots
library(ggplot2)
ggplot(combi, aes(Direction_Of_Wind, Footfall)) + geom_bar(stat = "identity")       #two peaks 1. dir:0-115:footfall: 
ggplot(combi, aes(Average_Breeze_Speed, Footfall)) + geom_bar(stat = "identity")    #positively skewed
ggplot(combi, aes(Average_Breeze_Speed)) + geom_bar(stat = "count")
ggplot(combi, aes(Max_Breeze_Speed, Footfall)) + geom_bar(stat = "identity")         #positively skewed
ggplot(combi, aes(Min_Breeze_Speed, Footfall)) + geom_bar(stat = "identity")          # positively skewed
ggplot(combi, aes(Average_Atmospheric_Pressure, Footfall)) + geom_bar(stat = "identity") #normal
ggplot(combi, aes(Max_Atmospheric_Pressure, Footfall)) + geom_bar(stat = "identity")  #normal
ggplot(combi, aes(Min_Atmospheric_Pressure, Footfall)) + geom_bar(stat = "identity")     #normal
ggplot(combi, aes(Min_Ambient_Pollution, Footfall)) + geom_bar(stat = "identity")         #random
ggplot(combi, aes(Max_Ambient_Pollution, Footfall)) + geom_bar(stat = "identity")      #negatively skewed
ggplot(combi, aes(Average_Ambient_Pollution, Footfall)) + geom_bar(stat = "identity")  #one small peak, one high peak 
ggplot(combi, aes(Average_Moisture_In_Park, Footfall)) + geom_bar(stat = "identity")   #moderately -ve skewed
ggplot(combi, aes(Max_Moisture_In_Park, Footfall)) + geom_bar(stat = "identity")   #slightly negatively skewed
ggplot(combi, aes(Min_Moisture_In_Park, Footfall)) + geom_bar(stat = "identity")  #normal
ggplot(combi, aes(Date, Footfall)) + geom_point(size = 1.5)  #Follows a recurring trend *important*


#ordering month wise
data_frame_month <- data.frame(Months = combi$Month, Footfall = combi$Footfall)
data_frame_month$one <- factor(data_frame_month$Months, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

p <- ggplot(data_frame_month, aes(y = Footfall)) 
p + geom_bar(data = data_frame_month, aes(x = one), stat = "identity")  #june-sept high footfall


#creating dataframe
data_frame <- as.data.frame(combi)

#Converting skewed curves into normalized curve
data_frame$DOF <- sqrt(combi$Direction_Of_Wind)
data_frame$ABS <- sqrt(combi$Average_Breeze_Speed)
data_frame$MaxBS <- sqrt(combi$Max_Breeze_Speed)
data_frame$MinBS <- sqrt(combi$Min_Breeze_Speed)
data_frame$var1_sqrt <- sqrt(combi$Var1)
data_frame$AAP <- sqrt(combi$Average_Atmospheric_Pressure)
data_frame$MaxAP <- sqrt(combi$Max_Atmospheric_Pressure)
data_frame$MinAP <- sqrt(combi$Min_Atmospheric_Pressure)
data_frame$MinAPOL <- sqrt(combi$Min_Ambient_Pollution)
data_frame$MaxAPOL <- sqrt(combi$Max_Ambient_Pollution)
data_frame$AAPOL <- sqrt(combi$Average_Ambient_Pollution)
data_frame$AMP <- sqrt(combi$Average_Moisture_In_Park)
data_frame$MaxMP <- sqrt(combi$Max_Moisture_In_Park)
data_frame$MinMP <-  sqrt(combi$Min_Moisture_In_Park)
data_frame$Footfall_sqrt <- sqrt(combi$Footfall)

#deleting unwantwed coloumns
data_frame <- data_frame[-c(1:20)]

#Finding correlation  DOF and other ind-variables
cor(data_frame$DOF,data_frame$ABS, use = "complete.obs")  #0.144
cor(data_frame$DOF,data_frame$MaxBS, use = "complete.obs") #0.180
cor(data_frame$DOF,data_frame$MinBS, use = "complete.obs") #0.06
cor(data_frame$DOF,data_frame$var1_sqrt, use = "complete.obs") #0.22
cor(data_frame$DOF,data_frame$AAP, use = "complete.obs")  #-0.138
cor(data_frame$DOF,data_frame$MaxAP, use = "complete.obs") #-0.12
cor(data_frame$DOF,data_frame$MinAP, use = "complete.obs") #-0.15
cor(data_frame$DOF,data_frame$MinAPOL, use = "complete.obs") #0.04
cor(data_frame$DOF,data_frame$MaxAPOL, use = "complete.obs") #0.08
cor(data_frame$DOF,data_frame$AAPOL, use = "complete.obs") #0.05
cor(data_frame$DOF,data_frame$AMP, use = "complete.obs") #0.15
cor(data_frame$DOF,data_frame$MaxMP, use = "complete.obs") #0.10
cor(data_frame$DOF,data_frame$MinMP, use = "complete.obs") #0.14
cor(data_frame$DOF,data_frame$var1_sqrt, use = "complete.obs") #0.22
cor(data_frame$DOF,data_frame$Footfall_sqrt, use = "complete.obs") #0.10

#finding coorelation 
cor(data_frame$AAP,data_frame$ABS, use = "complete.obs")  #-0.33
cor(data_frame$AAP,data_frame$MaxBS, use = "complete.obs") #-0.35
cor(data_frame$AAP,data_frame$MinBS, use = "complete.obs") #-0.26
cor(data_frame$AAP,data_frame$var1_sqrt, use = "complete.obs") #-0.47
cor(data_frame$AAP,data_frame$DOF, use = "complete.obs")  #-0.138
cor(data_frame$AAP,data_frame$MaxAP, use = "complete.obs") #0.97
cor(data_frame$AAP,data_frame$MinAP, use = "complete.obs") #0.97
cor(data_frame$AAP,data_frame$MinAPOL, use = "complete.obs") #-0.07
cor(data_frame$AAP,data_frame$MaxAPOL, use = "complete.obs") #-0.04
cor(data_frame$AAP,data_frame$AAPOL, use = "complete.obs") #-0.06
cor(data_frame$AAP,data_frame$AMP, use = "complete.obs") #-0.14
cor(data_frame$AAP,data_frame$MaxMP, use = "complete.obs") #-0.039
cor(data_frame$AAP,data_frame$MinMP, use = "complete.obs") #-0.16
cor(data_frame$AAP,data_frame$var1_sqrt, use = "complete.obs") #-.47
cor(data_frame$AAP,data_frame$Footfall_sqrt, use = "complete.obs") #-0.06

#imputing mssing values using mice package
imputed_dadta <- mice(data_frame, m = 1, maxit = 5, seed = 500, method = "pmm")
complete_data <- complete(imputed_dadta)

complete_data$ID <- combi$ID
complete_data$Park_ID <- combi$Park_ID
complete_data$Date <- combi$Date
complete_data$Month <- combi$Month
complete_data$Location_Type <- combi$Location_Type

#new features
ggplot(complete_data, aes(DOF)) + geom_density(fill = "blue", adjust = 1/5)  + geom_vline(xintercept = 11, color = "red")

complete_data$DOF_level <- as.factor(ifelse(complete_data$DOF < 11, "Low", "High"))
complete_data$var_level <- as.factor(ifelse(complete_data$var1_sqrt < 1, "High", "Low"))




complete_data <- complete_data[-c(18)]

#splitting data 
train_new <- complete_data[1:114539, ]
train_new$Footfall <- train$Footfall



test_new <- complete_data[114540:153959, ]


#converting into csv files 
write.csv(train_new, file = "trainh2o_var_level.csv")
write.csv(test_new, file = "testh2o_var_level.csv")





#h2o modeling
#loading as data.table
library(data.table)
train.h2o <- fread("trainh2o_var_level.csv")
test.h2o <- fread("testh2o_var_level.csv")

#converting into factors
train.h2o$Location_Type <- as.factor(train.h2o$Location_Type)
test.h2o$Location_Type <- as.factor(test.h2o$Location_Type)

train.h2o$var1_level <- as.factor(train.h2o$var1_level)
test.h2o$var1_level <- as.factor(test.h2o$var1_level)

train.h2o$Month <- as.factor(train.h2o$Month)
test.h2o$Month <- as.factor(test.h2o$Month)

#deleting date variable
train.h2o[, Date:=NULL]
test.h2o[, Date:=NULL]

#load
#coverting into h2o instance
library(h2o)
localH2O <- h2o.init(nthreads = -1, max_mem_size = "2g")

train.h2o <- as.h2o(train.h2o)
test.h2o <- as.h2o(test.h2o)

colnames(train.h2o)

#specifying dependent and independent variables
y.dep <- 20
x.indep <- c(3:19, 21)

#modeling
system.time(rforest_model_var_level <- h2o.randomForest(y= y.dep, x = x.indep, training_frame = train.h2o, ntrees = 600, mtries = 7, seed = 1122))
system.time(predict.rforest_var_level<- as.data.frame(h2o.predict(rforest_model_var_level,test.h2o)))
solution_rforest_var_level <- data.frame(ID = test.h2o$ID, Footfall = predict.rforest_var_level)
write.csv(solution_rforest_var_level, file = "solution_rforest_var_level.csv")


