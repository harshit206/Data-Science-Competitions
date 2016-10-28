#load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$actual_vote <- NA

#combining dataset
combi <- rbind(train,test)

#renaming coloumn names
colnames(combi) <- c("citizen_id", "party_voted", "donations_centaur", "donations_ebony", "donations_tokugawa", "donations_odyssey", "donations_cosmos", "rallies_centaur", "rallies_ebony", "rallies_tokugawa", "rallies_odyssey", "rallies_cosmos", "groups_centaur", "groups_ebony", "groups_tokugawa", "groups_odyssey", "groups_cosmos", "funds_centaur", "funds_ebony", "funds_tokugawa", "funds_odyssey", "funds_cosmos", "volunteer_centaur", "volunteer_ebony", "volunteer_tokugawa", "volunteer_odyssey", "volunteer_cosmos", "household_size", "age_group", "married","home_owner", "edu", "newspapers", "previous_residence", "current_residence", "actual_vote")

combi$married <- as.factor(combi$married)
combi$home_owner <- as.factor(combi$home_owner)



#data_exploration
summary(combi)


#removing outliers 


bench_do_centaur <- 3 + 1.5*IQR(combi$donations_centaur) 
combi$donations_centaur[combi$donations_centaur > bench_do_centaur] <- bench_do_centaur

bench_do_ebony <- 3 + 1.5*IQR(combi$donations_ebony)
combi$donations_ebony[combi$donations_ebony > bench_do_ebony] <- bench_do_ebony

bench_do_tokugawa <- 1.258*2.0
combi$donations_tokugawa[combi$donations_tokugawa > bench_do_tokugawa] <- bench_do_tokugawa

bench_do_odyssey <- 3 + 1.5*IQR(combi$donations_odyssey)
combi$donations_odyssey[combi$donations_odyssey > bench_do_odyssey] <- bench_do_odyssey

bench_do_cosmos <- 2 + 1.5*IQR(combi$donations_cosmos)
combi$donations_cosmos[combi$donations_cosmos > bench_do_cosmos] <- bench_do_cosmos


rally_centaur <- 1 + 1.5*IQR(combi$rallies_centaur)
combi$rallies_centaur[combi$rallies_centaur > rally_centaur] <- rally_centaur

rally_ebony <- 1 + 1.5*IQR(combi$rallies_ebony)
combi$rallies_ebony[combi$rallies_ebony > rally_ebony] <- rally_ebony

rally_tokugawa <- 2
combi$rallies_tokugawa[combi$rallies_tokugawa > rally_tokugawa] <- rally_tokugawa

rally_odyssey <- 2 + 1.5*IQR(combi$rallies_odyssey)
combi$rallies_odyssey[combi$rallies_odyssey > rally_odyssey] <- rally_odyssey

rally_cosmos <- 1 + 1.5*IQR(combi$rallies_cosmos)
combi$rallies_cosmos[combi$rallies_cosmos > rally_cosmos] <- rally_cosmos


group_centaur <-2
combi$groups_centaur[combi$groups_centaur > group_centaur] <- group_centaur

group_ebony <-2
combi$groups_ebony[combi$groups_ebony > group_ebony] <- group_ebony

group_tokugawa <-2
combi$groups_tokugawa[combi$groups_tokugawa > group_tokugawa] <- group_tokugawa

group_odyssey <-2
combi$groups_odyssey[combi$groups_odyssey > group_odyssey] <- group_odyssey

group_cosmos <-2
combi$groups_cosmos[combi$groups_cosmos > group_cosmos] <- group_cosmos

fund_centaur <- 3
combi$funds_centaur[combi$funds_centaur > fund_centaur] <- fund_centaur

fund_ebony <- 1 + 1.5*IQR(combi$funds_ebony)
combi$funds_ebony[combi$funds_ebony > fund_ebony] <- fund_ebony

fund_tokugawa <- 3
combi$funds_tokugawa[combi$funds_tokugawa > fund_tokugawa] <- fund_tokugawa

fund_odyssey <- 3
combi$funds_odyssey[combi$funds_odyssey > fund_odyssey] <- fund_odyssey

fund_cosmos <- 3
combi$funds_cosmos[combi$funds_cosmos > fund_cosmos] <- fund_cosmos



#creating new variable
combi$level_household <- ifelse(combi$household_size > 4, "Large", "Small")
combi$level_household <- as.factor(combi$level_household)

combi$total_rally_centaur <-  combi$rallies_centaur + combi$groups_centaur
combi$total_rally_ebony <-  combi$rallies_ebony + combi$groups_ebony
combi$total_rally_tokugawa <-  combi$rallies_tokugawa + combi$groups_tokugawa
combi$total_rally_odyssey <-  combi$rallies_odyssey + combi$groups_odyssey
combi$total_rally_cosmos <-  combi$rallies_cosmos + combi$groups_cosmos

combi$current_residence <- factor(combi$current_residence, levels= levels(combi$previous_residence))

combi$change_address <- ifelse(combi$previous_residence == combi$current_residence, "No", "Yes")

#converting into factors
combi$change_address <- as.factor(combi$change_address)


#missing values
combi$married[is.na(combi$married)] <- 1
combi$home_owner[is.na(combi$home_owner)] <- 1
combi$change_address[is.na(combi$change_address)] <- "No"

                           
                           
#splitting data back to train and test
train_new1 <- combi[1:60129, ]
test_new1 <- combi[60130:81336, ]

test_new1 <- test_new1[-36]
train_new1 <- train_new1[c(-34, -35)]
test_new1 <- test_new1[-c(34,35)]
train_new1 <- train_new1[-c(1)]
test_new1 <- test_new1[-c(1)]


#modeling-randomForest
library(randomForest)

rf1 <- randomForest(actual_vote~., data = train1, do.trace = 100, ntee  = 600, mtry = 7 )
solution_rf1 <- predict(rf1, test1)
solution_rf1 <- as.data.frame(solution_rf1)
write.csv(solution_rf1, file = "solution_rf1.csv")

sol_rf <- predict(rf1, train1[-33])



#drop features according to variable imp        #dropped till volunteer_cosmos from the bottom
rf2 <- randomForest(actual_vote~. -citizen_id -previous_residence -current_residence -funds_centaur -married -home_owner -funds_tokugawa -volunteer_odyssey -funds_ebony -rallies_tokugawa -rallies_cosmos -volunteer_centaur -rallies_centaur -volunteer_ebony -volunteer_cosmos, data = train_new1, do.trace = 100, ntree= 600, mtry = 7)
solution_rf2 <- predict(rf2, test_new1)
write.csv(solution_rf2, file = "solution_rf2.csv")   #score declined

rf3 <- randomForest(actual_vote~. -citizen_id , data = train_new1, do.trace = 100, ntree = 600)
solution_rf_3 <- predict(rf3, test_new1)
write.csv(solution_rf_3, file = "solution_rf_3.csv")

#modeling-NaiveBayes Classifier
train1 <- train_new1
test1 <- test_new1


library(e1071)
nb1 <- naiveBayes(actual_vote~., data = train1)


solution_sample_nb <- predict(nb1, test1)
write.csv(solution_sample_nb, file = "solution_sample_nb.csv")


sol_nb <- predict(nb1, train1[-33])


nb2 <- naiveBayes(actual_vote~. -citizen_id -current_residence -previous_residence -rallies_centaur -rallies_ebony -rallies_tokugawa -rallies_odyssey -rallies_cosmos -groups_centaur -groups_ebony -groups_odyssey -groups_tokugawa -groups_cosmos, data = train1)
solution_nb_2 <- predict(nb2, test1)
write.csv(solution_nb_2, file = "solution_nb_2.csv")

nb3 <- naiveBayes(actual_vote~. -level_household -married -citizen_id -current_residence -previous_residence -rallies_centaur -rallies_ebony -rallies_tokugawa -rallies_odyssey -rallies_cosmos -groups_centaur -groups_ebony -groups_odyssey -groups_tokugawa -groups_cosmos, data = train1)
solution_nb_3 <- predict(nb3, test1)
write.csv(solution_nb_3, file = "solution_nb_3.csv")

nb4 <- naiveBayes(actual_vote~. -citizen_id -previous_residence -current_residence, data = train_new1)
solution_nb_4 <- predict(nb4, test_new1)
write.csv(solution_nb_4, file = "solution_nb_4.csv")

#ensembling-nb



#modleing - xgboost
library(xgboost)
library(readr)
library(stringr)
library(MatrixModels)
library(data.table)

train1$party_voted <- gsub("Centaur", "0", train1$party_voted)
 train1$party_voted <- gsub("Ebony", "1", train1$party_voted)
 train1$party_voted <- gsub("Tokugawa", "2", train1$party_voted)
train1$party_voted <- gsub("Odyssey", "3", train1$party_voted)
 train1$party_voted <- gsub("Cosmos", "4", train1$party_voted)

 train1$party_voted <- as.numeric(train1$party_voted)
 train1$age_group <- as.numeric(train1$age_group)
  train1$married <- as.numeric(train1$married)
 train1$home_owner <- as.numeric(train1$home_owner)
 train1$edu <- as.numeric(train1$edu)
 
 
 train1$actual_vote <- gsub("Centaur", "0", train1$actual_vote)
 train1$actual_vote <- gsub("Ebony", "1", train1$actual_vote)
 train1$actual_vote <- gsub("Tokugawa", "2", train1$actual_vote)
 train1$actual_vote <- gsub("Odyssey", "3", train1$actual_vote)
 train1$actual_vote <- gsub("Cosmos", "4", train1$actual_vote)
 
 train1$actual_vote <- as.numeric(train1$actual_vote)
 y <- train1$actual_vote
 
 train1 <- train1[-36]
 set.seed(1)
 xgb1 <- xgboost(data = data.matrix(train1[, c(-35,-34,-36)]), 
                 label = y,
                 seed = 1,
                 eta = 0.3,
                 max_depth = 10,
                 nrounds = 25,
                 subsample  = 0.5,
                 colsample_bytree = 1,
                 seed = 1,
                 objective = "multi:softmax" ,
                 booster = "gbtree",
                 nthread = -1,
                 num_class = 5
                 )
                
                
#h2o
library(h2o)
localH2o <- h2o.init(nthreads = -1)

train.h2o <- as.h2o(train1)
test.h2o <- as.h2o(test1)
x.indep <- c(2:33, 37:42)
y.dep <- 36
system.time(deep_learning1 <- h2o.deeplearning(y = y.dep, x = x.indep, training_frame = train.h2o, epochs = 60, hidden = c(100,100), activation = "Rectifier", seed = 1122))
                 


#knn
knn_1 <- knn(train = train_new1[-33], test = test_new1, cl = train_new1$actual_vote, k = 10)
solution_knn_1 <- predict(knn_1, test_new1, method = "class")


#svm
svm_1 <- svm(actual_vote~., data = train_new1)
solution_svm_1 <- predict(svm_1, test_new1) 
write.csv(solution_svm_1, file = "solution_svm_1.csv")

sol_train_svm <- predict(svm_1, train_new1[-33])



#combi dataset
combi <- read.csv("combi.csv")
colnames(combi) <- c("citizen_id", "party_voted", "donations_centaur", "donations_ebony", "donations_tokugawa", "donations_odyssey", "donations_cosmos","groups_centaur", "groups_ebony", "groups_tokugawa", "groups_odyssey", "groups_cosmos", "funds_centaur", "funds_ebony", "funds_tokugawa", "funds_odyssey", "funds_cosmos", "volunteer_centaur", "volunteer_ebony", "volunteer_tokugawa", "volunteer_odyssey", "volunteer_cosmos", "household_size", "age_group", "married","home_owner", "edu", "newspapers", "previous_residence", "current_residence")
combi$rallies_centaur <- NA
combi$rallies_ebony <- NA
combi$rallies_tokugawa <- NA
combi$rallies_odyssey <- NA
combi$rallies_cosmos <- NA
combi <- combi[, c(1,2,3,4,5,6,7,31,32,33,34,35,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
combi$married <- as.factor(combi$married)
combi$home_owner <- as.factor(combi$home_owner)


combi <- read.csv("combi.csv")
colnames(combi) <- c("citizen_id", "party_voted", "donations_centaur", "donations_ebony", "donations_tokugawa", "donations_odyssey", "donations_cosmos", "rallies_centaur", "rallies_ebony", "rallies_tokugawa", "rallies_odyssey", "rallies_cosmos", "groups_centaur", "groups_ebony", "groups_tokugawa", "groups_odyssey", "groups_cosmos", "funds_centaur", "funds_ebony", "funds_tokugawa", "funds_odyssey", "funds_cosmos", "volunteer_centaur", "volunteer_ebony", "volunteer_tokugawa", "volunteer_odyssey", "volunteer_cosmos", "household_size", "age_group", "married","home_owner", "edu", "newspapers", "previous_residence", "current_residence")

final_combi <- rbind(final,combi)


table(is.na(combi))

combi$married <-as.factor(combi$married)
combi$home_owner <- as.factor(combi$home_owner)


#removing outliers
bench_do_centaur <- 3 + 1.5*IQR(combi$donations_centaur) 
combi$donations_centaur[combi$donations_centaur > bench_do_centaur] <- bench_do_centaur

bench_do_ebony <- 3 + 1.5*IQR(combi$donations_ebony)
combi$donations_ebony[combi$donations_ebony > bench_do_ebony] <- bench_do_ebony

bench_do_tokugawa <- 1.258*2.0
combi$donations_tokugawa[combi$donations_tokugawa > bench_do_tokugawa] <- bench_do_tokugawa

bench_do_odyssey <- 3 + 1.5*IQR(combi$donations_odyssey)
combi$donations_odyssey[combi$donations_odyssey > bench_do_odyssey] <- bench_do_odyssey

bench_do_cosmos <- 2 + 1.5*IQR(combi$donations_cosmos)
combi$donations_cosmos[combi$donations_cosmos > bench_do_cosmos] <- bench_do_cosmos


rally_centaur <- 1 + 1.5*IQR(combi$rallies_centaur)
combi$rallies_centaur[combi$rallies_centaur > rally_centaur] <- rally_centaur

rally_ebony <- 1 + 1.5*IQR(combi$rallies_ebony)
combi$rallies_ebony[combi$rallies_ebony > rally_ebony] <- rally_ebony

rally_tokugawa <- 2
combi$rallies_tokugawa[combi$rallies_tokugawa > rally_tokugawa] <- rally_tokugawa

rally_odyssey <- 2 + 1.5*IQR(combi$rallies_odyssey)
combi$rallies_odyssey[combi$rallies_odyssey > rally_odyssey] <- rally_odyssey

rally_cosmos <- 1 + 1.5*IQR(combi$rallies_cosmos)
combi$rallies_cosmos[combi$rallies_cosmos > rally_cosmos] <- rally_cosmos


group_centaur <-2
combi$groups_centaur[combi$groups_centaur > group_centaur] <- group_centaur

group_ebony <-2
combi$groups_ebony[combi$groups_ebony > group_ebony] <- group_ebony

group_tokugawa <-2
combi$groups_tokugawa[combi$groups_tokugawa > group_tokugawa] <- group_tokugawa

group_odyssey <-2
combi$groups_odyssey[combi$groups_odyssey > group_odyssey] <- group_odyssey

group_cosmos <-2
combi$groups_cosmos[combi$groups_cosmos > group_cosmos] <- group_cosmos

fund_centaur <- 3
combi$funds_centaur[combi$funds_centaur > fund_centaur] <- fund_centaur

fund_ebony <- 1 + 1.5*IQR(combi$funds_ebony)
combi$funds_ebony[combi$funds_ebony > fund_ebony] <- fund_ebony

fund_tokugawa <- 3
combi$funds_tokugawa[combi$funds_tokugawa > fund_tokugawa] <- fund_tokugawa

fund_odyssey <- 3
combi$funds_odyssey[combi$funds_odyssey > fund_odyssey] <- fund_odyssey

fund_cosmos <- 3
combi$funds_cosmos[combi$funds_cosmos > fund_cosmos] <- fund_cosmos

#imputing missing values
library(mice)
imputed_data <- mice(final_combi, m = 2, maxit = 5, seed=500)


final_combi_new$citizen_id <- final_combi$citizen_id
final_combi_new$party_voted <- final_combi$party_voted
final_combi_new$age_group <- final_combi$age_group
final_combi_new$married <- final_combi$married
final_combi_new$home_owner <- final_combi$home_owner
final_combi_new$previous_residence <- final_combi$previous_residence
final_combi_new$current_residence <- final_combi$current_residence
final_combi_new$edu <- final_combi$edu


final_combi_new <- final_combi_new[, c(29,30,1:26,30:32,35,27,33,34)]
final_combi_new <- final_combi_new[-29]
 final_combi_new$citizen_id <- final_combi$citizen_id
final_combi_new <- final_combi_new[, c(35,1,3:28,2, 29,30,31,32,33,34)]


#creating new variables
combi$level_household <- ifelse(combi$household_size > 4, "Large", "Small")
combi$level_household <- as.factor(combi$level_household)

combi$total_rally_centaur <-  combi$rallies_centaur + combi$groups_centaur
combi$total_rally_ebony <-  combi$rallies_ebony + combi$groups_ebony
combi$total_rally_tokugawa <-  combi$rallies_tokugawa + combi$groups_tokugawa
combi$total_rally_odyssey <-  combi$rallies_odyssey + combi$groups_odyssey
combi$total_rally_cosmos <-  combi$rallies_cosmos + combi$groups_cosmos

combi$current_residence <- factor(combi$current_residence, levels= levels(combi$previous_residence))

combi$change_address <- ifelse(combi$previous_residence == combi$current_residence, "No", "Yes")

#converting into factors
combi$change_address <- as.factor(combi$change_address)


#missing values
combi$married[is.na(combi$married)] <- 1
combi$home_owner[is.na(combi$home_owner)] <- 1
combi$change_address[is.na(combi$change_address)] <- "No"

combi <- combi[-c(1,35,34)]
