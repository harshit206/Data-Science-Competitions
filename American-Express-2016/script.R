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

#removing unwanted features such as citizen_id, etc.
test_new1 <- test_new1[-36]
train_new1 <- train_new1[c(-34, -35)]
test_new1 <- test_new1[-c(34,35)]
train_new1 <- train_new1[-c(1)]
test_new1 <- test_new1[-c(1)]


#modeling-NaiveBayes Classifier
train1 <- train_new1
test1 <- test_new1

#load library
library(e1071)
nb1 <- naiveBayes(actual_vote~., data = train1)
solution_sample_nb <- predict(nb1, test1)
write.csv(solution_sample_nb, file = "solution_sample_nb.csv")






#
