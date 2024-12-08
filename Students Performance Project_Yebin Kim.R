# Download kaggle data from github
url <- "https://raw.githubusercontent.com/yebinkim86/students_performance/refs/heads/main/StudentsPerformance.csv"
data <- read.csv(url)

#Install packages that could be needed in this project
install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")
library(tidyverse)
library(caret)
library(randomForest)

#Renaming column names
colnames(data)[2] <- "race_ethnicity"
colnames(data)[3] <- "parental_level_of_education"
colnames(data)[5] <- "test_preparation_course"
colnames(data)[6] <- "math_score"
colnames(data)[7] <- "reading_score"
colnames(data)[8] <- "writing_score"

#Data transformation
data$gender <- ifelse(data$gender == "male", 1, 0)
data$gender <- as.factor(data$gender)
data$lunch <- ifelse(data$lunch == "standard", 1, 0)
data$lunch <- as.factor(data$lunch)
data$test_preparation_course <- ifelse(data$test_preparation_course == "completed", 1, 0)
data$test_preparation_course <- as.factor(data$test_preparation_course)
data$race_ethnicity <- as.factor(data$race_ethnicity)
data$parental_level_of_education <- as.factor(data$parental_level_of_education)

#Create average score in the data set
data <- data %>% mutate(average_score=(math_score+reading_score+writing_score)/3)

#Data summarize of each score
##by gender
boxplot(math_score ~ gender, data=data, col="lightblue")
boxplot(reading_score ~ gender, data=data, col="violet")
boxplot(writing_score ~ gender, data=data, col="lightgreen")
boxplot(average_score ~ gender, data=data)
by_gender <- data %>% group_by(gender) %>% summarize(average_score=mean(average_score))

##by race/ethnicity
boxplot(math_score ~ race_ethnicity, data=data, col="lightblue")
boxplot(reading_score ~ race_ethnicity, data=data, col="violet")
boxplot(writing_score ~ race_ethnicity, data=data, col="lightgreen")
boxplot(average_score ~ race_ethnicity, data=data)
by_race_ethnicity <- data %>% group_by(race_ethnicity) %>% summarize(average_score=mean(average_score))

##by parental level of education
boxplot(math_score ~ parental_level_of_education, data=data, col="lightblue")
boxplot(reading_score ~ parental_level_of_education, data=data, col="violet")
boxplot(writing_score ~ parental_level_of_education, data=data, col="lightgreen")
boxplot(average_score ~ parental_level_of_education, data=data)
by_parental_level_of_education <- data %>% group_by(parental_level_of_education) %>% summarize(average_score=mean(average_score))

##by lunch
boxplot(math_score ~ lunch, data=data, col="lightblue")
boxplot(reading_score ~ lunch, data=data, col="violet")
boxplot(writing_score ~ lunch, data=data, col="lightgreen")
boxplot(average_score ~ lunch, data=data)
by_lunch <- data %>% group_by(lunch) %>% summarize(average_score=mean(average_score))

##by test preparation course
boxplot(math_score ~ test_preparation_course, data=data, col="lightblue")
boxplot(reading_score ~ test_preparation_course, data=data, col="violet")
boxplot(writing_score ~ test_preparation_course, data=data, col="lightgreen")
boxplot(average_score ~ test_preparation_course, data=data)
by_test_preparation_course<- data %>% group_by(test_preparation_course) %>% summarize(average_score=mean(average_score))

#Difference of each mean
df_g <- max(by_gender[,2])-min(by_gender[,2])
df_r <- max(by_race_ethnicity[,2])-min(by_race_ethnicity[,2])
df_p <- max(by_parental_level_of_education[,2])-min(by_parental_level_of_education[,2])
df_l <- max(by_lunch[,2])-min(by_lunch[,2])
df_t <- max(by_test_preparation_course[,2])-min(by_test_preparation_course[,2])
c(df_g, df_r, df_p, df_l, df_t)

#Relationship between each score
##math & reading
plot(math_score~reading_score, data=data)
cor(data$math_score, data$reading_score)
abline(lm(math_score~reading_score, data=data),col="red")

##math & writing
plot(math_score~writing_score, data=data)
cor(data$math_score, data$writing_score)
abline(lm(math_score~writing_score, data=data),col="red")

##reading & writing
plot(reading_score~writing_score, data=data)
cor(data$reading_score, data$writing_score)
abline(lm(reading_score~writing_score, data=data),col="red")


#Split the data set into training and test set
##Create train set and test set
set.seed(1)
test_index<-createDataPartition(y=data$average_score, times=1, p=0.2, list=FALSE)
train_set<-data[-test_index,]
test_set<-data[test_index,]

#The RMSE function that can be used in this project
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Standard deviation of average score in test set
summary(test_set$average_score)
just_sd <- sd(test_set$average_score)
just_sd

##Gender model
g_model <- lm(average_score~gender, data=train_set)
summary(g_model)
predicted_score <- predict(g_model, newdata = test_set)
g_rmse <- RMSE(test_set$average_score, predicted_score)

##race model
r_model <- lm(average_score~race_ethnicity, data=train_set)
summary(r_model)
predicted_score <- predict(r_model, newdata = test_set)
r_rmse <- RMSE(test_set$average_score, predicted_score)

##parental model
p_model <- lm(average_score~parental_level_of_education, data=train_set)
summary(p_model)
predicted_score <- predict(p_model, newdata = test_set)
p_rmse <- RMSE(test_set$average_score, predicted_score)

##Lunch model
l_model <- lm(average_score~lunch, data=train_set)
summary(l_model)
predicted_score <- predict(l_model, newdata = test_set)
l_rmse <- RMSE(test_set$average_score, predicted_score)

##test preparation model
t_model <- lm(average_score~test_preparation_course, data=train_set)
summary(t_model)
predicted_score <- predict(t_model, newdata = test_set)
t_rmse <- RMSE(test_set$average_score, predicted_score)

print(c(g_rmse, r_rmse, p_rmse, l_rmse, t_rmse))

##total model
model <- lm(average_score~gender+race_ethnicity+parental_level_of_education+lunch+test_preparation_course, data=train_set)
summary(model)
predicted_score <- predict(model, newdata = test_set)
rmse <- RMSE(test_set$average_score, predicted_score)
print(rmse)

par(mfrow=c(2,2)) 
plot(model) 
par(mfrow=c(1,1))

#Prediction by Random forest
##Find the best tuned mtry of model
tuneGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6))
control <- trainControl(method = "cv", number = 5)
model_rf <- train(average_score ~ gender+race_ethnicity+parental_level_of_education+lunch+test_preparation_course,
                  data = train_set,
                  method = "rf",
                  trControl = control,
                  tuneGrid = tuneGrid)

print(model_rf$bestTune)

##modeling
model_rf <- randomForest(average_score ~ gender+race_ethnicity+parental_level_of_education+lunch+test_preparation_course, 
                         data = train_set, 
                         ntree = 500,
                         mtry=2,
                         nodesize=5)
predicted_score_rf <- predict(model_rf, newdata = test_set)
rmse_rf <- RMSE(test_set$average_score, predicted_score_rf)
print(rmse_rf)
