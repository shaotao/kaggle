library("class")
library("DMwR")
library("gmodels")


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

setwd("d:\\Dropbox\\kaggle\\titanic")

# process train data
input_train_df <- read.csv("input\\train.csv", header=TRUE, stringsAsFactors=FALSE)
train_df <- input_train_df[c(3,5,6,7,8,10,12)]
train_df$Sex <- factor(train_df$Sex, levels = c("male", "female"), labels=c(0, 1))
train_df$Sex <- as.numeric(train_df$Sex)
train_df$Embarked <- factor(train_df$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))
train_df$Embarked <- as.numeric(train_df$Embarked)

#str(train_df)
#table(train_df$Pclass)

train_df <- knnImputation(train_df, k=3)

#train_df <- as.data.frame(lapply(train_df, normalize));
train_df <- as.data.frame(scale(train_df));

summary(train_df)

# process test dta
input_test_df <- read.csv("input\\test.csv", header=TRUE, stringsAsFactors=FALSE)
test_df <- input_test_df[c(2,4,5,6,7,9,11)]
test_df$Sex <- factor(test_df$Sex, levels = c("male", "female"), labels=c(0, 1))
test_df$Sex <- as.numeric(test_df$Sex)
test_df$Embarked <- factor(test_df$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))
test_df$Embarked <- as.numeric(test_df$Embarked)

test_df <- knnImputation(test_df, k=3)
#test_df <- as.data.frame(lapply(test_df, normalize));
test_df <- as.data.frame(scale(test_df))
summary(test_df)


train_labels <-as.numeric(input_train_df[,2])
summary(train_labels)
table(train_labels)
str(train_labels)

p <- knn(train=train_df, test = test_df, cl = train_labels, k = 10)

str(p)

haha <- input_test_df
haha$Survived <- as.numeric(p)-1

str(haha)
write.csv(haha[c("PassengerId", "Survived")], file="result.csv", row.names = FALSE)
