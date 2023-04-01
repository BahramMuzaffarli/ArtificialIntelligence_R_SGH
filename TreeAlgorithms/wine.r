library("rpart")
library("randomForest")
library("caret")

#########################
#       input data      #
#########################
wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), header = TRUE, sep = ";")
str(wine)

# classify the wines ranked as 5 and 6 as ?normal?, the lower ranked wines as ?bad?, and the wines ranked above as ?good?.
wine$taste <- ifelse(wine$quality < 5, "bad", "good")
wine$taste[wine$quality == 5] <- "normal"
wine$taste[wine$quality == 6] <- "normal"
wine$taste <- as.factor(wine$taste)
table(wine$quality)
table(wine$taste)

set.seed(9876)
train_size <- floor(0.8 * nrow(wine))
indexes <- sample(c(1:nrow(wine)), size = train_size, replace = FALSE)
train <- wine[indexes,]
test <- wine[-indexes,]

####################################
#     fit the model with rpart     #
####################################
tree <- rpart(taste ~ . - quality, data = train, control = rpart.control(cp = 0.00001))
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree <- prune(tree, cp = bestcp)
rpart.plot(tree, type = 4, extra = 101, tweak = 1.6)
confusionMatrix(table(train$taste, predict(tree, train, type="class")))
confusionMatrix(table(test$taste, predict(tree, test, type="class")))

############################################
#      fit the model with randomForest     #
############################################
tree2 <- randomForest(taste ~ . - quality, data = train)
confusionMatrix(table(train$taste, predict(tree2, train, type="class")))
confusionMatrix(table(test$taste, predict(tree2, test, type="class")))
importance(tree2)
varImpPlot(tree2)

