library("rpart")
library("rpart.plot")
library("tree")
library("randomForest")

#########################
#       input data      #
#########################
data(ptitanic)
# only lines with no NA
ptitanic <- ptitanic[apply(X = ptitanic, MARGIN = 1, FUN = function(x) sum(is.na(x))) == 0,]

set.seed(123)
train_size <- floor(0.8 * nrow(ptitanic))
indexes <- sample(c(1:nrow(ptitanic)), size = train_size, replace = FALSE)
train <- ptitanic[indexes,]
test <- ptitanic[-indexes,]

####################################
#     fit the model with rpart     #
####################################
tree <- rpart(survived ~ ., data = train, control = rpart.control(cp = 0.00001))
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree <- prune(tree, cp = bestcp)
plot(tree)
text(tree, cex = 0.8, use.n = TRUE, xpd = TRUE)

####################################
#      fit the model with tree     #
####################################
tree2 <- tree(survived ~ ., data=train)
summary(tree2)
plot(tree2)
text(tree2, cex = 0.8, xpd = TRUE)

############################################
#      fit the model with randomForest     #
############################################
tree.rf <- randomForest(survived ~ ., data=train, ntree=500)
print(tree.rf)
plot(tree.rf)

#  the total decrease in node impurities (Gini index) from splitting on the variable, averaged over all trees
importance(tree.rf)
varImpPlot(tree.rf)

#######################################
#       prediction, verification      #
#######################################
accuracy_computation <- function(tree, train, test) {
  # predict type = c("vector", "prob", "class", "matrix")
  conf.matrix_train <- table(train$survived, predict(tree, train, type="class"))
  rownames(conf.matrix_train) <- paste("Actual", rownames(conf.matrix_train), sep = ":")
  colnames(conf.matrix_train) <- paste("Pred", colnames(conf.matrix_train), sep = ":")
  print(conf.matrix_train)
  acc_train <- (conf.matrix_train[1,1]+conf.matrix_train[2,2])/sum(conf.matrix_train)
  
  conf.matrix_test <- table(test$survived, predict(tree, test, type="class"))
  rownames(conf.matrix_test) <- paste("Actual", rownames(conf.matrix_test), sep = ":")
  colnames(conf.matrix_test) <- paste("Pred", colnames(conf.matrix_test), sep = ":")
  print(conf.matrix_test)
  acc_test <- (conf.matrix_test[1,1]+conf.matrix_test[2,2])/sum(conf.matrix_test)
  
  cat("Accuracy (train): ", round(acc_train*100,2), ", accuracy (test): ", round(acc_test*100,2))
}

accuracy_computation(tree, train, test)
accuracy_computation(tree2, train, test)
accuracy_computation(tree.rf, train, test)

