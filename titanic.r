library("rpart")
library("rpart.plot")

#########################
#       input data      #
#########################
data(ptitanic)

# split the dataset into the training and test set (see also createDataPartition from the caret package)
set.seed(123)
train_size <- floor(0.8 * nrow(ptitanic))
indexes <- sample(c(1:nrow(ptitanic)), size = train_size, replace = FALSE)
# head(indexes)
train <- ptitanic[indexes,]
test <- ptitanic[-indexes,]


#########################
#     fit the model     #
#########################
# tree <- rpart(survived ~ sex+age, data = train, control = rpart.control(cp = 0.00001))
tree <- rpart(survived ~ ., data = train, control = rpart.control(cp = 0.00001))

# select the best cp (we want the cp value (with a simpler tree) that minimizes the xerror)
# The complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node. 
# It is is used to control the size of the decision tree and to select the optimal tree size.
printcp(tree)
plotcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# prune the tree using the best cp
tree.pruned <- prune(tree, cp = bestcp)
tree.pruned1 <- prune(tree, cp = 1)
tree.pruned2 <- prune(tree, cp = 0.25)

#########################
#       plotting        #
#########################
# http://www.milbo.org/rpart-plot/prp.pdf
tree.pruned

# use.n = TRUE adds number of observations at each node
# xpd = TRUE keeps the labels from extending outside the plot
plot(tree.pruned)
text(tree.pruned, cex = 0.9, xpd = TRUE)
plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

# faclen = 0 means to use full names of the factor labels
# extra = 1 adds number of observations at each node; equivalent to using use.n = TRUE in plot.rpart
prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]
prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1, box.col = boxcols)

tot_count <- function(x, labs, digits, varlen) {
  paste(labs, "\n\nn =", x$frame$n)
}
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count, box.col = boxcols)

# too much pruning
prp(tree.pruned1)
prp(tree.pruned2)

# full tree
prp(tree)


#######################################
#       prediction, verification      #
#######################################
# pred <- predict(tree.pruned, data.frame(test))
# pred <- predict(tree.pruned2, data.frame(test))
pred <- predict(tree, data.frame(test))
test$actual <- (test$survived == "survived")
test$predicted <- (pred[,2] > 0.5)

# pred <- predict(tree.pruned, data.frame(train))
# pred <- predict(tree.pruned2, data.frame(train))
pred <- predict(tree, data.frame(train))
train$actual <- (train$survived == "survived")
train$predicted <- (pred[,2] > 0.5)

# confusion matrix
conf.matrix_train <- table(train$actual, train$predicted)
rownames(conf.matrix_train) <- paste("Actual", rownames(conf.matrix_train), sep = ":")
colnames(conf.matrix_train) <- paste("Pred", colnames(conf.matrix_train), sep = ":")
print(conf.matrix_train)
acc_train <- (conf.matrix_train[1,1]+conf.matrix_train[2,2])/sum(conf.matrix_train)

conf.matrix_test <- table(test$actual, test$predicted)
rownames(conf.matrix_test) <- paste("Actual", rownames(conf.matrix_test), sep = ":")
colnames(conf.matrix_test) <- paste("Pred", colnames(conf.matrix_test), sep = ":")
print(conf.matrix_test)
acc_test <- (conf.matrix_test[1,1]+conf.matrix_test[2,2])/sum(conf.matrix_test)

cat("Accuracy (train): ", acc_train, ", accuracy (test): ", acc_test)


install.packages("kernlab")
library("rpart")
library("rpart.plot")
library("kernlab")

data(spam)
tree= rpart(type ~., data = spam,control = rpart.control(cp=0.00001))
tree
plotcp(tree)
