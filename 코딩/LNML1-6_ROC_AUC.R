library(dplyr)

set.seed(11)
x <- round(c(runif(20,0,6), runif(20,5,9)) %>% round(2),2)
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40
plot(df,pch=16)

logreg1 <- glm(y ~ x, data = df, family = "binomial")
w0_hat <- coef(logreg1)[1]
w1_hat <- coef(logreg1)[2]
f <- function(x) 
  1/(1+exp(-(w0_hat + w1_hat*x)))
curve(f, 0, 8, col="red", lwd=3, add=T)
abline(h=0.5, col="gray")

library(rootSolve)

threshold <- uniroot(function(x) f(x) - 0.5, c(0,8))$root
threshold
abline(v = threshold, lty = 2)
df$prob <- round(logreg1$fitted.values,2)
df$y_hat <- ifelse(df$prob > 0.5, 1, 0)
df

mean(df$y == df$y_hat)

#install.packages("caret")
library(caret)

y_pred <- as.factor(df$y_hat)
y_real <- as.factor(df$y)

# confusionMatrix(예측값, 실제값)
cfm <- confusionMatrix(y_pred, y_real)
cfm$table
cfm$overall[1]
subset(df, y != y_hat) 

# 시험문제 confusion matrix
# 좋은 모델이란 정확도가 1에 가깝고 tpr(sensitivity)은 1에 가까우면 fpr은 0에 가까우면 좋은 것이다

install.packages("ROCR")
library(ROCR)

#View(df)
prob <- logreg1$fitted.values
pr <- prediction(prob, df$y)
ROC1 <- performance(pr, measure = 'tpr',
                    x.measure = 'fpr')  

plot(ROC1, col="blue", lwd=3)
AUC1 <- performance(pr, measure = 'auc')
AUC1@y.values


