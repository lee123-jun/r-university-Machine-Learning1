state <- as.data.frame(state.x77)

sam <- sample(1:50, 35)
state_train <- state[sam,]
state_test <- state[-sam,]

# 일반선형회귀모델
lreg <- lm(Murder ~ .,data=state_train)
w_hat_lreg <- coef(lreg)
y_hat_lreg <- predict(lreg, state_test[,-5])

data.frame(state_test$Murder, y_hat_lreg)

sum((state_test$Murder - y_hat_lreg)^2)

# 능형회귀 (Ridge regression)
X_train <- data.matrix(state_train[,-5])
Y_train <- data.matrix(state_train[,5])

X_test <- data.matrix(state_test[,-5])
Y_test <- data.matrix(state_test[,5])

#install.packages("glmnet")
library(glmnet)

lambdas <- 10^seq(3, -3, by=-0.1)
cv_fit <- cv.glmnet(X_train, # 학습데이터 입력변수
                    Y_train,  # 학습데이터 출력변수
                    alpha = 0, # 능형회귀
                    lambda = lambdas) 

plot(cv_fit)
best_lambda <- cv_fit$lambda.min
rdreg <- glmnet(X_train, Y_train,
                alpha = 0, lambda = best_lambda)
coef(rdreg)

y_hat_rdreg <- predict(rdreg, X_test)
sum((Y_test - y_hat_rdreg)^2)

# 라쏘 (Lassoa)
cv_fit <- cv.glmnet(X_train, # 학습데이터 입력변수
                    Y_train,  # 학습데이터 출력변수
                    alpha = 1, # 라쏘
                    lambda = lambdas) 

plot(cv_fit)
best_lambda <- cv_fit$lambda.min
lassoreg <- glmnet(X_train, Y_train,
                alpha = 0, lambda = best_lambda)
coef(lassoreg)

y_hat_lassoreg <- predict(rdreg, X_test)

sum((Y_test - y_hat_rdreg)^2)
sum((Y_test - y_hat_lassoreg)^2)

coef(lassoreg)

# 엘라스틱넷 (elastic net)
cv_fit <- cv.glmnet(X_train, # 학습데이터 입력변수
                    Y_train,  # 학습데이터 출력변수
                    alpha = 0.5, # 엘라스틱넷
                    lambda = lambdas) 

plot(cv_fit)
best_lambda <- cv_fit$lambda.min
elnetreg <- glmnet(X_train, Y_train,
                   alpha = 0.5, lambda = best_lambda)
coef(elnetreg)

y_hat_elnetreg <- predict(elnetreg, X_test)

sum((Y_test - y_hat_elnetreg)^2)





