x1 <- 1:10
x2 <- 3:12
x2[c(5,6)] <- 11
y <- c(4,5,8,10,11,14,16,18,20,20)

df <- data.frame(x1,x2,y)
x <- cbind(1,x1,x2)

w_hat <- solve(t(x) %*% x) %*% t(x) %*% y
reg1 <- lm(y ~ ., data = df)
reg1















