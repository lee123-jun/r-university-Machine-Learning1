x <- 1:10
y <- c(3,3,3,6,6,9,9,9,10,11)

reg2 <- lm(y ~ x)

predict(reg2, newdata = data.frame(x = 11))