x1 <- c(14,16,13,10,18,17,16,15,11,10)
x2 <- c(37,43,38,42,36,33,40,35,34,29)
y <- c(850,970,730,940,920,830,940,900,760,710)

df <- data.frame(x1,x2,y)
reg1 <- lm(y ~ ., data = df)

summary(reg1)

SST <- sum((yoplet$y - mean(yoplet$y))^2)
SSE <- sum(reg1$residuals^2)
SSR <- sum((reg1$fitted.values - mean(yoplet$y))^2)

R2 <- SSR/SST
R2
