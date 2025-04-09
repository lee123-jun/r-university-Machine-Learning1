x <- c(1380,3120,3520,1130,1030,1720,3920,1490,
          1860,3430,2000,3660,2500,1220,1390)

y <- c(76,216,238,69,50,119,282,81,132,228,
          145,251,170,71,29)

house <- data.frame(x,y)

plot(house, pch = 16, cex = 2)

reg1 <- lm(y ~ x, data = house)

abline(reg1, col = "red", lwd =2)

reg1$coefficients

w0_hat <- reg1$coefficients[1]
w0_hat <- reg1$coefficients[2]

# predict 사용시 종속변수가 아닌 독립변수를 추가해서 명령어를 써야한다
# predict(reg1, newdata = data.frame(x=2227))

house$잔차 <- reg1$residuals
house$예측값 <- reg1$fitted.values
house


sum(house$잔차) # 잔차는 무조건 다 더하면 0이다 어떤한 회귀식도
