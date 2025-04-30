load("p4.RData")
plot(p4, pch=16, col="red", cex=2)

# y = w0 + w1x
lreg4 <- lm(y ~ x, data=p4)
abline(lreg4, lwd=3, col="blue")
summary(lreg4)
# 예측이 조금 괴리감이 있다
# 오목한 형태로 예측을하는게 좋을 것 같다

# y = w0 + w1(x-x+bar) + w2(x-x_bar)^2
x_bar <- mean(p4$x)
p4$x1 <- p4$x - x_bar
p4$x2 <- (p4$x - x_bar)^2
nlreg4 <- lm(y ~ x1 + x2,data=p4)

w0_hat <- nlreg4$coefficients[1]
w1_hat <- nlreg4$coefficients[2]
w2_hat <- nlreg4$coefficients[3]

curve(w0_hat + w1_hat*(x-x_bar) + w2_hat*(x-x_bar)^2,
      lwd=3, col="darkgreen", add=T)

summary(nlreg4)

SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat - y_bar)^2)
SSE <- sum((y - y_hat)^2)

SSR/SST # R^2

MSR <- SSR/2
MSE <- SSE/16
MSR/MSE # F통계량

1-pf(MSR/MSE,2,16) #P-VALUE

