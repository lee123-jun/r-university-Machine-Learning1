load("p2.RData")
plot(p2, pch=16, col="red", cex=1)

# 선형 회귀
lreg2 <- lm(y ~ x1, data = p2)
abline(lreg2, lwd=3, col="blue")
summary(lreg2)

# sin(x1) 항 추가
p2$x2 <- sin(p2$x1)

# 비선형 회귀 (선형 + 사인항)
nlreg1 <- lm(y ~ x1 + x2, data = p2)

# 계수 추출
w0_hat <- nlreg1$coefficients[1]
w1_hat <- nlreg1$coefficients[2]
w2_hat <- nlreg1$coefficients[3]

# 곡선 그리기
curve(w0_hat + w1_hat*x + w2_hat*sin(x),
      lwd = 2, col = "darkgreen", add = TRUE)

# 모델 요약
summary(nlreg1)