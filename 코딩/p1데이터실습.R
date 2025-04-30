load("p1.RData")
view(p1)

plot(p1,ch=16,col="red",cex=2) #포물선 경향이 나온다

# 선형회귀 분석
# linear regression
lreg1 <- lm(y ~ x1, data = p1)
abline(lreg1, lwd=3, col="blue")
summary(lreg1)
# p-value로써 예측의 도구로써 사용은 가능하지만
# 신뢰하기에는 R^2값이 낮다

#p-value가 낮다 → x1은 y에 영향을 주는 건 맞다.
#하지만 R²가 낮다 → 전체적인 예측력은 낮음.
#즉, 예측 "경향"은 보일 수 있지만, 실제 정확한 예측은 어렵다는 뜻입니다.


# y = w0 + w1*x + w2*x^2
p1$x2 <- p1$x1^2
nlreg1 <- lm(y ~ x1 + x2, data = p1)
w0_hat <- nlreg1$coefficients[1]
w1_hat <- nlreg1$coefficients[2]
w2_hat <- nlreg1$coefficients[3]

curve(w0_hat + w1_hat*x + w2_hat*x^2,
      lwd=3, col="darkgreen", add=T)

summary(nlreg1)

# 중요 
y <- p1$y
y_hat <- nlreg1$fitted.values
y_bar <- mean(y)

SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat - y_bar)^2)
SSE <- sum((y - y_hat)^2)

MSR <- SSR/2
MSE <- SSE/97
MSR/MSE 


