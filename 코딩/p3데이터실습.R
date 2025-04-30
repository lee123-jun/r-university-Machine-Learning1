load("p3.RData")
plot(p3, pch=16, col="red", cex=1)


# y = w0 + w1x1 + w2x2
lreg3 <- lm(y ~ ., data = p3)
summary(lreg3)

# # y = w0 + w1x1 + w2x2 + w3x1^2 + w4x2^2 + w5x1x2 다항회귀분석
p3$x3 <- p3$x1^2
p3$x4 <- p3$x2^2
p3$x5 <- p3$x1*p3$x2
p3 # 새로운 x3, x4, x5가 생겼음!
nlreg3 <- lm(y ~ ., data = p3)
summary(nlreg3)
# 예측력이 더 높아졌다 r^2로 판별