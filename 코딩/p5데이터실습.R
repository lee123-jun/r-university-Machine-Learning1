load("p5.RData")
plot(p5)

# y = w0e^w1x
# 지수 함수에 로그를 취해준다
# 로그 변환한 선형 회귀: log(y) = log(w0) + w1 * x
# 이런것을 선형화 작업이라고 한다. linearization
p5_filtered <- subset(p5, y > 0)  # y > 0인 데이터만 사용

log_y <- log(p5_filtered$y)
exp_model <- lm(log_y ~ x, data = p5_filtered)

# 계수 추출 및 곡선 그리기
log_w0_hat <- coef(exp_model)[1]
w1_hat <- coef(exp_model)[2]
w0_hat <- exp(log_w0_hat)

curve(w0_hat * exp(w1_hat * x),
      lwd = 3, col = "darkgreen", add = TRUE)

summary(exp_model)

