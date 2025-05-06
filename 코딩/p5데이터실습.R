getwd()
load("p5.RData")
plot(p5)

# 1. 일반 선형회귀
lin_model <- lm(y ~ x, data = p5)
abline(lin_model, lwd = 3, col = "blue")
summary(lin_model)

# 2. 비선형회귀
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

# y > 0인 데이터만 사용
p5_filtered <- subset(p5, y > 0)

# 로그 변환
log_y <- log(p5_filtered$y)

# 로그 변환된 y에 대해 선형 회귀
exp_model <- lm(log_y ~ x, data = p5_filtered)

# 예측값, 평균
log_y_hat <- fitted(exp_model)
log_y_bar <- mean(log_y)

# SST, SSR, SSE 계산 (모두 로그 스케일에서)
SST <- sum((log_y - log_y_bar)^2)
SSR <- sum((log_y_hat - log_y_bar)^2)
SSE <- sum((log_y - log_y_hat)^2)

# 결정계수
R_squared <- SSR / SST

# 자유도
df_reg <- 1
df_err <- length(log_y) - 2

# 평균 제곱
MSR <- SSR / df_reg
MSE <- SSE / df_err

# F-통계량
F_stat <- MSR / MSE

# p-value
p_value <- 1 - pf(F_stat, df_reg, df_err)

# 결과 출력
cat("SST:", SST, "\n")
cat("SSR:", SSR, "\n")
cat("SSE:", SSE, "\n")
cat("R^2:", R_squared, "\n")
cat("F-statistic:", F_stat, "\n")
cat("p-value:", p_value, "\n")
