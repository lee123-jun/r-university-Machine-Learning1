# 수정된 코드
load("quiz2.RData")  # 데이터프레임 'df'가 환경에 로드됨

str(df)
colnames(df)

cubic_model <- lm(y ~ x1 + I(x1^2) + I(x1^3), data = df)  # data=df 사용

# Fit cubic polynomial model
cubic_model <- lm(y ~ x1 + I(x1^2) + I(x1^3), data = df)

# Get R-squared from model summary
r_squared <- summary(cubic_model)$r.squared

# Plot data and fitted curve
plot(df$x, df$y)
curve(coef(cubic_model)[1] + coef(cubic_model)[2]*x + 
        coef(cubic_model)[3]*x^2 + coef(cubic_model)[4]*x^3,
      lwd=3, col="red", add=TRUE)

# Display results
cat("R-squared:", r_squared, "\n")

