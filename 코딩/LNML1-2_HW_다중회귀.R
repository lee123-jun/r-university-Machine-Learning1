library(Deriv)
state <- as.data.frame(state.x77)
state <- state[, -c(6, 8)]

# 1. 정규화 + 절편항 추가
y <- state$Murder
X_raw <- scale(as.matrix(state[, setdiff(names(state), "Murder")]))
X <- cbind(1, X_raw)

# 2. 새로운 형태의 오차 함수 정의 (X, y를 인자로 명시!)
rss <- function(beta, X, y) {
  y_hat <- X %*% beta
  sum((y - y_hat)^2)
}

# 3. 미분할 함수 정의: 벡터를 받아서 beta 개별 요소로 풀어서 처리
rss_vec <- function(b0, b1, b2, b3, b4, b5) {
  beta <- c(b0, b1, b2, b3, b4, b5)
  rss(beta, X, y)
}

# 4. Deriv로 편미분 함수 생성
grad_funcs <- list(
  Deriv(rss_vec, "b0"),
  Deriv(rss_vec, "b1"),
  Deriv(rss_vec, "b2"),
  Deriv(rss_vec, "b3"),
  Deriv(rss_vec, "b4"),
  Deriv(rss_vec, "b5")
)

# 5. 경사하강법 실행
beta <- rep(0, 6)
alpha <- 0.0001
epochs <- 3000
cost_history <- numeric(epochs)

for (i in 1:epochs) {
  grad <- sapply(grad_funcs, function(df) do.call(df, as.list(beta)))
  beta <- beta - alpha * grad
  cost_history[i] <- rss(beta, X, y)
  
  if (i %% 500 == 0) {
    cat(sprintf("Epoch %d: beta = %s\n", i, paste(round(beta, 4), collapse = ", ")))
  }
}

