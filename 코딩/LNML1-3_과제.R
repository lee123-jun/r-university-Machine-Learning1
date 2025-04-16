library(Deriv)

f <- function(x1, x2, x3)
  (x1 - 4)^2 + x3^2 * x1 + (x2 + 1)^2 + 6

x <- c(2, 2, 2)
alpha <- 0.1

f_prime_x1 <- Deriv(f, "x1")
f_prime_x2 <- Deriv(f, "x2")
f_prime_x3 <- Deriv(f, "x3")

for (i in 1:100) {
  grad <- c(
    f_prime_x1(x[1], x[2], x[3]),
    f_prime_x2(x[1], x[2], x[3]),
    f_prime_x3(x[1], x[2], x[3])
  )
  
  x <- x - alpha * grad
  cat("x:", x, "f(x):", f(x[1], x[2], x[3]), "\n")
  Sys.sleep(0.05)
}