f <- function(x1, x2)
  x1^2+x2^2-2*x1+x1*x2+1
x <- c(5,5)
alpha <- 0.1
f_prime_x1 <- Deriv(f,"x1")
f_prime_x2 <- Deriv(f,"x2")

for(i in 100){
  grad <- c(f_prime_x1(x[1],x[2])
            ,f_prime_x2(x[1],x[2]))
  x <- x - alpha*grad
  cat(x,f(x[1],x[2]), "\n")
  Sys.sleep(0.05)
}
