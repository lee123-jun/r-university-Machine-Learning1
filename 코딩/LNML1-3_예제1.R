f <- function(x) x^2-10*x+26
curve(f,1,9,lwd=2)

x <- 7
alpha <- 0.99
points(x,f(x),col="red",cex=2)

#install.packages("Deriv")
library(Deriv)

f_prime <- Deriv(f, "x")

for(i in 1:1000){
  x<- x - alpha*f_prime(x)
  points(x,f(x), col="red", cex=2, pch=16)
}

  