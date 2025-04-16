df <- data.frame(x = c(1,2.5,3), y = c(1,1.5,3))
plot(df, col="red", pch=16, cex=2)

reg1 <- lm(y ~ x, data = df)
abline(reg1, col="orange", lwd = 3)

w <- matrix(c(0,0))
alpha <- 0.07 

x <- cbind(1,df$x)
matrix(df$y)

for(i in 1:1000) {
  w <- w - alpha*t(x)%*%(x%*%w-y)/length(y)
  cat(as.vector(w), "\n")
}
abline(a=w[1], b=w[2], lwd=2, col="blue")


