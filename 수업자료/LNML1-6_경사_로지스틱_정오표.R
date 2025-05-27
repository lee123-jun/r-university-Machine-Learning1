#install.packages("dplyr")
library(dplyr)

set.seed(11)
x <- c(runif(20,0,6), runif(20,5,9)) %>% round(2)
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40

plot(df, pch=16)
#View(df)
logreg1 <- glm(y ~ x, data=df, family = "binomial")
w0_hat <- coef(logreg1)[1]
w1_hat <- coef(logreg1)[2]

curve(1/(1+exp(-(w0_hat+w1_hat*x))),
      add = T, col="red", lwd=3)
abline(h=0.5, col="gray")

#install.packages("rootSolve")
library(rootSolve)
threshold <- uniroot(function(x) 1/(1+exp(-(w0_hat+w1_hat*x)))-0.5,
        c(0,8))$root
abline(v=5.187739, lty=2) # 점선라인 threshold 그리기

#View(df)
df$prob <- logreg1$fitted.values

df$y_hat <- ifelse(df$prob > 0.5, 1, 0)

mean(df$y == df$y_hat)

df[df$y != df$y_hat,] # 같지 않은 관측치를 확인한다
