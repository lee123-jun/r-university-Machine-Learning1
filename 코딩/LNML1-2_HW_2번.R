x <- c(36.5, 28, 42.9, 52, 51.5, 53.8, 25.4,
       37.2, 50.9, 29.2)

y <- 100*c(14,9,15,20,21,25,9,13,20,10)

customer <- data.frame(x,y)

plot(customer, pch = 16, cex = 2)

reg1 <- lm(x ~ y, data = customer)

predict(reg1, newdata = data.frame(y = 1700))
          