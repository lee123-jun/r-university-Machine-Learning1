x1 <- c(14,16,13,10,18,17,16,15,11,10)
x2 <- c(37,43,38,42,36,33,40,35,34,29)
y <- c(850,970,730,940,920,830,940,900,760,710)

yoplet <- data.frame(x1,x2,y)
reg1 <- lm(y ~ ., data = yoplet)
summary(reg1)

yoplet1 <- yoplet
yoplet$x3 <- 10
reg2 <- lm(y ~ ., data = yoplet)
summary(reg2)

view(yoplet1)
# 의미 없는 변수가 추가가 되었더라도 
# 결정계수 값이 증가하지 못하게 막아주는 것
