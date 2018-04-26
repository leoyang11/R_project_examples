install.packages("neuralnet")


# importing data
library(MASS)
head(Boston)
str(Boston)
data <- Boston

# normalize data:: neural net 사용할 때는 반드시 normalize
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
maxs
mins
scaled.data <- scale(data, center=mins, scale = maxs-mins)  #scale 좀 더 살펴보자
scaled <- as.data.frame(scaled.data)
summary(scaled)

# train-test split
library(caTools)
split <- sample.split(scaled$medv, SplitRatio = 0.7)
train <- subset(scaled, split == T)
test <- subset(scaled, split == F)

# neural net에서는 y ~ . 을 쓸 수 없어서 변수 하나하나 입력이 필요.  그래서 찾은 꼼수?!
n <- names(train)   
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
n
f

# neural net
library(neuralnet)
nn <- neuralnet(f, data = train, hidden = c(5,3), linear.output = T)
plot(nn)

# prediction
predicted.nn.values <- compute(nn, test[1:13])
str(predicted.nn.values)
true.predictions <- predicted.nn.values$net.result * (max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test$medv) * (max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn

error.df <- data.frame(test.r, true.predictions)
head(error.df)

# visualization
library(ggplot2)
ggplot(error.df, aes(x=test.r, y=true.predictions)) + geom_point() + stat_smooth()




