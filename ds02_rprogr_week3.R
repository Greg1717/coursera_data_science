

# Quiz =====

library(datasets)
data(iris)
?iris
iris
str(iris)

mean(iris[iris$Species == "virginica", 1])

library(data.table)
dtiris <- as.data.table(iris)
dtiris[Species == "virginica", mean(Sepal.Length)]
colMeans(iris)
apply(iris[, 1:4], 2, mean)


mtcars
dtmtcars <- as.data.table(mtcars)
dtmtcars

tapply(mtcars$mpg, mtcars$cyl, mean)

with(mtcars, tapply(mpg, cyl, mean))

sapply(split(mtcars$mpg, mtcars$cyl), mean)

tapply(mtcars$hp, mtcars$cyl, mean)
dtmtcars[cyl == 8, mean(hp)]-dtmtcars[cyl == 4, mean(hp)]
