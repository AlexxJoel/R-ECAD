
data(mtcars)

cor(mtcars$mpg, mtcars[, c("disp", "hp", "drat", "qsec", "wt", "vs", "am", "gear", "carb")])
