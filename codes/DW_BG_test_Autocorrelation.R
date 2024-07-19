car_data <- read.csv("car data.csv") ## Loading the dataset
model <- lm(Selling_Price ~ Present_Price + Kms_Driven, data = car_data ) ## Fit regression model
durbinWatsonTest(model) ## Performing Durbin Watson test

###############
install.packages("lmtest")
library(lmtest) ## Loading lmtest package
## Performing Breusch-Godfrey test for serial correlation of order up to 3
bgtest(Selling_Price ~ Present_Price + Kms_Driven, order = 3, data = car_data)

install.packages("systemfit")
library( systemfit )


data("cars")
