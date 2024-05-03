library(lmtest)
library(zoo)
library(skedastic)
library(whitestrap)
library(cli)



# Load the dataset
real_estate <- read.csv("Real estate.csv")

# View the structure of the dataset
str(real_estate)

# View the first few rows of the dataset
head(real_estate)

# Renaming columns
colnames(real_estate)[2:8] <- c("x1", "x2", "x3", "x4", "x5", "x6" ,"y")
colnames(real_estate)
head(real_estate)

Y <- real_estate$y
X <- cbind(real_estate$x1,real_estate$x2,real_estate$x3,real_estate$x4,real_estate$x5,
           real_estate$x6)
head(Y)
head(X)

# Fitting a linear regression model
model <- lm(Y~X,real_estate)


summary(model)

# Glejser test
glejser(model)

#Bruesh-Pagan Test
bptest(model)

#Harvey's test
harvey(model)
harvey(model, auxdesign = "fitted.values")


#White's test
white_test(model)



