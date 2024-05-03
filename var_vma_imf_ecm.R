## Mth676 
### ====== VAR(p) ======= ### 
library(urca)
library(stats)
data("denmark")
# LRM ~ LPY
## LRM = log of real money
LRM <- ts(denmark$LRM, start = c(1947,1), end = c(1987,3),frequency = 4)
#plot.ts(LRM)

# LPY = log of price deflator
LPY <- ts(denmark$LPY, start = c(1947,1), end = c(1987,3),frequency = 4)
plot.ts(LPY)

## dynamic linear regression
library(dynlm)
VAR_eq1 <- dynlm(LRM ~ L(LRM,1:2) + L(LPY,1:2),
                 start =  c(1947,1), end = c(1987,3)) 

VAR_eq2 <- dynlm(LPY ~ L(LRM,1:2) + L(LPY,1:2),
                 start =  c(1947,1), end = c(1987,3)) 

names(VAR_eq1$coefficients) <- c("Intercept","LRM_t-1","LRM_t-2","LPY_t-1","LPY_t-2")
names(VAR_eq2$coefficients) <- names(VAR_eq1$coefficients)

# robust coefficient summaries
library(lmtest)
coeftest(VAR_eq1)
coeftest(VAR_eq2)


# using VAR() to obtain same estimates as OLS
library(vars)
VAR_data <- window(ts.union(LRM,LPY),start =  c(1947,1), end = c(1987,3))

VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est

# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$LRM)$adj.r.squared
summary(VAR_est$varresult$LPY)$adj.r.squared


### ====== VMA ======= ###
n <- 300
t <- as.Date(seq(n), origin = as.Date("01/01/2022", "%m/%d/%Y"))
set.seed(1)
y <- sin(pi*seq(-2,2, length.out = n)) + rnorm(n, sd = 0.5)
d <- data.frame(t, y)

library(ggplot2)
dev.off()
ggplot(d) + aes(t, y) + geom_line()

## VMA
library(stats)
d$ma3 <- filter(d$y, filter = rep(1/3, 3), sides = 2)

ggplot(d) + aes(t, y) + geom_line(alpha = 1/4) + geom_line(aes(y = ma3), color  = "red")


### ====== Impulse response function  ======= ###
# Download data
data <- read.table("http://www.jmulti.de/download/datasets/e1.dat", skip = 6, header = TRUE)

# Only use the first 76 observations so that there are 73 observations
# left for the estimated VAR(2) model after taking first differences.
data <- data[1:76, ]

# Convert to time series object
data <- ts(data, start = c(1960, 1), frequency = 4)

# Take logs and differences
data <- diff(log(data))

# Plot data
plot(data,  main = "Dataset E1 from Lütkepohl (2007)")

# Load package
library(vars)

# Estimate model
model <- VAR(data, p = 2, type = "const")

# Look at summary statistics
summary(model)

## forecast error impulse response
feir <- irf(model, impulse = "income", response = "cons",
            n.ahead = 8, ortho = FALSE, runs = 1000)

plot(feir)

## identity problem why impulse is starting from 0
# Calculate summary statistics
model_summary <- summary(model)

# Obtain variance-covariance matrix
model_summary$covres

## so off diagonal elements are non-zero
model_summary$corres

## Orthogonal impulse responses
## sigma = P.t(P), P = lower triangular matrix
# Cholesky Decomposition
t(chol(model_summary$covres))

oir <- irf(model, impulse = "income", response = "cons",
           n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir)

### ====== Error correction model  ======= ###
library(car)
library(lmtest)
library(ARDL)

library(readr)
df <- read_csv("April_data_6series.csv")
df <- df[ ,c(6,7)]
ln_gc <- log(df[,1])
ln_nq <- log(df[ ,2])
df <- data.frame(ln_gc,ln_nq)
df <- ts(df, start = c(2020), frequency = 1260)
ln_gc <- ts(ln_gc,start = c(2020), frequency = 1260)
ln_nq <- ts(ln_nq,start = c(2020), frequency = 1260)

plot(df, main = "GC and NQ")

## adf test for checking stationary
library(aTSA)
adf.test(ln_gc)
adf.test(ln_nq)

regression <- lm(ln_gc ~ ln_nq)
summary(regression)

ect <- regression$residuals
adf.test(ect)

ect <- ts(ect, start = c(2020), frequency = 1260)
l1_ect <- ts(ect, start = c(2020), frequency = 1260)

l1_ln_gc <- lag(ln_gc, -1)
l1_ln_nq <- lag(ln_nq, -1)

l1_ln_gc <- ts(l1_ln_gc, start = c(2020), frequency = 1260)
l1_ln_nq <- ts(l1_ln_nq, start = c(2020), frequency = 1260)
l1_ect <- l1_ect[-1]
new_df <- ts.union(ln_gc, ln_nq, ect, l1_ect, l1_ln_nq, l1_ln_nq)

ecm1 <- lm(diff(ln_gc) ~ diff(ln_nq) + l1_ect)
summary(ecm1)
l1_ln_gc <- l1_ln_gc[-1]
l1_ln_nq <- l1_ln_nq[-1]
ecm2 <- lm(diff(ln_gc) ~ diff(ln_nq) + l1_ln_gc + l1_ln_nq)
summary(ecm2)

ncvTest(ecm2)
bgtest(ecm2)

## short term run
c1 <- ecm2$coefficients[2]
c2 <- ecm2$coefficients[4]
c3 <- ecm1$coefficients[3]

## short term run
cons_sr_eq <- c1
cons_sr_eq

## long run term
cons_lr_eq <- c2/-c3
cons_lr_eq
