
library(WDI)
library(lmtest)

# Download FDI and GDP data for Germany
country <- "DEU"
indicators <- c("BX.KLT.DINV.CD.WD", "NY.GDP.MKTP.CD")
data <- WDI(country = country, indicator = indicators, start = 1990, end = 2020)

# Extracting FDI and GDP data
fdi <- data$BX.KLT.DINV.CD.WD
gdp <- data$NY.GDP.MKTP.CD


df <- data.frame(FDI = fdi, GDP = gdp)

# Checking stationarity using the ADF test
adf_test_fdi <- ur.df(df$FDI, type = "drift", lags = 1)
adf_test_gdp <- ur.df(df$GDP, type = "drift", lags = 1)

# Perform Granger causality test

# Test if FDI Granger causes GDP
grangertest(GDP ~ FDI, data = df, order = 1)

# Test if GDP Granger causes FDI
grangertest(FDI ~ GDP, data = df, order = 1)

