### mth676

### Panel data
library(readr)
PanelData <- read_csv("C:/Users/CHARITHA/OneDrive/Documents/R programming/MTH676")

library(plm)
Y <- PanelData$C
X <- cbind(PanelData$Q,PanelData$PF,PanelData$LF)

pdata <- pdata.frame(PanelData,index = c("I","T"))

summary(Y)
summary(X)

pooling <- plm(Y~X,data=pdata,model="pooling")
summary(pooling)

between <- plm(Y~X,data=pdata,model="between")
summary(between)

firstdiff <- plm(Y~X,data=pdata,model="fd")
summary(firstdiff)

fixed <- plm(Y~X,data=pdata,model="within")
summary(fixed)

random <- plm(Y~X,data=pdata,model="random")
summary(random)

# LM test for random effects v/s OLS
plmtest(pooling)

# LM test for fixed effects v/s OLS
pFtest(fixed,pooling)

# Hausman test for fixed v/s random effects model
phtest(random,fixed)


