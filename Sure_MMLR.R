##### SURE Model
install.packages("AER")
library(AER)


PRODUC_data <- data.frame(read_table('https://www.wiley.com/legacy/wileychi/baltagi/supp/PRODUC.prn'))
save(PRODUC_data, file = "PRODUC_data.Rdata")
data <- load("/Users/debarghyajana/Downloads/ECo/PRODUC_data.Rdata")
View(PRODUC_data)

Sure_model = systemfit(GSP ~ P_CAP + HWY + WATER + 
                  UTIL + PC + UNEMP, method = "SUR", data = PRODUC_data )
summary(Sure_model)

############ 
############## Multivariate Multiple Linear Regression Model

GSP_state <- list()
for(i in 1:length(data_1$STATE))
{
  GSP_state[[i]] <- data_1$GSP[which(data_1$STATE == data_1$STATE[i] )]
}

GSP_state_1 <- GSP_state[which(is.na(GSP_state) == FALSE)]
GSP_matrix <- do.call(rbind, GSP_state_1) 
dim(GSP_matrix)

###############

P_CAP_state <- list()
for(i in 1:length(data_1$STATE))
{
  P_CAP_state[[i]] <- data_1$P_CAP[which(data_1$STATE == data_1$STATE[i])]
}


P_CAP_state_1 <- P_CAP_state[which(is.na(P_CAP_state) == FALSE)]
P_CAP_state_1_matrix <- do.call(rbind, P_CAP_state_1)

################

HWY_state <- list()
for(i in 1:length(data_1$STATE))
{
  HWY_state[[i]] <- data_1$HWY[which(data_1$STATE == data_1$STATE[i])]
}


HWY_state_1 <- HWY_state[which(is.na(HWY_state) == FALSE)]
HWY_state_1_matrix <- do.call(rbind, HWY_state_1)

#################

WATER_state <- list()
for(i in 1:length(data_1$STATE))
{
  WATER_state[[i]] <- data_1$WATER[which(data_1$STATE == data_1$STATE[i])]
}


WATER_state_1 <- WATER_state[which(is.na(WATER_state) == FALSE)]
WATER_state_1_matrix <- do.call(rbind, WATER_state_1)

###################

UTIL_state <- list()
for(i in 1:length(data_1$STATE))
{
  UTIL_state[[i]] <- data_1$UTIL[which(data_1$STATE == data_1$STATE[i])]
}


UTIL_state_1 <- UTIL_state[which(is.na(UTIL_state) == FALSE)]
UTIL_state_1_matrix <- do.call(rbind, UTIL_state_1)

###################

PC_state <- list()
for(i in 1:length(data_1$STATE))
{
  PC_state[[i]] <- data_1$PC[which(data_1$STATE == data_1$STATE[i])]
}


PC_state_1 <- PC_state[which(is.na(PC_state) == FALSE)]
PC_state_1_matrix <- do.call(rbind, PC_state_1)

######################

UNEMP_state <- list()
for(i in 1:length(data_1$STATE))
{
  UNEMP_state[[i]] <- data_1$PC[which(data_1$STATE == data_1$STATE[i])]
}


UNEMP_state_1 <- UNEMP_state[which(is.na(UNEMP_state) == FALSE)]
UNEMP_state_1_matrix <- do.call(rbind, UNEMP_state_1)

########################

# Multivariate Multiple Linear Regression model
# Fitting the Model

model_1 <- lm(GSP_matrix ~ P_CAP_state_1_matrix )
summary(model_1)
##################
model_2 <- lm(GSP_matrix ~  HWY_state_1_matrix )
summary(model_2)
##################
model_3 <- lm(GSP_matrix ~  WATER_state_1_matrix )
summary(model_3)
##################
model_4 <- lm(GSP_matrix ~  UTIL_state_1_matrix )
summary(model_4)
##################
model_5 <- lm(GSP_matrix ~  PC_state_1_matrix )
summary(model_5)
##################
model_6 <- lm(GSP_matrix ~  UNEMP_state_1_matrix )
summary(model_6)
##################
model_7 <- lm(GSP_matrix ~  P_CAP_state_1_matrix + HWY_state_1_matrix)
summary(model_7)
##################
model_8 <- lm(GSP_matrix ~  P_CAP_state_1_matrix + WATER_state_1_matrix)
summary(model_8)
##################
model_9 <- lm(GSP_matrix ~  P_CAP_state_1_matrix + UTIL_state_1_matrix)
summary(model_9)
##################
model_10 <- lm(GSP_matrix ~  P_CAP_state_1_matrix + PC_state_1_matrix)
summary(model_10)
##################
model_11 <- lm(GSP_matrix ~  P_CAP_state_1_matrix + UNEMP_state_1_matrix)
summary(model_11)
##################
model_12 <- lm(GSP_matrix ~  HWY_state_1_matrix + WATER_state_1_matrix)
summary(model_12)
##################
model_13 <- lm(GSP_matrix ~  HWY_state_1_matrix + UTIL_state_1_matrix)
summary(model_13)
##################
model_14 <- lm(GSP_matrix ~  HWY_state_1_matrix + PC_state_1_matrix)
summary(model_14)
##################
model_15 <- lm(GSP_matrix ~  WATER_state_1_matrix + UTIL_state_1_matrix)
summary(model_15)
##################
model_16 <- lm(GSP_matrix ~  WATER_state_1_matrix + PC_state_1_matrix)
summary(model_16)
##################
model_17 <- lm(GSP_matrix ~  WATER_state_1_matrix + UNEMP_state_1_matrix)
summary(model_17)
##################
model_18 <- lm(GSP_matrix ~  UTIL_state_1_matrix + PC_state_1_matrix)
summary(model_18)
##################
model_19 <- lm(GSP_matrix ~  UTIL_state_1_matrix + UNEMP_state_1_matrix)
summary(model_19)
##################
model_20 <- lm(GSP_matrix ~  PC_state_1_matrix + UNEMP_state_1_matrix)
summary(model_20)





