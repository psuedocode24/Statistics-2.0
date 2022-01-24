######################
### Solved Example 9
######################

### From basic econometric theory, one would expect a positive 
### relationship between real wages and (labor) productivity – ceteris paribus, 
### the higher the level of labor productivity, the higher the real wages. 
### To shed some light on this, we explore the data on Solved Example 9, 
### which contains data on real wages (real compensation per hour) and 
### labor productivity (output per hour of all persons) for the business 
### sector of the U.S. economy for the time period 1959 to 2006.
### Our objective is in this solved example is to learn ways of detecting 
### and remedying autocorrection.

SolvedExample9 <- read.csv("SolvedExample9.csv")

Productivity <- SolvedExample9$Productivity
RealWages <- SolvedExample9$Compensation

model_9 <- lm(RealWages ~ Productivity)
summary(model_9)

### Check for autocorrelation

### Graphical - Residuals versus Time
resid_9 <- residuals(model_9)
timeperiods <- 0:47
plot(x=timeperiods, type="b", y=resid_9, pch=19, 
     xlab = "Time", ylab = "Residuals", 
     main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals don't seem to be randomly distributed.
### Shows signs of (positive) autocorrelation

### Graphical - Residuals(t) versus Residuals(t-1)

lag.plot(resid_9, lags = 1, do.lines = FALSE, 
         diag = FALSE, 
         main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows evidence of positive autocorrelation.

### Durbin-Watson d Statistic
### Calculating using principles
ts_resid_9 <- ts(resid_9)
ts_resid_9_lag_1 <- lag(ts_resid_9, -1)
D <- ts_resid_9 - ts_resid_9_lag_1
D_sq <- D^2
e_sq <- resid_9^2
dw <- sum(D_sq)/sum(e_sq)
dw

### Using the command
lmtest::dwtest(model_9)

### The p-value is very small, indicating there is an evidence of
### positive first-order autocorrelation.

### Detect using Runs Test
randtests::runs.test(resid_9, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Remedy

### The First Difference Method
RealWages_First_Diff <- diff(RealWages, lag = 1, differences = 1)
Productivity_First_Diff <- diff(Productivity, lag = 1, differences = 1)

### Run the regression-through-the-origin
model_9_1 <- lm(RealWages_First_Diff ~ 0 + Productivity_First_Diff)
summary(model_9_1)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_1 <- residuals(model_9_1)
timeperiods <- 0:(NROW(resid_model_9_1)-1)
plot(x=timeperiods, type="b", 
     y=resid_model_9_1, pch=19, 
     xlab = "Time", ylab = "Residuals", 
     main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals seem to be randomly distributed.
### Shows no signs of autocorrelation.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_1, lags = 1, 
         do.lines = FALSE, diag = FALSE, 
         main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows less evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_1, alternative = "greater")

### The p-value is 0.03692, indicating there is an evidence of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_1, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Estimating rho from OLS residuals
lagged_resids <- ts_resid_9[1:length(ts_resid_9)-1]
resids_start_2nd_row <- ts_resid_9[-1]

model_9_1_est_rho <- lm(resids_start_2nd_row ~ 0 + lagged_resids)
summary(model_9_1_est_rho)

### Use rho-hat = 0.8915

### Creating the differencing terms
rho_hat <- 0.8915

RealWages_adj <- RealWages*rho_hat
Productivity_adj <- Productivity*rho_hat
Y_t_star <- RealWages[-1] - RealWages_adj[1:length(RealWages_adj)-1]
X_t_star <- Productivity[-1] - Productivity_adj[1:length(Productivity_adj)-1]

model_9_2 <- lm(Y_t_star ~ X_t_star)
summary(model_9_2)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_2 <- residuals(model_9_2)
timeperiods <- 0:(NROW(resid_model_9_2)-1)
plot(x=timeperiods, type="b", y=resid_model_9_2, 
     pch=19, xlab = "Time", 
     ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals seems to be randomly distributed.
### Shows no signs of autocorrelation.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_2, lags = 1, 
         do.lines = FALSE, diag = FALSE, 
         main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows less evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_2, alternative = "greater")

### The p-value is 0.05819, indicating the remedying of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_2, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Prais-Winsten Transformation
Y_1_star_PW <- sqrt(1-rho_hat^2)*head(RealWages[1])
X_1_star_PW <- sqrt(1-rho_hat^2)*head(Productivity[1])

### Create a data frame containing adjusted values
df <- data.frame(c1 = Y_t_star, c2 = X_t_star)
df <- rbind(c(Y_1_star_PW, X_1_star_PW), df)
colnames(df) <- c("RealWages_Adj", "Productivity_Adj")

### Now consider this new dataframe of adjusted values
model_9_3 <- lm(df$RealWages_Adj ~ df$Productivity_Adj)
summary(model_9_3)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_3 <- residuals(model_9_3)
timeperiods <- 0:(NROW(resid_model_9_3)-1)
plot(x=timeperiods, type="b", y=resid_model_9_3, 
     pch=19, xlab = "Time", ylab = "Residuals", 
     main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals seems to be randomly distributed.
### Shows no signs of autocorrelation. May be - hard to see.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_3, lags = 1, do.lines = FALSE, 
         diag = FALSE, 
         main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows some evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_3, alternative = "greater")

### The p-value is 0, indicating the presence of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_3, alternative = "two.sided")

### p-value is small indicating some evidence of nonrandomness

######################
### Solved Example 10
######################

## The data on the Solved Example 10 CSV file shows 
## data on sales (SALES) (in millions) and advertising (ADV) (in thousands) 
## for the XYZ company. These annual data cover a 35-year period. 
## Verify the existence of autocorrelation and use a lagged value of 
## the dependent variable to correct for the first-order autocorrelation 
## in the model for sales.

SolvedExample10 <- read.csv("SolvedExample10.csv")

SALES <- SolvedExample10$SALES
ADV <- SolvedExample10$ADV

### ORIGINAL MODEL
model_10 <- lm(SALES ~ ADV)
summary(model_10)

resid10 <- residuals(model_10)

### Using the DW Test
lmtest::dwtest(model_10)

### Detect using Runs Test
randtests::runs.test(resid10, alternative = "two.sided")

### Plots
### Graphical - Residuals versus Time
# timeperiods <- 0:(NROW(resid10)-1)
timeperiods <- 1:(NROW(resid10))
plot(x=timeperiods, type="b", y=resid10, pch=19, 
     xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals does not seem to be randomly distributed.
### Shows signs of positive autocorrelation.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid10, lags = 1, do.lines = FALSE, 
         diag = FALSE, 
         main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows some evidence of positive autocorrelation.

### DURBIN'S H-TEST
SALES_LAG1 <- SALES[1:length(SALES)-1]
SALES_2nd_row <- SALES[-1]
ADV_2nd_row <- ADV[-1]

model_10_1 <- lm(SALES_2nd_row ~ SALES_LAG1 + ADV_2nd_row)
summary(model_10_1)

resid10_1 <- residuals(model_10_1)

timeperiods <- 1:(NROW(resid10_1))
plot(x=timeperiods, type="b", y=resid10_1, pch=19, 
     xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### Using the DW Test -- to be used to calculate h-statistic
dwstat <- lmtest::dwtest(model_10_1)

### Detect using Runs Test -- indicates randomness
randtests::runs.test(resid10_1, alternative = "two.sided")

## install.packages("ecm")
library(ecm)

durbinH(model_10_1, "SALES_LAG1") ## Using the package

## Durbin's h using principles
dwvalue <- dwstat$statistic
rho_hat <- 1 - dwvalue/2
n <- NROW(timeperiods)
output <- summary(model_10_1)
var_b2 <- (output$coefficients[2, 2])^2
durbin_h <- rho_hat*sqrt(n/(1-(n*var_b2)))

### Durbin's h statistic = -1.35 (using the package)
### Durbin's h statistic = -1.31 (using principles)
### H0: rho = 0
### H1: rho > 0

### z_alpha
qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)

### Reject H0 if h > 1.65

### Since h = -1.35 < 1.65, we fail to reject H0.
### Conclusion: First order autocorrelation is not a problem.
### Adding a lagged variable proved effective in correcting for autocorrelation.