#########################################################
#### Solved Example 1 - Polynomial Regression Models ####
#########################################################

QuadModel <- read.csv("SolvedExample1.csv")

plot(QuadModel$Output, QuadModel$AverageCost, 
     main = "Solved Example 1", xlab = "Output", 
     ylab = "Average Cost", pch=16)
abline(lm(QuadModel$AverageCost ~ QuadModel$Output), col = "red")
lines(lowess(QuadModel$Output, QuadModel$AverageCost), col="blue") ## lowess = Locally weighted Scatterplot Smoothing
# OR
scatter.smooth(x=QuadModel$Output, y=QuadModel$AverageCost, 
               main="Average Cost ~ Output")  # scatterplot

### Scatterplot shows average cost against output. We also superimpose linear and 
### quadratic trends on the scatterplot. At lower and higher levels of output, average 
### costs are highest. It appears that the average cost in this industry would best be 
### estimated using a quadratic regression model.

###### LINEAR REGRESSION MODEL #################
###### Y = Average Cost, X = OUTPUT ############
###### AC = beta0 + beta1*Output + error #######
################################################

AverageCost <- QuadModel$AverageCost
Output <- QuadModel$Output

model <- lm(AverageCost ~ Output)
summary(model)

### The linear regression model provides a poor fit, which is not surprising given the scatterplot.
### Not only is Output statistically insignificant, the R2 is very negligible. 
### This indicates that only 0.23% of variation in the average cost is explained by the model.

###### QUADRATIC MODEL ##########################################
###### Y = Average Cost, X1 = OUTPUT, X2 = OUTPUT^2 #############
###### AC = beta0 + beta1*Output + beta2*Output^2 + error #######
#################################################################

model_q <- lm(AverageCost ~ poly(Output, 2, raw = TRUE))
summary(model_q)

### OR

model_q_alt <- lm(AverageCost ~ Output + I(Output^2))
summary(model_q_alt)

lines(Output, predict(model_q), col = "blue") ### Not important

### The quadratic model is a better fit. The Adjusted R-sq is 45%, a tremendous improvement
### over the linear regression model. Also, the coefficients Output and Output^2 are both
### statistically significant.

### Interpretation of Intercept for the Quadratic Model
### Intercept = 10.525. 
### When the output is zero, the average cost is $10.52. 
### This represents the average fixed costs this company incurs. 

### b1 = -0.3073. It does not make sense to think of b1 in the quadratic model 
### as being the effect of changing a firm's output, holding the square 
### of the firm's output constant.

### Part c ###
predict(model_q, data.frame(Output = 7))
### Quadratic Model: AverageCost_predicted = 10.52 - 0.3073*Output + 0.0210*Output^2
### For Output = 7 units, AverageCost_Predicted = $9.40.

### Part d ###
### Minimum Average Cost occurs at Output = -b1/2b2 = -(-0.3073)/(2*0.0210) = 7.32 million units.

#########################################################
#### Solved Example 2 - Polynomial Regression Models ####
#########################################################

PolyModel <- read.csv("SolvedExample2.csv")

plot(PolyModel$Output, PolyModel$TotalCost, 
     main = "Solved Example 2", xlab = "Output", ylab = "Total Cost", pch=16)
abline(lm(PolyModel$TotalCost ~ PolyModel$Output), col = "red")
lines(lowess(PolyModel$Output, PolyModel$TotalCost), col="blue") ## lowess = Locally weighted Scatterplot Smoothing
# OR
scatter.smooth(x=PolyModel$Output, y=PolyModel$TotalCost, 
               main="Total Cost ~ Output")  # scatterplot

### Scatterplot shows that the model that would fit would be a 
### higher-order model. Possibly a cubic model.

### Let us run the linear regression to see how good of a fit is 
### the linear model.

###### LINEAR REGRESSION MODEL #################
###### Y = Total Cost, X = OUTPUT ############
###### TotalCost = beta0 + beta1*Output + error #######
################################################

TotalCost <- PolyModel$TotalCost
Output <- PolyModel$Output

linearmodel <- lm(TotalCost ~ Output)
summary(linearmodel)

###### QUADRATIC MODEL #################################################
###### Y = Total Cost, X1 = OUTPUT, X2 = OUTPUT^2 ######################
###### TotalCost = beta0 + beta1*Output + beta2*Output^2 + error #######
########################################################################

model_q2 <- lm(TotalCost ~ poly(Output, 2, raw = TRUE))
summary(model_q2)

### OR

model_q2_alt <- lm(TotalCost ~ Output + I(Output^2))
summary(model_q2_alt)

lines(Output, predict(model_q2), col = "blue")

### Adjusted R-squared increased. So quadratic model seems to be a better fit, 
### although the first order term is not significant.

###### CUBIC MODEL ####################################################################
###### Y = Total Cost, X1 = OUTPUT, X2 = OUTPUT^2, X3 = OUTPUT^3 ######################
###### TotalCost = beta0 + beta1*Output + beta2*Output^2 + beta3*Output^3 + error #####
#######################################################################################

model_c <- lm(TotalCost ~ poly(Output, 3, raw = TRUE))
summary(model_c)

### OR

model_c_alt <- lm(TotalCost ~ Output + I(Output^2) + I(Output^3))
summary(model_c_alt)

lines(Output, predict(model_c), col = "darkgreen")

### The cubic model seems to be a better fit.

#########################################################
#### Solved Example 3 - Reciprocal Models ###############
#########################################################

SolvedEx3 <- read.csv("SolvedExample3.csv")

plot(SolvedEx3$HP, SolvedEx3$HWYMPG, main = "Solved Example 3", 
     xlab = "HP", ylab = "HWYMPG", pch=16)
abline(lm(SolvedEx3$HWYMPG ~ SolvedEx3$HP), col = "red", lwd = 2)
lines(lowess(SolvedEx3$HP, SolvedEx3$HWYMPG), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing
# OR
scatter.smooth(x=SolvedEx3$HP, y=SolvedEx3$HWYMPG, 
               main="HWYMPG ~ HP")  # scatterplot

### Scatterplot shows that the linear model seems to be a poor fit.

### Let us first try and fit a linear model...
### HWYMPG = beta0 + beta1*HP + error

HWYMPG <- SolvedEx3$HWYMPG
HP <- SolvedEx3$HP

linearmodel <- lm(HWYMPG ~ HP)
summary(linearmodel)

### Observe the R-squared and the model standard error. 
### Are the coefficients significant?

### Now try to fit a reciprocal model...
### HWYMPG = beta0 + beta1*(1/HP) + error

HPINV <- 1/HP

### Before fitting the reciprocal model, let us examine the scatterplot.
plot(HPINV, HWYMPG, main = "Solved Example 3", xlab = "HPINV", ylab = "HWYMPG", pch=16)
abline(lm(HWYMPG ~ HPINV), col = "red", lwd = 2)

### Now run the reciprocal model
reciprocalmodel <- lm(HWYMPG ~ HPINV)
summary(reciprocalmodel)

### Observe the R-squared and the model standard error. 
### Are the coefficients significant?
### Which model fits better?

#########################################################
#### Solved Example 4 - Reciprocal Models ###############
#########################################################

SolvedEx4 <- read.csv("SolvedExample4.csv")

HRLYEARNINGS <- SolvedEx4$HourlyEarnings
URATE <- SolvedEx4$UnempRate

plot(SolvedEx4$UnempRate, SolvedEx4$HourlyEarnings, main = "Solved Example 4", xlab = "Unemployment Rate (%)", ylab = "Change in Hourly Earnings", pch=16)
abline(lm(SolvedEx4$HourlyEarnings ~ SolvedEx4$UnempRate), col = "red", lwd = 2)
lines(lowess(SolvedEx4$UnempRate, SolvedEx4$HourlyEarnings), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### Scatterplot shows that the linear model seems to be a poor fit.

### Let us first try and fit a linear model...
### RateEarnings = beta0 + beta1*URate + error

linearmodel <- lm(HRLYEARNINGS ~ URATE)
summary(linearmodel)

URATE_INV <- 1/URATE

### Before fitting the reciprocal model, let us examine the scatterplot.
plot(URATE_INV, HRLYEARNINGS, main = "Solved Example 4", xlab = "URATE_INV", ylab = "HRLYEARNINGS", pch=16)
abline(lm(HRLYEARNINGS ~ URATE_INV), col = "red", lwd = 2)

### Now run the reciprocal model
### RateEarnings = beta0 + beta1*(1/URate) + error
reciprocalmodel <- lm(HRLYEARNINGS ~ URATE_INV)
summary(reciprocalmodel)

### Compare the R-square and the standard error for the linear and reciprocal models.
### Seems like the reciprocal model is a better fit, 
### not only mathematically but also theoretically.

### Model building involves a good dose of theory, 
### some introspection, and 
### considerable hands-on experience.

#########################################################
#### Solved Example 5 - lin-log Models ##################
#########################################################

SolvedEx5 <- read.csv("SolvedExample5.csv")

PCE_Services <- SolvedEx5$Services 
PCE_Total <- SolvedEx5$PCE 

### Create a scatterplot first

plot(PCE_Total, PCE_Services, 
     main = "Solved Example 5", xlab = "PCE Total ($ billion)", 
     ylab = "PCE Services ($ billion)", pch=16)
abline(lm(PCE_Services ~ PCE_Total), col = "red", lwd = 2)
lines(lowess(PCE_Total, PCE_Services), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### Create a lin-log model
ln_PCE_Total <- log(PCE_Total)

### The model: PCE_Services = beta0 + beta1*ln(PCE_Total) + error
lin_log_model_1 <- lm(PCE_Services ~ ln_PCE_Total)
summary(lin_log_model_1)

### Model: PCE_Services = -12564.8 + 1844.2*ln(PCE_Total)

### Interpretation (usual fashion): If the log of total personal consumption  
### increased by 1 unit, the absolute change in the 
### expenditure on personal services is approx. $1844 billion.

### Interpretation (everyday language): If the aggregate personal consumption 
### expenditure increases 
### by 1%, on average, expenditure on services
### increases by approx. $18.44 billion.

#########################################################
#### Solved Example 6 - lin-log Models ##################
#########################################################

SolvedEx6 <- read.csv("SolvedExample6.csv")

FUELCON <- SolvedEx6$FUELCON
DENSITY <- SolvedEx6$POP/SolvedEx6$AREA

### Create a scatterplot first
plot(DENSITY, FUELCON, main = "Solved Example 6", 
     xlab = "DENSITY", ylab = "FUELCON", pch=16)
abline(lm(FUELCON ~ DENSITY), col = "red", lwd = 2)
lines(lowess(DENSITY, FUELCON), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### It is clearly not a linear relationship. Look at how the values are 
### clumped together on the left hand side of the X-axis.
### The scatterplot suggests a log transformation of X-variable (DENSITY).

LN_DENSITY <- log(DENSITY)

### Create a scatterplot again
plot(LN_DENSITY, FUELCON, main = "Solved Example 6", 
     xlab = "LN_DENSITY", ylab = "FUELCON", pch=16)
abline(lm(FUELCON ~ LN_DENSITY), col = "red", lwd = 2)

### Create the model.
### FUELCON = beta0 + beta1*ln(DENSITY) + error
lin_log_model_2 <- lm(FUELCON ~ LN_DENSITY)
summary(lin_log_model_2)

### The model is: FUELCON = 597.18 - 24.531*LN(DENSITY)

#########################################################
#### Solved Example 7 - log-linear Models ##################
#########################################################

SolvedEx7 <- read.csv("SolvedExample7.csv")

Score <- SolvedEx7$Math.SAT.Score
Income <- SolvedEx7$Annual.Family.Income

### Create a scatterplot first
plot(Income, Score, main = "Solved Example 7", 
     xlab = "Income", ylab = "Score", pch=16)
abline(lm(Score ~ Income), col = "red", lwd = 2)
lines(lowess(Income, Score), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

LN_Score <- log(Score)
LN_Income <- log(Income)

### Create a scatterplot again
plot(LN_Income, LN_Score, 
     main = "Solved Example 7", xlab = "LN_Income", 
     ylab = "LN_Score", pch=16)
abline(lm(LN_Score ~ LN_Income), col = "red", lwd = 2)
lines(lowess(LN_Income, LN_Score), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### Run a regression model

### LN_Score = beta0 + beta1*ln(Income) + error
log_lin_model_1 <- lm(LN_Score ~ LN_Income)
summary(log_lin_model_1)

### The model can be written as:
### LN(SCORE) = 4.887 + 0.1258*LN(Income)

### The slope gives a measure of constant score elasticity.
### For 1% increase in annual family income, the math SAT score increased by 
### approx. 0.13 percent.
### The constant score elasticity is 0.13, which is less than 1, 
### so the math scores are inelastic.
### Math scores increase proportionately less than the increase 
### in annual family income.

#########################################################
#### Solved Example 8 - log-linear Models ##################
#########################################################

SolvedEx8 <- read.csv("SolvedExample8.csv")

GDP <- SolvedEx8$GDP
Labor <- SolvedEx8$Employment
Capital <- SolvedEx8$Fixed.Capital

### Create a scatterplot matrix
pairs(~ GDP + Labor + Capital, main = "Scatterplot for Mexico GDP")

### Transform the variables to create the Cobb-Douglas model
LN_GDP <- log(GDP)
LN_Labor <- log(Labor)
LN_Capital <- log(Capital)

### Create the regression model...
### LN(GDP) = beta1 + beta2*LN(Labor) + beta3*LN(Capital) + error
CD_Model <- lm(LN_GDP ~ LN_Labor + LN_Capital)
summary(CD_Model)

### Model:
### LN(GDP) = -1.65233 + 0.33972*LN(Employment) + 0.846*LN(Fixed Capital)

### Elasticity of output with respect to labor input = partial slope 
### coefficient value of 0.33972
### Holding capital input constant, if the labor input increases 
### by 1%, then on average, the
### output goes up by 0.34%.

### Elasticity of output with respect to capital input = partial slope 
### coefficient value of 0.846
### Holding labor input constant, if the capital input increases 
### by 1%, then on average, the
### output goes up by 0.85%.

### Add the elasticity coefficients to obtain the returns to scale.
### 0.33972 + 0.846 = 1.1857.
### Increasing returns to scale for Mexico during this time period.

### NOTE: We used one-tailed test here so I divided the p-values of the slope 
### coeff. by 2.
### Both capital and labor are expected to have a positive 
### effect on the output.

#########################################################
#### Solved Example 9 - log-linear Models ##################
#########################################################

SolvedEx9 <- read.csv("SolvedExample9.csv")

GDP <- SolvedEx9$GDP
IMPORTS <- SolvedEx9$IMPORTS

### Create a scatterplot first
plot(GDP, IMPORTS, main = "Solved Example 9", 
     xlab = "GDP", ylab = "IMPORTS", pch=16)
abline(lm(IMPORTS ~ GDP), col = "red", lwd = 2)

### Describe the scatterplot. Why is the transformation below necessary?

LN_GDP <- log(GDP)
LN_IMPORTS <- log(IMPORTS)

### Before creating the regression model, create the scatterplot again.
plot(LN_GDP, LN_IMPORTS, main = "Solved Example 9", 
     xlab = "LN_GDP", ylab = "LN_IMPORTS", pch=16)
abline(lm(LN_IMPORTS ~ LN_GDP), col = "red", lwd = 2)

### Create the regression model
### LN(IMPORTS) = beta0 + beta1*LN(GDP) + error
log_lin_model_3 <- lm(LN_IMPORTS ~ LN_GDP)
summary(log_lin_model_3)

### Model:
### LN(IMPORTS) = -1.12746 + 0.86703*LN(GDP)

#########################################################
#### Solved Example 10 - log-lin (Growth) Models ########
#########################################################

SolvedEx10 <- read.csv("SolvedExample10.csv")

Population <- SolvedEx10$U.S..Population
Time <- SolvedEx10$Time

### Create a scatterplot of Population versus Time
plot(Time, Population, main = "Solved Example 10", 
     xlab = "Time", ylab = "Population", pch=16)
abline(lm(Population ~ Time), col = "red", lwd = 2)

### Now take the log of Population
LN_Population <- log(Population)

### Create a scatterplot of Ln(Population) versus Time
plot(Time, LN_Population, main = "Solved Example 10", 
     xlab = "Time", ylab = "LN_Population", pch=16)
abline(lm(LN_Population ~ Time), col = "red", lwd = 2)

### Run the regression model now...
### LN(Population) = beta0 + beta1*Time + error
semilog_model <- lm(LN_Population ~ Time)
summary(semilog_model)

### Interpretation of Slope:
### The slope coefficient of 0.01075 means on the average the log of 
### Population has been increasing at the
### rate of 0.0107 per year.
### Population has been increasing at the rate of 1.07% per year. 
### This is the instantaneous (at a point in time) growth rate.

### To compute the compound growth rate, we take the 
### slope to equal ln(1 + r).
### So r = antilog(0.0107) - 1
(r <- exp(0.0107) - 1)
### So, the compound growth rate is 1.0757% per year.

#################################################
#### Solved Example 11 - Model Selection ########
#################################################

ModelSelections <- read.csv("SolvedExample11.csv")

#### Plot scatterplots of response variable with each independent variable.
#### Response Variable (Y) = Rent.
#### Independent Variables (X) = Bed, Bath, SqFt

### Observe whether there is a linear relationship between each 
### explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable 
### would suggest transformations.
### Plot a scatterplot matrix
plot(ModelSelections) 
### OR
pairs(ModelSelections[1:4], panel = panel.smooth)

### Model 1 - Linear
### Develop the multiple regression model.
Model1 <- lm(Rent ~ Bed+Bath+Sqft, data = ModelSelections)
summary(Model1)

### Model 2 - Logarithmic
### Develop the multiple regression model.
Model2 <- lm(Rent ~ Bed+Bath+log(Sqft), data = ModelSelections)
summary(Model2)

### Model 3 - Exponential
### Develop the multiple regression model.
Model3 <- lm(log(Rent) ~ Bed+Bath+Sqft, data = ModelSelections)
summary(Model3)

### Model 4 - Log-Log
### Develop the multiple regression model.
Model4 <- lm(log(Rent) ~ Bed+Bath+log(Sqft), data = ModelSelections)
summary(Model4)

### Which model is the best?
### Refer to Model Comparison Slides on the PPT.
### Get the fitted values for Models 2 and 4
pred2 <- predict(Model2)   ### Fitted Values for Model 2
R_y_yhat2 <- cor(ModelSelections$Rent, pred2)^2 ### Find the R-square
Adj_R_y_yhat2 <- 1 - ((1 - R_y_yhat2))*(40 - 1)/(40 - 3 - 1) ### Value is 0.8355

pred4 <- predict(Model4)   ### Fitted Values for Model 4
exp(pred4) ### Why? Because the FITS values obtained in this model is 
           ### NOT Rent_hat. It is ln(Rent_hat). So, taking the antilog.
R_y_yhat4 <- cor(ModelSelections$Rent, exp(pred4))^2 ### Find the R-square
Adj_R_y_yhat4 <- 1 - ((1 - R_y_yhat4))*(40 - 1)/(40 - 3 - 1) ### Value is 0.7350

#################################################
#### Solved Example 12 - Scaling Effects ########
#################################################

library(stringr)

ScalingEffects <- read.csv("SolvedExample12.csv")

GPDI_BL <- as.numeric(str_remove_all(ScalingEffects$GPDIBL, ",")) # GPDI (Billions of $)
GPDI_MN <- as.numeric(str_remove_all(ScalingEffects$GPDIM, ",")) # GPDI (Millions of $)
GDP_BL <- as.numeric(str_remove_all(ScalingEffects$GDPB, ",")) # GDP (Billions of $)
GDP_MN <- as.numeric(str_remove_all(ScalingEffects$GDPM, ",")) # GDP (Millions of $)

### Case 1: Both in Billions of dollars
Both_Billions <- lm(GPDI_BL ~ GDP_BL)
summary(Both_Billions)

### Case 1: Both in Millions of dollars
Both_Millions <- lm(GPDI_MN ~ GDP_MN)
summary(Both_Millions)

### Observation: The intercept as well as its std error is 1000 times the 
### corresponding values in the regression using billions (note w1 = 1000
### in going from billions to millions of dollars). The slope coefficient
### and its std error are unchanged.

### Case 2: GPDI changed by 1000 (w1) but GDP unchanged (w2 = 1)
###         GPDI in millions of dollars and GDP in billions of dollars
Y_Millions_X_Billions <- lm(GPDI_MN ~ GDP_BL)
summary(Y_Millions_X_Billions)

### Observation: Both the intercept and the slope coefficients
### as well as their respective std errors are 1000 times their values 
### compared to when both were in billions.

### Case 3: GPDI unchanged (w1 = 1) but GDP in millions (w2)
###         GPDI in billions of dollars and GDP in millions of dollars
Y_Billions_X_Millions <- lm(GPDI_BL ~ GDP_MN)
summary(Y_Billions_X_Millions)

### Observation: The slope coefficient as well as its std error is 1/1000
### its value compared to when both were in billions.

