#########################################################################################
#### Solved Example 1 - Multicollinearity - Illustration of Perfect Collinearity ########
#########################################################################################

SolvedEx12 <- read.csv("SolvedExample12.csv")

### Let us create two regression models.
### 1. Qty = A1 + A2*Price + A3*Income + error
### 2. Qty = B1 + B2*Price + B3*Earnings + error

Qty <- SolvedEx12$quantity..Y
Price <- SolvedEx12$price.....X2
Income <- SolvedEx12$income.per.week.....X3
Earnings <- SolvedEx12$earnings.per.week.....X4

model_1 <- lm(Qty ~ Price + Income)
summary(model_1)

### Examine the correlation between Price and Income
cor(Price, Income)

### Message: In case of perfect linear relationship or perfect multicollinearity among 
### explanatory variables, we cannot obtain unique estimates of all parameters.
### Since we cannot obtain their unique estimates, we cannot draw statistical 
### inferences about them from a given sample.

### Regress Income on Price
model_intermediate <- lm(Income ~ Price)
summary(model_intermediate)

### Income variable and Price are PERFECTLY linearly related.

### Let us run the regression between Qty and Price only...
### Qty = B1 + B2*Price + error
model_2 <- lm(Qty ~ Price)
summary(model_2)

### Model is Qty = 49.667 - 2.1576*Price

### Next, run a regression between Qty, Price, and Earnings
### Qty = B1 + B2*Price + B3*Earnings + error

model_3 <- lm(Qty ~ Price + Earnings)
summary(model_3)

### Model is Qty = 145.3650 - 2.7975*Price - 0.3191*Earnings

### Regress Earnings on Price
model_intermediate_1 <- lm(Earnings ~ Price)
summary(model_intermediate_1)

### Income variable and Price are PERFECTLY linearly related.

### Observations from the two regression models:
### 1. Price coefficients have the same sign (negative) as expected. 
###    Numerical diff. between the two are not too vast.
### 2. The |t| for Price in Simple regression is very high and the std error 
###    is low. 
### 3. The |t| for Price in multiple regression is lower and the 
###    std error is high.
### 4. R-sq in Simple Regression is 0.9757 while in multiple regression 
###    is 0.9778 - an increase of only 0.0021.
### 5. Earnings is statistically insignificant but more importantly, 
###    the sign is wrong (normal good).

### The reason for these strange results is that there is near perfect 
### linear relationship between Price and Earnings.

### Consequences of multicollinearity
### 1. Large variances and standard errors - 
###    compare std error of Price in Simple regression versus 
###    Price in Multiple regression.
### 2. Insignificant t-ratios.
### 3. High R-squared but few t-values significant: R-squared for multiple 
###    regression is 0.9778 but only one X is significant.
### 4. OLS estimators and std errors are very sensitive to 
###    small changes in data - unstable.
### 5. Wrong signs of regression coefficients - Earnings has wrong sign.
### 6. Difficulty in assessing individual contributions. See example below...

### Model for Point 6 above
### Let us run a simple regression of Quantity on Earnings to see how 
### much variation Earnings alone explains.
model_4 <- lm(Qty ~ Earnings)
summary(model_4)

### 94% of variation in Qty is explained by Earnings alone. So, the earnings 
### coefficient is behaving according to the theoretical expectations.

#################################################################
#### SOLVED EXAMPLE - REMEDYING MULTICOLLINEARITY - SONY ########
#################################################################

Sony <- read.csv("Sony.csv")

#### SIMPLE LINEAR REGRESSION ####
#### Y = Sony % Change ####
#### X = Market % Change ####

#### Step 1 - Correlation Coefficient and Scatterplot ####

plot(Sony$Sony_Percent_Change, Sony$Market_Percent_Change, 
     main = "Sony versus Market", 
     xlab = "Market Change", ylab = "Sony Change")
abline(lm(Sony$Sony_Percent_Change ~ Sony$Market_Percent_Change))

cor(Sony$Sony_Percent_Change, Sony$Market_Percent_Change)

### If you want to find whether there is a correlation between Sony and Market,
### use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(Sony$Sony_Percent_Change, Sony$Market_Percent_Change, 
         alternative = "two.sided", method = "pearson")

#### Step 2 - Regression Model ####

Sony_SLR <- Sony$Sony_Percent_Change
Market_SLR <- Sony$Market_Percent_Change

lm.SLR <- lm(Sony_SLR ~ Market_SLR)
summary(lm.SLR)

#### Step 3 - Regression Diagnostics ####
#### Four conditions to be satisfied - linearity, normality, homoscedasticity, independence.
#### Assess these by viewing residual plots.

# Normality Assessment
# Look at the p-value from Anderson-Darling normality test
# H0: Residuals are normally distributed
# H1: Residuals are not normally distributed

nortest::ad.test(residuals(lm.SLR)) ## Package nortest
hist(residuals(lm.SLR)) ## Plot a histogram

### Combine multiple plots into one overall graph - create a multipanel plot
### Can use par() or layout() functions.
### Here, I have used the par() function.
### mfrow =c(nrows, ncols) creates a matrix of nrows x ncols

par(mfrow=c(2,2)) # Change panel layout to 2 x 2
plot(lm.SLR, which=1:4)
par(mfrow=c(1,1)) # Change it back to 1 x 1

### You can check all assumptions at once using the GVLMA package.
gvlma::gvlma(lm.SLR)

# Independence Assessment
## Durbin-Watson test will help us assess the condition of independence.
## If the data are not a time-series, we don't have to worry too much about this condition.

lmtest::dwtest(lm.SLR) ## in package lmtest

#### Plot a line chart to assess whether the residuals occur randomly.
#### Looking at the chart, you will see that the residuals occur randomly. 
plot(residuals(lm.SLR), type = "o", xlab = "Time", 
     ylab = "Residuals", main = "Residuals versus Time")

#### INTERPRETATIONS FROM SIMPLE LINEAR REGRESSION MODEL
#### Intercept b0 = -0.532 (p-value = 0.403 > alpha)
#### Intercept is not significantly different from zero.
#### Estimate of beta1 is 1.278 and is highly significant. (p-value approx. 0 < alpha)
#### The whole market index alone explains R-sq = 32% of the variation in percentage changes
#### in the Sony stock.
#### Perhaps, we can explain more variation with another stock index.

### MULTIPLE REGRESSION
### Step 2: Observe whether there is a linear relationship between each explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable would suggest transformations.
### Plot a scatterplot matrix
Sony_MR <- Sony$Sony_Percent_Change
Market_MR <- Sony$Market_Percent_Change
Dow_MR <- Sony$Dow_Percent_Change
SMB <- Sony$Small_Minus_Big
HML <- Sony$High_Minus_Low

lm.MR <- lm(Sony_MR ~ Market_MR + Dow_MR + SMB + HML)
summary(lm.MR)

plot(Sony[ ,2:6]) 
### OR
pairs(Sony[, 2:6][1:4], panel = panel.smooth)

### Assess regression assumptions
### You can check all assumptions at once using the GVLMA package.
gvlma::gvlma(lm.MR)

hist(residuals(lm.MR)) ### Histogram shows normality condition is reasonably satisfied. A couple of coutliers.

### Regression assumptions appear to be met.

#### F-Test for overall validity of the model
#### p-value < alpha, so we reject H0: beta1 = beta2 = beta3 = beta4 = 0
#### This model explains statistically significant variation in the percentage changes in the 
#### value of Sony stock.
#### This shows that the collection of explanatory variables explains statistically
#### significant variation in the response.

#### Comment on the Slope of the Market_Percent_Change
#### Simple Linear Regression -> Slope b1 = 1.278, p-value approx. 0
#### Multiple Regression -> Slope b1 = 0.971, p-value = 0.0155
#### The once important explanatory variable barely contributes statistically significant variation
#### to the multiple regression.

#### Reason: Multicollinearity
### Correlation matrix
### Using correlation matrix, we can assess the strength and direction of correlation.
### The package Hmisc includes rcorr() function that displays r and the p-values.
cor(Sony[, 2:6])
### OR
### Use the rcorr function in the Hmisc package. This displays the r and p-values
Hmisc::rcorr(as.matrix(Sony[, 2:6]))

#### High correlation between DJIA (Dow_Percent_Change) and Market_Percent_Change.
#### r = 0.907
#### R-sq = 82% meaning 82% of the variation in these two indices are shared.

car::vif(lm.MR) ### Used the vif function from the car package.

### Let us consider HML
### HML not statistically significant. One of the two reasons:
### 1. Redundant? As in case for Dow_Percent_Change
### 2. Simply unrelated to Sony_Percent_Change
### VIF(HML) = 1.276.
### HML has little effect on this variable. So HML has less influence on explaining Sony_Percent_Change
### once we have taken account of other explanatory variables.

### Multicollineary Remedy: one of the remedies = Re-express variables.
### (Market_Percent_Change + Dow_Percent_Change)/2 -> Average
### (Market_Percent_Change - Dow_Percent_Change) -> Difference
Sony$AverageMarket <- (Sony$Market_Percent_Change + Sony$Dow_Percent_Change)/2
Sony$DiffMarket <- (Sony$Market_Percent_Change - Sony$Dow_Percent_Change)

### Run the multiple Regression
Sony_MR2 <- Sony$Sony_Percent_Change
Market_Avg <- Sony$AverageMarket
Market_Diff <- Sony$DiffMarket
SMB <- Sony$Small_Minus_Big
HML <- Sony$High_Minus_Low

lm.MR2 <- lm(Sony_MR2 ~ Market_Avg + Market_Diff + SMB + HML)
summary(lm.MR2)

car::vif(lm.MR2)

#### Any multicollinearity between Market_Avg and Market_Diff
cor(Market_Avg, Market_Diff)

#### OBSERVATIONS AND INTERPRETATIONS
#### The advantage of the formulation is that the average and difference of the two stock indices
#### are almost uncorrelated r = 0.152 so there is little collinearity to cloud our interpretation 
#### of this regression. 

#### The average of the indices is clearly an important explanatory variable; the difference is not.

#### Removing Explanatory Variables
#### HML is not statistically significant. Since this variable is not statistically significant, 
#### we could remove it from the model without causing a significant drop in R-sq.
#### Nonetheless, we often keep explanatory variables of interest in the regression whether 
#### significant or not, so long as they do not introduce substantial collinearity.
#### On the other hand, we cannot leave both Market % Change and Dow % Change together in 
#### the regression. Which should we remove?  In this case, both statistics 
#### and substance point in the same direction: remove Dow_Percent_Change.
#### Dow_Percent_Change adds less to the regression model (p-value = 0.6815) than Market_Percent_Change 
#### (p-value = 0.0155).  Substantively, finance theory says to keep the whole market index as well; 
#### it represents a broader picture of returns on all stock as called for by the CAPM. 