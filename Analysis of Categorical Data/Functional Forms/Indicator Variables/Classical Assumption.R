#### Question 1 ####

Ques1 <- read.csv("Electronics.csv")

## Create dummy variables
Street_Dummy <- ifelse(Ques1$Location == "Street", 1, 0)  ##Base or Reference 
Mall_Dummy <- ifelse(Ques1$Location == "Mall", 1, 0)
Downtown_Dummy <- ifelse(Ques1$Location == "Downtown", 1, 0)

model_Ques1 <- lm(Sales ~ Households + Mall_Dummy + Downtown_Dummy, 
                              data = Ques1)
summary(model_Ques1)
sale_means <- tapply(Ques1$Sales, Ques1$Location, mean)

##Interpretation:
##𝑌_i = beta_1+ beta_2*D_2i + beta_3*D_3i + beta_4*X_ie_i
##Where Y is store sales volume in July of last year measured in $1000
##      X_i = the number of households in the store's area measured in 1000
##      D_2i = 1, if the location is in the Mall
##             0, otherwise
##      D_1i = 1, if the location is in the Downtown
##             0, otherwise
##E(sales) = 14.97769 - 28.373*D2_2i + 6.863*D_1i
## The mean sales in the in the Street Location is the base category which is about 15%
## The p -values for the differential intercept coefficients are statistically significant –suggesting there is a significant difference in the mean sales between the:
## Street, Mall and Downtown Location.
##Holding the locations constant as number of households increases by 1000, on average, the sales of households increases by 0.86859*1000 = $868.59
##Controlling for number of households the differential intercept coefficient is not significant for the Downtown Location while it is significant for the Mall Location.


#### Question 2####

Ques2 <- read.csv("Criminal Justice.csv")

Expend <- Ques2$EXPEND
Police <- Ques2$POLICE
### Create a scatterplot first
plot(Police, Expend, main = "Criminal Justice", 
     xlab = "Police", ylab = "Expend", pch=16)
abline(lm(Expend ~ Police), col = "red", lwd = 2)
lines(lowess(Police, Expend), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

LN_Expend <- log(Expend)
LN_Police <- log(Police)

### Create a scatterplot again
plot(LN_Police, LN_Expend, 
     main = "Criminal Justice", xlab = "LN_Police", 
     ylab = "LN_Expend", pch=16)
abline(lm(LN_Expend ~ LN_Police), col = "red", lwd = 2)
lines(lowess(LN_Police, LN_Expend), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### Run a regression model

### LN_Expend = beta0 + beta1*ln(Police) + error
log_lin_model_1 <- lm(LN_Expend ~ LN_Police)
summary(log_lin_model_1)

pred <- predict(log_lin_model_1, 
                data.frame(LN_Police = log(10000)), 
                interval = "prediction",
                level = 0.95)

##Point prediction of expenditures if 10000 police officers are hired.
exp(pred)
##95% Prediction Interval in expenditure if 10000 police officers are hired
##Where Lower is 16.3198 and Upper is 17.1312
exp(16.3198)
exp(17.1312)
### The model can be written as:
### LN(Expend) = 8.185+ 0.9272*LN(Police)

### The slope gives a measure of constant score elasticity.
### For 1% increase in total expenditure, the total number of police employed is increased by 
### approx. 0.92 percent.
### The constant score elasticity is 0.92, which is less than 1, 
### so the total number of police hired are inelastic.
### The total number of police hired increase proportionately less than the increase 
### in the total expenditure.

####Question 3####

Ques3 <- read.csv("Construction.csv")

plot(Ques3$Crew.Size, Ques3$Output, 
     main = "Construction", xlab = "Crew Size", ylab = "Output", pch=16)
abline(lm(Ques3$Output ~ Ques3$Crew.Size), col = "red")
scatter.smooth(x=Ques3$Crew.Size, y=Ques3$Output, 
               main="Output ~ Crew Size")  # scatterplot

### Scatterplot shows that the model that would fit would be a 
### higher-order model. Possibly a cubic model.
### The crew size that seems optimal 6 as seen from the graph it takes the maximum value at that point.

### Let us run the linear regression to see how good of a fit is 
### the linear model.

###### LINEAR REGRESSION MODEL #######################
###### Y = Crew Size, X = OUTPUT #####################
###### Crewsize = beta0 + beta1*Output + error #######
######################################################

Crewsize <- Ques3$Crew.Size
Output <- Ques3$Output

linearmodel <- lm(Output ~ Crewsize)
summary(linearmodel)
## The Multiple R-squared:  0.06879,	Adjusted R-squared:  -0.008811 

###### QUADRATIC MODEL #################################################
###### Y = Crew Size, X1 = OUTPUT, X2 = OUTPUT^2 ######################
###### Crewsize = beta0 + beta1*Output + beta2*Output^2 + error #######
########################################################################

model_q2 <- lm(Output ~ poly(Crewsize, 2, raw = TRUE))
summary(model_q2)

lines(Output, predict(model_q2), col = "blue")

### The Multiple R-squared:  0.6591,	Adjusted R-squared:  0.6307 
### Adjusted R-squared increased. So quadratic model seems to be a better fit, 
### although the first order term is significant.

pred <- predict(model_q2, 
                data.frame(Crewsize = 5)) 
                
### The number of jobs that a crew size of 5 can complete in a week is predicted using the quadratic model that best fits is 16(approx).

###### CUBIC MODEL ####################################################################
###### Y = Crew Size, X1 = OUTPUT, X2 = OUTPUT^2, X3 = OUTPUT^3 ######################
###### Crewsize = beta0 + beta1*Output + beta2*Output^2 + beta3*Output^3 + error #####
#######################################################################################

model_c <- lm(Output ~ poly(Crewsize, 3, raw = TRUE))
summary(model_c)

lines(Output, predict(model_c), col = "darkgreen")

###The Multiple R-squared:  0.6612,	Adjusted R-squared:  0.617 
###As the adjusted R-squared decreased and the only one of the terms are significant 
###Therefore the the cubic model does not seem to be a good fit.
###The quadratic model is hence the best fit.


####Question 4####
Ques4 <- read.csv("HappinessStudy.csv")

#### Plot scatterplots of response variable with each independent variable.
#### Response Variable (Y) = Happiness.
#### Independent Variables (X) = Age, Family Income 

### Observe whether there is a linear relationship between each 
### explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable 
### would suggest transformations.
### Plot a scatterplot matrix
plot(Ques4) 

### Model 1 - Linear
### Develop the multiple regression model.
Model1 <- lm(Happiness ~ Age+Family.Income, data = Ques4)
summary(Model1)
###The Multiple R-squared:  0.4967,	Adjusted R-squared:  0.4863 
###Where both age and income are statistically significant

### Model 2 - Logarithmic
### Develop the multiple regression model.
Model2 <- lm(Happiness ~ Age+log(Family.Income), data = Ques4)
summary(Model2)
###The Multiple R-squared:  0.5288,	Adjusted R-squared:  0.5191 
###Where both age and income are statistically significant.

### Model 3 - Exponential
### Develop the multiple regression model.
Model3 <- lm(log(Happiness) ~ Age+Family.Income, data =Ques4)
summary(Model3)
###The Multiple R-squared:  0.4803,	Adjusted R-squared:  0.4696 
###Where both age and income are statistically significant.

### Model 4 - Log-Log
### Develop the multiple regression model.
Model4 <- lm(log(Happiness) ~ Age+log(Family.Income), data = Ques4)
summary(Model4)
### The Multiple R-squared:  0.5222,	Adjusted R-squared:  0.5124 
### Where both age and income are statistically significant.

### Which model is the best?
### Get the fitted values for Models 2 and 4
pred2 <- predict(Model2)   ### Fitted Values for Model 2
R_y_yhat2 <- cor(Ques4$Happiness, pred2)^2 ### Find the R-square
Adj_R_y_yhat2 <- 1 - ((1 - R_y_yhat2))*(100 - 1)/(100 - 2 - 1) ### Value is 0.5190

pred4 <- predict(Model4)   ### Fitted Values for Model 4
exp(pred4) ### Why? Because the FITS values obtained in this model is 
### NOT Happiness_hat. It is ln(Happiness_hat). So, taking the antilog.
R_y_yhat4 <- cor(Ques4$Happiness, exp(pred4))^2 ### Find the R-square
Adj_R_y_yhat4 <- 1 - ((1 - R_y_yhat4))*(100 - 1)/(100 - 2 - 1) ### Value is 0.523

### As can be seen from the Adjusted R^2 Value which is 0.523 that is the highest among the all the models.
### Hence the Model 4 - Log-Log model is the best fit among all the models.

### Predicting Happiness with varying levels of age(30, 50 and 70) with a family income of $80,000
pred_30 <- predict(Model4, 
                data.frame(Age = 30, Family.Income = 80000))
### The happiness associated with Age of 30 years and family income of $80,000 is 4.23

pred_50 <- predict(Model4, 
                   data.frame(Age = 50, Family.Income = 80000))

### The happiness associated with Age of 50 years and family income of $80,000 is 4.29

pred_70 <- predict(Model4, 
                   data.frame(Age = 70, Family.Income = 80000))

### The happiness associated with Age of 50 years and family income of $80,000 is 4.34

### Predicting Happiness with varying levels of family incomes($50,000, $75000 and $100,000) with an age of 60 year old working adult.

pred_50k <- predict(Model4, 
                   data.frame(Age = 60, Family.Income = 50000))
### The happiness associated with Age of 60 years and family income of $50,000 is 4.23

pred_75k <- predict(Model4, 
                    data.frame(Age = 60, Family.Income = 75000))

### The happiness associated with Age of 60 years and family income of $75,000 is 4.3

pred_100k <- predict(Model4, 
                    data.frame(Age = 60, Family.Income = 100000))

### The happiness associated with Age of 60 years and family income of $75,000 is 4.35

####Question 5####

### Interpretation of Slope:
### When Beta_2(Slope) is 0.0302
### The slope coefficient of 0.0302 means on the average the log of 
### Real GNP has been increasing at the
### rate of 3.02% per year.
### Real GNP has been increasing at the rate of 3.02% per year. 
### This is the instantaneous (at a point in time) growth rate.

### To compute the compound growth rate, we take the 
### slope to equal ln(1 + r).
### So r = antilog(0.0302) - 1
(r <- exp(0.0302) - 1)
### So, the compound growth rate is 3.066% per year.

### When Beta_2(Slope) is 0.053
### The slope coefficient of 0.0302 means on the average the log of 
### Labour Force Participation Rate has been increasing at the
### rate of 5.30% per year.
### Labour Force Participation Rate has been increasing at the rate of 5.30% per year. 
### This is the instantaneous (at a point in time) growth rate.

### To compute the compound growth rate, we take the 
### slope to equal ln(1 + r).
### So r = antilog(0.053) - 1
(r <- exp(0.053) - 1)
### So, the compound growth rate is 5.44% per year.

### When Beta_2(Slope) is 0.0456
### The slope coefficient of 0.0456 means on the average the log of 
### S&P 500 Index has been increasing at the
### rate of 4.56% per year.
### S&P 500 Index has been increasing at the rate of 4.56% per year. 
### This is the instantaneous (at a point in time) growth rate.

### To compute the compound growth rate, we take the 
### slope to equal ln(1 + r).
### So r = antilog(0.0456) - 1
(r <- exp(0.0456) - 1)
### So, the compound growth rate is 4.66% per year.

### When Beta_2(Slope) is 0.0114 
### The slope coefficient of 0.0114 means on the average the log of 
### S&P 500 Index Quaterly has been increasing at the
### rate of 1.14% per year.
### S&P 500 Index has been increasing at the rate of 1.14% per year. 
### This is the instantaneous (at a point in time) growth rate.

### To compute the compound growth rate, we take the 
### slope to equal ln(1 + r).
### So r = antilog(0.0114) - 1
(r <- exp(0.0114) - 1)
### So, the compound growth rate is 1.15% per year.
### Model 3 is estimated using annual data while model 4 is estimated using quaterly data
### Model 4 represents instantaneous growth rate per quarter by 4 (4 quarters in a year)
### So the quaterly growth rate will be 0.0114*4 = 0.0456

####Question 6####

###### LINEAR REGRESSION MODEL #################
###### Y = Sales, X = Time ############
###### Sales = beta0 + beta1*Time + error #######
################################################

Ques6 <- read.csv("WinterFun.csv")

Sales <- Ques6$SALES
Time <- Ques6$TIME

model <- lm(Sales ~ Time)
summary(model)

### Sales = 199.017 + 2.556*TIME + error ####

plot(Ques6$TIME,Ques6$SALES, 
     main = "Seasonality Plot",
     xlab = "Time (in years)",
     ylab = "Sales (in 1000s dollars)", type = 'o')

### As can be seen from the plot the sales increases as the number of years increases. 
### Thus seasonality is present in the Sales vs Time plot.

Q1 <- ifelse(Ques6$QUARTER == 1, 1, 0)   
Q2 <- ifelse(Ques6$QUARTER == 2, 1, 0)
Q3 <- ifelse(Ques6$QUARTER == 3, 1, 0)
Q4 <- ifelse(Ques6$QUARTER == 4, 1, 0)

model_winterfun <- lm(Ques6$SALES ~ Time + Q2 + Q3 + Q4, 
                        data = Ques6)

summary(model_winterfun)
anova(model, model_winterfun)
### H0: The reduced and full are not different. Hence choose the reduced model.
### H1: The reduced and full model are different. Hence choose the full model.
### As can be seen from the p-value which is highly significant. 
### Thus the null hypothesis is rejected and the full model is chosen.

### The partial coefficient of determination = (SSE_R - SSE_F)/(SSE_R)
### SSE of Reduced Model is 15.91 and SSE of full model is 7.19
coff <- (15.91-7.19)/15.91
### Partial coefficient of determination helps us in understanding the additional variance of 0.548 in the data when seasonality is accounted for.

#### Question 7####

Ques7 <- read.csv("EmploymentDiscrimination.csv")
Gender_Male <- ifelse(Ques7$GENDER == "MALE", 1, 0)
Gender_Female <- ifelse(Ques7$GENDER == "FEMALE", 1, 0)

###### LINEAR REGRESSION MODEL #############################################
###### Y = Salary, X1= Educ , X2 = Gender_FEMALE##############################
###### Salary = beta0 + beta1*Educ+ beta_2*Gender_FEMALE + error ##############
############################################################################

Salary <- Ques7$SALARY
Edu <- Ques7$EDUCAT

model <- lm(Salary ~ Edu+ Gender_Female)
summary(model)

### Salary = 4864.93+ 80.70*Educ -691.81*Gender_Female + error
### Differential Intercept Coefficient is -691.81 which means that after controlling for education females earn $691.81  
### Less than the male counterparts when they have the similar level of education.
### Parameter estimate for education is 80.70 this means that for every unit increase in the level of education 
### The salary increases by $80.70.

### Interaction Plot

Males <- subset(Ques7, Ques7$GENDER == "MALE")
Females <- subset(Ques7, Ques7$GENDER == "FEMALE")

plot(Ques7$EDUCAT, Ques7$SALARY, main = "Interaction Plot", xlab = "Education (in years)", ylab = "Salary (in 000s dollars)", col = ifelse(Ques7$GENDER == "MALE", "blue", "red"))
legend("topleft",  pch = c(1, 1),  c("Female", "Male"), col = c("red", "blue"))
abline(lm(Females$SALARY ~ Females$EDUCAT), col = "red")
abline(lm(Males$SALARY ~ Males$EDUCAT), col = "blue")

### As stated in the above interpretation it can be seen from the graph that the line for males in blue is placed higher than the 
### Line for females in red.
### Thus proving that gender discrimination exists.


### New Model
model_new <- lm(Salary ~ Edu + Gender_Female + Edu*Gender_Female)
summary(model_new)

### After accounting for interaction there is no gender discrimination that exists.
### It can be seen that there is difference in the significance of the gender female
### The Gender Female is not significant after accounting for the interaction which is counter-intutive 
### To the significance of gender female in the non-interactive model.
### The model with interaction has Multiple R-squared:  0.3728,	Adjusted R-squared:  0.3516 
### The model without interaction has Multiple R-squared:  0.3634,	Adjusted R-squared:  0.3492 
### The Standard error for model without interaction is 387.88 and the Standard error for model with interaction is 750.87
### The t-test individual vales for the model without interaction is higher when compared to the model with interaction.


#### Question 8####
### Use interaction
Ques8 <- read.csv("Downloads.csv")
MS_Dummy <- ifelse(Ques8$Vendor == 'MS', 1, 0)

modelQues8_Interaction <- lm(Ques8$Transfer.Time..secs. ~ Ques8$File.Size..MB. + MS_Dummy + 
                                   Ques8$File.Size..MB.*MS_Dummy, data = Ques8)

summary(modelQues8_Interaction)

### Create an interaction plot
Vendor_MS <- subset(Ques8, Ques8$Vendor == "MS")
Vendor_NP <- subset(Ques8, Ques8$Vendor == "NP")

plot(Ques8$File.Size..MB.,Ques8$Transfer.Time..secs.,
     main = "Interaction Plot",
     xlab = "Transfer Time (in secs)",
     ylab = "File Size (in MB's)",
     col = ifelse(Ques8$Vendor == "MS", "blue", "red"))
legend("topleft", 
       pch = c(1, 1), 
       c("NP", "MS"), 
       col = c("red", "blue"))
abline(lm(Vendor_MS$File.Size..MB. ~ Vendor_MS$Transfer.Time..secs.), col = "red")
abline(lm(Vendor_NP$File.Size..MB. ~ Vendor_NP$Transfer.Time..secs.), col = "blue")
#### y = alpha_1 + alpha_2*D1i + beta_1*X + beta_2*(Dxi) + e
#### where y is Transfer Time in secs
####       D1i = 0 when MS = 0
####       D1i = 1 when MS = 1
#### Choice of vendor has no affect on the overall model. 
#### As can be seen from the summary that when D1i =1, Y = (4.89 + 4.76) + (0.40-0.18)X
#### When D1i = 0, Y = 4.89 + 4.76X
#### Thus when the file size is 1 MB then it takes 0.40 secs for NP vendor files to transfer
#### and when file size is 1 MB then it takes (0.40 -0.18 = 0.22 secs) for MS vendor files to transfer.
#### Hence the MS vendor is recommended as it has lesser transfer time.


#### Question 9####

Ques9 <- read.csv("Repair.csv")

plot(Ques9$TIME, Ques9$EXPER, 
     main = "Repair", xlab = "Time", 
     ylab = "Experience", pch=16)
abline(lm(Ques9$EXPER ~ Ques9$TIME), col = "red")
lines(lowess(Ques9$TIME, Ques9$EXPER), col="blue")
plot(Ques9$TIME, Ques9$NUMBER, 
     main = "Repair", xlab = "Time", 
     ylab = "NUMBER", pch=16)
abline(lm(Ques9$NUMBER ~ Ques9$TIME), col = "red")
lines(lowess(Ques9$TIME, Ques9$NUMBER), col="blue")
## lowess = Locally weighted Scatterplot Smoothing


### Scatterplot shows experience against time and number against time. We also superimpose linear and 
### quadratic trends on the scatterplot. At lower and higher levels of time, years of experience of service persons
### are highest. It appears that the number of machines to repair would best be 
### estimated using a quadratic regression model.

###### QUADRATIC MODEL ##############################################################
###### Y = Time, X1 = Exper X2 = (NUMBER), X3 = (NUMBER)^2               #############
###### AC = beta_0 + beta_1*(Exper,) + beta_2*(NUMBER) + beta_3*(NUMBER)^2 + error #######
#########################################################################################

###poly_model <- lm(time ~ experience + poly(num_appl, 2, raw = TRUE))
##summary(poly_model)
model_q <- lm(Ques9$TIME ~ Ques9$EXPER + poly(Ques9$NUMBER, 2, raw = TRUE))
summary(model_q)

#### Time = 65.7294 + 0.37118*Exper + 3.88741*Number + 0.94317*Number^2


#### Question 10####

Ques10 <- read.csv("Camrys.csv")

Price <- Ques10$Asking.Price...000.
Age <- Ques10$Age..years.

model <- lm(Price ~ Age)
summary(model)
### The Multiple R-squared:  0.8579,	Adjusted R-squared:  0.853 
### Price = 15.726 -  1.04167*Age+ error ####

plot(Age,Price, 
     main = "Plot",
     xlab = "Age (in years)",
     ylab = "Asking rice (in 1000s dollars)")
abline(lm(Age ~ Price), col = "red", lwd = 2)
lines(lowess(Price, Age), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### It is clearly not a linear relationship. Look at how the values are 
### clumped together on some areas of the X-axis.
### The scatter plot suggests a log transformation of X-variable (Price).

LN_Price <- log(Price)

### Create a scatterplot again
plot(Age, LN_Price, main = "Price of the cars with Age", 
     xlab = "Age", ylab = "Price", pch=16)
abline(lm(LN_Price ~ Age), col = "red", lwd = 2)

### Create the model.
### FPrice = beta0 + beta1*ln(Age) + error
lin_log_model_2 <- lm(LN_Price ~ Age)
summary(lin_log_model_2)

pred_linear <- predict(model)   
R_y_yhat2 <- cor(Price, pred_linear)^2 
Adj_R2_linear <- 1 - ((1 - R_y_yhat2))*(31 - 1)/(31 - 1 - 1)
Adj_R2_linear

pred_loglin <- predict(lin_log_model_2)   
R_y_yhat4 <- cor(Price, exp(pred_loglin))^2 
Adj_R2_loglin <- 1 - ((1 - R_y_yhat4))*(31 - 1)/(31 - 1 - 1) 
Adj_R2_loglin


### The model is: Price = 22.28 - 7.727*LN(Age)
### The Multiple R-squared:  0.8896,	Adjusted R-squared:  0.8858 
### Linear Model
### Coefficient of Intercept: 15.72611. This indicates the value of the car when the age is 0.  
### Coefficient of Age: -1.04167. This indicates that increasing the value of age by 1 year reduces the value of the car by $1,041.67
### LogLin Model
### Coefficient of Intercept: 3.35975. This e^3.359 indicates the value of the car when age is 0.  
### Coefficient of Age: -0.20473. This indicates that increasing the value of age by 1 unit reduces the value of the car by 20.47%
### As can be seen the from the values of R^2 and Adjusted R^2 is greater for the lin-log model as compared to the linear model
### The lin-log model scatterplat also shows that the curve which was present in the linear model scartterplot 
### Is no longer presetn after the transformation of the Price variable to Log(Price)
### Thus the lin-log model is good fit for the understanding the resale value of the cars over the years.
### 


### Predcition of asking price of :
### 1 year old car

pred_1 <- predict(lin_log_model_2, 
                data.frame(Age = 1)) 

### 2 year old car
pred_2 <- predict(lin_log_model_2, 
                data.frame(Age = 2)) 

### 11 year old car
pred_3<- predict(lin_log_model_2, 
                data.frame(Age = 11)) 

### 12 year old car
pred_4 <- predict(lin_log_model_2, 
                data.frame(Age = 12)) 
sprintf("The difference in the asking price for cars having age 1 and 2 is %f", (exp(pred_1) - exp(pred_2))*1000)
sprintf("The difference in the asking price for cars having age 11 and 12 is %f", (exp(pred_3) - exp(pred_4))*1000)


####Question 11####
### Linear Model :
### y = beta_0 + beta_1*X

Ques11 <- read.csv("Fisher Index.csv")

Y <- Ques11$Y
X <- Ques11$X

model <- lm(Y~ X)
summary(model)
### Year = 1.279 + 1.0691*X
### The Multiple R-squared:  0.7155,	Adjusted R-squared:   0.68 

plot(X,Y, 
     main = "Plot",
     xlab = "X ",
     ylab = "Y")
abline(lm(X ~ Y), col = "red", lwd = 2)
lines(lowess(Y, X), col="blue", lwd = 2) ## lowess = Locally weighted Scatterplot Smoothing

### It is clearly not a linear relationship. Look at how the values are 
### clumped together on some areas of the X-axis.

#### Model Without Intercept
### y = beta_1*X

without_intercept <- lm( Y ~ X -1)
summary(without_intercept)

### The Multiple R-squared:  0.7825,	Adjusted R-squared:  0.7583 
### Years = 1.0899*X
Ques11$x_square <- Ques11$X^2
Ques11$y_square <- Ques11$Y^2

raw_r_square <- sum(Ques11$X*Ques11$Y)^2/(sum(Ques11$x_square)*sum(Ques11$y_square))
raw_r_square

#### As can be seen from the model summary that the R^2 for the without intercept  model is higher that the model with intercept.
#### Hence this means that the model without intercept is better at explaining the variance in the varibales than the model with intercept.
#### Therefore the model without intercept should be used.

#### Question 13####

Ques13 <- read.csv("CorporateFinancials.csv")

Dividend <- Ques13$Dividend
Profit <- Ques13$After_Tax_Profit

plot(Ques13$Dividend, Ques13$Quarter, 
     main = "Profit vs Dividend", xlab = "Profit", 
     ylab = "Dividend", pch=16)
abline(lm(Ques13$Dividend ~ Ques13$After_Tax_Profit), col = "red")
lines(lowess(Ques13$After_Tax_Profit, Ques13$Dividend), col="blue") ## lowess = Locally weighted Scatterplot Smoothing
# OR
scatter.smooth(x=Ques13$Dividend, y=Ques13$After_Tax_Profit, 
               main="Dividend ~ Price") 

### ###### LINEAR REGRESSION MODEL #################
###### Y = Dividend, X = Profits ############
###### Dividend = beta0 + beta1*Profits + error #######
################################################


model <- lm(Dividend ~ Profit)
summary(model)


### The Multiple R-squared:  0.9035,	Adjusted R-squared:  0.9013 
### Quarters are taken as the dummy variable 

Q1 <- ifelse(Ques13$Quarter == 1, 1, 0)   
Q2 <- ifelse(Ques13$Quarter == 2, 1, 0)
Q3 <- ifelse(Ques13$Quarter == 3, 1, 0)
Q4 <- ifelse(Ques13$Quarter == 4, 1, 0)

model_dividend <- lm(Ques13$Dividend ~ Ques13$After_Tax_Profit + Q2 + Q3 + Q4 + Ques13$After_Tax_Profit*Q2 + Ques13$After_Tax_Profit*Q3 
                       + Ques13$After_Tax_Profit*Q4,
                      data = Ques13)

summary(model_dividend)
### As none of the interaction terms nor the quarter terms indicator variables are significant 
### Thus we can conclude that there is no effect of seasonality.


####Question 12####

Ques12 <- read.csv("UKConsData.csv")

#### Plot scatterplots of response variable with each independent variable.
#### Response Variable (Y) = Expenditure.
#### Independent Variables (X) = Advertising Expenditure.

### Observe whether there is a linear relationship between each 
### explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable 
### would suggest transformations.
### Plot a scatterplot matrix
plot(Ques12) 

### Model 1 - Linear
### Develop the multiple regression model.
Model1 <- lm(Exp ~ Adexp, data = Ques12)
summary(Model1)
###The Multiple R-squared:  0.5938,	Adjusted R-squared:  0.5788 
###Where adexp is are statistically significant

### Model 2 - Logarithmic
### Develop the multiple regression model.
Model2 <- lm(Exp ~ log(Adexp), data = Ques12)
summary(Model2)
###The Multiple R-squared:  0.314,	Adjusted R-squared:  0.2886 
###Where adexp is are statistically significant.

### Model 3 - Exponential
### Develop the multiple regression model.
Model3 <- lm(log(Exp) ~ Adexp, data =Ques12)
summary(Model3)
###The Multiple R-squared:  0.251,	Adjusted R-squared:  0.2232 
###Where adexp is statistically significant.

### Model 4 - Log-Log
### Develop the multiple regression model.
Model4 <- lm(log(Exp) ~ log(Adexp), data = Ques12)
summary(Model4)
### The Multiple R-squared:  0.5222,	Adjusted R-squared:  0.5124 
### Where adexp is are statistically significant.

### Which model is the best?
### Get the fitted values for Models 2 and 4
pred2 <- predict(Model1)   ### Fitted Values for Model 2
R_y_yhat2 <- cor(Ques12$Exp, pred2)^2 ### Find the R-square
Adj_R_y_yhat2 <- 1 - ((1 - R_y_yhat2))*(29 - 1)/(29 - 1 - 1) ### Value is 0.5787

pred4 <- predict(Model4)   ### Fitted Values for Model 4
exp(pred4) ### Why? Because the FITS values obtained in this model is 
### NOT Exp_hat. It is ln(Happiness_hat). So, taking the antilog.
R_y_yhat4 <- cor(Ques12$Exp, exp(pred4))^2 ### Find the R-square
Adj_R_y_yhat4 <- 1 - ((1 - R_y_yhat4))*(29 - 1)/(29 - 1 - 1) ### Value is 0.5593

### As can be seen from the Adjusted R^2 Value which is 0.5787 that is the highest among the all the models.
### Hence the Model 2 - Logarithmic model is the best fit among all the models.




