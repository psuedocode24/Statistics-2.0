#####################################################
### Solved Example 1 -- Consequences of underfitting
#####################################################

SolvedExample1 <- read.csv("SolvedExample1.csv")

CM <- SolvedExample1$ChildMortality
PGNP <- SolvedExample1$PerCapita_GNP
FLR <- SolvedExample1$FemaleLiteracyRate

### Run the TRUE model CM = Beta1 + Beta2*PGNP + Beta3*FLR
model_sol_ex_1 <- lm(CM ~ PGNP + FLR)
summary(model_sol_ex_1)
### Model: CM_hat = 263.64 - 0.005647*PGNP - 2.2315*FLR

### Now omit the variable FLR and run the MISSPECIFIED model
model_sol_ex_1a <- lm(CM ~ PGNP)
summary(model_sol_ex_1a)
### Model: CM_hat = 157.42 - 0.0113*PGNP

### Observations
### 1. The misspecified model shows that as per capita GNP (PGNP)
### increases by a dollar, on average the child mortality reduces by about 0.01.
### On the other hand, in the true model, if PGNP increases by a dollar, 
### the average child mortality rate decreases by only about 0.006.

### Disregarding the sign, the misspecified model overestimates the 
### impact that PGNP has on the child mortality rate, i.e. it has upward bias.

### By dropping the variable FLR, we are not only ignoring the impact of FLR
### on CM, but also the impact of FLR on PGNP.

### PGNP in the misspecified model thus has to carry the burden of this omission.

### 2. Intercept is also biased. The misspecified model underestimates
### the true intercept term.

### 3. R-squares are also substantially different. 

####################################################
### Solved Example 2 -- Consequences of overfitting
####################################################
SolvedExample2 <- read.csv("SolvedExample2.csv")

FoodExp <- SolvedExample2$Food.Expenditure
IncomeAfterTax <- SolvedExample2$Income.After.Tax
Male_Dummy <- ifelse(SolvedExample2$Gender == "Male", 1, 0)   

### Starting with the underfitted model
model_sol_ex_2_u <- lm(FoodExp ~ Male_Dummy)
summary(model_sol_ex_2_u)

### Model with Income but without interaction term (differential slope coefficient)
model_sol_ex_2 <- lm(FoodExp ~ IncomeAfterTax + Male_Dummy)
summary(model_sol_ex_2)

### Observations by adding the Income:
### Notice the upward bias in Male_Dummy.
### Also, notice the difference in the standard errors of the Male_Dummy.

### Model including the interaction term (differential slope coefficient)
model_sol_ex_2a <- lm(FoodExp ~ IncomeAfterTax + Male_Dummy + IncomeAfterTax:Male_Dummy) 
summary(model_sol_ex_2a)

### Observations after adding the interaction term
### Neither the differential intercept coefficient nor the differential slope coefficient
### are now significant. Expenditures on food by male definitely differs from that 
### by females. It is quite likely that the interaction term is superfluous.
### Also note that the std errors of the coefficients are larger.

#####################################################################
### Solved Example 3a --- Errors of Measurement
#####################################################################
SolvedExample3a <- read.csv("SolvedExample3a.csv")

Y_star <- SolvedExample3a$Y.
X_star <- SolvedExample3a$X.
Y <- SolvedExample3a$Y
X <- SolvedExample3a$X

### True Model
true_model <- lm(Y_star ~ X_star)
summary(true_model)

### Measurement Error in Y only
error_in_Y_model <- lm(Y ~ X_star)
summary(error_in_Y_model)

### Measurement Error in X only
error_in_X_model <- lm(Y_star ~ X)
summary(error_in_X_model)

### Measurement Error in Y and X
error_in_X_and_Y_model <- lm(Y ~ X)
summary(error_in_X_and_Y_model)

#####################################################################
### Solved Example 3 --- Detecting Presence of Unnecessary Variables
#####################################################################
SolvedExample3 <- read.csv("SolvedExample3.csv")

LE <- SolvedExample3$LE
Income <- SolvedExample3$INCOME
Access <- SolvedExample3$ACCESS

### Model 1
### LE = Beta1 + Beta2*Income + Beta3*Access + Error
model_sol_ex_3 <- lm(LE ~ Income + Access)
summary(model_sol_ex_3)

### Model 2 - Is LE increasing at an increasing rate or increasing at a decreasing rate
### wrt Income.
### LE = Beta1 + Beta2*Income + Beta3*Access + Beta4*Income-sq + Error
model_sol_ex_3a <- lm(LE ~ Income + Access + I(Income^2))
summary(model_sol_ex_3a)

### Model 3 - Is LE increasing at an increasing rate or increasing at a decreasing rate
### wrt Access.
### LE = Beta1 + Beta2*Income + Beta3*Access + Beta4*Income-sq + Beta5*Access-sq + Error
model_sol_ex_3b <- lm(LE ~ Income + Access + I(Income^2) + I(Access^2))
summary(model_sol_ex_3b)

### Does this mean Access and Access-sq are both superfluous?
### Let us test using partial F-test

### Full Model: LM = Beta1 + Beta2*Income + Beta3*Income-sq + Beta4*Access + Beta5*Access-sq + Error
### Reduced Model: LM = Beta1 + Beta2*Income + Beta3*Income-sq + Error

### Full Model
fullmodel <- lm(LE ~ Income + Access + I(Income^2) + I(Access^2))
summary(fullmodel)

### Reduced Model
reducedmodel <- lm(LE ~ Income + I(Income^2))
summary(reducedmodel)

anova(reducedmodel, fullmodel)

### We note from the Partial F-test that Access and Access-sq are not superfluous
### Is Access-sq superfluous? We can examine this using the t-test for beta.
### We see from model 2 that removing Access-sq makes Access significant.
### So, we leave Access in the final model.

############################################
### Solved Example 4 --- Ramsey RESET Test
############################################
SolvedExample4 <- read.csv("SolvedExample4.csv")

ImportsExp <- SolvedExample4$Exp.on.Imports
PDI <- SolvedExample4$PDI

### Model: Imports = Beta1 + Beta2*PDI + Error
model_sol_ex_4 <- lm(ImportsExp ~ PDI)
summary(model_sol_ex_4)

### Obtain the residuals
resids <- residuals(model_sol_ex_4)

### Obtain the predicted values
pred <- predict(model_sol_ex_4)

### Create a residual versus fits plot
plot(pred, resids)
### Residual plots show a pattern indicating a misspecification.
### Let us validate this formally using Ramsey's RESET test.

### Create a model using Pred^2 and Pred^3 terms.
pred_sq <- predict(model_sol_ex_4)^2
pred_cu <- predict(model_sol_ex_4)^3

### Fit the new model
### Yt = Beta1 + Beta2*Xt + Beta3*Pred^2 + Beta4*Pred^3 + Error
model_sol_ex_4a <- lm(ImportsExp ~ PDI + pred_sq + pred_cu)
summary(model_sol_ex_4a)

R_sq_new <- summary(model_sol_ex_4a)$r.sq
R_sq_old <- summary(model_sol_ex_4)$r.sq
n_new_reg <- 2
n_num_param <- 4 ### including the intercept

F <- ((R_sq_new - R_sq_old)/n_new_reg)/((1 - R_sq_new)/(nrow(SolvedExample4) - n_num_param))

### F-value significant at any reasonable significance level.
### Conclusion - the model is misspecified.

### Alternative: Using partial F-test
anova(model_sol_ex_4, model_sol_ex_4a)

############################################
### Solved Example 4a --- MWD Test
############################################
SolvedExample4a <- read.csv("SolvedExample4a.csv")

Year <- SolvedExample4a$Year
Exp_Imports <- SolvedExample4a$Exp.on.Imports # Expenditure on Imports
PDI <- SolvedExample4a$PDI # Personal Disposable Income
log_Exp_Imports <- log(Exp_Imports)
log_PDI <- log(PDI)

## Linear Model
lin_model <- lm(Exp_Imports ~ PDI + Year)
summary(lin_model)

## Obtain predicted values
pred_linear_model <- predict(lin_model)

## Log-Linear Model
log_linear_model <- lm(log_Exp_Imports ~ log_PDI + Year)
summary(log_linear_model)

## Obtain predicted values
pred_log_linear_model <- predict(log_linear_model)

## Z_1i
Z_1i <- log(pred_linear_model) - pred_log_linear_model

## Regress Y on X's and Z_1i
Test_Linear_Specification <- lm(Exp_Imports ~ PDI + Year + Z_1i)
summary(Test_Linear_Specification)

## Since Z_1i is significant, we reject H0
## H0: Linear Model: Y is a linear function of the X's
## H1: Log-Linear Model: ln(Y) is a linear function of the X's or a log of X's

## Obtain Z_2i
Z_2i <- exp(pred_log_linear_model) - pred_linear_model

## Regress ln(Y) on the X's or logs of X's and Z_2i
Test_Log_Linear_Specification <- lm(log_Exp_Imports ~ log_PDI + Year + Z_2i)
summary(Test_Log_Linear_Specification)

## Since Z_2i is significant, we reject H1.
## H0: Linear Model: Y is a linear function of the X's
## H1: Log-Linear Model: ln(Y) is a linear function of the X's or a log of X's

## Looking at the results, either model is reasonable. 

#########################################
### Solved Example 5 - Variable Selection
#########################################
SolvedExample5 <- read.csv("SolvedExample5.csv")

### Create Engineer_Dummy
Engineer_Dummy <- ifelse(SolvedExample5$Engineer == "Yes", 1, 0)

Sales <- SolvedExample5$Sales
Wonder <- SolvedExample5$Wonder
SC <- SolvedExample5$SC
Experience <- SolvedExample5$Experience

### First run a regression model with all variables for a quick check 
### for multicollinearity
model_sol_ex_5 <- lm(Sales ~ Wonder + SC + Experience + Engineer_Dummy)

car::vif(model_sol_ex_5) ### No evidence of multicollinearity 

### Store all vars + Dummy in a separate dataframe. 
### Could have included in the original dataframe...
df_SolvedEx5 <- data.frame(Sales, Wonder, SC, Experience, Engineer_Dummy)

### Run Forward Selection
fwd <- ols_step_forward_p(model_sol_ex_5, penter = 0.10, details = TRUE)
fwd

### Run Backward Elimination
bwd <- ols_step_backward_p(model_sol_ex_5, prem = 0.10, details = TRUE)
bwd

### Run Stepwise Regression
model <- lm(Sales ~ ., data = df_SolvedEx5)
k <- olsrr::ols_step_both_p(model, prem = 0.10, pent = 0.10, details = TRUE)
plot(k) # Optional if you are bored by the dull output and want some entertainment.

### Run Best Subsets Regression
best_subsets <- ols_step_best_subset(model_sol_ex_5, details = TRUE)
best_subsets

### Consider selected variables only and create a separate data frame
df_sel_var <- data.frame(Sales, SC, Engineer_Dummy)

### Quickly create a scatterplot to see if there is any evidence of nonlinearity.
plot(df_sel_var) 
### OR
pairs(df_sel_var[1:2], panel = panel.smooth)

model_sol_ex_5a <- lm(Sales ~ SC + Engineer_Dummy)
summary(model_sol_ex_5a)

### Regression diagnostics...
resids_solved_ex5 <- residuals(model_sol_ex_5a)
pred_solved_ex5 <- predict(model_sol_ex_5a)

plot(pred_solved_ex5, resids_solved_ex5)

hist(resids_solved_ex5)

#########################################
### Solved Example 6 - Variable Selection
#########################################

SolvedExample6 <- read.csv("SolvedExample6.csv")

Sales <- SolvedExample6$SALES
Adv <- SolvedExample6$ADV
Bonus <- SolvedExample6$BONUS
Mktshr <- SolvedExample6$MKTSHR
Compet <- SolvedExample6$COMPET

### Create SOUTH, MIDWEST dummies
South_Dummy <- ifelse(SolvedExample6$REGION == "SOUTH", 1, 0)
Midwest_Dummy <- ifelse(SolvedExample6$REGION == "MIDWEST", 1, 0)

### First run a regression model with all variables for a quick check 
### for multicollinearity
model_sol_ex_6 <- lm(Sales ~ Adv + Bonus + Mktshr + Compet + 
                       South_Dummy + Midwest_Dummy)

car::vif(model_sol_ex_6) ### No evidence of multicollinearity 

### Store all vars + Dummy in a separate dataframe. 
### Could have included in the original dataframe...
df_SolvedEx6 <- data.frame(Sales, Adv, Bonus, Mktshr, 
                           Compet, South_Dummy, Midwest_Dummy)

model <- lm(Sales ~ ., data = df_SolvedEx6)

### Forward Selection
fwd <- ols_step_forward_p(model, penter = 0.05, details = TRUE)

### Backward Elimination
bwd <- ols_step_backward_p(model, prem = 0.05, details = TRUE)

### Run Stepwise Regression
stepwise <- olsrr::ols_step_both_p(model, prem = 0.05, pent = 0.05, details = TRUE)
plot(stepwise)

### Run Best Subset Regression
k <- ols_step_best_subset(model, details = TRUE)
k

## Data frame without Dummies
df_SolvedEx6_woDummy <- data.frame(Sales, Adv, Bonus, Mktshr, Compet)

### Run Stepwise Regression without dummy variables
model <- lm(Sales ~ ., data = df_SolvedEx6_woDummy)
k <- olsrr::ols_step_both_p(model, prem = 0.10, pent = 0.10, details = FALSE)
k1 <- olsrr::ols_step_backward_aic(model)
plot(k)

k2 <- olsrr::ols_step_all_possible(model)
plot(k2)

### Consider selected variables only and create a separate data frame
df_sel_var_6 <- data.frame(Sales, Adv, Bonus)

### Quickly create a scatterplot to see if there is any evidence of nonlinearity.
plot(df_sel_var_6) 
### OR
pairs(df_sel_var[1:3], panel = panel.smooth)

model_sol_ex_6a <- lm(Sales ~ Adv + Bonus)
summary(model_sol_ex_6a)

### Regression diagnostics...
resids_solved_ex6 <- residuals(model_sol_ex_6a)
pred_solved_ex6 <- predict(model_sol_ex_6a)

plot(pred_solved_ex6, resids_solved_ex6)

hist(resids_solved_ex6)

### Next run a Partial F-test to assess the significance of 
### the dummy variables

### Create a df with dummies
df_withdummies <- data.frame(Sales, Adv, Bonus, Midwest_Dummy, South_Dummy)

full_model_6 <- lm(Sales ~ Adv + Bonus + Midwest_Dummy + South_Dummy, data = df_withdummies)
summary(full_model_6)

reduced_model_6 <- lm(Sales ~ Adv + Bonus)
summary(reduced_model_6)

anova(reduced_model_6, full_model_6)

### Result of the Partial F-test indicates that the REGION variable is significant.

### Assess normality again...
resid_6_full_model <- residuals(full_model_6)
hist(resid_6_full_model)
