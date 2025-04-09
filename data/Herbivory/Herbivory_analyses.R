
### Look at the data. I added the tree to be a random factor (strom). It's needed to inspect whether it has an effect. Also, I standardized the
# herbivory rate for a one gram of caterpillar mass (Hong) dividing the herbivory rate (herb) by the weight of the caterpillars measured before the experiment (WB).

library(lme4)
library(emmeans)

setwd("C:/Users/annam/OneDrive - Biologické centrum AV ČR, v. v. i/Anawork/Students/Vendula Bilkova/Data/herbivory") # this is a cool function -
# you SET the Working Directory (setwd) so when you need to save the pictures, or datasets that you created within your code, you don't need to setup the path to
# the directory again.

herbivory_data <- read.csv("herbivory.csv")
herbivory_data

shapiro_result <- shapiro.test(herbivory_data$Hong)
print(shapiro_result)

anova_model <- aov(Hong ~ treat * druh, data = herbivory_data)
summary(anova_model)

plot(fitted(anova_model), residuals(anova_model),
     main = "Residuals vs Fitted Values (Hong)",
     xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red", lty = 2)

# First, we will check for the data distribution to decide which test or data transformation to choose:

ggplot(herbivory_data, aes(x = Hong)) + 
  geom_histogram(binwidth = 0.00001, color = "black", fill = "gray") +
  labs(title = "Histogram of Standardized Herbivory Rate (Hong)", 
       x = "Standardized Herbivory Rate (Hong)", y = "Frequency")
### the histogrm shows a right-skewed distribution, indicating it is not normally distributed.

# Q-Q plot for Hong variable just to confirm what the plot above showed us
qqPlot(herbivory_data$Hong, main = "Q-Q Plot of Standardized Herbivory Rate (Hong)")
### and yes, you can see that the data are not normally distributed.

# Calculating mean and variance for the Hong variable to check the overdispersion:
mean_hong <- mean(herbivory_data$Hong, na.rm = TRUE)
var_hong <- var(herbivory_data$Hong, na.rm = TRUE)

mean_hong
var_hong
### The mean of the Hong variable is approximately 0.000115, and the variance is 2.74×10−82. The variance is much smaller than the mean,
### suggesting no overdispersion.

# Fit a simple linear model using species and treatment as fixed effects to be able to check for homoscedasticity
model_hong <- lm(Hong ~ druh * treat, data = herbivory_data)

# Plot residuals vs fitted values
plot(fitted(model_hong), residuals(model_hong),
     main = "Residuals vs Fitted Values (Hong)",
     xlab = "Fitted Values", ylab = "Residuals", pch = 20)
abline(h = 0, col = "red", lty = 2)
### The residuals vs. fitted values plot for the Hong variable shows some variability in the spread of residuals across the range of fitted values,
### indicating possible heteroscedasticity (non-constant variance).

### Given the non-normality and heteroscedasticity, it would be better to use a generalized linear model.
### Based on the characteristics of the data (standardized herbivory rate, which is continuous and positively skewed),
### the Gamma family with a log link is appropriate. The Gamma distribution can handle the positive, continuous nature of our response variable, 
### accounting for heteroscedasticity.

# Now, the model itself using the Gamma family
# Because our data are not strictly possitive (we have some 0s there) and Gamma models can't handle zero or negative values, we have to add a little value to 0s:
herbivory_data$Hong_adjusted <- herbivory_data$Hong + 0.00000000001

# Fit a GLMM with Gamma family and log link, including treeID as random effect and adjusted herbivory values to avoid 0s:
glmm_gamma <- glmer(Hong_adjusted ~ druh * treat + (1 | strom), 
                    data = herbivory_data, 
                    family = Gamma(link = "log"))
###  we got an error note:
###  > glmm_gamma <- glmer(Hong_adjusted ~ druh * treat + (1 | strom), 
###                        +                     data = herbivory_data, 
###                        +                     family = Gamma(link = "log"))
### boundary (singular) fit: see help('isSingular')

### This indicates that the model has reached a boundary in its estimation, and this often suggests a singular fit. A singular
### fit typically occurs when the random effects structure is too complex given the data, or when there is not enough variability
### at the level of the random effect to justify its inclusion.

### We should assess the variance of the random effect and determine whether it's contributing meaningful variability to the model - two ways:

# 1. asking whether the model is singular:
isSingular(glmm_gamma)
### TRUE - so we have a problem :D

# 2. checking for the value of strom variance in the Summary of the model - if very close to zero, we have a problem :D 
summary(glmm_gamma)
### Its 0 - thus, we have a problem :D

# We will remove the random effect from the model (it does not contribute meaningful variance) and see if the model improves:
glmm_gamma_simplified <- glm(Hong_adjusted ~ druh * treat, 
                             data = herbivory_data, 
                             family = Gamma(link = "log"))

# there are some problems with convergence:
# Error in glm.fit(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  : 
# NA/NaN/Inf in 'x'
# In addition: Warning message:
#  step size truncated due to divergence 

# We have to check what's the problem with the data.

# As we know that there are no missing values in the dataset (normally, I propose you to do this if you have this problem),
# we can directly check for extreme values in the response variable (Hong_adjusted):
range(herbivory_data$Hong_adjusted, na.rm = TRUE)
### [1] 0.0000000001 0.0006161551 - these are two identified extreme values, so we will try to lower the constant we added to Hong variable:

herbivory_data$Hong_adjusted3 <- herbivory_data$Hong + 0.0000001

# Fit a simplified (without random effect) GLMM again with changed constant:
glmm_gamma_simplified <- glm(Hong_adjusted3 ~ druh * treat, 
                             data = herbivory_data, 
                             family = Gamma(link = "log"))
### Cool! the model finally converged. So we can see the results:

#Resuts:
summary(glmm_gamma_simplified)

### Results:
# Call:
# glm(formula = Hong_adjusted2 ~ druh * treat, family = Gamma(link = "log"), 
#     data = herbivory_data)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -9.72870    0.41492 -23.447   <2e-16 ***
#   druhpid            1.04517    0.58679   1.781   0.0857 .  
# treatmeja          0.24191    0.58679   0.412   0.6833    
# druhpid:treatmeja -0.08832    0.82985  -0.106   0.9160    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for Gamma family taken to be 1.377302)

# Null deviance: 55.293  on 31  degrees of freedom
# Residual deviance: 47.327  on 28  degrees of freedom
# AIC: -509.37

# Number of Fisher Scoring iterations: 6

### Interpretation of the results:
### The main effect of species (druhpid) is marginally significant (p = 0.0857), suggesting that there may be some difference in herbivory between species,
# but not at the typical significance level (0.05).
#The main effect of treatment (treatmeja) is not significant (p = 0.6833), suggesting that, on average, treatment (methyl jasmonate) does not have a strong
#effect on herbivory rate.
#The interaction between species and treatment (druhpid) is also not significant (p = 0.916), indicating that there is no strong evidence that the effect of
#treatment differs between species.

###########################
# The following test is for a very deep understanding of how the model reacts and why. It is really not needed. It's just for you, if you are interested, to 
# deeply understand it.

# Check residuals
# Since glmer does not have default diagnostic plots, we should check the residuals and use residuals and fitted values manually
par(mfrow = c(1, 1))  # Reset to single plot
residuals_glmm <- residuals(glmm_gamma_simplified, type = "pearson")
fitted_glmm <- fitted(glmm_gamma_simplified)

plot(fitted_glmm, residuals_glmm, 
     xlab = "Fitted Values", ylab = "Pearson Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

### If you look at the plot, the residuals seem to be spread quite unevenly across the range of fitted values. While some clustering is expected in Gamma GLMs,
### there seems to be a vertical "banding" effect at certain fitted values. This could indicate that the model might not be capturing all the structure in
### the data, particularly for some levels of the predictors.

### Also, a few of the residuals are quite large (positive or negative), indicating that there are some outlying observations. However, Gamma GLMs can tolerate such
### variability because they assume non-constant variance (heteroscedasticity), but it's still worth investigating whether these outliers are affecting the fit.

# We can identify these outliers by examining the largest residuals:
outliers <- which(abs(residuals_glmm) > 2)
herbivory_data[outliers, ]

#    treat   druh       herb strom    WB        Hong Hong_adjusted Hong_adjusted2
# 4   meja ostruh 0.04699564     4 168.9 0.000278245   0.000288245    0.000288245
# 15  meja    pid 0.03610668    15  58.6 0.000616155   0.000626155    0.000626155
# 23 kontr ostruh 0.08182405    23 479.6 0.000170609   0.000180609    0.000180609
# 32 kontr    pid 0.03293929    32  64.6 0.000509896   0.000519896    0.000519896

### These trees have relatively higher herbivory rates compared to the rest of the data, which might explain why the model is struggling to fit them.
### Herbivory rates vary significantly across these trees, and this could be due to natural variability or specific environmental factors. As it varies randomly
### between caterpillar species and treatments, and that wee had really low overall herbivory rate, I would assume, that these only Were the caterpillars, that
### behaved normally = ate the leaves a little bit ;)


# post hoc
# Conduct post-hoc pairwise comparisons using emmeans
# This will compare all levels of 'treat' and 'druh' for the GLM model
posthoc_emm <- emmeans(glm(Hong_adjusted3 ~ druh * treat, family = Gamma(link = "log"), data = herbivory_data), 
                       pairwise ~ druh * treat,
                       type = "response")
print(posthoc_emm$contrasts)

# for treatment only
posthoc_emm_tr <- emmeans(glm(Hong_adjusted3 ~ treat, family = Gamma(link = "log"), data = herbivory_data), 
                       pairwise ~ treat,
                       type = "response")
print(posthoc_emm_tr$contrasts)

# for species only
posthoc_emm_sp <- emmeans(glm(Hong_adjusted3 ~ druh, family = Gamma(link = "log"), data = herbivory_data), 
                          pairwise ~ druh,
                          type = "response")
print(posthoc_emm_sp$contrasts)


