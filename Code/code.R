# ********************************************************************************************************
# LIBRARIES
# ********************************************************************************************************

library(ALSM)
library(readr)
library(car)
library(MASS)
library(Hmisc)


# ********************************************************************************************************
# DATA IMPORT
# ********************************************************************************************************

# Import the data first

salaries.dat <- read_csv("Salaries.csv", 
                         col_types = cols(X1 = col_skip()))

# There is an error using the step function if we keep it as a tibble; concatenated it into a data.frame
# to fix it.
salaries.dat <- data.frame(salaries.dat)


# ********************************************************************************************************
# CLEANING THE DATA
# ********************************************************************************************************

# We need to clean the data, updating the qualitative variables from words into actual numbers. This 
# an initial assignment of things; can be modified in the future.
# RANK:
# Broken up into two qualitative variables
# SEX:
# Male = 0, Female=1
# FACTOR:
# A(theoretical) = 0, B(applied) = 1

# Cleans the sex and discipline
clean2 <- function(name){
  if (name == "Female"){
    return(as.integer(1))
  } else if (name == "B"){
    return(as.integer(1))
  } else{
    return(as.integer(0))
  }
}

# Checks if they are an associate professor or not
clean3 <- function(name){
  if(name=="AssocProf"){
    return(1)
  }
  else{
    return(0)
  }
}

# Checks if they are an assistant professor or not
clean4 <- function(name){
  if(name=="AsstProf"){
    return(1)
  }
  else{
    return(0)
  }
}

# Now we make a new data frame containing this information

discipline <- sapply(salaries.dat$discipline, clean2)
sex <- sapply(salaries.dat$sex, clean2)
asstprof <- sapply(salaries.dat$rank, clean4)
assocprof <- sapply(salaries.dat$rank, clean3)

salaries <- data.frame(salaries.dat$salary, salaries.dat$yrs.since.phd, salaries.dat$yrs.service,
                       discipline, sex, asstprof, assocprof)

# Rename columns since this messes things up

colnames(salaries) <- c("salary", "yrs.since.phd", "yrs.service", "discipline", "sex", "asstprof", "assocprof")

# Justification for two variables for rank: it's a rule of thumb from the book. This will not assume
# that there is a constant change in mean between these factors.

# ********************************************************************************************************
# SCATTER PLOT OF DATA
# ********************************************************************************************************

# Scatter plot of the data
png("Plots/ScatterPlotofData.png")
plot(salaries)
dev.off()

# Correlation matrix
rcorr(as.matrix(cor(salaries)))

# Definitely looks like there is some multicollinearity issues with years since PhD and years of service
# (which was to be expected)

# ********************************************************************************************************
# STEP FUNCTION TO FIND BEST MODEL
# ********************************************************************************************************

# Before doing this section, reload data 

# Here, we run the step function with both directions, starting from the function with all
# interaction
salaries.mod <- step(lm(salary ~ (.)^2, salaries), salaries, direction=("both"))
summary(salaries.mod)

# This settles on the model which includes all of our base variables as well as some interaction

# We also try starting from nothing and seeing if it will settle on the same model.
salaries.mod2 <- step(lm(salary~1, salaries), scope = list(lower = lm(salary~1, salaries), 
                                                          upper = lm(salary~(.)^2, salaries)), 
                     direction="both")
summary(salaries.mod2)

# It seems they do not settle on the same thing.

# We will go with the first one, since it uses all of the first order variables.
# We append the interaction at the end of the dataset
salaries[,8] <- salaries[,2]*salaries[,3]
salaries[,9] <- salaries[,2] * salaries[,4]
salaries[,10] <- salaries[,3] * salaries[,4]
salaries[,11] <- salaries[,3] * salaries[,7]

# The best model according to Cp and PRESSp seems to be model 9, which is omitting the
# yrs.service variable.
BestSub(salaries[,2:11], salaries$salary, num=1)

salaries.mod <- lm(salary ~ . - yrs.service, salaries)


# ********************************************************************************************************
# DIAGNOSTICS ON THE MODEL
# ********************************************************************************************************

# First, we check summary
summary(salaries.mod)

# Next, let's get a QQ-plot
png("Plots/QQPlotSalariesStepSub.png")
plot(salaries.mod, which=(2))
dev.off()

# Next let's get a residual plot
png("Plots/ResidVsFittedStepSub.png")
plot(salaries.mod, which=(1))
dev.off()

# Can see very clearly that there is non-constant variance, non-normal distribution.
# Independence seems okay.

# Let's do some tests to make sure.

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's try doing a Brown Forsythe test
# H0 : Error variance constant
# Ha: Error variance nonconstant
sur <- data.frame(salaries.mod$fitted.values, salaries.mod$residuals)
colnames(sur) <- c("fit", "resid")
sur$fit
sur$group <- rep(1,dim(sur)[1])
sur$group <- cut(sur$fit, 5)
bf.test(resid~group, sur)
# p-value greater than 0.05, fail to reject null
# CONCLUSION: While BF failed, it's pretty clear that there's something going on.


# Since there is a failure of normality AND variance, we may want to explore a Box-Cox Transform


# We kill of years.service
salaries <- salaries[,-3]

# ********************************************************************************************************
# BOX-COX TRANSFORM
# ********************************************************************************************************

salaries.mod<-lm(salary~., salaries)

bmcle <- boxcox(salaries.mod, lambda=seq(-3,3, by=0.1))
lambda <- bmcle$x[which.max(bmcle$y)]

# Let's transform the y-var now
salaries$salary <- (salaries$salary)^(lambda)

salaries.mod<-lm(salary~., salaries)

# Let's get a QQ-plot
png("Plots/QQPlotSalariesStepSubA.png")
plot(salaries.mod, which=(2))
dev.off()

# Next let's get a residual plot
png("Plots/ResidVsFittedStepSubA.png")
plot(salaries.mod, which=(1))
dev.off()

# I actually think all the factors seem to be okay. Looks linear, looks like other than some
# outliers everything's good, looks normal, looks independent.

# Let's do some tests to make sure.

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's try doing a Brown Forsythe test
# H0 : Error variance constant
# Ha: Error variance nonconstant
sur <- data.frame(salaries.mod$fitted.values, salaries.mod$residuals)
colnames(sur) <- c("fit", "resid")
sur$fit
sur$group <- rep(1,dim(sur)[1])
sur$group <- cut(sur$fit, 5)
bf.test(resid~group, sur)
# p-value greater than 0.05, fail to reject null
# CONCLUSION: While BF failed, it's pretty clear that there's something going on.

# While they failed, it was pretty close. Maybe removing some influential outliers may fix
# things?

# ********************************************************************************************************
# OUTLIER EXPLORATION
# ********************************************************************************************************

# From both QQ and Resid, we see 365, 318, 283 look like outliers.

n <- dim(salaries)[1]
p <- dim(salaries)[2]-2

# The package CAR has an outliertest function

outlierTest(salaries.mod)


# Let's first try just throwing those out
salaries <- salaries[-365,]
salaries <- salaries[-318,]
salaries <- salaries[-299,]
salaries <- salaries[-283,]



salaries.mod <- lm(salary ~ ., salaries)
summary(salaries.mod)

# Let's get a QQ-plot
png("Plots/QQPlotSalariesStepSubAA.png")
plot(salaries.mod, which=(2))
dev.off()

# Next let's get a residual plot
png("Plots/ResidVsFittedStepSubAA.png")
plot(salaries.mod, which=(1))
dev.off()


# Everything looks great here, constant variance, independence, normality.

# Let's try doing a Brown Forsythe test, alpha = 0.05
# H0 : Error variance constant
# Ha: Error variance nonconstant
sur <- data.frame(salaries.mod$fitted.values, salaries.mod$residuals)
colnames(sur) <- c("fit", "resid")
sur$fit
sur$group <- rep(1,dim(sur)[1])
sur$group <- cut(sur$fit, 5)
bf.test(resid~group, sur)
# p-value greater than 0.05, fail to reject null
# CONCLUSION: BF fail to reject, and it looks good!

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value greater than 0.05, fail to reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are are normally distributed

# The model works!

# Save the model into a separate text file
sink('final_model.txt')
summary(salaries.mod)
sink()

# ********************************************************************************************************
# AVPLOTS
# ********************************************************************************************************

# Let's now explore avPlots of our data

png("Plots/avPlotsSalariesMod.png")
avPlots(salaries.mod)
dev.off()

# It looks to me like maybe sex is not as significant as the other variables.
# Everything looks pretty linear to me!


# ********************************************************************************************************
# MULTICOLLINEARITY EXPLORATION
# ********************************************************************************************************

# Next, we check for multicollinearity


# We make plots again
plot(salaries)

# Correlation matrix
rcorr(as.matrix(cor(salaries)))

# Multicollinearity between the interaction variables, as expected really. 
vif(salaries.mod)

# We see that yrs.since.phd, V8, V9, V10 all have high VIF.

# QUESTION: HOW DO WE DEAL WITH THESE?

# Let's make a ridge trace plot

mod1 <- lm.ridge(salary~.,data=salaries,lambda=seq(0,30,0.01))
plot(mod1)
png("Plots/RidgeTracePlot.png")
select(mod1)
dev.off()
# GCV at 0.53

library(lmridge)
mod2 <- lmridge(salary~., data=salaries, K = seq(0,0.2,0.01))
plot(mod2)
vif(mod2)
# k = 0.17 seems best

# GET PARAMETERS
salaries.mod2 <- lmridge(salary~., data=salaries, K = 0.19)
summary(salaries.mod2)
plot(salaries.mod2)


# ********************************************************************************************************
# THINGS TO DO
# ********************************************************************************************************
# Explore how to fix the multicollinearity issue (certainly not with ridge trace)
# Explore whether certain variables can be dropped
# Look into k-fold cross validation of model
# Examine implications of model.