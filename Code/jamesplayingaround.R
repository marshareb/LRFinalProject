# ********************************************************************************************************
# LIBRARIES
# ********************************************************************************************************

library(ALSM)
library(readr)
library(car)
library(MASS)
library(Hmisc)
library(onewaytests)

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

# Drop years since phd
salaries <- salaries[,-3]

# Try stepwise algorithm
#salaries.mod <- step(lm(salary ~ (.)^2, salaries), salaries, direction=("both"))
#summary(salaries.mod)


# We also try starting from nothing and seeing if it will settle on the same model.
#salaries.mod2 <- step(lm(salary~1, salaries), scope = list(lower = lm(salary~1, salaries), 
#                                                           upper = lm(salary~(.)^2, salaries)), 
#                      direction="both")
#summary(salaries.mod2)

# They do settle on the same thing

# As a final test, try running best sub
#BestSub(salaries[,2:length(salaries)], salaries$salary)
#colnames(salaries)

# Everything except yrs.since.phd is significant if we go by PRESSp and Cp

salaries.mod <- lm(salary~discipline + sex + asstprof + assocprof, salaries)

#plot(salaries.mod)


bmcle <- boxcox(salaries.mod, lambda=seq(-3,3, by=0.1))
lambda <- bmcle$x[which.max(bmcle$y)]
salaries$salary <- (salaries$salary)^(lambda)

salaries.mod <- lm(salary~discipline + sex + asstprof + assocprof, salaries)

#plot(salaries.mod)

#lf <- influence.measures(salaries.mod)

#lf$infmat[318,]
#lf$infmat[283,]

#intersect(outlierTest(salaries.mod), seq(1,397, by=1)[apply(lf$is.inf, 1, any)])

#influencePlot(salaries.mod)
#dfbetasPlots(salaries.mod)

salaries <- salaries[-318,]
salaries <- salaries[-283,]
salaries.mod <- lm(salary~discipline + sex + asstprof + assocprof, salaries)

# Final Model

summary(salaries.mod)
plot(salaries.mod)

# Tests


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

anova(salaries.mod)
residualPlot(salaries.mod)
# H0: Beta1=0, Ha: Beta1<>0
# Fs> Fc, hence we reject H0 so we agree that discipline is significan to our model

# H0: Beta2=0, Ha: Beta2<>0

# After going to statistical consulting session, Shiwei confirms that our 
# model looks good. It seems reasonable to remove case [318, 283], because 
# considering our n=400, removing 2 outliers does not seem like a major deal.
# Also, after removing the outliers, the plots looked really good, 
# shows clear normality and constant variance. 
# He further commented that our plots look fine for models with many 
# categorical variables.

# He also suggests us to reason why these cases are outliers,
# Case 283 and Case 318, both are professors, have been in the field for 
# 46-51 years, but their salaries are $57,800 and $67,559 respectively.
# Comparing to case 13, even an assistant prof in the same college,
# who only works for a year, earns $77,700. 
# Our possible assumptions:
#         1. Less adaptable to new technologies, 
#           - may cause low productivity compared to new prof
#           - teach less classes/session
#         2. Economic recession in 2008-2009
#           - Since it's hard to find a new job, so they might consider 
#             stick with their current job
#           - Also, since salaries are given based on yearly contracts, 
#             the recession might play a role here. 



