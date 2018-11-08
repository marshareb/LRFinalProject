# ********************************************************************************************************
# LIBRARIES
# ********************************************************************************************************

library(ALSM)
library(readr)
library(car)
library(Hmisc)


# ********************************************************************************************************
# DATA IMPORT
# ********************************************************************************************************

# Import the data first

salaries.dat <- read_csv("Code/Salaries.csv", 
                     col_types = cols(X1 = col_skip()))

# There is an error using the step function if we keep it as a tibble; concatenated it into a data.frame
# to fix it.
salaries.dat <- data.frame(Salaries_)


# ********************************************************************************************************
# CLEANING THE DATA
# ********************************************************************************************************

salaries <- salaries.dat

# We need to clean the data, updating the qualitative variables from words into actual numbers. This 
# an initial assignment of things; can be modified in the future.
# RANK:
# Prof = 0, AsstProf = 1, AssocProf = 2
# SEX:
# Male = 0, Female=1
# FACTOR:
# A(theoretical) = 0, B(applied) = 1

clean <- function(name){
  if (name == "AsstProf"){
    return(as.integer(1))
  } else if (name == "AssocProf"){
    return(as.integer(2))
  } else if (name == "Female"){
    return(as.integer(1))
  } else if (name == "B"){
    return(as.integer(1))
  } else{
    return(as.integer(0))
  }
}

# Now, we need to run this function over the different variables. We do this by using sapply. The way sapply 
# works is it runs a function over a list and outputs a list. The general format is 
# sapply(LIST, FUNCTION)

salaries$rank <- sapply(salaries$rank, clean)
salaries$discipline <- sapply(salaries$discipline, clean)
salaries$sex <- sapply(salaries$sex, clean)

# ALTERNATIVELY:

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

salaries2 <- data.frame(salaries.dat$salary, salaries.dat$yrs.since.phd, salaries.dat$yrs.service,
                        discipline, sex, asstprof, assocprof)

# Rename columns since this messes things up

colnames(salaries2) <- c("salary", "yrs.since.phd", "yrs.service", "discipline", "sex", "asstprof", "assocprof")

# I think this is better than what I had before

# ********************************************************************************************************
# FULL MODEL EXPLORATION USING SALARIES
# ********************************************************************************************************

# Naively just create a model using all of the variables
salaries.mod <- lm(salary ~rank + discipline + yrs.since.phd + yrs.service + sex, salaries)
summary(salaries.mod)

# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalariesMod.png")
plot(salaries.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalariesMod.png")
plot(salaries.mod, which=(2))
dev.off()

# POSSIBLE OUTLIERS: CASES 250, 365, 44

# The residuals vs. fitted looks like there is an issue of nonconstant variance (fanning behavior). Linearity
# seems okay.
# The QQ-plot may have some top heavy outliers
# Conclusion: Non-normality and non-constant variance.
# Y-transformation through box-cox might fix it.

# Check partial regression plot for each predictor variable
png("Plots/PartialRegressionPlotsSalaries.png")
avPlots(salaries.mod)
dev.off()
# QUESTION: When using qualitative variables, do we still use the measure of slope as to whether or not a 
# variable is significant?
# Years since Ph.D., discipline, and rank seem significant.


# ********************************************************************************************************
# FULL MODEL EXPLORATION USING SALARIES2
# ********************************************************************************************************

# Naively just create a model using all of the variables
salaries2.mod <- lm(salary ~., salaries2)
summary(salaries2.mod)

# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalaries2Mod.png")
plot(salaries2.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalaries2Mod.png")
plot(salaries2.mod, which=(2))
dev.off()

# POSSIBLE OUTLIERS: CASES 250, 365, 44

# The residuals vs. fitted looks like there is an issue of nonconstant variance (fanning behavior). Linearity
# however seems great.
# The QQ-plot has issues; definitely not normal.
# Conclusion: Non-normality and non-constant variance.
# Y-transformation through box-cox might fix it.

# Check partial regression plot for each predictor variable
png("Plots/PartialRegressionPlotsSalaries2.png")
avPlots(salaries2.mod)
dev.off()
# QUESTION: When using qualitative variables, do we still use the measure of slop as to whether or not a 
# variable is significant?
# Everything except sex seems to have contribution. Linear issue with discipline maybe?

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
# VARIABLE/MODEL SELECTION (SALARIES1)
# ********************************************************************************************************

# Backwards step function applied to data:
salaries.mod3 <- step(salaries.mod, salaries, direction=("backward"))
summary(salaries.mod3)

# Forward step function applied to data:
salaries.mod4 <- step(lm(salary~1, salaries), scope = list(lower = lm(salary~1, salaries), 
                                                           upper = lm(salary~., salaries)), 
                      direction="forward")
summary(salaries.mod4)

# Give identical results. Both include yrs.since.phd and yrs.service. We may want to explore dropping this
# variable due to the multicollinearity issue.

# ********************************************************************************************************
# VARIABLE/MODEL SELECTION (SALARIES2)
# ********************************************************************************************************

# Backwards step function applied to data:
salaries2.mod2 <- step(lm(salary~., salaries2), salaries2, direction=("backward"))
summary(salaries2.mod2)

# Forward step function applied to data:
salaries2.mod3 <- step(lm(salary~1, salaries2), scope = list(lower = lm(salary~1, salaries2), 
                                                           upper = lm(salary~., salaries2)), 
                      direction="forward")
summary(salaries2.mod3)

# ********************************************************************************************************
# THINGS TO DO
# ********************************************************************************************************

# Are there any variables which need quadratic + terms?
# Any multicollinearity issues? (probably)
# Other diagnostics


# ********************************************************************************************************
# SIMULTANEOUSLY CHECKING ALL HIGHER ORDER INTERACTION (SALARIES2)
# ********************************************************************************************************

salaries2.mod4 <- lm(salary~(.)^2, salaries2)
summary(salaries2.mod4)

# This is not super informative due to the large amount of data, but it does lend me to think that there
# might be a relation between yrs.since.phd. and discipline and yrs.service and discipline.

salaries2.mod5 <- lm(salary~. + yrs.since.phd*discipline + yrs.service*discipline, salaries2)
summary(salaries2.mod5)

# This agrees that there is some non-trivial interaction between these variables we should consider. Let's try
# running the backwards step algorithm on this

salaries2.mod6 <- step(salaries2.mod5, salaries2, direction=("backward"))
summary(salaries2.mod6)

# We see that sex is killed off in this.

# We then go ahead with choosing the best model from: salaries, salaries2, and salaries3 (omitting yrs.since.phd).

BestSub(salaries[,2:6], salaries$salary, num=1)
#Model(4)this suggests to drop sex

BestSub(salaries2[,2:7], salaries2$salary, num=1)
#Model(5) also suggests to drop sex

salaries3<-data.frame(salaries.dat$salary, salaries.dat$yrs.service,
                      discipline, sex, asstprof, assocprof)
BestSub(salaries3[,2:6], salaries3$salaries.dat.salary, num=1)
#Model(3) using salaries3 suggests that we should drop sex and years.service
#create a new data set, omitting sex (since all 3 subsets suggests the same thing)
salaries4<-data.frame(salaries.dat$salary, salaries.dat$yrs.service,discipline,asstprof, assocprof)
newmodel<-lm(salaries.dat.salary~salaries.dat.yrs.service+asstprof+assocprof, salaries4)
summary(newmodel)
