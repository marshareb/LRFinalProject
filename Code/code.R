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

salaries <- read_csv("~/Desktop/LRFinalProject/Code/Salaries.csv", 
                     col_types = cols(X1 = col_skip()))

# There is an error using the step function if we keep it as a tibble; concatenated it into a data.frame
# to fix it.
salaries <- data.frame(salaries)


# ********************************************************************************************************
# CLEANING THE DATA
# ********************************************************************************************************

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

# ********************************************************************************************************
# FULL MODEL EXPLORATION
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
# Y-transformation through box-cox might fix it.

# Check partial regression plot for each predictor variable
png("Plots/PartialRegressionPlotsSalaries.png")
avPlots(salaries.mod)
dev.off()
# QUESTION: When using qualitative variables, do we still use the measure of slop as to whether or not a 
# variable is significant?
# Years since Ph.D., discipline, and rank seem significant.

# Scatter plot of the data
png("Plots/ScatterPlotofData.png")
plot(salaries)
dev.off()

# Correlation matrix
rcorr(as.matrix(cor(salaries)))

# Definitely looks like there is some multicollinearity issues with years since PhD and years of service
# (which was to be expected)

# ********************************************************************************************************
# VARIABLE/MODEL SELECTION
# ********************************************************************************************************

# Backwards step function applied to data:
salaries.mod2 <- step(salaries.mod, salaries, direction=("backward"))
summary(salaries.mod2)

# Forward step function applied to data:
salaries.mod3 <- step(lm(salary~1, salaries), scope = list(lower = lm(salary~1, salaries), 
                                                           upper = lm(salary~., salaries)), 
                      direction="forward")
summary(salaries.mod3)

# Give identical results. Both include yrs.since.phd and yrs.service. We may want to explore dropping this
# variable due to the multicollinearity issue.


# ********************************************************************************************************
# THINGS TO DO
# ********************************************************************************************************

# Are there any variables which need quadratic + terms?
# Any multicollinearity issues? (probably)
# Other diagnostics