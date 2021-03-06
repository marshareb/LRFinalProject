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
# FULL MODEL EXPLORATION USING SALARIES
# ********************************************************************************************************

# Initialize some basic variables
n <- dim(salaries)[1]
p <- dim(salaries)[2] -2 + 5
# Naively just create a model using all of the variables
# Based on guess work, we would assume that sex and discipline and sex and assoc/asst prof would have
# interaction. Also discipline and years since phd/years of service
salaries.mod <- lm(salary ~. + sex*discipline + sex * assocprof + sex * asstprof + discipline*yrs.since.phd +
                     discipline*yrs.service, salaries)
summary(salaries.mod)

# Based on this, it seems we can drop discipline*sex

salaries.mod <- lm(salary ~.+ sex * assocprof + sex * asstprof + discipline*yrs.since.phd +
                     discipline*yrs.service, salaries)
summary(salaries.mod)

# seems we can drop sex*assocprof


salaries.mod <- lm(salary ~.+ sex * asstprof + discipline*yrs.since.phd +
                     discipline*yrs.service, salaries)
summary(salaries.mod)

# seems we can drop sex*asstprof

salaries.mod <- lm(salary ~.+ discipline*yrs.since.phd +
                     discipline*yrs.service, salaries)
summary(salaries.mod)

# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalariesMod.png")
plot(salaries.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalariesMod.png")
plot(salaries.mod, which=(2))
dev.off()

# The residuals vs. fitted looks like there is an issue of nonconstant variance (fanning behavior).
# Linearity however seems great.
# The QQ-plot has issues; definitely not normal.

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's do a Breusch-Pagan test with alpha = 0.05 to see about nonconstant variance
# H0: Error variance constant
# Ha: Error variance nonconstant
ncvTest(salaries.mod)
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on residual plot and Breusch-Pagan, we conclude that there is a non-constant variance

# Let's see if maybe doing a log salary will fix this (without doing Box-Cox)
salaries$salary <- log(salaries$salary)

# Rerun everything

# Naively just create a model using all of the variables
salaries.mod <- lm(salary ~.+ discipline*yrs.since.phd +
                     discipline*yrs.service, salaries)
summary(salaries.mod)

# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalariesModA.png")
plot(salaries.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalariesModA.png")
plot(salaries.mod, which=(2))
dev.off()

# The residuals vs. fitted looks like there is an issue of nonconstant variance (fanning behavior).
# Linearity however seems great.
# The QQ-plot has issues; definitely not normal.

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's do a Breusch-Pagan test with alpha = 0.05 to see about nonconstant variance
# H0: Error variance constant
# Ha: Error variance nonconstant
ncvTest(salaries.mod)
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on residual plot and Breusch-Pagan, we conclude that there is a non-constant variance

# Let's also double check scatter plot and matrix
# Scatter plot of the data
png("Plots/ScatterPlotofDataA.png")
plot(salaries)
dev.off()

# Correlation matrix
rcorr(as.matrix(cor(salaries)))

# It does seem as though a log transformation didn't do anything. Revert it back
salaries$salary <- exp(salaries$salary)

# ********************************************************************************************************
# BOX-COX
# ********************************************************************************************************


# Let's try doing a Box-Cox transformation here
bmcle <- boxcox(salaries.mod, lambda = seq(-3,3, by=0.1))
lambda <- bmcle$x[which.max(bmcle$y)]
png("Plots/BoxCoxTransformationSalaries.png")
plot(bmcle)
dev.off()
lambda
# Since Box-Cox is an estimate, take lambda = -1. So transform our data so that Y' = Y^-1
salaries$salary <- (salaries$salary)^(lambda)

# Repeat everything from before
# Naively just create a model using all of the variables
salaries.mod <- lm(salary ~. + sex*yrs.service + discipline*yrs.service + asstprof*yrs.service + assocprof*yrs.service, salaries)
summary(salaries.mod)

# Backwards step function applied to data:
salaries.mod <- step(salaries.mod, salaries, direction=("backward"))
summary(salaries.mod)



# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalariesModA.png")
plot(salaries.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalariesModA.png")
plot(salaries.mod, which=(2))
dev.off()

# Seems to have fixed the non constant variance but not the non-normality.
# Rerun tests to see

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's do a Breusch-Pagan test with alpha = 0.05 to see about nonconstant variance
# H0: Error variance constant
# Ha: Error variance nonconstant
ncvTest(salaries.mod)
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on residual plot and Breusch-Pagan, we conclude that there is a non-constant variance



# ********************************************************************************************************
# TEST FOR OUTLIERS
# ********************************************************************************************************


# Check for outlying X
hatm <- hatvalues(salaries.mod)
p <- dim(salaries)[2]-2
g <- seq(1,n,by=1)
g <- g[hatm > 2 *p/n]

# Check for outlying Y
z <- seq(1,n,by=1)
stud.del.resid <- abs(rstudent(salaries.mod))
z <- z[stud.del.resid > qt(1-0.1/(2*n), n-p-1)]
# Cases 283, 318
z <- union(z,g)
# z is the vector which contains all 

# Check for influential points
# COOKS:
g1 <- cooks.distance(salaries.mod)[cooks.distance(salaries.mod) > qf(0.5,4,40)]
# None

# DFFITS: (data big so we use alternative criteria)
y <- seq(1, n, by=1)
g2 <- y[abs(dffits(salaries.mod)) > 2 *sqrt(p/n)]

# DFBETAS
checkDFBETA <- function(ncol){
  y <- seq(1,n, by=1)
  df <- dfbetas(salaries.mod)[,ncol]
  return(y[df > 2/sqrt(n)])
}
g <- seq(2,6,by=1)
g3 <- unlist(sapply(g, checkDFBETA))

# k is all the possible influential points
k <- union(g1,g2)
k <- union(k,g3)

# l is going to be all the possible influential points which were also possibly outliers
l <- intersect(z,k)

# Let's try removing the points
salaries <-salaries[-l,]
# Repeat everything from before
# Naively just create a model using all of the variables
salaries.mod <- lm(salary ~. + sex*yrs.service + discipline*yrs.service + asstprof*yrs.service + assocprof*yrs.service, salaries)
summary(salaries.mod)

# Backwards step function applied to data:
salaries.mod <- step(salaries.mod, salaries, direction=("backward"))
summary(salaries.mod)

# Residual vs. Fitted plot
png("Plots/ResidVsFittedSalariesModAA.png")
plot(salaries.mod, which=(1))
dev.off()

# QQ-Plot
png("Plots/QQPlotSalariesModAA.png")
plot(salaries.mod, which=(2))
dev.off()

# The residuals vs. fitted looks like there is an issue of nonconstant variance (fanning behavior).
# Linearity however seems great.
# The QQ-plot has issues; definitely not normal.

# Let's do a Shaprio-Wilks with alpha = 0.05 to see
# H0: Residuals normal
# Ha: Residuals not normal
shapiro.test(residuals(salaries.mod))
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on QQ-plot and Shapiro-Wilks, we conclude that the residuals are NOT normally distributed

# Let's do a Breusch-Pagan test with alpha = 0.05 to see about nonconstant variance
# H0: Error variance constant
# Ha: Error variance nonconstant
ncvTest(salaries.mod)
# p-value less than 0.05, reject null hypothesis
# CONCLUSION: Based on residual plot and Breusch-Pagan, we conclude that there is a non-constant variance

# REMARKS:

# It seems like our naieve attempt at just throwing in the full model did not work.
# We would like to be able to find the best model, and since there seems to be interaction
# this entices us to use something like the step function.

# Since there is 6! different possibilities for interaction, we will need to explore with
# the step algorithm

# We will use step with both on the dataset including all possible interaction.

