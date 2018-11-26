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
salaries[318,]
salaries[283,]
plot(salaries)
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

# Check multicollinearity
vif(salaries.mod)

# Findings from statistical consulting session
# Shiwei commented that our plots look good, considering we have
# categorical variables. Also, he agrees with our decision in removing
# the two outliers, case [283, 318]. Considering we have n=397, removing
# these two points will not be such a big deal. 

# Also, he suggested us to include reasons why the points are outliers
# For both cases, they are male full-time professors, have been teaching for 
# roughly 50 years, but their salaries are low compared to other observations
# Case 283, earns $57,800 and case 318 earns $67,559. 
# These are the assumptions that we came up with:
#   1. Low adaptability to new technologies
#      - low productivity as they might teach few classes
#   2. Recession during 2008-2009
#      - due to the recession, these professors might want to stick with 
#        their job, despite the low pay. 
#      - Also, it could be that since salaries are based on yearly contracts,
#        at the beginning of 2008, the start of the recession, the agreed
#        salaries for these professors are low in the first place. 


# Copy the salarie data into a new data called data2
data2<-salaries
# add a column called rank into the data2 from salarie.dat after removing the two outliers
data2$rank=salaries.dat$rank[-c(318,283)]

# transforming all the independent variables to be categorical/factors
data2$rank=factor(data2$rank, levels=unique(data2$rank))
data2$discipline=factor(data2$discipline, levels=unique(data2$discipline))
data2$sex=factor(data2$sex, levels=unique(data2$sex))
data2$asstprof=factor(data2$asstprof)
data2$assocprof=factor(data2$assocprof)

# fit the final model( we can see that both models with categorical and non categorical independent variables yied 
# the same summary results, proceed with the modeltrial2)
modtrial<- lm(salary~discipline+sex+asstprof+assocprof , data=salaries)
modtrial2<- lm(salary~discipline+sex+rank , data=data2)
summary(modtrial)
summary(modtrial2)

# ready to build constrast for comparing mean salaries for the different ranks as well as computing mean salaries for
# each rank
leastsquare=lsmeans(modtrial2,"rank")
contrasts = list(asstvsprof= c(-1,1,0), # comparing asst prof mean salary to professor mean salary
            assocvsprof=c(-1,0,1),
            asstvsassoc=c(0,1,-1),
            prof=c(1,0,0), # computin mean salary for professor
            asst=c(0,1,0),
            assoc=c(0,0,1))
# run contrast and get results
contrast(leastsquare, contrasts, adjust="none")


