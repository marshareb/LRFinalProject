# Import the data first
library(readr)
salaries <- read_csv("~/Desktop/LRFinalProject/Code/Salaries.csv", 
                     col_types = cols(X1 = col_skip()))

# There is an error using the step function if we keep it as a tibble; concatenated it into a data.frame
# to fix it.
salaries <- data.frame(salaries)

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

# Naively just create a model using all of the variables
salaries.mod <- lm(salary ~rank + discipline + yrs.since.phd + yrs.service + sex, salaries)
summary(salaries.mod)

# Try doing backwards step function on this model
salaries.mod2 <- step(salaries.mod, salaries, direction=("backward"))

png("Plots/plot1.png")
plot(salaries)
dev.off()

# NEED TO SAVE STILL:
plot(salaries.mod)

# THINGS TO DO:
# Are there any variables which need quadratic + terms?
# Any multicollinearity issues? (probably)
# Other diagnostics