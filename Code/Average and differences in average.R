# Copy the salaries data into a new data called data2
data2<-salaries
# add a column called rank into the data2 from salarie.dat after removing the two outliers
data2$rank=salaries.dat$rank[-c(318,283)]

# transforming all the predictors to be categorical/factors
data2$rank=factor(data2$rank, levels=unique(data2$rank))
data2$discipline=factor(data2$discipline, levels=unique(data2$discipline))
data2$sex=factor(data2$sex, levels=unique(data2$sex))
data2$asstprof=factor(data2$asstprof)
data2$assocprof=factor(data2$assocprof)

# fit the final model( we can see that both models with categorical and non categorical independent variables yield 
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
                 prof=c(1,0,0), # computing mean salary for professor
                 asst=c(0,1,0),
                 assoc=c(0,0,1))
# run contrast and get results
contrast(leastsquare, contrasts, adjust="none")
