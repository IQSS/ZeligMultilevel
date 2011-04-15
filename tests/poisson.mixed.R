library(mixed.zelig)

data(homerun)

z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")

summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),

x.out <- setx(z.out)

##  Simulating draws using the default bootstrap method.

s.out <- sim(z.out, x = x.out)


##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
