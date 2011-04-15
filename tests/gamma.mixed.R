library(mixed.zelig)

data(coalition2)

z.out <- zelig(
               duration ~ invest + fract + polar + numst2 + crisis + tag(1 | country),
               data   = coalition2, 
               model  = "gamma.mixed", 
               family = Gamma(link="log")
               )

summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
##  comparing the ruling coalition in the minority to the ruling
##  coalition in the majority.
x.high <- setx(z.out, numst2 = 1)
x.low <- setx(z.out, numst2 = 0)

##  Simulating draws using the default bootstrap method.

s.out <- sim(z.out, x = x.low, x1=x.high)

##  Viewing the simulated quantities of interest, for every
##  observation
summary(s.out)
