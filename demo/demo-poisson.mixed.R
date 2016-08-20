data(homerun)

##----- lmer

library(lme4)
fm1 <- glmer(homeruns ~ player + (player - 1 | month), homerun, family = poisson("log"))
summary(fm1)
formula(fm1)

##----- Zelig
data(homerun)
z5 <- zpoissonmixed$new()
z5
z5$zelig(homeruns ~ player + (player - 1 | month), homerun)
z5
z5$setx()
z5$setx(player = "McGwire")
# z5$setx(month = "June")
z5
z5$setx()
z5$setx.out$x[[1]]
z5$sim(100)
z5
