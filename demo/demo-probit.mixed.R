# mixed.zelig unit test

library(ZeligMixed)

data(voteincome)

##----- lmer

library(lme4)
fm1 <- glmer(vote ~ education + age + female + (1 | state),
             data = voteincome,
             family = binomial("probit"))
summary(fm1)
formula(fm1)

##----- Zelig

z5 <- zprobitmixed$new()
z5
z5$zelig(vote ~ education + age + female + (1 | state),
         data = voteincome)
z5
z5$setx()
z5$setx.out$x$mm[[1]]
z5$sim()
z5

