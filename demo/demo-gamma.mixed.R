# gamma.mixed unit test
library(ZeligMultilevel)

data(coalition2)

##----- lmer

library(lme4)
fm1 <- glmer(duration ~ invest + fract + polar + numst2 + crisis + (1 | country), coalition2,
             family = Gamma("log"))
summary(fm1)
formula(fm1)

##----- Zelig

z5 <- zgammamixed$new()
z5
z5$zelig(duration ~ invest + fract + polar + numst2 + crisis + (1 | country), coalition2)
z5
z5$setx()
z5$setx.out$x$mm[[1]]
z5$sim(300)
z5
plot(z5)
