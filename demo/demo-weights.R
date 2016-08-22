# ls.mixed unit test

##----- lmer

library(lme4)
data("sleepstudy")
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)
formula(fm1)

##----- Zelig

z5 <- zlsmixed$new()
z5
set.seed(1)
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy, weights = sample(1:3, nrow(sleepstudy), replace = TRUE))
z5
z5$setx(Days = 8)
z5
z5$sim(num = 300)
z5
plot(z5)
