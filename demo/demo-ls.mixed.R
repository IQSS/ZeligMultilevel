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
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5
z5$setx(Days = 10, Subject = 308)
# z5$setx(Days = 10)
z5$mm.RE
# z5$setx()
z5
z5$setx.out$x$mm[[1]]
z5$sim(num = 500)
z5
plot(z5)
