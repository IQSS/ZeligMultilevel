# ls.mixed unit test

# library(ZeligMixed)

library(Zelig)

##----- lmer
library(lme4)
data("sleepstudy")
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
fm1
summary(fm1)
formula(fm1)
head(model.matrix(fm1))
head(model.frame(fm1))
head(model.matrix(fm1, type = "fixed"))
head(model.matrix(fm1, type = "random"))
fixef(fm1)
ranef(fm1)
formula(fm1, fixed.only = TRUE)

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5
z5$setx(Days = 5)
z5
z5$sim()

z5$sim.out
# z5$simparam$simparam[[1]][[1]]
z5
vcov(fm1)

data("voteincome")

zz <- zlsmixed$new()
zz$zelig(income ~ education + age + female + (1 | state), data = voteincome)
zz
zz$setx(education = quantile(voteincome$education, 0.8))
zz$setx1(education = quantile(voteincome$education, 0.2))
zz$sim()
zz$simparam$simparam
zz
plot(zz)

summary(z.out)
