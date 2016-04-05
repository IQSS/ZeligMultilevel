# ls.mixed unit test

# library(ZeligMixed)

library(Zelig)

##----- lmer
library(lme4)
library(arm)
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
s <- arm::sim(fm1, 5)
s
head(fitted(s, fm1))
fitted

##----- Zelig

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5
# z5$setx(Days = 5)
z5$setx()
z5

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5$setx()
z5$sim(num = 10, group = "none")
z5
plot(z5)
z5$simparam$simparam
z5$sim.out$x$pv[[1]]

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5
# z5$setx(Days = 5)
z5$setx()
z5
z5$sim(num = 10, group = "all")
z5
z5$simparam$simparam
z5$sim.out$x$pv[[1]]
plot(z5)

z.out <- z5$zelig.out$z.out[[1]]
s <- arm::sim(z.out, 5)
f <- fitted(s, z.out)
summary(colMeans(f))

z5$sim(num = 10, group = "all", group.value = "352")
z5
z5$simparam$simparam
V.beta
z5$sim.out
z5$sim.out$x$ev
z5$sim.out$x$pv
p <- z5$sim.out$x$pv[[1]]
p[, "352", drop = FALSE]

as.matrix(p[, colnames(p) == "352"])
colnames(p)
rownames(z5$sim.out$x$ev)
z5
z.out <- z5$zelig.out$z.out[[1]]
arm::sim(z.out, 5)

fixef(z.out)
ranef(z.out)

mm <- colMeans(model.matrix(z.out))
mm

plot(z5)
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
