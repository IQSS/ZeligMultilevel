# mixed.zelig unit test

library(ZeligMultilevel)

data(voteincome)
dim(voteincome)

##----- lmer

library(lme4)
fm1 <- glmer(vote ~ education + age + female + (1 | state),
             data = voteincome,
             family = binomial("logit"))
summary(fm1)
formula(fm1)
library(merTools)
set.seed(1234)
outs <- predictInterval(fm1, draw(fm1), type = "probability",
                        returnSims = TRUE, n.sims = 10)
outs
ev <- as.matrix(t(attr(outs, "sim.results")))
ev
fm1@resp$family$linkinv(ev)

apply(outs, 2, merMod@resp$family$linkinv)

ev
sim <- binomial()$linkinv(ev)
mean(sim)
max(sim)
min(sim)

set.seed(1234)
outs <- predictInterval(fm1, draw(fm1), type = "linear.prediction",
                        returnSims = TRUE, n.sims = 10)
outs
ev <- as.matrix(t(attr(outs, "sim.results")))
ev
sim <- binomial()$linkinv(ev)
set.seed(1234)
rbinom(10, 1, sample(sim, 1))

##----- Zelig

data(voteincome)

z5 <- zlogitmixed$new()
z5
z5$zelig(vote ~ education + age + female + (1 | state),
         data = voteincome)
z5

z5$setx()
z5$sim(num = 500)
z5
plot(z5)

z5$setx(state = "AR")
z5$mm.RE
z5$sim(num = 5)

z5$setx.out$x$mm[[1]]
z5$sim(num = 5)
z5$sim
z5$simx1
z5$simparam$simparam
plot(z5)
z5$linkinv(z5$sim.out$x$ev[[1]])
