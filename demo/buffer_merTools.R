library(lme4)
head(InstEval)

m1 <- lmer(y ~ service + lectage + studage + (1|d) + (1|s), data=InstEval)

library(merTools)
fastdisp(m1)

feEx <- FEsim(m1, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))

arm::sim(m1, n.sims = 3)

example1 <- draw(m1, type = 'random')
head(example1)

PI <- predictInterval(m1, newdata = example1, n.sims = 10, returnSims = TRUE)
yhat <- attr(PI, "sim.results")
dim(yhat)
yhat

t(apply(yhat, 1, quantile, prob = c(0.5, 0.95, 0.05), na.rm=TRUE))
apply(yhat, 1, mean, na.rm=TRUE)

# predict it
predict(m1, newdata = example1)
#>    15368
#> 1.988076
# change values
example1$service <- "1"
predict(m1, newdata = example1)
