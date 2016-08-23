##----- Simulate data

set.seed(101)
# simulate some data for a single predictor, a five level factor, and 
# a grouping term with 10 levels
# taken from the merTools test suite
d <- expand.grid(fac1=LETTERS[1:5], grp=factor(1:10),
                 obs=1:100)
d$y <- simulate(~fac1+(1|grp),family = gaussian,
                newdata=d,
                newparams=list(beta=c(2,1,3,4,7), theta=c(.25),
                               sigma = c(.23)), seed = 4548)[[1]]
# sample it down
subD <- d[sample(row.names(d), 1000),]

##----- lme4 and merTools

g1 <- lmer(y~fac1+(1|grp), data=subD)
summary(g1)
set.seed(101)
merTools::predictInterval(g1, newdata = data.frame(fac1 = "C", grp = "10"))
# fit      upr      lwr
# 1 5.007372 5.288214 4.708537

##----- ZeligMultilevel

g2 <- zlsmixed$new()
g2$zelig(y~fac1+(1|grp), subD)
g2
g2$setx(fac1 = "C", grp = 10)
g2$setx.out$x$mm[1]
# y fac1 grp
# 1 5.007392    C  10
set.seed(101)
g2$sim()
Zelig::statmat(g2$sim.out$x$ev[[1]])
# mean        sd      50%     2.5%   97.5%
# [1,] 5.000066 0.2321668 5.007372 4.505913 5.44128
