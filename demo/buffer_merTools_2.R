set.seed(271828)
data(sleepstudy)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
display(fm1)
x.out <- draw(fm1, "average")
x.out
wiggle(x.out, "Days", 5)
set.seed(1234)
sleepstudy$Group <- rbinom(nrow(sleepstudy), 1, .4) + 1

z5 <- zlsmixed()
z5$zelig(Reaction ~ Days + I(Days^2) + (Days|Subject), by = "Group", data = sleepstudy)
z5

z5$setx()
z5$setx.out$x$mm
z5$sim(5)

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy, by = "Group")
z5

PI <- predictInterval(merMod = fm1, newdata = sleepstudy,
                      level = 0.95, n.sims = 5,
                      stat = "median", type = "linear.prediction",
                      include.resid.var = TRUE, returnSims = TRUE)

head(PI)
fe <- fixef(fm1)
fe["Days"] <- 8
fe


terms(fm1, "variables")

PI <- predictInterval(merMod = fm1, newdata = x.out,
                      level = 0.95, n.sims = 5,
                      stat = "median", type="linear.prediction",
                      include.resid.var = TRUE, returnSims = TRUE)
PI

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5
z.out <- z5$zelig.out$z.out[[1]]
z.out
z5$setx()
z5$setx.out$x$mm[[1]]

formula(z.out, fixed.only = TRUE)
formula(z.out, randon.only = TRUE)
names(ranef(fm1))


x.out <- cbind(draw(fm1, type = "average"), extra = 75)
x.out
wiggle(x.out, var = "Days", values = 5)
wiggle(x.out, var = "Subject", values = 310)

x.out
reduce(dataset = "MEANINGLESS ARGUMENT", list(Days = 5), 
       formula = formula(fm1), 
       data = x.out)

predictInterval(merMod = fm1, newdata = x.out,
                level = 0.95, n.sims = 5,
                stat = "median", type="linear.prediction",
                include.resid.var = TRUE, returnSims = TRUE)

# z5$setx(Days = 5)
z5$setx()
x.out <- z5$setx.out$x$mm[[1]]
x.out


# http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

x.out <- NULL
for (i in 1:100)
  x.out <- rbind(x.out, sleepstudy[1, ])

PI2 <- predictInterval(merMod = fm1, newdata = x.out,
                       level = 0.95, n.sims = 500,
                       stat = "median", type="linear.prediction",
                       include.resid.var = TRUE, returnSims = TRUE)

yhat <- attr(PI2, "sim.results")
head(yhat)
dim(yhat)
yhat
mean(yhat)

hist(apply(yhat, 2, mean))

library(ggplot2);
ggplot(aes(x=1:30, y=fit, ymin=lwr, ymax=upr), data=PI[1:30,]) +
  geom_point() +
  geom_linerange() +
  labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()

merTools:::predictInterval
