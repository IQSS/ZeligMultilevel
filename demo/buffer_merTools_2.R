set.seed(271828)
data(sleepstudy)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
display(fm1)

PI.time <- system.time(
  PI <- predictInterval(merMod = fm1, newdata = sleepstudy,
                        level = 0.95, n.sims = 1000,
                        stat = "median", type="linear.prediction",
                        include.resid.var = TRUE, returnSims = TRUE)
)

PI.time
head(PI)

fe <- fixef(fm1)
fe["Days"] <- 8
fe

PI2 <- predictInterval(merMod = fm1, newdata = sleepstudy,
                       level = 0.95, n.sims = 10,
                       stat = "median", type="linear.prediction",
                       include.resid.var = TRUE, returnSims = TRUE, fe.tmp = fe
)

attr(PI2, "sim.results")

PI2

library(ggplot2);
ggplot(aes(x=1:30, y=fit, ymin=lwr, ymax=upr), data=PI[1:30,]) +
  geom_point() +
  geom_linerange() +
  labs(x="Index", y="Prediction w/ 95% PI") + theme_bw()
