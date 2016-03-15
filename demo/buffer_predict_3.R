(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)) 
## fixed effects 
vcov(fm1) 
## random effects 
VarCorr(fm1) 
arm::display(fm1)
simulate


d <- sleepstudy
d$Subject <- factor(rep(1:18, each=10))
d
fm1 <- lmer(Reaction ~ Days + (Days|Subject), d)
summary(fm1)
d <- rbind(sleepstudy, sleepstudy)
d$Subject <- factor(rep(1:36, each=10))
d$Reaction <- ifelse(d$Subject %in% 19:36, NA, d$Reaction)
d
d$predicted <- predict (fm1, newdata=d, allow.new.levels=T)
d$simulated <- simulate(fm1, seed=1, newdata=d[-1], re.form=NA,
                        allow.new.levels=T)$sim_1

getME(fm1, "Tlist")
getME(fm1, "theta")
getME(fm1, "k")
