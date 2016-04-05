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

formula(fm1, fixed.only = TRUE)
sims <- arm::sim(fm1, 5)
sims
head(f <- fitted(sims, fm1))
f

fixed_effects <- fixef(fm1)
random_effects <- ranef(fm1)

sim_fixed_effects <- sims@fixef
sim_random_effects <- sims@ranef

# sim_random_effects$Subject[, "308", ]
unique(rownames(Zt <- getME(fm1, "Zt")))

dim(sim_random_effects$Subject)


mm <- colMeans(model.matrix(fm1))
betas <- sim_fixed_effects %*% mm # betas

names(sim_random_effects)

si <- sim_random_effects$Subject[, "308", ]
colnames(Zt)
zi <- Zt[rownames(Zt) == "308", ]
zi
z <- zi[, apply(zi, 2, function(x) sum(x) != 0)]

p <- si %*% z

t(as.vector(betas) + as.matrix(p))

library(lme4)
data(sleepstudy)
sleepstudy <- sleepstudy[sleepstudy$Subject %in% c(308, 309), ]
rownames(sleepstudy) <- NULL
fm2<-lmer(Reaction~Days+(Days|Subject), sleepstudy)

Xi <- getME(fm2,"mmList")
Xi

data("Pixel", package="nlme")
mform <- formula(fm1)
(bar.f <- findbars(mform)) # list with 3 terms
mf <- model.frame(subbars(mform),data=sleepstudy)
rt <- mkReTrms(bar.f,mf)
names(rt)
rt$Ztlist

mf <- model.frame(subbars(),data=sleepstudy)
