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
head(f <- fitted(s, fm1))
fitted

fixed_effects <- fixef(fm1)
random_effects <- ranef(fm1)

sim_fixed_effects <- sims@fixef
sim_random_effects <- sims@ranef

sim_random_effects$Subject[, "308", ]
getME(fm1, "Zt")

mm <- colMeans(model.matrix(fm1))

betas <- sim_fixed_effects %*% mm # betas

f

b <- cbind(sleepstudy, f)
b[b$Subject == "308", ]

si <- sim_random_effects$Subject[, "308", ]
colnames(Zt)
Zt[, names(Zt) == "309"]
zi <- Zt[rownames(Zt) == "309", ]

p <- si %*% zi
tp <- t(p[, 1:10])

as.vector(betas) + as.matrix(tp)

library(lme4)
data(sleepstudy)
sleepstudy <- sleepstudy[sleepstudy$Subject %in% c(308, 309), ]
rownames(sleepstudy) <- NULL
fm2<-lmer(Reaction~Days+(Days|Subject), sleepstudy)

Xi <- getME(fm2,"mmList")
Xi

getME(fm2, "Zt")
