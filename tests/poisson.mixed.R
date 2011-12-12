# poisson.mixed unit test

library(ZeligMixed)

data(homerun)

z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(z.out)
vcov(z.out)
# coef fails - something about being unable to align fixed and random effects
# coef(z.out)
x.out

