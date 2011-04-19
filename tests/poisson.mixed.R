# poisson.mixed unit test

library(mixed.zelig)

data(homerun)

z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(z.out)
vcov(z.out)
coef(z.out)
x.out

