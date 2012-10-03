# poisson.mixed unit test

library(ZeligMixed)

data(homerun)

z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(s.out)
vcov(z.out)
