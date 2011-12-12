# ls.mixed unit test

library(ZeligMixed)

data(voteincome)

z.out <- zelig(
               income ~ education + age + female + tag(1 | state),
               data=voteincome,
               model="ls.mixed"
               )

x.low <- setx(z.out, education=quantile(voteincome$education, 0.8))
x.high <- setx(z.out, education=quantile(voteincome$education, 0.2))

s.out <- sim(z.out, x = x.low, x1 = x.high)

summary(z.out)
vcov(z.out)
coef(z.out)
x.low
x.high
