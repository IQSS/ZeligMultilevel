library(ZeligMultilevel)

data(coalition2)
data(voteincome)
data(homerun)

# GAMMA

z.out <- zelig(
               duration ~ invest + fract + polar + numst2 + crisis + tag(1 | country),
               data   = coalition2[1:100, ], 
               model  = "gamma.mixed", 
               family = Gamma(link="log")
               )


x.high <- setx(z.out, numst2 = 1)
x.low <- setx(z.out, numst2 = 0)

s.out <- sim(z.out, x = x.low, x1=x.high, num = 10)

# LOGIT


z.out <- zelig(
               vote ~ education + age + female + tag(1 | state),
               data = voteincome[1:100, ],
               model="logit.mixed"
               )

x.low <- setx(z.out, education=quantile(education, 0.8))
x.high <- setx(z.out, education=quantile(education, 0.2))

s.out <- sim(z.out, x = x.low, x1 = x.high, num = 10)

# LEAST SQUARES

z.out <- zelig(
               income ~ education + age + female + tag(1 | state),
               data=voteincome[1:100, ],
               model="ls.mixed"
               )

x.low <- setx(z.out, education=quantile(education, 0.8))
x.high <- setx(z.out, education=quantile(education, 0.2))

s.out <- sim(z.out, x = x.low, x1 = x.high, num = 10)

summary(z.out)

# POISSON

z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out, num = 10)

summary(s.out)

# PROBIT

z.out <- zelig(vote ~ education + age + female + tag(1 | state),
                   data=voteincome, model="probit.mixed")

x.low <- setx(z.out, education=quantile(voteincome$education, 0.8))
x.high <- setx(z.out, education=quantile(voteincome$education, 0.2))

s.out <- sim(z.out, x = x.low, x1 = x.high)
