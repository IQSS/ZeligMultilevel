# gamma.mixed unit test
library(ZeligMixed)

data(coalition2)

z.out <- zelig(
               duration ~ invest + fract + polar + numst2 + crisis + tag(1 | country),
               data   = coalition2, 
               model  = "gamma.mixed", 
               family = Gamma(link="log")
               )


x.high <- setx(z.out, numst2 = 1)
x.low <- setx(z.out, numst2 = 0)

##  Simulating draws using the default bootstrap method.

s.out <- sim(z.out, x = x.low, x1=x.high)

summary(s.out)
