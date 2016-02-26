# ls.mixed unit test

# library(ZeligMixed)

library(Zelig)
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

##----- lmer

library(lme4)

data("sleepstudy")

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

fm1
summary(fm1)

formula(fm1)

head(model.matrix(fm1))
head(model.frame(fm1))

head(model.matrix(fm1, type = "fixed"))
head(model.matrix(fm1, type = "random"))

lme4:::model.matrix.merMod

fixef(fm1)
ranef(fm1)

z5 <- zlsmixed$new()
z5
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5

data(voteincome)
z5 <- zlsmixed()
z5$zelig(income ~ education + age + female + (1 | state), voteincome)
z5

ff <- lmer(income ~ education + age + female + (1 | state), voteincome)
summary(ff)

head(model.matrix(ff))

debug(z5$set)
z5$setx()

.self <- z5

.self$data %>%
  group_by_(.self$by) %>%
  do(mm = model.matrix(.self$formula, reduce(dataset = ., s, formula = .self$formula, data = .self$data)))

z.out <- .self$zelig.out$z.out[[1]]

model.matrix(formula(z.out), voteincome)

ff@frame

ff@frame$income

lme4:::model.matrix.merMod
