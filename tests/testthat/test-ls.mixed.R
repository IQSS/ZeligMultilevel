# REQUIRE TEST for ls.mixed ----------------------------------------------------

test_that('REQUIRE TEST for ls.mixed', {
data(voteincome)
  z.out <- zelig(income ~ education + age + female + (1 | state),
                data = voteincome, model = "ls.mixed")

  x.high <- setx(z.out, education = quantile(voteincome$education, 0.8))
  x.low <- setx(z.out, education = quantile(voteincome$education, 0.2))

  s.out <- sim(z.out, x = x.high, x1 = x.low)
  summary(s.out)
  plot(s.out)
})

