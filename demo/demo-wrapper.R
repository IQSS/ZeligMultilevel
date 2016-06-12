data("sleepstudy")

##----- Zelig

z5 <- zlsmixed$new()
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)

##----- Wrapper

z.out <- zelig(Reaction ~ Days + (Days | Subject), sleepstudy, model = "ls-mixed")
z5
z5$setx(Days = 10, Subject = 309)
# z5$setx(Days = 10)
z5$mm_RE
# z5$setx()
z5
z5$setx.out$x$mm[[1]]
z5$sim(num = 100)
z5
plot(z5)