data("sleepstudy")

##----- Zelig

z5 <- zlsmixed$new()
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)
z5

##----- Wrapper

z.out <- zelig(Reaction ~ Days + (Days | Subject), sleepstudy, model = "ls.mixed")
z.out
