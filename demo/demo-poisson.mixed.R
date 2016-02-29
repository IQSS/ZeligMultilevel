data(homerun)

z5 <- zpoissonmixed$new()
z5
z5$zelig(homeruns ~ player + (player - 1 | month), homerun)
z5
z5$setx(player = "McGwire")
z5
z5$setx()
z5$sim()
z5
