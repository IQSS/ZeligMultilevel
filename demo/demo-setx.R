data(sleepstudy)
z5 <- zlsmixed$new()
z5$zelig(Reaction ~ Days + (Days | Subject), sleepstudy)

set.seed(1)
z5$setx(Days = 8, Subject = "308")
z5$sim(num = 10000)
z5
# sim x :
#   -----
#   ev
# mean       sd      50%     2.5%    97.5%
# [1,] 335.1931 13.33051 335.1865 308.4308 360.8223
# pv
# mean       sd      50%     2.5%   97.5%
# [1,] 337.9656 28.35018 337.8895 282.2901 393.376


set.seed(1)
z5$setx(Days = 8)
z5$sim(num = 10000)
z5
# sim x :
#   -----
#   ev
# mean       sd      50%     2.5%    97.5%
# [1,] 335.1931 13.33051 335.1865 308.4308 360.8223
# pv
# mean       sd      50%     2.5%    97.5%
# [1,] 335.508 26.04314 335.5407 284.0255 387.2522