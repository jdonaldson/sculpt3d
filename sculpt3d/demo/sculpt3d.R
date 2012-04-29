### Name: sculpt3d
### Title: Example Plot

x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
sculpt3d(x, y, z, col=rainbow(1000), type='s', alpha = .5, radius = runif(1000)/5)



