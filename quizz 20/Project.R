
#function to compute epsilon

epsilon = function(n,lambda,p) { 
  a = 2*((log(n))^0.5)
  b = n^0.5
  f=dexp(qexp(p,rate=lambda),rate=lambda)
  return(a/(f*b))
}

# other way to compute epsilon analytically
epsilon2 = function(n,lambda,p) {
  a = 2*((log(n))^0.5)
  b = n^0.5
  f= lambda * (1-p)
  return(a/(f*b))
}

#plot epsilon with different lambdas

#variables

p       = 0.9

nmax    = 500
nmin    = 2
steps   = 50

lmax    = 6
lmin    = 1
lsteps  = 5

n       = seq(nmax, nmin, by = -(nmax - nmin)/(steps))
lambdas = seq(lmax, lmin, by = -(lmax - lmin)/lsteps)

#prepare value storage
espsilon_n  = matrix(0, (steps + 1), length(lambdas))

# calculation of epsilon for the different n at constant lambda
for (i in 1:length(lambdas)) {
  espsilon_n[, i] = epsilon(n, lambdas[i], p)
}
dev.new()
plot(n, espsilon_n[, 1], col = "red", xlab = "n", ylab = "epsilon_n", main = paste(c("epsilon for different exp(lambda)","p=", p)) , ylim = c(0,max(espsilon_n)))
lines(n, espsilon_n[, 1], col = "red")

points(n, espsilon_n[, 2], col = "orange")
points(n, espsilon_n[, 3], col = "yellow")
points(n, espsilon_n[, 4], col = "green")
points(n, espsilon_n[, 5], col = "blue")
points(n, espsilon_n[, 6], col = "purple")

lines(n, espsilon_n[, 2], col = "orange", lwd = 2)
lines(n, espsilon_n[, 3], col = "yellow", lwd = 2)
lines(n, espsilon_n[, 4], col = "green", lwd = 2)
lines(n, espsilon_n[, 5], col = "blue", lwd = 2)
lines(n, espsilon_n[, 6], col = "purple", lwd = 2)

legend("topright", c(paste("lambda =", round(lambdas[1:6], digits = 2))), col = c("red","orange","yellow","green","blue", 
                                                                            "purple" ), lty = 1) 

#plot epsilon with different p

#variables

lambda     = 1

nmax   = 500
nmin   = 2
steps  = 50

pmax   = 0.95
pmin   = 0.70
psteps = 5

n      = seq(nmax, nmin, by = -(nmax - nmin)/(steps))
p      = c(0.05,0.10,0.50,0.90,0.95)

#prepare value storage
espsilon_p  = matrix(0, (steps + 1), length(p))

# calculation of epsilon for the different n at constant p
for (i in 1:length(p)) {
  espsilon_p[, i] = epsilon(n, lambda, p[i])
}

dev.new()
plot(n, espsilon_p[, 1], col = "red", xlab = "n", ylab = "epsilon_n", main = paste(c("epsilon for different quantiles","lambda=", lambda)), ylim = c(0,max(espsilon_p)))
lines(n, espsilon_p[, 1], col = "red")

points(n, espsilon_p[, 2], col = "orange")
points(n, espsilon_p[, 3], col = "yellow")
points(n, espsilon_p[, 4], col = "green")
points(n, espsilon_p[, 5], col = "blue")


lines(n, espsilon_p[, 2], col = "orange", lwd = 2)
lines(n, espsilon_p[, 3], col = "yellow", lwd = 2)
lines(n, espsilon_p[, 4], col = "green", lwd = 2)
lines(n, espsilon_p[, 5], col = "blue", lwd = 2)


legend("topright", c(paste("p =", round(p[1:5], digits = 2))), col = c("red","orange","yellow","green","blue", 
                                                                                  "purple" ), lty = 1) 