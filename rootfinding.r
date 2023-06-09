
# Newton-Raphson method for root solving

f <- function(x){
  3*x^3+2*x^2-3*x
}

f_prime <- function(x){
  9*x^2+4*x-3
}

curve(f, -3,3)
abline(h=0, lty =3)

library(rootSolve)
uniroot.all(f, c(-3,3)

x<--0.2
iterations <-0
tol <- 1e-8
while (abs(f(x))>tol) {
  x <- x-f(x)/f_prime(x)
  iterations <-iterations+1
}

x




# fixed point method
fixedpoint <- function(fun, x0, tol=1e-07, niter=500){
  ## fixed-point algorithm to find x such that fun(x) == x
  ## assume that fun is a function of a single variable
  ## x0 is the initial guess at the fixed point
  
  xold <- x0
  xnew <- fun(xold)
  for (i in 1:niter) {
    xold <- xnew
    xnew <- fun(xold)
    if ( abs((xnew-xold)) < tol )
      return(xnew)
  }
  stop("exceeded allowed number of iterations")
}

f<- function(x) log(x)-exp(-x)
gfun <- function(x) x-log(x)+exp(-x)
fixedpoint(gfun, 1)

curve(f, from=0, to =3)
abline(h=0, add=T, col ="lightgray",lty = 2)

uniroot(f, c(1,2))

#secant method
secant <- function(fun, x0, x1, tol=1e-07, niter=500){
  for ( i in 1:niter ) {
    x2 <- x1-fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
    if (abs(fun(x2)) < tol)
      return(x2)
    x0 <- x1
    x1 <- x2
  }
  stop("exceeded allowed number of iteractions")
}

f<- function(x) log(x)-exp(-x)
secant(f, x0=1, x1=2)
