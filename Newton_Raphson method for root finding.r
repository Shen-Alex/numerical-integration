# Newton-Raphson for 1D equation solution------------------------------------------

# To use the Newton-Raphson method to solve for an equation in R, follow these steps:
# 1.Define the equation you want to solve for in terms of the variable x.
# 2.Define the derivative of the equation with respect to x.
# 3.Choose an initial guess for the solution x0.
# 4.Use the following iterative formula to update your estimate of the solution:
#  x1 = x0 - f(x0) / f'(x0)
# 5.Repeat step 4 until the desired accuracy is achieved or a maximum number of iterations is reached.
# 6.The final value of x is an estimate of the solution to the equation.

# Function to solve using Newton-Raphson method
newtonRaphson <- function(x0, f, f_prime, tol=1e-8, max_iter=100) {
  x <- x0
  for (i in 1:max_iter) {
    x_new <- x - f(x) / f_prime(x)
    if (abs(x_new - x) < tol) {
      break
    }
    x <- x_new
  }
  return(x)
}

# Example function and its derivative
f <- function(x) x^3-5*x^2-3*x+5

f_prime <- function(x) 3*x^2-10*x-3

# Initial guess
x0 <- -2

# Solve using Newton-Raphson method
solution <- newtonRaphson(x0, f, f_prime)

# Print the solution
print(solution)

curve(f(x),-3,6)
abline(h=0, lty = 3, add = T)

library(rootSolve)
uniroot.all(f, lower =-5, upper =10 )

# Newton-Raphson Method for optimization problem ------------------
# https://www.youtube.com/watch?v=8z4I348eayg

# Initial guess
xk <- 0

# Number of iterations
niter <- 100

# Define the function 
myfn <- function(x){
  p <- -0.3*x^4+3*x+4
  return(p)
}

#Newton-Raphson method(1D) using central difference approximation for first derivative and second derivative
# https://lcn.people.uic.edu/classes/che205s17/docs/che205s17_reading_01e.pdf

h = 0.01
for (k in 1:niter){
  Q<- h/2*((myfn(xk+h)-myfn(xk-h))/(myfn(xk+h)-2*myfn(xk)+myfn(xk-h)))
  xnew <- xk-Q
  print(c(k,xk,xnew))
  if (abs(xnew - xk) < 1e-8) {
    break
  }
  xk<-xnew
}


