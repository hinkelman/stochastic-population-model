# R and Rcpp code from http://www.seascapemodels.org/rstats/2017/02/26/speeding-up-sims.html

dyn.load("stochastic-logistic-subroutines.so")

# test if fortran rnorm gives expected results
n = 1000000L
rnorm_result <- .Fortran("rnorm", n, 7.8, 8.3, vector("numeric", n))
# .Fortran returns a list; 4th element of list in this case contains the vector
mean(rnorm_result[[4]])
sd(rnorm_result[[4]])

# mismatched types led to unfilled vectors rather than errors
t <- 100L
yinit <- 1.0
r <- 1.4
k <- 20.0
thetasd <- 0.1

logmodr <- function(t, yinit, r, k, thetasd){
  y <- numeric(t)
  y[1] <- yinit
  theta <- rnorm(t, 0, thetasd)
  for(i in 2:t){
    y[i] <- y[i-1]*(r - r*(y[i-1]/k)) * exp(theta[i])
  }
  return(y)
}

library(Rcpp)
cppFunction("NumericVector logmodc(int t, double yinit, double r,
            double k, double thetasd){
            NumericVector y(t);
            y[0] = yinit;
            NumericVector theta = rnorm(t, 0, thetasd);
            for (int i = 1; i < t; ++i){
            y[i] = y[i-1]*(r - r*(y[i-1] / k)) * exp(theta[i]);
            }
            return y;
            }
            ")

library(microbenchmark)
microbenchmark(
  Rcpp = logmodc(t, yinit, r, k, thetasd),
  Fortran = .Fortran("logmodf", t, yinit, r, k, thetasd, vector("numeric", t)),
  R = logmodr(t, yinit, r, k, thetasd),
  times = 500L
)

rseq <- seq(1.1, 2.2, length.out = 10)
microbenchmark(
  Rcpp = lapply(rseq, function(x) logmodc(t, yinit, x, k, thetasd)),
  Fortran = lapply(rseq, function(x) .Fortran("logmodf", t, yinit, x, k, thetasd, vector("numeric", t))),
  R = lapply(rseq, function(x) logmodr(t, yinit, x, k, thetasd)),
  times = 500L
)

