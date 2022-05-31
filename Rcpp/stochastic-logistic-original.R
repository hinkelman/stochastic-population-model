# code from http://www.seascapemodels.org/rstats/2017/02/26/speeding-up-sims.html

t <- 100
yinit <- 1
k <- 20
thetasd <- 0.1
r <- 1.4

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
  logmodc(t, yinit, r, k, thetasd),
  logmodr(t, yinit, r, k, thetasd)
)

rseq <- seq(1.1, 2.2, length.out = 10)

set.seed(42)
yc <- purrr::map(rseq, ~logmodc(t, yinit, .x, k, thetasd))
set.seed(42)
yr <- purrr::map(rseq, ~logmodr(t, yinit, .x, k, thetasd))

plot(yr[[2]], type = "l", col = "DarkBlue", lwd = 2)
points(yc[[2]], pch = 16, col = "Tomato", cex = 0.8)
legend('topleft', legend = c("R solution","C solution"),
       pch = 16, col = c("DarkBlue", "Tomato"))

mb2 <- microbenchmark(
  purrr::map(rseq, ~logmodc(t, yinit, .x, k, thetasd)),
  purrr::map(rseq, ~logmodr(t, yinit, .x, k, thetasd))
)
mb2


