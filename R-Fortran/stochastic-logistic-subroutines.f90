! R help recommended that subroutines should not have underscores in the names
! compile fortran code for use with R: R CMD SHLIB stochastic-logistic-subroutines.f90

! https://masuday.github.io/fortran_tutorial/random.html
subroutine rstduniform(u)
  implicit none
  double precision,intent(out) :: u
  double precision :: r
  call random_number(r)
  u = 1 - r
end subroutine rstduniform

! modified from https://masuday.github.io/fortran_tutorial/random.html
! based on https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
subroutine rnormal(mu, sigma, x)
  implicit none
  double precision,intent(in) :: mu, sigma
  double precision,intent(out) :: x
  double precision,parameter :: pi=3.14159265
  double precision :: u1,u2
  call rstduniform(u1)
  call rstduniform(u2)
  x = mu + sigma * cos(2 * pi * u1) * sqrt(-2 * log(u2))
end subroutine rnormal

! rnorm is used to test rnormal from R; not used in logmodf
subroutine rnorm(reps, mu, sigma, arr)
  implicit none
  integer :: i
  integer, intent(in) :: reps
  double precision, intent(in) :: mu, sigma
  double precision, intent(inout) :: arr(reps)
  do i=1,reps
    call rnormal(mu, sigma, arr(i))
  end do
end subroutine rnorm

subroutine logmodf(t, yinit, r, k, thetasd, y)
  implicit none
  integer :: i
  integer, intent(in) :: t
  double precision, intent(in) :: yinit, r, k, thetasd
  double precision :: mu, theta
  double precision, intent(inout) :: y(t)
  mu = 0.0 
  y(1) = yinit        
  do i=2,t
    call rnormal(mu, thetasd, theta)
    y(i) = y(i-1) * (r - r * (y(i-1) / k)) * exp(theta)
  end do
end subroutine logmodf
