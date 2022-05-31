! compile this program: gfortran stochastic-logistic.f90 -o logmod
! run the compiled program, for example: ./logmod 20 1.0 1.4 20.0 0.1

program main
  implicit none

  character(len=20), dimension(5) :: args
  integer :: i, t, n
  real, allocatable :: y(:)
  real :: yinit, r, k, thetasd, theta

  if (command_argument_count().NE.5) then
    write(*,*) "ERROR: five command-line arguments required"
    stop
  end if

  do i=1,command_argument_count()
    call get_command_argument(i, args(i))
  end do

  read(args(1), *) t
  read(args(2), *) yinit
  read(args(3), *) r
  read(args(4), *) k
  read(args(5), *) thetasd

  allocate(y(t))
  y(1) = yinit        
  do n=2,t
    call random_normal(0.0,thetasd,theta)
    y(n) = y(n-1) * (r - r * (y(n-1) / k)) * exp(theta)
  end do
  print *, y
  deallocate(y)
  
end program main

! https://masuday.github.io/fortran_tutorial/random.html
subroutine random_stduniform(u)
  implicit none
  real,intent(out) :: u
  real :: r
  call random_number(r)
  u = 1 - r
end subroutine random_stduniform

! modified from https://masuday.github.io/fortran_tutorial/random.html
! based on https://www.cse.wustl.edu/~jain/books/ftp/ch5f_slides.pdf
subroutine random_normal(mu,sigma,x)
  implicit none
  real,intent(in) :: mu,sigma
  real,intent(out) :: x
  real,parameter :: pi=3.14159265
  real :: u1,u2
  call random_stduniform(u1)
  call random_stduniform(u2)
  x = mu + sigma * cos(2 * pi * u1) * sqrt(-2 * log(u2))
end subroutine random_normal
