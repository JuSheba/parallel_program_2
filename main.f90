program main
  use :: mpi
  use :: Task
  implicit none
  integer :: x1, y1, x2, y2
  real(8), allocatable :: A(:,:)
  real(8) :: time1, time2
  integer(4) :: mpiErr, mpiSize, mpiRank

  allocate(A(1337, 1337))

  call random_number(A)
  A = 2*A - 1d0

  call mpi_init(mpiErr)

  

  call mpi_finalize(mpiErr)
