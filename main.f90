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
  A = 2d0 * A - 1d0

  call mpi_init(mpiErr)

  time1 = mpi_wtime()
  call GetMaxCoordinates(A, x1, y1, x2, y2)
  time2 = mpi_wtime()

  write(*,*) 'Coordinates: ', x1, y1, x2, y2
  write(*,*) 'Time: ', time2 - time1

  call mpi_finalize(mpiErr)

end program
