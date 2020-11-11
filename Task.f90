module Task
  use :: mpi
  implicit none
  contains

  subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
    implicit none
    real(8), intent(in), dimension(:,:) :: A
    integer(4), intent(out) :: x1, y1, x2, y2
    integer(4) :: n, L, R, Up, Down, m, tmp
    integer(4) :: mpiErr, mpiSize, mpiRank, mpiMaxRank, mpiGlobalMaxRank
    real(8), allocatable :: current_column(:)
    real(8) :: current_sum, max_sum, max_sum_glob

    call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
    call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

    m = size(A, dim=1)
    n = size(A, dim=2)

    allocate(current_column(m))

    x1=1
    y1=1
    x2=1
    y2=1
    max_sum = A(1,1)

    do L = mpiRank + 1, n, mpiSize
      current_column = A(:, L)

      do R = L, n
        if (R > L) then
          current_column = current_column + A(:, R)
        endif

        call FindMaxInArray(current_column, current_sum, Up, Down)

        if (current_sum > max_sum) then
          max_sum = current_sum
          x1 = Up
          x2 = Down
          y1 = L
          y2 = R
        endif
      end do
    end do
    deallocate(current_column)

    call mpi_reduce(max_sum, max_sum_glob, 1, MPI_REAL8,&
                    MPI_MAX, 0, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(max_sum_glob, 1, MPI_REAL8, 0, MPI_COMM_WORLD, mpiErr)
    mpiMaxRank = 0
    if(max_sum == max_sum_glob) then
      mpiMaxRank = mpiRank
    end if

    call mpi_reduce(mpiMaxRank, mpiGlobalMaxRank, 1, MPI_INTEGER4,&
                    MPI_MAX, 0, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(mpiGlobalMaxRank, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(x1, 1, MPI_INTEGER4, mpiGlobalMaxRank, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(y1, 1, MPI_INTEGER4, mpiGlobalMaxRank, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(x2, 1, MPI_INTEGER4, mpiGlobalMaxRank, MPI_COMM_WORLD, mpiErr)
    call mpi_bcast(y2, 1, MPI_INTEGER4, mpiGlobalMaxRank, MPI_COMM_WORLD, mpiErr)

  end subroutine

  subroutine FindMaxInArray(A, Summ, Up, Down)
    implicit none
    real(8), intent(in), dimension(:) :: A
    integer(4), intent(out) :: Up, Down
    real(8), intent(out) :: Summ
    real(8) :: cur_sum
    integer(4) :: minus_pos, i

    Summ = A(1)
    Up = 1
    Down = 1
    cur_sum = 0
    minus_pos = 0

    do i=1, size(A)
      cur_sum = cur_sum + A(i)
      if (cur_sum > Summ) then
        Summ = cur_sum
        Up = minus_pos + 1
        Down = i
      endif

      if (cur_sum < 0) then
        cur_sum = 0
        minus_pos = i
      endif
    enddo

  end subroutine
end module
