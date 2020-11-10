module Task
  use :: mpi
  implicit none
  contains

  subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
    implicit none
    real(8), intent(in), dimension(:,:) :: A
    integer(4), intent(out) :: x1, y1, x2, y2
    integer(4) :: n, L, R, Up, Down, m, tmp
    integer(4) :: mpiErr, mpiSize, mpiRank
    integer(4), dimension(1) :: mpiRankMax
    real(8), allocatable :: current_column(:)
    real(8) :: current_sum
    real(8), dimension(1) :: max_sum

    call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)   ! кол-во связанных коммуникатором проц-ов
    call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)   ! номер процесса в комм-ре (от 0 до size-1)

    m = size(A, dim=1)
    n = size(A, dim=2)

    allocate(current_column(m))

    x1=1
    y1=1
    x2=1
    y2=1
    max_sum(1)= A(1,1)

    do L = mpiRank + 1, n, mpiSize    ! каждому процессу по итерации и так до конца (+1, т.к. от нуля)
      current_column = A(:, L)

      do R = L, n
        if (R > L) then
          current_column = current_column + A(:, R)
        endif

        call FindMaxInArray(current_column, current_sum, Up, Down)

        if (current_sum > max_sum(1)) then
          max_sum(1) = current_sum
          x1 = Up
          x2 = Down
          y1 = L
          y2 = R
        endif
      end do
    end do
    deallocate(current_column)

    call mpi_reduce(max_sum(1), max_sum(2), 1, MPI_REAL8, MPI_MAX, 0, MPI_COMM_WORLD, mpiErr) ! можно и Gather, но имхо в книжке понятнее написан  Reduce

    

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
