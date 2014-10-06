program euler006
  use euler_mod

  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine solution()
    implicit none

    integer(kind=8), parameter :: N = 100_8
    integer(kind=8) :: sum_of_squares = 0, square_of_sum = 0, i

    do i = 1_8, N
      sum_of_squares = sum_of_squares + i*i
      square_of_sum = square_of_sum + i
    end do

    square_of_sum = square_of_sum * square_of_sum

    print *, abs(sum_of_squares - square_of_sum)
  end subroutine
end program
