program euler012
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F16.12, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod, only: count_divisors

    implicit none

    integer(kind=8), dimension(:), allocatable :: r
    integer(kind=8) :: i

    do i = 1_8, 100_8
      print *, i, count_divisors(i)
    end do
  end subroutine
end program
