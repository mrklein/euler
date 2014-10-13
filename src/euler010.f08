program euler010
  implicit none

  real :: start, finish
  integer :: i
  integer(kind=8) :: n

  call cpu_time(start)
  do i = 1, 200
    call solution(n)
  end do
  call cpu_time(finish)

  print '("Result: ", I15)', n
  print '("Elapsed time: ", F9.6, " seconds")', finish - start

  call cpu_time(start)
  do i = 1, 200
    call solution1(n)
  end do
  call cpu_time(finish)

  print '("Result: ", I15)', n
  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine solution(n)
    use euler_mod, only: primes_below

    implicit none
    integer(kind=8), intent(out) :: n

    n = sum(primes_below(1999999_8))
  end subroutine

  subroutine solution1(n)
    use euler_mod, only: primes_below_sub

    implicit none

    integer(kind=8), dimension(:), allocatable :: r
    integer(kind=8), intent(out) :: n

    call primes_below_sub(1999999_8, r)
    n = sum(r)
  end subroutine
end program
