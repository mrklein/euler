program euler010
  implicit none

  real :: start, finish
  integer :: i
  integer(kind=8) :: n

  call cpu_time(start)
  call solution(n)
  call cpu_time(finish)

  print '("Result: ", I15)', n
  print '("Elapsed time: ", F15.11, " seconds")', finish - start
contains
  subroutine solution(n)
    use euler_mod, only: primes_below

    implicit none
    integer(kind=8), intent(out) :: n

    n = sum(primes_below(1999999_8))
  end subroutine
end program
