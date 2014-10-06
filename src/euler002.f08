program euler002
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod

    implicit none

    integer, parameter :: N = 4000000
    integer :: a = 1, b = 2, c, r

    r = 0
    do while (a < N)
      if (mod(a, 2) == 0) r = r + a
      c = a + b
      a = b
      b = c
    enddo

    print *, r
  end subroutine
end program
