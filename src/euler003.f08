program euler003
  implicit none

  real :: start, finish
  integer i

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod

    implicit none

    integer(kind=8), parameter :: N = 600851475143_8
    integer(kind=8) :: d = 2_8, t = N, r

    do while (t > 1)
      if (mod(t, d) == 0) then
        t = t / d
        r = max(r, d)
        d = 2_8
      else
        d = d + 1
      endif
    end do

    print *, r
  end subroutine
end program
