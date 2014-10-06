program euler001
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

    integer, parameter :: N = 999
    integer :: i, r

    r = 0

    do i = 1, N
      if (mod(i, 3) == 0 .or. mod(i, 5) == 0) then
        r = r + i
      endif
    end do

    print *, r
  end subroutine
end program
