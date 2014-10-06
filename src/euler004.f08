program euler004

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

    integer :: s = 100, e = 999, i, j, n, r = 0

    do i = s, e
      do j = i, e
        n = i*j
        if (is_palindrome(n)) r = max(r, n)
      end do
    end do

    print *, r
  end subroutine
end program
