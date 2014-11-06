program euler012
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F16.12, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod, only: primes_below

    implicit none

    integer(kind=8), dimension(:), allocatable :: p, w
    integer(kind=8) :: n, i, j, k, c

    n = 1
    i = 1
    do
      p = primes_below(int(sqrt(real(n)), 8))
      allocate(w(size(p)))
      w = 0
      k = n
      j = 1
      do
        if (mod(k, p(j)) == 0) then
          w(j) = w(j) + 1
          k = k / p(j)
        else
          j = j + 1
        end if
        if (j > size(p)) exit
      end do

      if (k /= 1) then
        c = 2*product(w + 1)
      else
        c = product(w + 1)
      end if

      deallocate(w)
      if (c > 500) exit

      i = i + 1
      n = n + i
    end do

    print *, n
  end subroutine
end program
