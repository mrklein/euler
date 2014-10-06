module euler_mod
  public :: factorial
contains
  pure function factorial(n) result (r)
    implicit none

    integer, intent (in) :: n
    integer :: r, i

    if (n < 1) then
      r = 1
    endif

    do i = 2, n
      r = r * i
    end do
  end function

  pure function is_palindrome(n) result (r)
    implicit none

    integer, intent (in) :: n
    logical :: r

    integer :: t, rn, d

    rn = 0
    t = n
    do while (t > 0)
      d = mod(t, 10)
      rn = 10*rn + d
      t = t / 10
    end do

    r = n == rn
  end function

  pure function is_prime(n) result (r)
    implicit none

    integer, intent(in) :: n
    logical :: r

    integer i, t

    t = int(sqrt(real(n)))

    r = .true.
    do i = 2, t
      if (mod(n, i) == 0) then
        r = .false.
        exit
      end if
    end do
  end function
end module
