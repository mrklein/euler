module euler_mod
  implicit none

  type :: int_pair_t
    integer(kind=8) :: n1
    integer(kind=8) :: n2
  end type

  public :: factorial, is_palindrome, is_prime, primes_below, &
    count_divisors
contains
  pure function factorial(n) result (r)
    implicit none

    integer(kind=8), intent (in) :: n
    integer(kind=8) :: r, i

    if (n < 1_8) then
      r = 1
    endif

    do i = 2, n
      r = r * i
    end do
  end function

  !> Checks if n is palindromic number
  !! @param n [in] number to check
  !! @returns if n is palindromic
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

  !> Naive prime check function
  !! @param n [in] number to check
  !! @returns if n is prime
  pure function is_prime(n) result (r)
    implicit none

    integer, intent(in) :: n
    logical :: r

    integer i, t

    t = int(sqrt(real(n))) + 1

    r = .true.
    do i = 2, t
      if (mod(n, i) == 0) then
        r = .false.
        exit
      end if
    end do
  end function

  !> Generate an array of primes smaller than n using sieve of Atkin
  !! @param n [in] limit
  !! @returns r an array of primes
  pure function primes_below(n) result (r)
    implicit none

    integer(kind=8), intent (in) :: n
    integer(kind=8), dimension(:), allocatable :: r

    logical(kind=1), dimension(n) :: sieve
    integer(kind=8) :: l, i, j, idx

    if (n == 2) then
      allocate(r(1))
      r(1) = 2
      return
    end if

    if (n == 5) then
      allocate(r(2))
      r(1) = 2
      r(2) = 3
      return
    end if

    sieve = .false.

    sieve(2) = .true.
    sieve(3) = .true.

    l = int(sqrt(real(n)))

    do i = 1, l
      do j = 1, l
        idx = 4*i*i + j*j
        if (idx <= n .and. (mod(idx, 12) == 1 .or. mod(idx, 12) == 5)) &
          sieve(idx) = .not. sieve(idx)

        idx = 3*i*i + j*j
        if (idx <= n .and. mod(idx, 12) == 7) &
          sieve(idx) = .not. sieve(idx)

        idx = 3*i*i - j*j
        if (i > j .and. idx <= n .and. mod(idx, 12) == 11) &
          sieve(idx) = .not. sieve(idx)
      end do
    end do

    do i = 5, l
      if (sieve(i)) then
        j = 1
        do
          idx = j*i*i
          if (idx > n) exit
          sieve(idx) = .false.
          j = j + 1
        end do
      end if
    end do

    allocate(r(count(sieve)))

    idx = 1
    do i = 1, n
      if (sieve(i)) then
        r(idx) = i
        idx = idx + 1
      end if
    end do
  end function

  pure function prime_factor_weights(n) result (r)
    implicit none

    integer(kind=8), intent(in) :: n
    type(int_pair_t), dimension(:), allocatable :: r
  end function

  pure function count_divisors(n) result (r)
    implicit none

    integer(kind=8), intent(in) :: n
    integer(kind=8) :: r

    integer(kind=8), dimension(:), allocatable :: factors

    factors = primes_below(int(sqrt(real(n)), kind=8) + 1_8)
  end function
end module
