program matrix_ref_dp
  implicit none
  integer :: nc, i, n, r, c
  real(kind=8), allocatable :: a(:,:), b(:), work(:)
  integer, allocatable :: indx(:)

  read(*,*,end=900) nc
  do i = 1, nc
    read(*,*,end=900) n
    allocate(a(n,n), b(n), work(n), indx(n))
    read(*,*,end=900) ((a(r,c), c=1,n), r=1,n)
    read(*,*,end=900) (b(r), r=1,n)
    call ludcmp_dp(n, a, indx, work)
    call baksub_dp(n, a, indx, b)
    write(*,'(200(1X,ES23.15))') (b(r), r=1,n)
    deallocate(a, b, work, indx)
  end do

900 continue
contains

  subroutine ludcmp_dp(n, a, indx, work)
    integer, intent(in) :: n
    real(kind=8), intent(inout) :: a(n,n)
    integer, intent(out) :: indx(n)
    real(kind=8), intent(out) :: work(n)
    integer :: i, j, k, imax
    real(kind=8) :: aamax, sum, dum

    do i = 1, n
      aamax = 0.0d0
      do j = 1, n
        aamax = max(abs(a(i,j)), aamax)
      end do
      work(i) = 1.0d0 / aamax
    end do

    do j = 1, n
      do i = 1, j-1
        sum = a(i,j)
        do k = 1, i-1
          sum = sum - a(i,k) * a(k,j)
        end do
        a(i,j) = sum
      end do

      aamax = 0.0d0
      imax = j
      do i = j, n
        sum = a(i,j)
        do k = 1, j-1
          sum = sum - a(i,k) * a(k,j)
        end do
        a(i,j) = sum
        dum = work(i) * abs(sum)
        if (dum .ge. aamax) then
          aamax = dum
          imax = i
        end if
      end do

      if (j .ne. imax) then
        do k = 1, n
          dum = a(imax,k)
          a(imax,k) = a(j,k)
          a(j,k) = dum
        end do
        work(imax) = work(j)
      end if

      indx(j) = imax
      if (j .ne. n) then
        dum = 1.0d0 / a(j,j)
        do i = j+1, n
          a(i,j) = a(i,j) * dum
        end do
      end if
    end do
  end subroutine ludcmp_dp

  subroutine baksub_dp(n, a, indx, b)
    integer, intent(in) :: n
    real(kind=8), intent(in) :: a(n,n)
    integer, intent(in) :: indx(n)
    real(kind=8), intent(inout) :: b(n)
    integer :: i, j, ii, ll
    real(kind=8) :: sum

    ii = 0
    do i = 1, n
      ll = indx(i)
      sum = b(ll)
      b(ll) = b(i)
      if (ii .ne. 0) then
        do j = ii, i-1
          sum = sum - a(i,j) * b(j)
        end do
      else if (sum .ne. 0.0d0) then
        ii = i
      end if
      b(i) = sum
    end do

    do i = n, 1, -1
      sum = b(i)
      if (i .lt. n) then
        do j = i+1, n
          sum = sum - a(i,j) * b(j)
        end do
      end if
      b(i) = sum / a(i,i)
    end do
  end subroutine baksub_dp

end program matrix_ref_dp
