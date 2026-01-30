program lapacksubs_d_ref
  implicit none
  integer :: nc, i, n, r, c
  real(kind=8), allocatable :: a(:,:), b(:)
  integer, allocatable :: ipiv(:)
  integer :: info
  character(len=1) :: trans
  integer :: nrhs

  read(*,*,end=900) nc
  do i = 1, nc
    read(*,*,end=900) n
    allocate(a(n,n), b(n), ipiv(n))
    read(*,*,end=900) ((a(r,c), c=1,n), r=1,n)
    read(*,*,end=900) (b(r), r=1,n)
    call dgetrf(n, n, a, n, ipiv, info)
    trans = 'N'
    nrhs = 1
    call dgetrs(trans, n, nrhs, a, n, ipiv, b, n, info)
    write(*,'(200(1X,ES23.15))') (b(r), r=1,n)
    deallocate(a, b, ipiv)
  end do
900 continue
end program lapacksubs_d_ref
