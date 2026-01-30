program matrix_ref
  implicit none
  integer :: nc, i, n, r, c
  real, allocatable :: a(:,:), b(:), work(:)
  integer, allocatable :: indx(:)

  read(*,*,end=900) nc
  do i = 1, nc
    read(*,*,end=900) n
    allocate(a(n,n), b(n), work(n), indx(n))
    read(*,*,end=900) ((a(r,c), c=1,n), r=1,n)
    read(*,*,end=900) (b(r), r=1,n)
    call ludcmp(n, n, a, indx, work)
    call baksub(n, n, a, indx, b)
    write(*,'(200(1X,ES15.7))') (b(r), r=1,n)
    deallocate(a, b, work, indx)
  end do

900 continue
end program matrix_ref
