program vector_test
  use vector_object
  implicit none

  type(v_t) :: v

  integer :: iv(4)
  real    :: rv(4)
  logical :: lv(4) = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  character(:), allocatable :: cv(:)

  iv = [1,2,3,4]
  rv = [1.,2.,3.,4.]
  lv = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  cv = ['ab','cd','ef','gh']

  v = iv
  write(*,'(a)',advance='no') "integer vector:"
  call v%write()
  iv = v

  v = rv
  write(*,'(/,a)',advance='no') "real vector:"
  call v%write()
  rv = v

  v = lv
  write(*,'(/,a)',advance='no') "logical vector:"
  call v%write()
  lv =v

  v = cv
  write(*,'(/,a)',advance='no') "character vector:"
  call v%write()
  cv = v

  write(*,*)

  call v%destory()

end program vector_test
