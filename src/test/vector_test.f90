program vector_test
  use fof_vector
  implicit none

  type(vector) :: v

  integer :: iv(4)
  real    :: rv(4)
  logical :: lv(4) = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  character(:), allocatable :: cv(:)

  iv = [1,2,3,4]
  rv = [1.,2.,3.,4.]
  lv = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  cv = ['ab','cd','ef','gh']

  v = iv
  write(*,*) "integer vector = "//v%to_str()
  iv = v

  v = rv
  write(*,*) "real vector = "//v%to_str()
  rv = v

  v = lv
  write(*,*) "logical vector = "//v%to_str()
  lv =v

  v = cv
  write(*,*) "character vector = "//v%to_str()
  cv = v



end program vector_test
