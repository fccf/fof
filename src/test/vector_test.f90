program vector_test
  use fof_vector
  use fof_test_data
  implicit none

  type(vector) :: v

  integer, allocatable :: aiv(:)
  real   , allocatable :: arv(:)
  logical, allocatable :: alv(:)
  character(:), allocatable :: acv(:)



  v = iv
  write(*,*) "integer vector = "//v%to_str()
  aiv = v
  write(*,*) "allocatable vector = ", aiv

  v = rv
  write(*,*) "real vector = "//v%to_str()
  arv = v
  write(*,*) "allocatable vector = ", arv

  v = lv
  write(*,*) "logical vector = "//v%to_str()
  alv =v
  write(*,*) "allocatable vector = ", alv

  v = cv
  write(*,*) "character vector = "//v%to_str()
  acv = v
  write(*,*) "allocatable vector = ", acv


end program vector_test
