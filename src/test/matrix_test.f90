program matrix_test
  use fof_matrix
  use fof_test_data
  implicit none

  type(matrix) :: m
  integer, allocatable :: aim(:,:)
  real   , allocatable :: arm(:,:)
  logical, allocatable :: alm(:,:)
  character(:), allocatable :: acm(:,:)


  m = im
  write(*,*) "integer matrix = "//m%to_str()
  aim = m

  m = rm
  write(*,*) "real matrix = "//m%to_str()
  arm = m

  m = lm
  write(*,*) "logical matrix = "//m%to_str()
  alm =m

  m = cm
  write(*,*) "character matrix = "//m%to_str()
  acm = m


end program matrix_test
