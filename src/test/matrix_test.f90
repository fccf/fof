program matrix_test
  use fof_matrix
  implicit none

  type(matrix) :: m
  integer   :: im(2,2)
  real      :: rm(2,2)
  logical   :: lm(2,2)
  character(:),allocatable :: cm(:,:)

  im = reshape([1,2,3,4],[2,2])
  rm = reshape([1.,2.,3.,4.],[2,2])
  lm = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2])
  cm = reshape(['ab','cd','ef','gh'],[2,2])

  m = im
  write(*,*) "integer matrix = "//m%to_str()
  im = m

  m = rm
  write(*,*) "real matrix = "//m%to_str()
  rm = m

  m = lm
  write(*,*) "logical matrix = "//m%to_str()
  lm =m

  m = cm
  write(*,*) "character matrix = "//m%to_str()
  cm = m


end program matrix_test
