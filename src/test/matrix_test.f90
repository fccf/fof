program matrix_test
  use matrix_object
  implicit none

  type(m_t) :: m
  integer   :: im(2,2)
  real      :: rm(2,2)
  logical   :: lm(2,2)
  character(:),allocatable :: cm(:,:)

  im = reshape([1,2,3,4],[2,2])
  rm = reshape([1.,2.,3.,4.],[2,2])
  lm = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2])
  cm = reshape(['ab','cd','ef','gh'],[2,2])

  m = im
  write(*,'(a)',advance='no') "integer matrix:"
  call m%write()
  im = m

  m = rm
  write(*,'(/,a)',advance='no') "real matrix:"
  call m%write()
  rm = m

  m = lm
  write(*,'(/,a)',advance='no') "logical matrix:"
  call m%write()
  lm =m

  m = cm
  write(*,'(/,a)',advance='no') "character matrix:"
  call m%write()
  cm = m

  write(*,*)

  call m%destory()

end program matrix_test
