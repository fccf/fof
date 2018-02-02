program tensor_test
  use tensor_object
  implicit none

  type(t_t) :: t
  integer   :: it(2,2,2)
  real      :: rt(2,2,2)
  logical   :: lt(2,2,2)
  character(:),allocatable :: ct(:,:,:)

  it = reshape([1,2,3,4,5,6,7,8],[2,2,2])
  rt = reshape([1.,2.,3.,4.,5.,6.,7.,8.],[2,2,2])
  lt = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2,2])
  ct = reshape(['ab','cd','ef','gh','ij','kl','mn','op'],[2,2,2])

  t = it
  write(*,'(a)',advance='no') "integer tensor:"
  call t%write()
  it = t

  t = rt
  write(*,'(/,a)',advance='no') "real tensor:"
  call t%write()
  rt = t

  t = lt
  write(*,'(/,a)',advance='no') "logical tensor:"
  call t%write()
  lt =t

  t = ct
  write(*,'(/,a)',advance='no') "character tensor:"
  call t%write()
  ct = t

  write(*,*)

  call t%destory()

end program tensor_test
