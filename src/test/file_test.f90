program file_test
  use fof_object
  implicit none

  type(fof_list) :: ff
  integer   :: im(2,2), iv(4), is
  real      :: rm(2,2), rv(4), rs
  logical   :: lm(2,2), lv(4), ls
  character(:),allocatable :: cm(:,:), cv(:), cs

  is = 1
  rs = 1.0
  ls = .TRUE.
  cs = 'xxxxxxx'

  iv = [1,2,3,4]
  rv = [1.,2.,3.,4.]
  lv = [.TRUE.,.FALSE.,.FALSE.,.TRUE.]
  cv = ['ab','cd','ef','gh']

  im = reshape([1,2,3,4],[2,2])
  rm = reshape([1.,2.,3.,4.],[2,2])
  lm = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2])
  cm = reshape(['ab','cd','ef','gh'],[2,2])

  call ff%add('fof/scalar/integer',is)
  call ff%add('fof/scalar/real',rs)
  call ff%add('fof/scalar/logical',ls)
  call ff%add('fof/scalar/string',cs)

  call ff%add('fof/vector/integer',iv)
  call ff%add('fof/vector/real',rv)
  call ff%add('fof/vector/logical',lv)
  call ff%add('fof/vector/string',cv)

  call ff%add('fof/matrix/integer',im)
  call ff%add('fof/matrix/real',rm)
  call ff%add('fof/matrix/logical',lm)
  call ff%add('fof/matrix/string',cm)

  write(*,*) ff%to_json()


end program file_test
