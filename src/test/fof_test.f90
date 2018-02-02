program fof_test
  use fof_object
  implicit none

  type(fof_t) :: fo, fp

  call fp%add('scalar',1)
  call fp%add('vector',[1,2,3,4])
  call fp%add('matrix',reshape([1,2,3,4],[2,2]))
  call fp%add('tensor',reshape([1,2,3,4,5,6,7,8],[2,2,2]))
  call fo%insert('integer', fp)
  call fp%clear()

  call fp%add('scalar',1.)
  call fp%add('vector',[1.,2.,3.,4.])
  call fp%add('matrix',reshape([1.,2.,3.,4.],[2,2]))
  call fp%add('tensor',reshape([1.,2.,3.,4.,5.,6.,7.,8.],[2,2,2]))
  call fo%insert('real', fp)
  call fp%clear()

  call fp%add('scalar',.TRUE.)
  call fp%add('vector',[.TRUE.,.FALSE.,.FALSE.,.TRUE.])
  call fp%add('matrix',reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2]))
  call fp%add('tensor',reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2,2]))
  call fo%insert('logical', fp)
  call fp%clear()

  call fp%add('scalar','ab')
  call fp%add('vector',['ab','cd','ef','gh'])
  call fp%add('matrix',reshape(['ab','cd','ef','gh'],[2,2]))
  call fp%add('tensor',reshape(['ab','cd','ef','gh','ij','kl','mn','op'],[2,2,2]))
  call fo%insert('character', fp)
  call fp%clear()
  !
  call fo%write_json()
  call fo%destory()

end program fof_test
