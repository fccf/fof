program object_test
  use fof_object
  implicit none

  type(fof_list), pointer ::  fp => null(), fo => null()

  allocate(fp)
  allocate(fo)

  call fp%add('scalar',1)
  call fp%add('vector',[1,2,3,4])
  call fp%add('matrix',reshape([1,2,3,4],[2,2]))
  call fo%add('integer', fp)

  nullify(fp)
  ! write(*,*) fo%to_json()

  allocate(fp)
  call fp%add('scalar',1.)
  call fp%add('vector',[1.,2.,3.,4.])
  call fp%add('matrix',reshape([1.,2.,3.,4.],[2,2]))
  call fo%add('real', fp)
  ! write(*,*) fo%to_json()
  nullify(fp)

  allocate(fp)
  call fp%add('scalar',.TRUE.)
  call fp%add('vector',[.TRUE.,.FALSE.,.FALSE.,.TRUE.])
  call fp%add('matrix',reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2]))
  call fo%add('logical', fp)
  ! write(*,*) fo%to_json()
  nullify(fp)

  allocate(fp)
  call fp%add('scalar','ab')
  call fp%add('vector',['ab','cd','ef','gh'])
  call fp%add('matrix',reshape(['ab','cd','ef','gh'],[2,2]))
  call fo%add('character', fp)
  write(*,*) fo%to_json()
  nullify(fp)

  call fo%destory()
  nullify(fo)


end program object_test
