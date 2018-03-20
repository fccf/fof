program object_test
  use fof_object
  use fof_test_data
  implicit none

  type(fof_list), pointer ::  fp => null(), fo => null()
  allocate(fo)

  allocate(fp)
  call fp%add('scalar',is)
  call fp%add('vector',iv)
  call fp%add('matrix',im)
  call fo%add('integer', fp)

  block
    integer :: is
    call fp%get('scalar', is)
    write(*,*) 'is = ',is
  end block
  nullify(fp)


  allocate(fp)
  call fp%add('scalar',rs)
  call fp%add('vector',rv)
  call fp%add('matrix',rm)
  call fo%add('real', fp)
  nullify(fp)

  allocate(fp)
  call fp%add('scalar',ls)
  call fp%add('vector',lv)
  call fp%add('matrix',lm)
  call fo%add('logical', fp)
  nullify(fp)

  allocate(fp)
  call fp%add('scalar',cs)
  call fp%add('vector',cv)
  call fp%add('matrix',cm)
  call fo%add('character', fp)
  nullify(fp)

  write(*,*) fo%to_json()

  block
    integer :: is
    call fo%get('integer/scalar', is)
    write(*,*) 'is = ',is
  end block

  call fo%destory()
  nullify(fo)


end program object_test
