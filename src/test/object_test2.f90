program object_test2
  use fof_object
  use fof_test_data
  implicit none

  type(fof_list) :: ff


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


end program object_test2
