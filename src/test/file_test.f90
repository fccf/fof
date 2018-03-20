program file_test
  use fof_object
  implicit none

  type(fof_list) :: ff


  call ff%add('x/x/name1',1.0)
  call ff%add('x/x/name2',2.0)

  write(*,*) ff%to_json()


end program file_test
