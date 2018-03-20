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

  block
    integer :: is
    logical :: ls
    real    :: rs
    character(:), allocatable :: cs
    type(fof_list), pointer :: fp

    call ff%get('fof/scalar/integer',is)
    call ff%get('fof/scalar/real',rs)
    call ff%get('fof/scalar/logical',ls)
    call ff%get('fof/scalar/string',cs)

    write(*,*) 'is = ',is
    write(*,*) 'rs = ',rs
    write(*,*) 'ls = ',ls
    write(*,*) 'cs = ',cs

    call ff%get('fof/scalar',fp)
    write(*,*) fp%to_json()

    call fp%get('integer', is)
    write(*,*) 'is = ',is


  end block


end program object_test2
