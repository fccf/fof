program object_test2
  use fof_object
  use fof_test_data
  use fof_string
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
    integer :: ais
    logical :: als
    real    :: ars
    character(:), allocatable :: acs

    call ff%get('fof/scalar/integer',ais)
    call ff%get('fof/scalar/real',ars)
    call ff%get('fof/scalar/logical',als)
    call ff%get('fof/scalar/string',acs)

    write(*,*) 'ais = ',ais
    write(*,*) 'ars = ',ars
    write(*,*) 'als = ',als
    write(*,*) 'acs = ',acs
  end block


  block
    type(fof_list), pointer :: fp

    integer, allocatable :: aiv(:)
    real, allocatable :: arv(:)
    logical, allocatable :: alv(:)
    character(:), allocatable :: acv(:)


    call ff%get('fof/vector',fp)
    write(*,*) fp%to_json()

    call fp%get('integer', aiv)
    call fp%get('real', arv)
    call fp%get('logical', alv)
    call fp%get('string',acv)

    write(*,*) 'aiv = '//to_str(aiv)
    write(*,*) 'arv = '//to_str(arv)
    write(*,*) 'alv = '//to_str(alv)
    write(*,*) 'acv = '//to_str(acv)
  end block

  block
    type(fof_list), pointer :: fp

    integer, allocatable :: aim(:,:)
    real, allocatable :: arm(:,:)
    logical, allocatable :: alm(:,:)
    character(:), allocatable :: acm(:,:)


    call ff%get('fof/matrix',fp)
    write(*,*) fp%to_json()

    call fp%get('integer', aim)
    call fp%get('real', arm)
    call fp%get('logical', alm)
    call fp%get('string',acm)

    write(*,*) 'aim = '//to_str(aim)
    write(*,*) 'arm = '//to_str(arm)
    write(*,*) 'alm = '//to_str(alm)
    write(*,*) 'acm = '//to_str(acm)
  end block


end program object_test2
