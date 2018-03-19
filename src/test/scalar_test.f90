program scalar_test
  use fof_scalar
  implicit none

  type(scalar) :: s
  integer   :: is
  real      :: rs
  logical   :: ls
  character(:),allocatable :: cs

  s = 1
  write(*,*) 'scalar integer = '//s%to_str()
  is = s
  write(*,*) 'convert to integer = ', is

  s = 1.2
  write(*,*) 'scalar real = '//s%to_str()
  rs = s
  write(*,*) 'convert to real = ', rs

  s = .TRUE.
  write(*,*)  'scalar logical = '//s%to_str()
  ls = s
  write(*,*) 'convert to logical = ', ls

  s = 'xxxxxxxxxxxxx'
  write(*,*) 'scalar string = ' //s%to_str()
  cs = s
  write(*,*) 'convert to character(:) = ', cs

  select type (v_p => s%v_p())
  type is(character(*))
    write(*,*) 'value pointer = ', v_p
  end select


end program scalar_test
