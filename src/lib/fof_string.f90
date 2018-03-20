module fof_string

  implicit none

  interface to_str
    module procedure :: scalar_to_string, vector_to_string, matrix_to_string
  end interface to_str

contains

  function scalar_to_string(value) result(s)
    class(*), intent(in) :: value
    character(:), allocatable :: s
    character(32) :: ls

    select type(v_p => value)
    type is(integer)
      write(ls,'(i0)') v_p
      s = trim(adjustl(ls))
    type is(real)
      write(ls,fmt=*) v_p
      s = trim(adjustl(ls))
    type is(logical)
      if (v_p) then
        write(ls,'(a)') 'true'
      else
        write(ls,'(a)') 'false'
      end if
      s = trim(adjustl(ls))
    type is(character(*))
      s = '"'//trim(adjustl(v_p))//'"'

      class default
      write(ls,'(a)') '***'
      s = trim(adjustl(ls))
    end select

  end function scalar_to_string



  function vector_to_string(value, vsep) result(s)
    class(*), intent(in) :: value(:)
    character(*), intent(in), optional :: vsep
    character(:), allocatable :: s

    character(:),allocatable :: lsep
    ! logical   :: lshell = .FALSE.
    integer :: n

    lsep = ','
    if(present(vsep)) lsep = vsep

    s = '['
    do n = 1, size(value)
      if (n > 1) s = s//lsep
      s = s // to_str(value(n))
    end do

    s = s// ']'

  end function vector_to_string



  function matrix_to_string(value, vsep, msep) result(s)
    class(*), intent(in) :: value(:,:)
    character(*), intent(in), optional :: vsep
    character(*), intent(in), optional :: msep

    character(:), allocatable :: s

    character(:),allocatable :: lvsep, lmsep
    integer   :: n, m
    lvsep = ','
    lmsep = ';'
    if(present(vsep)) lvsep = vsep
    if(present(msep)) lmsep = msep

    s = '['
    do n = 1, size(value,2)
      if (n > 1) s = s//lmsep
      s = s//'['
      do m = 1, size(value, 1)
        if (m > 1) s = s//lvsep
        s = s // to_str(value(m,n))
      enddo
      s = s//']'
    enddo
    s = s//']'

  end function matrix_to_string


end module fof_string
