module scalar_object
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, output_unit
  implicit none



  public :: s_t, scalar, assignment(=)


  private

  !_____________________________________________________________________________
  type :: s_t
    !< scalar type
    !___________________________________________________________________________
    private
    class(*), allocatable :: value
  contains
    !---------------
    !> constructor
    !--------------
    procedure :: set => scalar_set_value
    generic :: assignment(=) => set

    !-------------
    !> Accessors
    !------------
    procedure :: get => scalar_get_value
    procedure :: v_p => scalar_value_p

    !---------
    !> output
    !---------
    procedure :: write  => write_scalar
    procedure :: to_str => to_string

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => scalar_destory

  end type s_t



  interface assignment(=)
    module procedure :: scalar_to_logical, scalar_to_character,scalar_to_int32, &
      & scalar_to_int64, scalar_to_real32, scalar_to_real64
  end interface



contains



  !_____________________________________________________________________________
  subroutine scalar_destory(this)
    !< Destructor
    !___________________________________________________________________________
    class(s_t), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine scalar_destory



  !_____________________________________________________________________________
  function scalar(value) result (obj)
    !< return a scalar object type
    !___________________________________________________________________________
    class(*), intent(in) :: value
    type(s_t) :: obj

    call obj%set(value)

  end function scalar



  !_____________________________________________________________________________
  subroutine scalar_set_value (this, value)
    !< set scalar value for any class.
    !___________________________________________________________________________
    class(s_t), intent(out) :: this
    class(*), intent(in) :: value

    allocate(this%value, source=value)

  end subroutine scalar_set_value



  !_____________________________________________________________________________
  function scalar_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(s_t), target, intent(in) :: this
    class(*), pointer :: v_p

    v_p => this%value

  end function scalar_value_p



  !_____________________________________________________________________________
  subroutine scalar_get_value(this, value)
    !< get scalar value for any class.
    !___________________________________________________________________________
    class(s_t), intent(in) :: this
    class(*), allocatable, intent(out) :: value

    allocate(value, source=this%value)

  end subroutine scalar_get_value


  !_____________________________________________________________________________
  subroutine scalar_to_logical (value, this)
    !< convert to logical value
    !___________________________________________________________________________
    class(s_t), intent(in) :: this
    logical, intent(out) :: value

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to LOGICAL"
    end select

  end subroutine scalar_to_logical



  !_____________________________________________________________________________
  subroutine scalar_to_character (value, this)
    !< convert to allocatable character
    !___________________________________________________________________________
    type(s_t), intent(in) :: this
    character(:), allocatable, intent(out) :: value

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to CHARACTER(:)"
    end select

  end subroutine scalar_to_character



  !_____________________________________________________________________________
  subroutine scalar_to_int32 (value, this)
    !< convert to int32 integer value.
    !___________________________________________________________________________
    type(s_t), intent(in) :: this
    integer(int32), intent(out) :: value

    select type (v => this%value)
    type is (integer(int32))
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to INT32"
    end select

  end subroutine scalar_to_int32



  !_____________________________________________________________________________
  subroutine scalar_to_int64 (value, this)
    !< convert to int64 integer value.
    !___________________________________________________________________________
    type(s_t), intent(in) :: this
    integer(int64), intent(out) :: value

    select type (v => this%value)
    type is (integer(int64))
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to INT64"
    end select

  end subroutine scalar_to_int64



  !_____________________________________________________________________________
  subroutine scalar_to_real32 (value, this)
    !< convert to real32 real value.
    !___________________________________________________________________________
    type(s_t), intent(in) :: this
    real(real32), intent(out) :: value

    select type (v => this%value)
    type is (real(real32))
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to REAL32"
    end select

  end subroutine scalar_to_real32



  !_____________________________________________________________________________
  subroutine scalar_to_real64 (value, this)
    !< convert to real64 real value.
    !___________________________________________________________________________
    type(s_t), intent(in) :: this
    real(real64), intent(out) :: value

    select type (v => this%value)
    type is (real(real64))
      value = v
    class default
      error stop " Can't convert TYPE(s_t) to REAL64"
    end select

  end subroutine scalar_to_real64



  !_____________________________________________________________________________
  subroutine write_scalar (this, unit)
    !< write scalar object.
    !___________________________________________________________________________
    class(s_t), intent(in) :: this
    integer, intent(in), optional :: unit
    character(len=31) :: string

    integer :: lunit

    lunit = output_unit
    if(present(unit)) lunit = unit

    select type (v_p => this%value)
    type is (integer(int32))
      write(lunit,'(i0)',advance='no') v_p

    type is (integer(int64))
      write(lunit,'(i0)',advance='no') v_p

    type is (real(real32))
      write(string,fmt=*) v_p
      write(lunit,'(a)',advance='no') trim(adjustl(string))

    type is (real(real64))
      write(string,fmt=*) v_p
      write(lunit,'(a)',advance='no') trim(adjustl(string))

    type is (logical)
      if (v_p) then
        write(lunit,'(a)',advance='no') 'true'
      else
        write(lunit,'(a)',advance='no') 'false'
      end if

    type is (character(*))
      write(lunit,'(3a)',advance='no') '"', v_p, '"'

      class default
      write(lunit,'(a)',advance='no') '???'

    end select

  end subroutine write_scalar



  !_____________________________________________________________________________
  function to_string (this) result(str)
    !< write scalar object.
    !___________________________________________________________________________
    class(s_t), intent(in) :: this

    character(:),allocatable :: str

    character(len=31) :: string

    select type (v_p => this%value)
    type is (integer(int32))
      write(string,'(i0)') v_p
      str = trim(adjustl(string))

    type is (integer(int64))
      write(string,'(i0)') v_p
      str = trim(adjustl(string))

    type is (real(real32))
      write(string,fmt=*) v_p
      str = trim(adjustl(string))

    type is (real(real64))
      write(string,fmt=*) v_p
      str = trim(adjustl(string))

    type is (logical)
      if (v_p) then
        write(string,'(a)') 'true'
      else
        write(string,'(a)') 'false'
      end if
      str = trim(adjustl(string))

    type is (character(*))
      str = v_p

    class default
      str = '???'

    end select

  end function to_string

end module scalar_object
