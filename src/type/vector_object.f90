module vector_object
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, output_unit
  implicit none



  public :: v_t, vector, assignment(=)


  private

  !_____________________________________________________________________________
  type :: v_t
    !< vector type
    !___________________________________________________________________________
    private
    class(*), allocatable :: value(:)
  contains
    !---------------
    !> constructor
    !--------------
    procedure :: set => vector_set_value
    generic :: assignment(=) => set

    !-------------
    !> Accessors
    !------------
    procedure :: get => vector_get_value
    procedure :: v_p => vector_value_p

    !---------
    !> output
    !---------
    procedure :: write  => write_vector

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => vector_destory

  end type v_t



  interface assignment(=)
    module procedure :: vector_to_logical, vector_to_character,vector_to_int32, &
      & vector_to_int64, vector_to_real32, vector_to_real64
  end interface



contains



  !_____________________________________________________________________________
  subroutine vector_destory(this)
    !< Destructor
    !___________________________________________________________________________
    class(v_t), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine vector_destory



  !_____________________________________________________________________________
  function vector(value) result (obj)
    !< return a vector object type
    !___________________________________________________________________________
    class(*), intent(in) :: value(:)
    type(v_t) :: obj

    call obj%set(value)

  end function vector



  !_____________________________________________________________________________
  subroutine vector_set_value (this, value)
    !< set vector value for any class.
    !___________________________________________________________________________
    class(v_t), intent(out) :: this
    class(*), intent(in) :: value(:)

    allocate(this%value, source=value)

  end subroutine vector_set_value



  !_____________________________________________________________________________
  function vector_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(v_t), target, intent(in) :: this
    class(*), pointer :: v_p(:)

    v_p => this%value

  end function vector_value_p



  !_____________________________________________________________________________
  subroutine vector_get_value(this, value)
    !< get vector value for any class.
    !___________________________________________________________________________
    class(v_t), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)

    allocate(value, source=this%value)

  end subroutine vector_get_value


  !_____________________________________________________________________________
  subroutine vector_to_logical (value, this)
    !< get logical value
    !___________________________________________________________________________
    class(v_t), intent(in) :: this
    logical, intent(out) :: value(:)

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to LOGICAL"
    end select

  end subroutine vector_to_logical



  !_____________________________________________________________________________
  subroutine vector_to_character (value, this)
    !< convert to allocatable character
    !___________________________________________________________________________
    type(v_t), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:)

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to CHARACTER(:)"
    end select

  end subroutine vector_to_character



  !_____________________________________________________________________________
  subroutine vector_to_int32 (value, this)
    !< convert to int32 integer value.
    !___________________________________________________________________________
    type(v_t), intent(in) :: this
    integer(int32), intent(out) :: value(:)

    select type (v => this%value)
    type is (integer(int32))
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to INT32"
    end select

  end subroutine vector_to_int32



  !_____________________________________________________________________________
  subroutine vector_to_int64 (value, this)
    !< convert to int64 integer value.
    !___________________________________________________________________________
    type(v_t), intent(in) :: this
    integer(int64), intent(out) :: value(:)

    select type (v => this%value)
    type is (integer(int64))
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to INT64"
    end select

  end subroutine vector_to_int64



  !_____________________________________________________________________________
  subroutine vector_to_real32 (value, this)
    !< convert to real32 real value.
    !___________________________________________________________________________
    type(v_t), intent(in) :: this
    real(real32), intent(out) :: value(:)

    select type (v => this%value)
    type is (real(real32))
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to REAL32"
    end select

  end subroutine vector_to_real32



  !_____________________________________________________________________________
  subroutine vector_to_real64 (value, this)
    !< convert to real64 real value.
    !___________________________________________________________________________
    type(v_t), intent(in) :: this
    real(real64), intent(out) :: value(:)

    select type (v => this%value)
    type is (real(real64))
      value = v
    class default
      error stop " Can't convert TYPE(v_t) to REAL64"
    end select

  end subroutine vector_to_real64

  !_____________________________________________________________________________
  subroutine write_vector (this, unit)
    !< write vector object type.
    !___________________________________________________________________________
    class(v_t), intent(in) :: this
    integer, intent(in), optional :: unit

    integer :: n
    integer :: lunit
    character(len=31) :: string

    lunit = output_unit
    if(present(unit)) lunit = unit

    select type (v_p => this%value)

    type is (integer(int32))

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'(i0)',advance='no') v_p(n)
      end do
      write(lunit,'("]")',advance='no')

    type is (integer(int64))

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'(i0)',advance='no') v_p(n)
      end do
      write(lunit,'("]")',advance='no')

    type is (real(real32))

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(string,fmt=*) v_p(n)
        write(lunit,'(a)',advance='no') trim(adjustl(string))
      end do
      write(lunit,'("]")',advance='no')

    type is (real(real64))

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(string,fmt=*) v_p(n)
        write(lunit,'(a)',advance='no') trim(adjustl(string))
      end do
      write(lunit,'("]")',advance='no')

    type is (logical)

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        if (v_p(n)) then
          write(lunit,'("true")',advance='no')
        else
          write(lunit,'("false")',advance='no')
        end if
      end do
      write(lunit,'("]")',advance='no')

    type is (character(*))

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'(3a)',advance='no') '"', trim(v_p(n)), '"'
      end do
      write(lunit,'("]")',advance='no')

    class default

      write(lunit,'("[")',advance='no')
      do n = 1, size(v_p)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'(a)',advance='no') '*'
      end do
      write(lunit,'("]")',advance='no')

    end select

  end subroutine write_vector

end module vector_object
