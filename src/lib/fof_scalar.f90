module fof_scalar
  use fof_string
  implicit none



  public :: scalar, assignment(=)


  private


  type :: scalar
    !< scalar type
    ! private
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
    procedure :: to_str => to_string

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => scalar_destory
    final     :: scalar_final

  end type scalar



  interface assignment(=)
    module procedure :: scalar_to_logical, scalar_to_character,scalar_to_integer, scalar_to_real
  end interface

  interface scalar
    module procedure :: set_scalar
  end interface scalar

contains

  subroutine scalar_final(this)
    type(scalar), intent(inout) :: this
    call this%destory()
  end subroutine scalar_final


  subroutine scalar_destory(this)
    !< Destructor
    class(scalar), intent(inout) :: this
    if(allocated(this%value)) deallocate(this%value)
  end subroutine scalar_destory


  function set_scalar(value) result (obj)
    !< return a vector object type
    class(*), intent(in) :: value
    type(scalar) :: obj
    call obj%set(value)
  end function set_scalar



  subroutine scalar_set_value (this, value)
    !< set scalar value for any class.
    class(scalar), intent(out) :: this
    class(*), intent(in) :: value
    allocate(this%value, source=value)
  end subroutine scalar_set_value




  function scalar_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(scalar), target, intent(in) :: this
    class(*), pointer :: v_p
    v_p => this%value
  end function scalar_value_p




  subroutine scalar_get_value(this, value)
    !< get scalar value for any class.
    class(scalar), intent(in) :: this
    class(*), allocatable, intent(out) :: value
    allocate(value, source=this%value)
  end subroutine scalar_get_value



  subroutine scalar_to_logical (value, this)
    !< convert to logical value
    class(scalar), intent(in) :: this
    logical, intent(out) :: value
    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(scalar) to LOGICAL"
    end select
  end subroutine scalar_to_logical




  subroutine scalar_to_character (value, this)
    !< convert to allocatable character
    type(scalar), intent(in) :: this
    character(:), allocatable, intent(out) :: value
    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(scalar) to CHARACTER(:)"
    end select
  end subroutine scalar_to_character




  subroutine scalar_to_integer (value, this)
    !< convert to integer value.
    type(scalar), intent(in) :: this
    integer, intent(out) :: value
    select type (v => this%value)
    type is (integer)
      value = v
    class default
      error stop " Can't convert TYPE(scalar) to INTEGER"
    end select
  end subroutine scalar_to_integer



  subroutine scalar_to_real (value, this)
    !< convert to real value.
    type(scalar), intent(in) :: this
    real, intent(out) :: value
    select type (v => this%value)
    type is (real)
      value = v
    class default
      error stop " Can't convert TYPE(scalar) to REAL"
    end select
  end subroutine scalar_to_real




  function to_string (this) result(s)
    !< convert scalar object to string.
    class(scalar), intent(in) :: this
    character(:),allocatable :: s
    s = to_str(this%value)
  end function to_string

end module fof_scalar
