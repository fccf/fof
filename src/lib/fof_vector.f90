module fof_vector
  use fof_string
  implicit none



  public :: vector, assignment(=)


  private


  type :: vector
    !< vector type
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
    procedure :: to_str => to_string

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => vector_destory
    final     :: vector_final

  end type vector


  interface assignment(=)
    module procedure :: vector_to_logical, vector_to_character,vector_to_integer, vector_to_real
  end interface

  interface vector
    module procedure :: set_vector
  end interface vector


contains

  subroutine vector_final(this)
    type(vector), intent(inout) :: this
    call this%destory()
  end subroutine vector_final



  subroutine vector_destory(this)
    !< Destructor
    class(vector), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine vector_destory



  function set_vector(value) result (obj)
    !< return a vector object type
    class(*), intent(in) :: value(:)
    type(vector) :: obj

    call obj%set(value)

  end function set_vector



  subroutine vector_set_value (this, value)
    !< set vector value for any class.
    class(vector), intent(out) :: this
    class(*), intent(in) :: value(:)

    ! this%value = value
    allocate(this%value, source=value)

  end subroutine vector_set_value



  function vector_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(vector), target, intent(in) :: this
    class(*), pointer :: v_p(:)

    v_p => this%value

  end function vector_value_p



  subroutine vector_get_value(this, value)
    !< get vector value for any class.
    class(vector), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)

    ! value = this%value
    allocate(value, source=this%value)

  end subroutine vector_get_value



  subroutine vector_to_logical (value, this)
    !< get logical value
    class(vector), intent(in) :: this
    logical, allocatable, intent(out) :: value(:)

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(vector) to LOGICAL"
    end select

  end subroutine vector_to_logical



  subroutine vector_to_character (value, this)
    !< convert to allocatable character
    type(vector), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:)

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(vector) to CHARACTER(:)"
    end select

  end subroutine vector_to_character



  subroutine vector_to_integer (value, this)
    !< convert to integer value.
    type(vector), intent(in) :: this
    integer, allocatable, intent(out)  :: value(:)

    select type (v => this%value)
    type is (integer)
      ! allocate(value, source = v)
      value = v
    class default
      error stop " Can't convert TYPE(vector) to INTEGER"
    end select

  end subroutine vector_to_integer


  subroutine vector_to_real (value, this)
    !< convert to real value.

    type(vector), intent(in) :: this
    real, allocatable, intent(out) :: value(:)

    select type (v => this%value)
    type is (real)
      value = v
    class default
      error stop " Can't convert TYPE(vector) to REAL"
    end select

  end subroutine vector_to_real


  function to_string (this, vsep) result(str)
    !< write scalar object.
    class(vector), intent(in) :: this
    character(:),allocatable :: str
    character(*), intent(in), optional :: vsep

    str = to_str(this%value,vsep)

  end function to_string



end module fof_vector
