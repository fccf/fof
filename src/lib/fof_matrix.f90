module fof_matrix
  use fof_string
  implicit none



  public :: matrix,  assignment(=)


  private


  type :: matrix
    !< matrix type
    private
    class(*), allocatable :: value(:,:)
  contains
    !---------------
    !> constructor
    !--------------
    procedure :: set => matrix_set_value
    generic :: assignment(=) => set

    !-------------
    !> Accessors
    !------------
    procedure :: get => matrix_get_value
    procedure :: v_p => matrix_value_p

    !---------
    !> output
    !---------
    procedure :: to_str => to_string

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => matrix_destory
    final     :: matrix_final
  end type matrix


  interface assignment(=)
    module procedure :: matrix_to_logical, matrix_to_character,matrix_to_integer, matrix_to_real
  end interface

  interface matrix
    module procedure :: set_matrix
  end interface matrix

contains

  subroutine matrix_final(this)
    type(matrix), intent(inout) :: this
    call this%destory()
  end subroutine matrix_final


  subroutine matrix_destory(this)
    !< Destructor
    class(matrix), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine matrix_destory



  function set_matrix(value) result (obj)
    !< return a matrix object type
    class(*), intent(in) :: value(:,:)
    type(matrix) :: obj

    call obj%set(value)

  end function set_matrix



  subroutine matrix_set_value (this, value)
    !< set matrix value for any class.
    class(matrix), intent(out) :: this
    class(*), intent(in) :: value(:,:)

    allocate(this%value, source=value)

  end subroutine matrix_set_value



  function matrix_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(matrix), target, intent(in) :: this
    class(*), pointer :: v_p(:,:)

    v_p => this%value

  end function matrix_value_p



  subroutine matrix_get_value(this, value)
    !< get matrix value for any class.
    class(matrix), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:,:)

    ! value = this%value
    allocate(value, source=this%value)

  end subroutine matrix_get_value



  subroutine matrix_to_logical (value, this)
    !< get logical value
    class(matrix), intent(in) :: this
    logical, intent(out) :: value(:,:)

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(matrix) to LOGICAL"
    end select

  end subroutine matrix_to_logical



  subroutine matrix_to_character (value, this)
    !< convert to allocatable character
    type(matrix), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:,:)

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(matrix) to CHARACTER(:)"
    end select

  end subroutine matrix_to_character



  subroutine matrix_to_integer (value, this)
    !< convert to integer integer value.

    type(matrix), intent(in) :: this
    integer, intent(out)  :: value(:,:)

    select type (v => this%value)
    type is (integer)
      value = v
    class default
      error stop " Can't convert TYPE(matrix) to integer"
    end select

  end subroutine matrix_to_integer



  subroutine matrix_to_real (value, this)
    !< convert to real real value.
    type(matrix), intent(in) :: this
    real, intent(out) :: value(:,:)
    select type (v => this%value)
    type is (real)
      value = v
    class default
      error stop " Can't convert TYPE(matrix) to real"
    end select
  end subroutine matrix_to_real



  function to_string (this,vsep, msep) result(str)
    !< write scalar object.
    class(matrix), intent(in) :: this
    character(:),allocatable :: str
    character(*), intent(in), optional :: vsep, msep
    str = to_str(this%value,vsep = vsep, msep = msep)
  end function to_string


end module fof_matrix
