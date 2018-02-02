module matrix_object
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, output_unit
  implicit none



  public :: m_t, matrix, assignment(=)


  private

  !_____________________________________________________________________________
  type :: m_t
    !< matrix type
    !___________________________________________________________________________
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
    procedure :: write  => write_matrix

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => matrix_destory

  end type m_t



  interface assignment(=)
    module procedure :: matrix_to_logical, matrix_to_character,matrix_to_int32, &
      & matrix_to_int64, matrix_to_real32, matrix_to_real64
  end interface



contains



  !_____________________________________________________________________________
  subroutine matrix_destory(this)
    !< Destructor
    !___________________________________________________________________________
    class(m_t), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine matrix_destory



  !_____________________________________________________________________________
  function matrix(value) result (obj)
    !< return a matrix object type
    !___________________________________________________________________________
    class(*), intent(in) :: value(:,:)
    type(m_t) :: obj

    call obj%set(value)

  end function matrix



  !_____________________________________________________________________________
  subroutine matrix_set_value (this, value)
    !< set matrix value for any class.
    !___________________________________________________________________________
    class(m_t), intent(out) :: this
    class(*), intent(in) :: value(:,:)

    allocate(this%value, source=value)

  end subroutine matrix_set_value



  !_____________________________________________________________________________
  function matrix_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(m_t), target, intent(in) :: this
    class(*), pointer :: v_p(:,:)

    v_p => this%value

  end function matrix_value_p



  !_____________________________________________________________________________
  subroutine matrix_get_value(this, value)
    !< get matrix value for any class.
    !___________________________________________________________________________
    class(m_t), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:,:)

    allocate(value, source=this%value)

  end subroutine matrix_get_value


  !_____________________________________________________________________________
  subroutine matrix_to_logical (value, this)
    !< get logical value
    !___________________________________________________________________________
    class(m_t), intent(in) :: this
    logical, intent(out) :: value(:,:)

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to LOGICAL"
    end select

  end subroutine matrix_to_logical



  !_____________________________________________________________________________
  subroutine matrix_to_character (value, this)
    !< convert to allocatable character
    !___________________________________________________________________________
    type(m_t), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:,:)

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to CHARACTER(:)"
    end select

  end subroutine matrix_to_character



  !_____________________________________________________________________________
  subroutine matrix_to_int32 (value, this)
    !< convert to int32 integer value.
    !___________________________________________________________________________
    type(m_t), intent(in) :: this
    integer(int32), intent(out) :: value(:,:)

    select type (v => this%value)
    type is (integer(int32))
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to INT32"
    end select

  end subroutine matrix_to_int32



  !_____________________________________________________________________________
  subroutine matrix_to_int64 (value, this)
    !< convert to int64 integer value.
    !___________________________________________________________________________
    type(m_t), intent(in) :: this
    integer(int64), intent(out) :: value(:,:)

    select type (v => this%value)
    type is (integer(int64))
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to INT64"
    end select

  end subroutine matrix_to_int64



  !_____________________________________________________________________________
  subroutine matrix_to_real32 (value, this)
    !< convert to real32 real value.
    !___________________________________________________________________________
    type(m_t), intent(in) :: this
    real(real32), intent(out) :: value(:,:)

    select type (v => this%value)
    type is (real(real32))
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to REAL32"
    end select

  end subroutine matrix_to_real32



  !_____________________________________________________________________________
  subroutine matrix_to_real64 (value, this)
    !< convert to real64 real value.
    !___________________________________________________________________________
    type(m_t), intent(in) :: this
    real(real64), intent(out) :: value(:,:)

    select type (v => this%value)
    type is (real(real64))
      value = v
    class default
      error stop " Can't convert TYPE(m_t) to REAL64"
    end select

  end subroutine matrix_to_real64

  !_____________________________________________________________________________
  subroutine write_matrix (this, unit)
    !< write matrix object type.
    !___________________________________________________________________________
    class(m_t), intent(in) :: this
    integer, intent(in), optional :: unit

    integer :: n, m
    integer :: lunit
    character(len=31) :: string

    lunit = output_unit
    if(present(unit)) lunit = unit

    select type (val => this%value)

    type is (integer(int32))

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'(i0)',advance='no') val(m,n)
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    type is (integer(int64))

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'(i0)',advance='no') val(m,n)
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    type is (real(real32))

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(string,fmt=*) val(m,n)
          write(lunit,'(a)',advance='no') trim(adjustl(string))
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    type is (real(real64))

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(string,fmt=*) val(m,n)
          write(lunit,'(a)',advance='no') trim(adjustl(string))
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    type is (logical)

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          if (val(m,n)) then
            write(lunit,'("true")',advance='no')
          else
            write(lunit,'("false")',advance='no')
          end if
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    type is (character(*))

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'(3a)',advance='no') '"', trim(val(m,n)), '"'
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    class default

      write(lunit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'(a)',advance='no') '*'
        end do
        write(lunit,'("]")',advance='no')
      end do
      write(lunit,'("]")',advance='no')

    end select

  end subroutine write_matrix

end module matrix_object
