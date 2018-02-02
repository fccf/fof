module tensor_object
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, output_unit
  implicit none



  public :: t_t, tensor, assignment(=)


  private

  !_____________________________________________________________________________
  type :: t_t
    !< tensor type
    !___________________________________________________________________________
    private
    class(*), allocatable :: value(:,:,:)
  contains
    !---------------
    !> constructor
    !--------------
    procedure :: set => tensor_set_value
    generic :: assignment(=) => set

    !-------------
    !> Accessors
    !------------
    procedure :: get => tensor_get_value
    procedure :: v_p => tensor_value_p

    !---------
    !> output
    !---------
    procedure :: write  => write_tensor

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => tensor_destory

  end type t_t



  interface assignment(=)
    module procedure :: tensor_to_logical, tensor_to_character,tensor_to_int32, &
      & tensor_to_int64, tensor_to_real32, tensor_to_real64
  end interface



contains



  !_____________________________________________________________________________
  subroutine tensor_destory(this)
    !< Destructor
    !___________________________________________________________________________
    class(t_t), intent(inout) :: this

    if(allocated(this%value)) deallocate(this%value)

  end subroutine tensor_destory



  !_____________________________________________________________________________
  function tensor(value) result (obj)
    !< return a tensor object type
    !___________________________________________________________________________
    class(*), intent(in) :: value(:,:,:)
    type(t_t) :: obj

    call obj%set(value)

  end function tensor



  !_____________________________________________________________________________
  subroutine tensor_set_value (this, value)
    !< set tensor value for any class.
    !___________________________________________________________________________
    class(t_t), intent(out) :: this
    class(*), intent(in) :: value(:,:,:)

    allocate(this%value, source=value)

  end subroutine tensor_set_value



  !_____________________________________________________________________________
  function tensor_value_p (this) result (v_p)
    !< return a pointer to the value.
    class(t_t), target, intent(in) :: this
    class(*), pointer :: v_p(:,:,:)

    v_p => this%value

  end function tensor_value_p



  !_____________________________________________________________________________
  subroutine tensor_get_value(this, value)
    !< get tensor value for any class.
    !___________________________________________________________________________
    class(t_t), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:,:,:)

    allocate(value, source=this%value)

  end subroutine tensor_get_value


  !_____________________________________________________________________________
  subroutine tensor_to_logical (value, this)
    !< get logical value
    !___________________________________________________________________________
    class(t_t), intent(in) :: this
    logical, intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (logical)
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to LOGICAL"
    end select

  end subroutine tensor_to_logical



  !_____________________________________________________________________________
  subroutine tensor_to_character (value, this)
    !< convert to allocatable character
    !___________________________________________________________________________
    type(t_t), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (character(*))
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to CHARACTER(:)"
    end select

  end subroutine tensor_to_character



  !_____________________________________________________________________________
  subroutine tensor_to_int32 (value, this)
    !< convert to int32 integer value.
    !___________________________________________________________________________
    type(t_t), intent(in) :: this
    integer(int32), intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (integer(int32))
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to INT32"
    end select

  end subroutine tensor_to_int32



  !_____________________________________________________________________________
  subroutine tensor_to_int64 (value, this)
    !< convert to int64 integer value.
    !___________________________________________________________________________
    type(t_t), intent(in) :: this
    integer(int64), intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (integer(int64))
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to INT64"
    end select

  end subroutine tensor_to_int64



  !_____________________________________________________________________________
  subroutine tensor_to_real32 (value, this)
    !< convert to real32 real value.
    !___________________________________________________________________________
    type(t_t), intent(in) :: this
    real(real32), intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (real(real32))
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to REAL32"
    end select

  end subroutine tensor_to_real32



  !_____________________________________________________________________________
  subroutine tensor_to_real64 (value, this)
    !< convert to real64 real value.
    !___________________________________________________________________________
    type(t_t), intent(in) :: this
    real(real64), intent(out) :: value(:,:,:)

    select type (v => this%value)
    type is (real(real64))
      value = v
    class default
      error stop " Can't convert TYPE(t_t) to REAL64"
    end select

  end subroutine tensor_to_real64

  !_____________________________________________________________________________
  subroutine write_tensor (this, unit)
    !< write tensor object type.
    !___________________________________________________________________________
    class(t_t), intent(in) :: this
    integer, intent(in), optional :: unit

    integer :: i, j, k
    integer :: lunit
    character(len=31) :: string

    lunit = output_unit
    if(present(unit)) lunit = unit

    select type (v_P => this%value)

    type is (integer(int32))

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(lunit,'(i0)',advance='no') v_p(i,j,k)
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    type is (integer(int64))

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(lunit,'(i0)',advance='no') v_p(i,j,k)
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    type is (real(real32))

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(string,fmt=*) v_p(i,j,k)
            write(lunit,'(a)',advance='no') trim(adjustl(string))
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    type is (real(real64))

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(string,fmt=*) v_p(i,j,k)
            write(lunit,'(a)',advance='no') trim(adjustl(string))
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    type is (logical)

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            if (v_p(i,j,k)) then
              write(lunit,'("true")',advance='no')
            else
              write(lunit,'("false")',advance='no')
            end if
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    type is (character(*))

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(lunit,'(3a)',advance='no') '"', trim(v_p(i,j,k)), '"'
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    class default

      write(lunit,'("[")',advance='no')
      do k = 1, size(v_p,3)
        if (k > 1) write(lunit,'(", ")',advance='no')
        write(lunit,'("[")',advance='no')

        do j = 1, size(v_p,2)
          if (j > 1) write(lunit,'(", ")',advance='no')
          write(lunit,'("[")',advance='no')

          do i = 1, size(v_p,1)
            if (i > 1) write(lunit,'(", ")',advance='no')
            write(lunit,'(a)',advance='no') '*'
          end do
          write(lunit,'("]")',advance='no')
        end do
        write(lunit,'("]")',advance='no')
      enddo
      write(lunit,'("]")',advance='no')

    end select

  end subroutine write_tensor

end module tensor_object
