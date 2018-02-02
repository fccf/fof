module fof_object
  use,intrinsic :: iso_fortran_env, only: output_unit
  use scalar_object
  use vector_object
  use matrix_object
  use tensor_object
  implicit none



  public :: o_t, fof_t



  private

  !_____________________________________________________________________________
  type :: o_t
    !< scalar object list
    !___________________________________________________________________________
    private

    character(:), allocatable :: name
    class(*), allocatable :: value

    type(o_t), pointer :: prev     => null()
    type(o_t), pointer :: next     => null()
  contains
    procedure :: clear => obj_clear
    procedure :: destory => obj_destory
    ! final :: obj_destory
  end type o_t



  !_____________________________________________________________________________
  type :: fof_t
    !< fortran object format
    !___________________________________________________________________________
    type(o_t), pointer :: first => null()

  contains
    !---------------
    !> constructor
    !--------------
    generic :: add => fof_add_scalar,fof_add_vector, fof_add_matrix, fof_add_tensor
    procedure :: insert => fof_insert_obj
    procedure :: update => fof_update_obj
    procedure :: append => fof_append_obj
    procedure :: remove => fof_remove_obj


    !-------------
    !> Accessors
    !------------
    procedure :: is_empty => fof_is_empty
    procedure :: is_exist => fof_is_exist_obj
    procedure :: number   => fof_obj_number
    procedure :: o_p => fof_obj_pointer
    procedure :: v_p => fof_value_pointer
    !TODO:
    ! procedure :: get => get_scalar, get_vector, get_tensor


    !---------
    !> output
    !---------
    procedure :: write_json => fof_write_json

    !---------------
    !> Destructor
    !---------------
    procedure :: clear => fof_clear
    procedure :: destory => fof_destory
    ! final :: fof_destory

    !----------
    !> private
    !----------
    procedure, private :: fof_add_scalar,fof_add_vector,fof_add_matrix,fof_add_tensor
  end type fof_t



contains



  !_____________________________________________________________________________
  function new_obj(name, value) result(o_p)
    !< creat a new object, return a pointer
    !___________________________________________________________________________
    character(*), intent(in) :: name
    class(*), intent(in)     :: value
    type(o_t), pointer       :: o_p

    allocate(o_p)
    o_p%name  = name
    o_p%value = value
    ! allocate(o_p%value, source = value)
    o_p%prev => o_p

  end function new_obj



  !_____________________________________________________________________________
  recursive subroutine obj_clear (this)
    !< clear the objects
    !___________________________________________________________________________
    class(o_t), intent(inout) :: this

    if (associated(this%next)) then
      ! nullify(this%next)
      call this%next%clear()
    endif

  end subroutine obj_clear



  !_____________________________________________________________________________
  recursive subroutine obj_destory (this)
    !< destory the object.
    !___________________________________________________________________________
    class(o_t), intent(inout) :: this

    if (associated(this%next)) then
      call this%next%destory()
      deallocate(this%name, this%value)
    endif


  end subroutine obj_destory



  !_____________________________________________________________________________
  recursive subroutine fof_clear (this)
    !< clear fof
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this

    if (associated(this%first)) call this%first%clear()
    nullify(this%first)

  end subroutine fof_clear



  !_____________________________________________________________________________
  recursive subroutine fof_destory(this)
    !< destory the fof .
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this

    if (associated(this%first)) call this%first%destory()
    nullify(this%first)

  end subroutine fof_destory



  !_____________________________________________________________________________
  function fof_obj_number(this) result(num)
    !< returns the number of object in the object list.
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    integer :: num

    type(o_t), pointer :: o_p

    num = 0
    o_p => this%first
    do while (associated(o_p))
      num = num + 1
      o_p => o_p%next
    end do

  end function fof_obj_number



  !_____________________________________________________________________________
  function fof_is_exist_obj(this, name) result(exist)
    !< return true if object NAME exists.
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    character(*), intent(in) :: name
    logical :: exist

    exist = associated(this%o_p(name))

  end function fof_is_exist_obj



  !_____________________________________________________________________________
  function fof_is_empty(this) result(isempty)
    !< return true if the list contains no object.
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    logical :: isempty

    isempty = .not.associated(this%first)

  end function fof_is_empty



  !_____________________________________________________________________________
  subroutine fof_append_obj(this, o_p)
    !< append an obj to the list.
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    type(o_t), pointer, intent(in) :: o_p

    type(o_t), pointer :: tail

    if (associated(this%first)) then
      tail      => this%first%prev
      tail%next => o_p
      o_p%prev  => tail
      this%first%prev => o_p
    else
      o_p%prev   => o_p
      this%first => o_p
    end if

  end subroutine fof_append_obj



  !_____________________________________________________________________________
  subroutine fof_insert_obj(this, name, value)
    !< insert an obj to a list.
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this

    character(*), intent(in) :: name
    class(*), intent(in) :: value

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      o_p => new_obj(name, value)
      call this%append(o_p)
    end if

  end subroutine fof_insert_obj



  !_____________________________________________________________________________
  subroutine fof_update_obj(this, name, value)
    !< update an exist object.
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this

    character(*), intent(in) :: name
    class(*), intent(in) :: value

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      !! Replace existing value with the given one.
      if (allocated(o_p%value)) deallocate(o_p%value)
      allocate(o_p%value, source = value)

    else
      error stop "the object '" //name //"' does not exist here"
    end if

  end subroutine fof_update_obj



  !_____________________________________________________________________________
  subroutine fof_remove_obj(this, name)
    !< remove an obj in the list.
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    character(*), intent(in) :: name

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)

    if (associated(o_p)) then

      if (associated(o_p%prev, o_p)) then ! single obj list
        this%first => null()

      else if (associated(this%first, o_p)) then ! first obj of multiple
        this%first => o_p%next
        o_p%next%prev => o_p%prev

      else if (.not.associated(o_p%next)) then ! last obj of multiple
        o_p%prev%next => o_p%next
        this%first%prev => o_p%prev

      else ! interior obj of multiple
        o_p%prev%next => o_p%next
        o_p%next%prev => o_p%prev

      end if
      o_p%next => null() ! stop recursive finalization when obj is deallocated

      deallocate(o_p)

    else
      error stop "the object '" //name //"' does not exist here"

    end if

  end subroutine fof_remove_obj



  !_____________________________________________________________________________
  subroutine fof_add_scalar(this, name, value)
    !< add scalar object to the list
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      o_p => new_obj(name, scalar(value))
      call this%append(o_p)
    end if

  end subroutine fof_add_scalar



  !_____________________________________________________________________________
  subroutine fof_add_vector(this, name, value)
    !< add vector object to the list
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:)

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      o_p => new_obj(name, vector(value))
      call this%append(o_p)
    end if

  end subroutine fof_add_vector



  !_____________________________________________________________________________
  subroutine fof_add_matrix(this, name, value)
    !< add matrix object to the list
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:,:)

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      o_p => new_obj(name, matrix(value))
      call this%append(o_p)
    end if

  end subroutine fof_add_matrix



  !_____________________________________________________________________________
  subroutine fof_add_tensor(this, name, value)
    !< add tensor object to the list
    !___________________________________________________________________________
    class(fof_t), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:,:,:)

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      o_p => new_obj(name, tensor(value))
      call this%append(o_p)
    end if

  end subroutine fof_add_tensor



  !_____________________________________________________________________________
  function fof_value_pointer(this, name) result(v_p)
    !< returns a pointer to the value of obj, or a null pointer.
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    character(*), intent(in) :: name
    class(*), pointer :: v_p

    type(o_t), pointer :: o_p

    o_p => this%o_p(name)

    if (associated(o_p)) then
      v_p => o_p%value
    else
      v_p => null()
    end if

  end function fof_value_pointer



  !_____________________________________________________________________________
  function fof_obj_pointer(this, name) result (o_p)
    !< return a pointer to an obj, or a null pointer.
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    character(*), intent(in) :: name

    type(o_t), pointer :: o_p

    o_p => this%first
    do while (associated(o_p))
      if (o_p%name == name) exit
      o_p => o_p%next
    end do

  end function fof_obj_pointer



  !_____________________________________________________________________________
  subroutine fof_write_json(this, unit)
    !< write object in json format
    !___________________________________________________________________________
    class(fof_t), intent(in) :: this
    integer, intent(in), optional :: unit

    integer :: lunit

    lunit = output_unit
    if(present(unit)) lunit = unit

    write(lunit,'(a)') '{'
    call fof_to_json(this, '  ', lunit)
    write(lunit,'(/,a)') '}'

  end subroutine fof_write_json



  !_____________________________________________________________________________
  recursive subroutine fof_to_json(f, indent, unit)
    !< recursive write json format object
    !___________________________________________________________________________
    type(fof_t), intent(in)   :: f
    character(*), intent(in) :: indent
    integer, intent(in) :: unit

    logical :: first

    type(o_t), pointer :: o_p

    o_p => f%first

    first= .true.
    do while (associated(o_p))
      if (first) then
        first = .false.
      else
        write(unit,'(a)') ','
      end if
      write(unit,'(a)',advance='no') indent // '"' // o_p%name // '"'

      select type (v_p => o_p%value)
      type is (fof_t)
        write(unit,'(a)') ': {'
        call fof_to_json(v_p, indent//'  ', unit)
        write(unit,'(/,a)',advance='no') indent // '}'
      type is (s_t)
        write(unit,'(a)',advance='no') ': '
        call v_p%write(unit)
      type is (v_t)
        write(unit,'(a)',advance='no') ': '
        call v_p%write (unit)
      type is (m_t)
        write(unit,'(a)',advance='no') ': '
        call v_p%write (unit)
      type is (t_t)
        write(unit,'(a)',advance='no') ': '
        call v_p%write (unit)
      end select
      o_p => o_p%next
    end do

  end subroutine fof_to_json

end module fof_object
