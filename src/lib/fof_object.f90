module fof_object
  use fof_scalar
  use fof_vector
  use fof_matrix
  use fof_string
  implicit none



  public :: object, fof_list



  private
  character(*), parameter  :: path_separator_ = '/'

  type, extends(scalar) :: object
    !< scalar object list
    private
    character(:), allocatable :: name
    type(object), pointer :: prev     => null()
    type(object), pointer :: next     => null()
  contains
    final :: obj_final
  end type object




  type :: fof_list
    !< fortran object format

    type(object), pointer :: first => null()

  contains
    !---------------
    !> constructor
    !--------------
    generic   :: add    => fof_add_scalar,&
                          fof_add_vector, &
                          fof_add_matrix

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
    generic:: get => fof_get_scalar_integer, &
    fof_get_scalar_real,&
    fof_get_scalar_logical,&
    fof_get_scalar_character


    !---------
    !> output
    !---------
    procedure :: to_json => to_json_string

    !---------------
    !> Destructor
    !---------------
    procedure :: destory => fof_destory
    final :: fof_final

    !----------
    !> private
    !----------
    procedure, private :: fof_add_scalar,fof_add_vector,fof_add_matrix
    procedure, private :: fof_get_scalar_integer, &
    fof_get_scalar_real, &
    fof_get_scalar_logical, &
    fof_get_scalar_character
  end type fof_list



contains


  recursive subroutine obj_final (this)
    !< destory the object.
    type(object), intent(inout) :: this
    if(allocated(this%name)) deallocate(this%name)
    if(allocated(this%value)) deallocate(this%value)
    if(associated(this%next)) then
      deallocate(this%next)
      ! nullify(this%next)
    endif
  end subroutine obj_final


  recursive subroutine fof_destory(this)
    class(fof_list), intent(inout) :: this
    if (associated(this%first)) then
      deallocate(this%first)
      ! nullify(this%first)
    endif
  end subroutine fof_destory


  recursive subroutine fof_final (this)
    !< clear fof
    type(fof_list), intent(inout) :: this
    call this%destory()
  end subroutine fof_final




  function fof_obj_number(this) result(num)
    !< returns the number of object in the object list.
    class(fof_list), intent(in) :: this
    integer :: num
    type(object), pointer :: o_p

    num = 0
    o_p => this%first
    do while (associated(o_p))
      num = num + 1
      o_p => o_p%next
    end do
  end function fof_obj_number




  function fof_is_exist_obj(this, name) result(exist)
    !< return true if object NAME exists.
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: name
    logical :: exist
    exist = associated(this%o_p(name))
  end function fof_is_exist_obj




  function fof_is_empty(this) result(isempty)
    !< return true if the list contains no object.
    class(fof_list), intent(in) :: this
    logical :: isempty
    isempty = .not.associated(this%first)
  end function fof_is_empty




  subroutine fof_append_obj(this, o_p)
    !< append an obj to the list.
    class(fof_list), intent(inout) :: this
    type(object), pointer, intent(in) :: o_p

    type(object), pointer :: tail

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




  subroutine fof_insert_obj(this, name, value)
    !< insert an obj to a list.
    class(fof_list), intent(inout) :: this

    character(*), intent(in) :: name
    class(*), intent(in) :: value

    type(object), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
    else
      allocate(o_p)
      o_p%name = name
      o_p%value = value
      call this%append(o_p)
    end if

  end subroutine fof_insert_obj




  subroutine fof_update_obj(this, name, value)
    !< update an exist object.
    class(fof_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value

    type(object), pointer :: o_p

    o_p => this%o_p(name)
    if (associated(o_p)) then
      !! Replace existing value with the given one.
      if (allocated(o_p%value)) deallocate(o_p%value)
      allocate(o_p%value, source = value)

    else
      error stop "the object '" //name //"' does not exist here"
    end if

  end subroutine fof_update_obj




  subroutine fof_remove_obj(this, name)
    !< remove an obj in the list.

    class(fof_list), intent(inout) :: this
    character(*), intent(in) :: name

    type(object), pointer :: o_p

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




  recursive subroutine fof_add_scalar(this, path, value)
    !< add scalar object to the list
    class(fof_list), intent(inout) :: this
    character(*), intent(in) :: path
    class(*), intent(in) :: value

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)
    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%add(lpath,value)
        class default
        o_p%name = name
        ! o_p%value = scalar(value)
        o_p%value = value
        call this%append(o_p)
      end select

    else
      allocate(o_p)
      if(sp>1) then
        o_p%name = name
        allocate(fof_list :: o_p%value)
        select type(v_p =>o_p%value)
        type is(fof_list)
          call v_p%add(lpath, value)
        end select
      else
        o_p%name = name
        o_p%value = value
      endif
      call this%append(o_p)
    end if

  end subroutine fof_add_scalar



  recursive subroutine fof_add_vector(this, path, value)
    !< add vector object to the list
    class(fof_list), intent(inout) :: this
    character(*), intent(in) :: path
    class(*), intent(in) :: value(:)

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)
    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%add(lpath,value)
        class default
        o_p%name = name
        ! o_p%value = scalar(value)
        o_p%value = vector(value)
        call this%append(o_p)
      end select

    else
      allocate(o_p)
      if(sp>1) then
        o_p%name = name
        allocate(fof_list :: o_p%value)
        select type(v_p =>o_p%value)
        type is(fof_list)
          call v_p%add(lpath, value)
        end select
      else
        o_p%name = name
        o_p%value = vector(value)
      endif
      call this%append(o_p)
    end if

  end subroutine fof_add_vector




  subroutine fof_add_matrix(this, path, value)
    !< add matrix object to the list
    class(fof_list), intent(inout) :: this
    character(*), intent(in) :: path
    class(*), intent(in) :: value(:,:)

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)
    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%add(lpath,value)
        class default
        o_p%name = name
        ! o_p%value = scalar(value)
        o_p%value = matrix(value)
        call this%append(o_p)
      end select

    else
      allocate(o_p)
      if(sp>1) then
        o_p%name = name
        allocate(fof_list :: o_p%value)
        select type(v_p =>o_p%value)
        type is(fof_list)
          call v_p%add(lpath, value)
        end select
      else
        o_p%name = name
        o_p%value = matrix(value)
      endif
      call this%append(o_p)
    end if

  end subroutine fof_add_matrix

  recursive subroutine  fof_get_scalar_integer(this, path, value)
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: path
    integer, intent(out) :: value

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)

    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%get(lpath,value)
      type is (integer)
        value = v_p
      class default
        error stop "The object is not integer type"
      end select

    else
      error stop "The object doesn't exist"
    end if

  end subroutine  fof_get_scalar_integer


  recursive subroutine  fof_get_scalar_real(this, path, value)
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: path
    real, intent(out) :: value

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)

    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%get(lpath,value)
      type is (real)
        value = v_p
      class default
        error stop "The object is not real type"
      end select

    else
      error stop "The object doesn't exist"
    end if

  end subroutine  fof_get_scalar_real


  recursive subroutine  fof_get_scalar_logical(this, path, value)
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: path
    logical, intent(out) :: value

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)

    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%get(lpath,value)
      type is (logical)
        value = v_p
      class default
        error stop "The object is not logical type"
      end select

    else
      error stop "The object doesn't exist"
    end if

  end subroutine  fof_get_scalar_logical


  recursive subroutine  fof_get_scalar_character(this, path, value)
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: path
    character(:),allocatable, intent(out) :: value

    character(:),allocatable :: lpath, name
    type(object), pointer :: o_p
    integer :: ls, sp

    lpath = trim(adjustl(path))
    ls = len(lpath)
    sp = index(lpath, path_separator_)

    name = lpath
    if(sp > 1) name = lpath(1:sp-1)
    lpath = lpath(sp+1:)

    o_p => this%o_p(name)

    if (associated(o_p)) then
      ! error stop "'"//name// "' has already existed, call this%update(name, value) to update it."
      select type(v_p => o_p%value)
      type is(fof_list)
        call v_p%get(lpath,value)
      type is (character(*))
        value = v_p
      class default
        error stop "The object is not character type"
      end select

    else
      error stop "The object doesn't exist"
    end if

  end subroutine  fof_get_scalar_character


  function fof_value_pointer(this, name) result(v_p)
    !< returns a pointer to the value of obj, or a null pointer.

    class(fof_list), intent(in) :: this
    character(*), intent(in) :: name
    class(*), pointer :: v_p

    type(object), pointer :: o_p

    o_p => this%o_p(name)

    if (associated(o_p)) then
      v_p => o_p%value
    else
      v_p => null()
    end if

  end function fof_value_pointer




  function fof_obj_pointer(this, name) result (o_p)
    !< return a pointer to an obj, or a null pointer.
    class(fof_list), intent(in) :: this
    character(*), intent(in) :: name

    type(object), pointer :: o_p

    o_p => this%first
    do while (associated(o_p))
      if (o_p%name == name) exit
      o_p => o_p%next
    end do

  end function fof_obj_pointer




  function to_json_string(this) result(s)
    !< write object in json format
    class(fof_list), intent(in) :: this
    character(:), allocatable:: s

    s = '{' //new_line('a')//fof_to_json_string(this, '  ')//new_line('a')//'}'

  end function to_json_string




  recursive function fof_to_json_string(f, indent) result(s)
    !< recursive write json format object
    type(fof_list), intent(in)  :: f
    character(*), intent(in) :: indent
    character(:), allocatable:: s

    logical :: first

    type(object), pointer :: o_p

    o_p => f%first

    s = ''
    first= .true.
    do while (associated(o_p))
      if (first) then
        first = .false.
      else
        s = s// ','//new_line('a')
      end if
      s = s // indent // '"' // o_p%name // '"'

      select type (v_p => o_p%value)
      type is (fof_list)
        s = s// ': {' //new_line('a')//fof_to_json_string(v_p, indent//'  ')//new_line('a')//indent // '}'
      type is (scalar)
        s = s // ': '//v_p%to_str()
      type is (vector)
        s = s // ': '//v_p%to_str(vsep=',')
      type is (matrix)
        s = s // ': '//v_p%to_str(vsep=',',msep=',')
        class default
        s = s // ': '//to_str(v_p)
      end select
      o_p => o_p%next
    end do

  end function fof_to_json_string

end module fof_object
