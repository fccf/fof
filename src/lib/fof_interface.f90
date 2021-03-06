! module fof_interface
!   use fof_object
!   use stringifor
!   implicit none
!
!
!   type fof_file
!     integer :: unit
!     character(:), allocatable :: filename
!     character(1) :: comment_char   = '!'
!     character(1) :: path_separator = '/'
!     type(fof_list), pointer:: data => null()
!
!   contains
!     procedure :: init
!
!     procedure :: add => add_scalar_by_path
!
!
!     procedure :: write_json
!
!
!   end type fof_file
!
! contains
!   subroutine init(this)
!     class(fof_file), intent(inout) :: this
!
!     allocate(this%data)
!   end subroutine init
!
!
!   recursive subroutine add_scalar_by_path(this, path, value)
!     class(fof_file), intent(inout) :: this
!     character(*), intent(in) :: path
!     class(*), intent(in)     :: value
!
!     type(fof_list), pointer   :: fp => null()
!
!     character(:),allocatable :: lpath, name
!
!     integer :: ls, sp
!
!
!     ls = len_trim(path)
!     sp = index(path,this%path_separator)
!     name = path(1:sp)
!
!
!     if(associated(this%data%o_p(name))) then
!     else
!
!
!
!
!
!   end subroutine add_scalar_by_path
!
!
!
!   subroutine write_json(this)
!     class(fof_file), intent(in) :: this
!
!     write(*,*) this%data%to_json()
!
!   end subroutine write_json
!
!
! end module fof_interface
