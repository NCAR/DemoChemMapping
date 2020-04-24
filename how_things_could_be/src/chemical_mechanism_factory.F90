!> Chemical mechanism factory
!!
!! Build specific chemical mechanisms based on input data
!!
module chemical_mechanism_factory

  use chemical_mechanism,              only : chemical_mechanism_t
  use chemical_mechanism_qx5,          only : chemical_mechanism_qx5_t
  use chemical_mechanism_qxz,          only : chemical_mechanism_qxz_t

  implicit none
  private

  public :: new_chemical_mechanism

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Build a chemical mechanism
  function new_chemical_mechanism( config_file_name ) result( new_mech )

    !> New mechanism
    class(chemical_mechanism_t), pointer :: new_mech
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    integer :: file_unit, stat
    character(len=15) :: mechanism_name

    file_unit = 63
    open( file_unit, file=config_file_name, IOSTAT=stat )
    if( stat .ne. 0 ) then
      write(*,*) "Error opening file ", config_file_name
      stop 3
    end if
    read( file_unit, *, IOSTAT=stat ) mechanism_name
    if( stat .ne. 0 ) then
      write(*,*) "File IO error for ", config_file_name
      stop 3
    end if

    if( trim( mechanism_name ) .eq. 'QX5' ) then
      new_mech => chemical_mechanism_qx5_t( config_file_name )
    else if( trim( mechanism_name ) .eq. 'QXZ' ) then
      new_mech => chemical_mechanism_qxz_t( config_file_name )
    else
      write(*,*) "Unknown chemical mechanism '", trim( mechanism_name ), "'"
      stop 3
    end if

  end function new_chemical_mechanism

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module chemical_mechanism_factory
