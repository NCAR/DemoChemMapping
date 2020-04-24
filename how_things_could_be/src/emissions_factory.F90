!> Emissions module factory
!!
!! Build specific emissions modules based on input data
!!
module emissions_factory

  use emissions,                       only : emissions_t
  use emissions_marge,                 only : emissions_marge_t
  use emissions_ares,                  only : emissions_ares_t

  implicit none
  private

  public :: new_emissions

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Build a emissions module
  function new_emissions( config_file_name ) result( new_mod )

    !> New module
    class(emissions_t), pointer :: new_mod
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    integer :: file_unit, stat
    character(len=15) :: module_name

    file_unit = 63
    open( file_unit, file=config_file_name, IOSTAT=stat )
    if( stat .ne. 0 ) then
      write(*,*) "Error opening file ", config_file_name
      stop 3
    end if
    read( file_unit, *, IOSTAT=stat ) module_name
    if( stat .ne. 0 ) then
      write(*,*) "File IO error for ", config_file_name
      stop 3
    end if

    if( trim( module_name ) .eq. 'MARGE' ) then
      new_mod => emissions_marge_t( config_file_name )
    else if( trim( module_name ) .eq. 'ARES' ) then
      new_mod => emissions_ares_t( config_file_name )
    else
      write(*,*) "Unknown emissions module '", trim( module_name ), "'"
      stop 3
    end if

  end function new_emissions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module emissions_factory
