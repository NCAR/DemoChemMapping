!> Photolysis module factory
!!
!! Build specific photolysis modules based on input data
!!
module photolysis_factory

  use photolysis,                      only : photolysis_t
  use photolysis_fastj,                only : photolysis_fastj_t
  use photolysis_tuv,                  only : photolysis_tuv_t

  implicit none
  private

  public :: new_photolysis

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Build a photolysis module
  function new_photolysis( config_file_name ) result( new_mod )

    !> New module
    class(photolysis_t), pointer :: new_mod
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

    if( trim( module_name ) .eq. 'TUV' ) then
      new_mod => photolysis_tuv_t( config_file_name )
    else if( trim( module_name ) .eq. 'FASTJ' ) then
      new_mod => photolysis_fastj_t( config_file_name )
    else
      write(*,*) "Unknown photolysis module '", trim( module_name ), "'"
      stop 3
    end if

  end function new_photolysis

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module photolysis_factory
