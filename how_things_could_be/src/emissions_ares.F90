!> ARES emissions module
!!
module emissions_ares

  use emissions,                       only : emissions_t
  use map,                             only : name_id_pair_t, component_map_t

  implicit none
  private

  public :: emissions_ares_t

  !> ARES emissions module
  type, extends(emissions_t) :: emissions_ares_t
  contains
    !> Species name-id pairs
    procedure :: species_name_id_pairs
    !> Calculate the emissions rates
    procedure :: get_rates
  end type emissions_ares_t

  !> ARES emissions module constructor
  interface emissions_ares_t
    procedure :: constructor
  end interface emissions_ares_t

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> ARES emissions module constructor
  function constructor( config_file_name ) result( new_mod )

    !> New ARES emissions module
    type(emissions_ares_t), pointer :: new_mod
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    ! The config file could be used at this point to run-time build the
    ! module including its namespace
    allocate( new_mod )

    write(*,*) "Creating the ARES emissions module"

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Species name-id-pairs
  function species_name_id_pairs( this ) result( pairs )

    !> Species name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Emissions module
    class(emissions_ares_t), intent(in) :: this

    allocate( pairs( 3 ) )

    pairs( 1 )%name = "ETHENE"
    pairs( 1 )%id   = 1

    pairs( 2 )%name = "BUTENE"
    pairs( 2 )%id   = 2

    pairs( 3 )%name = "CYCLOHEXENE"
    pairs( 3 )%id   = 3

  end function species_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the emissions rates
  subroutine get_rates( this, emission_rates__num_m3_s, map )

    !> Emissions module
    class(emissions_ares_t), intent(in) :: this
    !> Emission rates [#/m3/s]
    real, intent(inout) :: emission_rates__num_m3_s(:)
    !> Map to local namespace
    type(component_map_t), intent(in) :: map

    write(*,*) "Running the ARES emissions module and mapping from "//       &
               "the local namespace without caring where the rates are going"
    write(*,*) "Using map:"
    call map%print( )

  end subroutine get_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module emissions_ares
