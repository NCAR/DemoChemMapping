!> TUV photolysis module
!!
module photolysis_tuv

  use photolysis,                      only : photolysis_t
  use map,                             only : name_id_pair_t, component_map_t

  implicit none
  private

  public :: photolysis_tuv_t

  !> TUV photolysis module
  type, extends(photolysis_t) :: photolysis_tuv_t
  contains
    !> Reaction name-id pairs
    procedure :: reaction_name_id_pairs
    !> Calculate the photolysis_tuv reaction rates
    procedure :: get_rates
  end type photolysis_tuv_t

  !> TUV photolysis module constructor
  interface photolysis_tuv_t
    procedure :: constructor
  end interface photolysis_tuv_t

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> TUV photolysis module constructor
  function constructor( config_file_name ) result( new_mod )

    !> New TUV photolysis module
    type(photolysis_tuv_t), pointer :: new_mod
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    ! The config file could be used at this point to run-time build the
    ! module including its namespace
    allocate( new_mod )

    write(*,*) "Creating the TUV photolysis module"

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reaction name-id-pairs
  function reaction_name_id_pairs( this ) result( pairs )

    !> Reaction name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Photolysis module
    class(photolysis_tuv_t), intent(in) :: this

    allocate( pairs( 3 ) )

    pairs( 1 )%name = "ETHENE"
    pairs( 1 )%id   = 1

    pairs( 2 )%name = "BUTENE"
    pairs( 2 )%id   = 2

    pairs( 3 )%name = "ISOPRENE"
    pairs( 3 )%id   = 3

  end function reaction_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photolysis_tuv reaction rates
  subroutine get_rates( this, photolysis_rates__s, map )

    !> Photolysis module
    class(photolysis_tuv_t), intent(in) :: this
    !> Photolysis rates [1/s]
    real, intent(inout) :: photolysis_rates__s(:)
    !> Map to local namespace
    type(component_map_t), intent(in) :: map

    write(*,*) "Running the TUV photolysis module and mapping from "//        &
               "the local namespace without caring where the rates are going"
    write(*,*) "Using map:"
    call map%print( )

  end subroutine get_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module photolysis_tuv
