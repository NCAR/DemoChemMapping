!> FASTJ photolysis module
!!
module photolysis_fastj

  use photolysis,                      only : photolysis_t
  use map,                             only : name_id_pair_t, component_map_t

  implicit none
  private

  public :: photolysis_fastj_t

  !> FASTJ photolysis module
  type, extends(photolysis_t) :: photolysis_fastj_t
  contains
    !> Reaction name-id pairs
    procedure :: reaction_name_id_pairs
    !> Calculate the photolysis_fastj reaction rates
    procedure :: get_rates
  end type photolysis_fastj_t

  !> FASTJ photolysis module constructor
  interface photolysis_fastj_t
    procedure :: constructor
  end interface photolysis_fastj_t

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> FASTJ photolysis module constructor
  function constructor( config_file_name ) result( new_mod )

    !> New FASTJ photolysis module
    type(photolysis_fastj_t), pointer :: new_mod
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    ! The config file could be used at this point to run-time build the
    ! module including its namespace
    allocate( new_mod )

    write(*,*) "Creating the FASTJ photolysis module"

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reaction name-id-pairs
  function reaction_name_id_pairs( this ) result( pairs )

    !> Reaction name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Photolysis module
    class(photolysis_fastj_t), intent(in) :: this

    allocate( pairs( 3 ) )

    pairs( 1 )%name = "ETHENE"
    pairs( 1 )%id   = 1

    pairs( 2 )%name = "BUTENE"
    pairs( 2 )%id   = 2

    pairs( 3 )%name = "CYCLOHEXENE"
    pairs( 3 )%id   = 3

  end function reaction_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photolysis reaction rates
  subroutine get_rates( this, photolysis_rates__s, map )

    !> Photolysis module
    class(photolysis_fastj_t), intent(in) :: this
    !> Photolysis rates [1/s]
    real, intent(inout) :: photolysis_rates__s(:)
    !> Map to local namespace
    type(component_map_t), intent(in) :: map

    write(*,*) "Running the FASTJ photolysis module and mapping from "//      &
               "the local namespace without caring where the rates are going"
    write(*,*) "Using map:"
    call map%print( )

  end subroutine get_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module photolysis_fastj
