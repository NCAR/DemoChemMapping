!> The QXZ chemical mechanism
!!
module chemical_mechanism_qxz

  use chemical_mechanism,              only : chemical_mechanism_t
  use map,                             only : name_id_pair_t

  implicit none
  private

  public :: chemical_mechanism_qxz_t

  !> The QXZ chemical mechanism
  type, extends(chemical_mechanism_t) :: chemical_mechanism_qxz_t
  contains
    !> Number of gas-phase species
    procedure :: num_gas_species
    !> Number of photolysis reactions
    procedure :: num_photolysis_reactions
    !> Species name-id pairs
    procedure :: species_name_id_pairs
    !> Reaction name-id pairs
    procedure :: reaction_name_id_pairs
    !> Advance the chemical state for a given time step
    procedure :: solve
  end type chemical_mechanism_qxz_t

  !> QXZ mechanism constructor
  interface chemical_mechanism_qxz_t
    procedure :: constructor
  end interface chemical_mechanism_qxz_t

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the nubmer of gas-phase species
  function num_gas_species( this )

    !> Number of gas-phase species
    integer :: num_gas_species
    !> Chemical mechanism
    class(chemical_mechanism_qxz_t), intent(in) :: this

    num_gas_species = 4

  end function num_gas_species

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the nubmer of photolysis reactions
  function num_photolysis_reactions( this )

    !> Number of photolysis reactions
    integer :: num_photolysis_reactions
    !> Chemical mechanism
    class(chemical_mechanism_qxz_t), intent(in) :: this

    num_photolysis_reactions = 4

  end function num_photolysis_reactions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> QXZ mechanism constructor
  function constructor( config_file_name ) result( new_mech )

    !> New mechanism
    type(chemical_mechanism_qxz_t), pointer :: new_mech
    !> Configuration file name
    character(len=*), intent(in) :: config_file_name

    ! The config file could be used at this point to run-time build the
    ! chemical mechanism species and reactions
    allocate( new_mech )

    write(*,*) "Creating QXZ chemical mechanism"

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Species name-id-pairs
  function species_name_id_pairs( this ) result( pairs )

    !> Species name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Chemical mechanism
    class(chemical_mechanism_qxz_t), intent(in) :: this

    allocate( pairs( 4 ) )

    pairs( 1 )%name = "ETHENE"
    pairs( 1 )%id   = 1

    pairs( 2 )%name = "TALK"
    pairs( 2 )%id   = 2

    pairs( 3 )%name = "IALK"
    pairs( 3 )%id   = 3

    pairs( 4 )%name = "BIGALK"
    pairs( 4 )%id   = 4

  end function species_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reaction name-id-pairs
  function reaction_name_id_pairs( this ) result( pairs )

    !> Reaction name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Chemical mechanism
    class(chemical_mechanism_qxz_t), intent(in) :: this

    allocate( pairs( 4 ) )

    pairs( 1 )%name = "ETHENE"
    pairs( 1 )%id   = 1

    pairs( 2 )%name = "TALK"
    pairs( 2 )%id   = 2

    pairs( 3 )%name = "IALK"
    pairs( 3 )%id   = 3

    pairs( 4 )%name = "BIGALK"
    pairs( 4 )%id   = 4

  end function reaction_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Solve the chemical system
  subroutine solve( this,                                                     &
                    gas_species_concentrations__num_m3,                       &
                    emission_rates__num_m3_s,                                 &
                    photolysis_rates__s )

    !> Chemical mechanism
    class(chemical_mechanism_qxz_t), intent(in) :: this
    !> Gas-phase species concentrations [#/m3]
    real, intent(inout) :: gas_species_concentrations__num_m3(:)
    !> Emission rates [#/m3/s]
    real, intent(in) :: emission_rates__num_m3_s(:)
    !> Photolysis rates [1/s]
    real, intent(in) :: photolysis_rates__s(:)

    write(*,*) "Solving QXZ mechanism with photolysis and emissions rates "// &
               "in local order and units without caring where they came from."

  end subroutine solve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module chemical_mechanism_qxz
