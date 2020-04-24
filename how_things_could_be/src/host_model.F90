!> Host Model
!!
!! Mock version of a host model that is run-time configurable and uses
!! mapping objects to associate local names of explicit and lumped
!! chemical species among model components.
!!
!! Notice that there is no reference to specific chemical mechanisms,
!! photolysis modules, or emissions modules in the host model code.
!! There are also no species names.
!!
program host_model

  use chemical_mechanism,              only : chemical_mechanism_t
  use chemical_mechanism_factory,      only : new_chemical_mechanism
  use emissions,                       only : emissions_t
  use emissions_factory,               only : new_emissions
  use photolysis,                      only : photolysis_t
  use photolysis_factory,              only : new_photolysis
  use map,                             only : component_map_t, create_map

  implicit none

  class(chemical_mechanism_t), pointer :: chemical_mechanism
  class(emissions_t),          pointer :: emissions
  class(photolysis_t),         pointer :: photolysis

  type(component_map_t) :: chemistry_emissions_map
  type(component_map_t) :: chemistry_photolysis_map

  !> Gas-phase chemical species concentrations [#/m3]
  real, allocatable :: gas_species_conc__num_m3(:)
  !> Gas-phase species emissions rates [#/m3/s]
  real, allocatable :: emission_rates__num_m3_s(:)
  !> Gas-phase photolysis rates [1/s]
  real, allocatable :: photolysis_rates__s(:)

  integer :: argc, i_arg
  character(len=256), dimension(5) :: argv

  if( command_argument_count( ) .ne. 5 ) then
    write(*,*) "Usage: host_model chemistry.config emissions.config "//      &
               "photolysis.config chemistry_emissions.map "//                &
               "chemistry_photolysis.map"
    stop 3
  end if

  ! get the configuration files
  do i_arg = 1, 5
    call get_command_argument( i_arg, argv( i_arg ) )
  end do

  ! Initialization

  ! initialize the model components
  chemical_mechanism => new_chemical_mechanism( argv( 1 ) )
  emissions          => new_emissions(          argv( 2 ) )
  photolysis         => new_photolysis(         argv( 3 ) )

  ! initialize the maps
  chemistry_emissions_map  =                                                  &
    create_map( argv( 4 ),                                                    &
                chemical_mechanism%species_name_id_pairs( ),                  &
                emissions%species_name_id_pairs( ) )
  chemistry_photolysis_map =                                                  &
    create_map( argv( 5 ),                                                    &
                chemical_mechanism%reaction_name_id_pairs( ),                 &
                photolysis%reaction_name_id_pairs( ) )

  ! allocate the state arrays
  allocate( gas_species_conc__num_m3(                                         &
    chemical_mechanism%num_gas_species( ) ) )
  allocate( emission_rates__num_m3_s(                                         &
    chemical_mechanism%num_gas_species( ) ) )
  allocate( photolysis_rates__s(                                              &
    chemical_mechanism%num_photolysis_reactions( ) ) )

  write(*,*) "Running model"

  ! Run Time!

  ! time/grid loops

    ! for one cell
    emission_rates__num_m3_s(:) = 0.0
    photolysis_rates__s(:)       = 0.0
    call emissions%get_rates(      emission_rates__num_m3_s,                  &
                                   chemistry_emissions_map )
    call photolysis%get_rates(     photolysis_rates__s,                       &
                                   chemistry_photolysis_map )
    call chemical_mechanism%solve( gas_species_conc__num_m3,                  &
                                   emission_rates__num_m3_s,                  &
                                   photolysis_rates__s )

  ! end time/grid loops

  write(*,*) "Finished model!"

end program host_model
