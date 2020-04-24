!> Abstract chemical mechanism
!!
!! Defines functionality of chemical mechanisms
!!
module chemical_mechanism

  implicit none
  private

  public :: chemical_mechanism_t

  !> Abstract chemical mechanism
  type, abstract :: chemical_mechanism_t
  contains
    !> Number of gas-phase species
    procedure(num_gas_species), deferred :: num_gas_species
    !> Number of photolysis reactions
    procedure(num_photolysis_reactions), deferred :: num_photolysis_reactions
    !> Species name-id pairs
    procedure(species_name_id_pairs), deferred :: species_name_id_pairs
    !> Reaction name-id pairs
    procedure(reaction_name_id_pairs), deferred :: reaction_name_id_pairs
    !> Advance the chemical state for a given time step
    procedure(solve), deferred :: solve
  end type chemical_mechanism_t

interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the nubmer of gas-phase species
  function num_gas_species( this )
    import chemical_mechanism_t

    !> Number of gas-phase species
    integer :: num_gas_species
    !> Chemical mechanism
    class(chemical_mechanism_t), intent(in) :: this

  end function num_gas_species

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the nubmer of photolysis reactions
  function num_photolysis_reactions( this )
    import chemical_mechanism_t

    !> Number of photolysis reactions
    integer :: num_photolysis_reactions
    !> Chemical mechanism
    class(chemical_mechanism_t), intent(in) :: this

  end function num_photolysis_reactions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Species name-id-pairs
  function species_name_id_pairs( this ) result( pairs )
    use map,                           only : name_id_pair_t
    import chemical_mechanism_t

    !> Species name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Chemical mechanism
    class(chemical_mechanism_t), intent(in) :: this

  end function species_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reaction name-id-pairs
  function reaction_name_id_pairs( this ) result( pairs )
    use map,                           only : name_id_pair_t
    import chemical_mechanism_t

    !> Reaction name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Chemical mechanism
    class(chemical_mechanism_t), intent(in) :: this

  end function reaction_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Solve the chemical system
  subroutine solve( this,                                                     &
                    gas_species_concentrations__num_m3,                       &
                    emission_rates__num_m3_s,                                 &
                    photolysis_rates__s )
    import chemical_mechanism_t

    !> Chemical mechanism
    class(chemical_mechanism_t), intent(in) :: this
    !> Gas-phase species concentrations [#/m3]
    real, intent(inout) :: gas_species_concentrations__num_m3(:)
    !> Emission rates [#/m3/s]
    real, intent(in) :: emission_rates__num_m3_s(:)
    !> Photolysis rates [1/s]
    real, intent(in) :: photolysis_rates__s(:)

  end subroutine solve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end interface

end module chemical_mechanism
