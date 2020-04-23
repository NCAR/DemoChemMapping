!> Parameters for the QXZ chemical mechanism
!!
module chemical_mechanism_qxz_data

  implicit none
  private

  ! System dimensions

  !> Number of gas-phase species
  integer, parameter, public :: NUM_SPEC = 3
  !> Number of gas-phase reactions (all photolysis for this example)
  integer, parameter, public :: NUM_RXN  = 4

  ! Gas-phase species ids

  !> Explicit - Ethene (H2C=CH2)
  integer, parameter, public :: GAS_SPEC_ETHENE = 1
  !> Lumped - Any terminal alkene (-CH2=CH3) with less than 7 carbons that isn't ethene
  integer, parameter, public :: GAS_SPEC_TALK   = 2
  !> Lumped - Any internal alkene (-CH2=CH2-) with less than 7 carbons
  integer, parameter, public :: GAS_SPEC_IALK   = 3
  !> Lumped - Any alkene with at least 7 carbons
  !! Notice that this big alk is different from that of our mock QX5
  integer, parameter, public :: GAS_SPEC_BIGALK = 4

  ! Gas-phase reactions (all photolysis for this example)

  !> ethene + hv -> stuff
  integer, parameter, public :: GAS_RXN_ETHENE = 1
  !> talk + hv -> stuff
  integer, parameter, public :: GAS_RXN_TALK   = 2
  !> ialk + hv -> stuff
  integer, parameter, public :: GAS_RXN_IALK   = 3
  !> bigalk + hv -> stuff
  integer, parameter, public :: GAS_RXN_BIGALK = 4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module chemical_mechanism_qxz_data
