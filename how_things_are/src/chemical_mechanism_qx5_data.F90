!> Parameters for the QX5 mechanism
!!
module chemical_mechanism_qx5_data

  implicit none
  private

  ! System dimensions

  !> Number of gas-phase species
  integer, parameter, public :: NUM_SPEC = 3
  !> Number of gas-phase reactions (all photolysis for this example)
  integer, parameter, public :: NUM_RXN  = 4

  ! Gas-phase species ids

  !> Explicit - Ethene (H2C=CH2)
  integer, parameter, public :: GAS_SPEC_ETHENE  = 1
  !> Explicit - Propene (H3C-CH=CH2)
  integer, parameter, public :: GAS_SPEC_PROPENE = 2
  !> Lumped - Any alkene (ie. something with a C=C bond) that isn't ethene or propene
  !! Notice that this big alk is different from that of our mock QXZ
  integer, parameter, public :: GAS_SPEC_BIGALK  = 3

  ! Gas-phase reactions (all photolysis for this example)

  !> ethene + hv -> stuff
  integer, parameter, public :: GAS_RXN_ETHENE   = 1
  !> propene + hv -> stuff
  integer, parameter, public :: GAS_RXN_PROPENE  = 2
  !> bigalk + hv -> stuff
  integer, parameter, public :: GAS_RXN_BIGALK_1 = 3
  !> bigalk + hv -> other stuff (different products than rxn 3, usually based on hv energy)
  integer, parameter, public :: GAS_RXN_BIGALK_2 = 4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module chemical_mechanism_qx5_data
