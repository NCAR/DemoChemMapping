!> Wrapper for the ARES emissions model
!!
module emissions_ares

  implicit none
  private

  public :: calculate_rates

  !> Number of rates available
  integer, parameter :: NUM_RATE = 4

  ! ARES emissions species ids

  !> Explicit - Ethene (CH2=CH2)
  integer, parameter :: RATE_ETHENE      = 1
  !> Explicit - 1-Butene (CH3-CH2-CH=CH2)
  integer, parameter :: RATE_BUTENE      = 2
  !> Explicit - Cyclohexene (6C ring with one double bond)
  integer, parameter :: RATE_CYCLOHEXENE = 3

  !> Emissions rates from ARES [#/m3/s]
  real, dimension(NUM_RATE) :: ares_emission_rates__num_m3_s

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the emission rates
  subroutine calculate_rates( config_id, emission_rates__num_m3_s )

    use host_model_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Emission rates [#/m3/s]
    !! (ordered according to the chemical mechanism species)
    real, intent(out) :: emission_rates__num_m3_s(:)

    select case( config_id )
      case( CONFIG_QX5_TUV_ARES )
        call calculate_rates_qx5( config_id, emission_rates__num_m3_s )
      case( CONFIG_QX5_FASTJ_ARES )
        call calculate_rates_qx5( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_TUV_ARES )
        call calculate_rates_qxz( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_FASTJ_ARES )
        call calculate_rates_qxz( config_id, emission_rates__num_m3_s )
    end select

  end subroutine calculate_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the emission rates for the QX5 mechanism
  subroutine calculate_rates_qx5( config_id, emission_rates__num_m3_s )

    use chemical_mechanism_qx5_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Emission rates [#/m3/s]
    !! (ordered according to the chemical mechanism species)
    real, intent(out) :: emission_rates__num_m3_s(:)

    write(*,*) "Calculating ARES emissions for QX5"

    ! ethene is ethene
    emission_rates__num_m3_s( GAS_SPEC_ETHENE ) =                             &
      ares_emission_rates__num_m3_s( RATE_ETHENE )
    ! propene scales more closely with 1-butene than ethene
    ! (but in MARGE only ethene is available)
    emission_rates__num_m3_s( GAS_SPEC_PROPENE ) =                            &
      0.7 * ares_emission_rates__num_m3_s( RATE_BUTENE )
    ! BIGALK is cyclohexene (unlike for QXZ)
    emission_rates__num_m3_s( GAS_SPEC_BIGALK ) =                             &
      ares_emission_rates__num_m3_s( RATE_CYCLOHEXENE )

  end subroutine calculate_rates_qx5

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the emission rates for the QXZ mechanism
  subroutine calculate_rates_qxz( config_id, emission_rates__num_m3_s )

    use chemical_mechanism_qxz_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Emission rates [#/m3/s]
    !! (ordered according to the chemical mechanism species)
    real, intent(out) :: emission_rates__num_m3_s(:)

    write(*,*) "Calculating ARES emissions for QXZ"

    ! ethene is ethene
    emission_rates__num_m3_s( GAS_SPEC_ETHENE ) =                             &
      ares_emission_rates__num_m3_s( RATE_ETHENE )
    ! cyclohexene is an internal alkene
    emission_rates__num_m3_s( GAS_SPEC_IALK ) =                               &
      ares_emission_rates__num_m3_s( RATE_CYCLOHEXENE )
    ! butene is a 1<C<7 terminal alkene
    emission_rates__num_m3_s( GAS_SPEC_TALK ) =                               &
      ares_emission_rates__num_m3_s( RATE_BUTENE )
    ! no BIGALK species (unlike QX5)
    emission_rates__num_m3_s( GAS_SPEC_BIGALK ) = 0

  end subroutine calculate_rates_qxZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module emissions_ares
