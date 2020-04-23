!> Wrapper for the MARGE emissions model
!!
module emissions_marge

  implicit none
  private

  public :: calculate_rates

  !> Number of rates available
  integer, parameter :: NUM_RATE = 4

  ! MARGE emissions species ids

  !> Explicit - ethene (CH2=CH2)
  integer, parameter :: RATE_ETHENE      = 1
  !> Explicit - Isoprene (CH2=C(-CH3)-CH=CH2)
  integer, parameter :: RATE_ISOPRENE    = 2
  !> Explicit - Monoterpenes
  integer, parameter :: RATE_MONOTERPENE = 3

  !> Emissions rates from MARGE [#/m3/s]
  real, dimension(NUM_RATE) :: marge_emission_rates__num_m3_s

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
      case( CONFIG_QX5_TUV_MARGE )
        call calculate_rates_qx5( config_id, emission_rates__num_m3_s )
      case( CONFIG_QX5_FASTJ_MARGE )
        call calculate_rates_qx5( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_TUV_MARGE )
        call calculate_rates_qxz( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_FASTJ_MARGE )
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

    write(*,*) "Calculating MARGE emissions for QX5"

    ! ethene is ethene
    emission_rates__num_m3_s( GAS_SPEC_ETHENE ) =                             &
      marge_emission_rates__num_m3_s( RATE_ETHENE )
    ! propene can scale with ethene
    emission_rates__num_m3_s( GAS_SPEC_PROPENE ) =                            &
      0.3 * marge_emission_rates__num_m3_s( RATE_ETHENE )
    ! BIGALK is a combination of isoprene and monoterpenes (unlike QX5)
    emission_rates__num_m3_s( GAS_SPEC_BIGALK ) =                             &
      marge_emission_rates__num_m3_s( RATE_ISOPRENE ) +                       &
      marge_emission_rates__num_m3_s( RATE_MONOTERPENE )

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

    write(*,*) "Calculating MARGE emissions for QXZ"

    ! ethene is ethene
    emission_rates__num_m3_s( GAS_SPEC_ETHENE ) =                             &
      marge_emission_rates__num_m3_s( RATE_ETHENE )
    ! there are no 1<C<7 alkenes in MARGE
    emission_rates__num_m3_s( GAS_SPEC_IALK ) = 0
    ! isoprene is a 1<C<7 terminal alkene
    emission_rates__num_m3_s( GAS_SPEC_TALK ) =                               &
      marge_emission_rates__num_m3_s( RATE_ISOPRENE )
    ! BIGALK gets the monoterpenes (unlike QX5)
    emission_rates__num_m3_s( GAS_SPEC_BIGALK ) =                             &
      marge_emission_rates__num_m3_s( RATE_MONOTERPENE )

  end subroutine calculate_rates_qxZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module emissions_marge
