!> Wrapper for the QXZ chemical mechanism
!!
module chemical_mechanism_qxZ

  use photolysis_tuv,                                                        &
    only : tuv_calculate_rates => calculate_rates
  use photolysis_fastj,                                                      &
    only : fastj_calculate_rates => calculate_rates
  use emissions_marge,                                                       &
    only : marge_calculate_rates => calculate_rates
  use emissions_ares,                                                        &
    only : ares_calculate_rates => calculate_rates
  use chemical_mechanism_qxz_data,     only : NUM_SPEC, NUM_RXN
  use host_model_data

  implicit none
  private

  public :: solve

  !> Photolysis reaction rates [1/s]
  !!
  !! Photolysis is actually tied to specific reactions, not species.
  !! For example O3 has two separate photolysis reactions with different
  !! products and different rates:
  !!   O3 + hv -> O(1D) + O2
  !!   O3 + hv -> O(3P) + O2
  !!
  real :: photolysis_rates__s(      NUM_RXN  )

  !> Emissions rates per gas-phase species [#/m3/s]
  real :: emission_rates__num_m3_s( NUM_SPEC )

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Solve the mechanism for a single grid cell
  subroutine solve( config_id, species_conc__num_m3 )

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Gas-phase species number concentations [#/m3]
    real, intent(in) :: species_conc__num_m3( NUM_SPEC )

    select case( config_id )
      case( CONFIG_QXZ_TUV_MARGE   )
        call tuv_calculate_rates(   config_id, photolysis_rates__s      )
        call marge_calculate_rates( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_TUV_ARES    )
        call tuv_calculate_rates(   config_id, photolysis_rates__s      )
        call ares_calculate_rates(  config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_FASTJ_MARGE )
        call fastj_calculate_rates( config_id, photolysis_rates__s      )
        call marge_calculate_rates( config_id, emission_rates__num_m3_s )
      case( CONFIG_QXZ_FASTJ_ARES  )
        call fastj_calculate_rates( config_id, photolysis_rates__s      )
        call ares_calculate_rates(  config_id, emission_rates__num_m3_s )
    end select

    ! Add emissions to concentrations based on rate and time step

    ! Update the photolysis rates in the qxZ native rates array

    ! Here the qxZ mechanism would be run
    write(*,*) "Running QXZ"

  end subroutine solve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module chemical_mechanism_qxz
