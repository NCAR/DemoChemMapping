!> Wrapper for the TUV photolysis module
!!
module photolysis_tuv

  implicit none
  private

  public :: calculate_rates

  !> Number of rates available
  integer, parameter :: NUM_RATE = 3

  ! TUV photolysis rate ids

  !> ethene + hv (CH2=CH2)
  integer, parameter :: RATE_ETHENE   = 1
  !> propene + hv (CH3-CH2-CH=CH2)
  integer, parameter :: RATE_BUTENE   = 2
  !> isoprene + hv (CH2=C(-CH3)-CH=CH2)
  integer, parameter :: RATE_ISOPRENE = 3

  !> Photolysis rates from TUV [1/s]
  real, dimension(NUM_RATE) :: tuv_photolysis_rates__s

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate photolysis rates
  subroutine calculate_rates( config_id, photolysis_rates__s )

    use host_model_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Photolysis rates [1/s]
    !! (ordered according to the chemical mechanism reactions)
    real, intent(out) :: photolysis_rates__s(:)

    select case( config_id )
      case( CONFIG_QX5_TUV_MARGE )
        call calculate_rates_qx5( config_id, photolysis_rates__s )
      case( CONFIG_QX5_TUV_ARES )
        call calculate_rates_qx5( config_id, photolysis_rates__s )
      case( CONFIG_QXZ_TUV_MARGE )
        call calculate_rates_qxz( config_id, photolysis_rates__s )
      case( CONFIG_QXZ_TUV_ARES )
        call calculate_rates_qxz( config_id, photolysis_rates__s )
    end select

  end subroutine calculate_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate photolysis rates for the QX5 mechanism
  subroutine calculate_rates_qx5( config_id, photolysis_rates__s )

    use host_model_data
    use chemical_mechanism_qx5_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Photolysis rates [1/s]
    !! (ordered according to the chemical mechanism reactions)
    real, intent(out) :: photolysis_rates__s(:)

    ! call the actual TUV to update tuv_photolysis_rates__s
    ! call TUV( tuv_photolysis_rates__s )

    write(*,*) "Calculating TUV photolysis for QX5"

    select case( config_id )
      case( CONFIG_QX5_TUV_MARGE )
        ! With MARGE, most of BIGALK is isoprene and monoterpenes
        ! but the monoterpenes photolyze slower following only reaction path
        ! 1 for BIGALK in QX5
        ! (this is all made up but representative of the type of logic used
        !  to build maps)
        photolysis_rates__s( GAS_RXN_BIGALK_1 ) =                             &
          0.9 * tuv_photolysis_rates__s( RATE_ISOPRENE )
        photolysis_rates__s( GAS_RXN_BIGALK_2 ) = 0.0
      case( CONFIG_QX5_TUV_ARES )
        ! With ARES, most of BIGALK comes from cyclohexene reactions
        ! for which photolysis roughly follows butene photolysis for path
        ! 1 of BIGALK in QX5 and ethene photolysis for path 2
        ! (this is all made up but representative of the type of logic used
        !  to build maps)
        photolysis_rates__s( GAS_RXN_BIGALK_1 ) =                             &
          0.72 * tuv_photolysis_rates__s( RATE_BUTENE )
        photolysis_rates__s( GAS_RXN_BIGALK_2 ) =                             &
          1.2 * tuv_photolysis_rates__s( RATE_ETHENE )
    end select

    ! ethene is ethene
    photolysis_rates__s( GAS_RXN_ETHENE ) =                                   &
      tuv_photolysis_rates__s( RATE_ETHENE )
    ! propene scales with butene
    photolysis_rates__s( GAS_RXN_PROPENE ) =                                  &
      1.16 * tuv_photolysis_rates__s( RATE_BUTENE )

  end subroutine calculate_rates_qx5

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate photolysis rates for the QXZ mechanism
  subroutine calculate_rates_qxz( config_id, photolysis_rates__s )

    use host_model_data
    use chemical_mechanism_qxz_data

    !> Configuration ID
    integer, intent(in) :: config_id
    !> Photolysis rates [1/s]
    !! (ordered according to the chemical mechanism reactions)
    real, intent(out) :: photolysis_rates__s(:)

    ! call the actual TUV to update tuv_photolysis_rates__s
    ! call TUV( tuv_photolysis_rates__s )

    write(*,*) "Calculating TUV photolysis for QXZ"

    select case( config_id )
      case( CONFIG_QX5_TUV_MARGE )
        ! With MARGE, isoprene and monoterpenes
        ! are the only large alkenes, but the monoterpenes photolyze slower.
        ! In QXZ, monoterpenes are in BIGALK, but isoprene is in TALK, unlike
        ! the case of QX5 (where they're both in BIGALK).
        ! This means the difference in photolysis rates between monoterpenes
        ! and isoprene can be more explicitly captured for QXZ than QX5
        ! (this is all made up but representative of the type of logic used
        !  to build maps)
        photolysis_rates__s( GAS_RXN_TALK ) =                                 &
          tuv_photolysis_rates__s( RATE_ISOPRENE )
        photolysis_rates__s( GAS_RXN_BIGALK ) =                               &
          0.7 * tuv_photolysis_rates__s( RATE_ISOPRENE )
        ! there are no IALK species in MARGE (maybe we assume a rate based on
        ! butene photolysis)
        photolysis_rates__s( GAS_RXN_IALK ) =                                 &
          0.8 * tuv_photolysis_rates__s( RATE_BUTENE )
      case( CONFIG_QX5_TUV_ARES )
        ! With ARES cyclohexene is the only large alkene emission, but
        ! unlike QX5, cyclohexene ends up in IALK instead of BIGALK.
        ! Cyclohexe photolysis roughly follows butene photolysis for path
        ! 1 of BIGALK in QX5 and ethene photolysis for path 2, but QXZ groups
        ! these two reactions together, so we do a linear combination of the
        ! rates based on what a scientist determines are their average
        ! relative rates under some conditions of their choosing.
        ! (this is all made up but representative of the type of logic used
        !  to build maps)
        photolysis_rates__s( GAS_RXN_IALK ) =                                 &
          0.32 * tuv_photolysis_rates__s( RATE_BUTENE ) +                     &
          0.6 * tuv_photolysis_rates__s( RATE_ETHENE )
        ! Butene is the only 1<C<6 terminal alkene in ARES, so we get lucky
        photolysis_rates__s( GAS_RXN_TALK ) =                                 &
          tuv_photolysis_rates__s( RATE_BUTENE )
        ! There are no BIGALK species in ARES, so we scale them based on
        ! butene
        photolysis_rates__s( GAS_RXN_BIGALK ) =                               &
          0.6 * tuv_photolysis_rates__s( RATE_BUTENE )
    end select

    ! ethene is ethene
    photolysis_rates__s( GAS_RXN_ETHENE ) =                                   &
      tuv_photolysis_rates__s( RATE_ETHENE )

  end subroutine calculate_rates_qxz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module photolysis_tuv
