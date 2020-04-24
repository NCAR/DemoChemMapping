!> Host Model
!!
!! Mock version of a host model that uses hard-coded mapping between
!! components (not that this is how models really are - just to
!! demonstrate how mapping looks for mixtures of explicit and lumped
!! chemical species)
!!
!! !!! Extremely Dangerous !!! Do not try at home
program host_model

  use chemical_mechanism_qx5_data,     only : QX5_NUM_SPEC => NUM_SPEC
  use chemical_mechanism_qx5,          only : qx5_solve => solve
  use chemical_mechanism_qxz_data,     only : QXZ_NUM_SPEC => NUM_SPEC
  use chemical_mechanism_qxz,          only : qxz_solve => solve
  use host_model_data

  implicit none

  real, allocatable :: gas_species_conc__num_m3(:)
  character(len=32) :: arg
  integer :: config_id

  call get_command_argument( 1, arg )
  if( len_trim( arg ) .eq. 0 ) then
    write(*,*) "Useage: host_model CONFIG_ID"
    write(*,*) ""
    write(*,*) "CONFIG_ID = 1: QX5 with TUV with MARGE"
    write(*,*) "CONFIG_ID = 2: QX5 with FastJ with MARGE"
    write(*,*) "CONFIG_ID = 3: QX5 with TUV with ARES"
    write(*,*) "CONFIG_ID = 4: QX5 with FastJ with ARES"
    write(*,*) "CONFIG_ID = 5: QXZ with TUV with MARGE"
    write(*,*) "CONFIG_ID = 6: QXZ with FastJ with MARGE"
    write(*,*) "CONFIG_ID = 7: QXZ with TUV with ARES"
    write(*,*) "CONFIG_ID = 8: QXZ with FastJ with ARES"
    stop 3
  end if

  read( arg, '(I2)' ) config_id

  ! Initialization

  ! chemical species concetrations
  select case( config_id )
    case( CONFIG_QX5_TUV_MARGE )
      allocate( gas_species_conc__num_m3( QX5_NUM_SPEC ) )
    case( CONFIG_QX5_FASTJ_MARGE )
      allocate( gas_species_conc__num_m3( QX5_NUM_SPEC ) )
    case( CONFIG_QX5_TUV_ARES )
      allocate( gas_species_conc__num_m3( QX5_NUM_SPEC ) )
    case( CONFIG_QX5_FASTJ_ARES )
      allocate( gas_species_conc__num_m3( QX5_NUM_SPEC ) )
    case( CONFIG_QXZ_TUV_MARGE )
      allocate( gas_species_conc__num_m3( QXZ_NUM_SPEC ) )
    case( CONFIG_QXZ_FASTJ_MARGE )
      allocate( gas_species_conc__num_m3( QXZ_NUM_SPEC ) )
    case( CONFIG_QXZ_TUV_ARES )
      allocate( gas_species_conc__num_m3( QXZ_NUM_SPEC ) )
    case( CONFIG_QXZ_FASTJ_ARES )
      allocate( gas_species_conc__num_m3( QXZ_NUM_SPEC ) )
  end select

  write(*,*) "Running model with config: ", config_id, " and ",              &
             size( gas_species_conc__num_m3 ), " species"

  ! Run time!

  ! time/grid loops

    ! for one cell
    select case( config_id )
      case( CONFIG_QX5_TUV_MARGE )
        call qx5_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QX5_FASTJ_MARGE )
        call qx5_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QX5_TUV_ARES )
        call qx5_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QX5_FASTJ_ARES )
        call qx5_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QXZ_TUV_MARGE )
        call qxz_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QXZ_FASTJ_MARGE )
        call qxz_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QXZ_TUV_ARES )
        call qxz_solve( config_id, gas_species_conc__num_m3 )
      case( CONFIG_QXZ_FASTJ_ARES )
        call qxz_solve( config_id, gas_species_conc__num_m3 )
    end select

  ! end time/grid loops

  write(*,*) "Finished model!"

end program host_model
