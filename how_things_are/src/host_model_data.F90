!> Host model parameters
!!
module host_model_data

  implicit none
  private

  integer, parameter, public :: CONFIG_QX5_TUV_MARGE   = 1
  integer, parameter, public :: CONFIG_QX5_FASTJ_MARGE = 2
  integer, parameter, public :: CONFIG_QX5_TUV_ARES    = 3
  integer, parameter, public :: CONFIG_QX5_FASTJ_ARES  = 4
  integer, parameter, public :: CONFIG_QXZ_TUV_MARGE   = 5
  integer, parameter, public :: CONFIG_QXZ_FASTJ_MARGE = 6
  integer, parameter, public :: CONFIG_QXZ_TUV_ARES    = 7
  integer, parameter, public :: CONFIG_QXZ_FASTJ_ARES  = 8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module host_model_data
