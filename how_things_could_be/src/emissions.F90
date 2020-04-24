!> Abstract emissions module
!!
!! Defines functionality of emissions modules
!!
module emissions

  implicit none
  private

  public :: emissions_t

  !> Abstract emissions module
  type, abstract :: emissions_t
  contains
    !> Species name-id pairs
    procedure(species_name_id_pairs), deferred :: species_name_id_pairs
    !> Calculate the emissions rates
    procedure(get_rates), deferred :: get_rates
  end type emissions_t

interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Species name-id-pairs
  function species_name_id_pairs( this ) result( pairs )
    use map,                           only : name_id_pair_t
    import emissions_t

    !> Species name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Emissions module
    class(emissions_t), intent(in) :: this

  end function species_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the emissions rates
  subroutine get_rates( this, emission_rates__num_m3_s, map )
    use map,                           only : component_map_t
    import emissions_t

    !> Emissions module
    class(emissions_t), intent(in) :: this
    !> Emissions rates [#/m3/s]
    real, intent(inout) :: emission_rates__num_m3_s(:)
    !> Map to local namespace
    type(component_map_t), intent(in) :: map

  end subroutine get_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end interface

end module emissions
