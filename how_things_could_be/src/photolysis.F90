!> Abstract photolysis module
!!
!! Defines functionality of photolysis modules
!!
module photolysis

  implicit none
  private

  public :: photolysis_t

  !> Abstract photolysis module
  type, abstract :: photolysis_t
  contains
    !> Reaction name-id pairs
    procedure(reaction_name_id_pairs), deferred :: reaction_name_id_pairs
    !> Calculate the photolysis reaction rates
    procedure(get_rates), deferred :: get_rates
  end type photolysis_t

interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reaction name-id-pairs
  function reaction_name_id_pairs( this ) result( pairs )
    use map,                           only : name_id_pair_t
    import photolysis_t

    !> Reaction name-id pairs
    type(name_id_pair_t), allocatable :: pairs(:)
    !> Photolysis module
    class(photolysis_t), intent(in) :: this

  end function reaction_name_id_pairs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photolysis reaction rates
  subroutine get_rates( this, photolysis_rates__s, map )
    use map,                           only : component_map_t
    import photolysis_t

    !> Photolysis module
    class(photolysis_t), intent(in) :: this
    !> Photolysis rates [1/s]
    real, intent(inout) :: photolysis_rates__s(:)
    !> Map to local namespace
    type(component_map_t), intent(in) :: map

  end subroutine get_rates

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end interface

end module photolysis
