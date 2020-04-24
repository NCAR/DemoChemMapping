!> Mapping between local scheme names
!!
!! Exposes functionality for mapping between local namespaces with model
!! components. The \c map_t type can be extended for more complex mapping, if
!! for example, the scaling of values relies on environmental conditions.
!!
module map

  implicit none
  private

  public :: component_map_t, name_id_pair_t, create_map

  !> Maximum length of map names
  integer, parameter, public :: MAX_NAME_LENGTH = 15

  !> Map between local model component namespaces
  type :: component_map_t
  private
    !> Primary id
    integer, allocatable :: primary_id(:)
    !> Secondary id
    integer, allocatable :: secondary_id(:)
    !> Scaling factor
    real, allocatable :: scaling_factor(:)
  contains
    !> Transfer values from the namespace of the secondary component to the
    !! namespace of the primary component. Values are added to the primary
    !! array
    procedure :: to_primary
    !> Print the map
    procedure :: print => do_print
  end type component_map_t

  !> Name-Index pair for building maps
  type :: name_id_pair_t
    character(len=MAX_NAME_LENGTH) :: name
    integer :: id
  end type name_id_pair_t

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Create a map between two model component namespaces
  !!
  !! Configuration files comprise only lines of the form:
  !!
  !! primary_name        secondary_name       scaling_factor
  !!
  function create_map( map_file_name, primary_ids, secondary_ids )            &
      result( new_map )

    !> Generated map
    type(component_map_t) :: new_map
    !> File name for map configuration
    character(len=*), intent(in) :: map_file_name
    !> Id lookup by name for primary component
    type(name_id_pair_t), intent(in) :: primary_ids(:)
    !> Id lookup by name for secondary component
    type(name_id_pair_t), intent(in) :: secondary_ids(:)

    integer :: file_unit, stat, map_size, map_id
    character(len=15) :: primary_name, secondary_name
    real :: scaling

    file_unit = 42
    map_size = 0

    open( file_unit, file=map_file_name, IOSTAT=stat)
    if( stat .ne. 0 ) then
      write(*,*) "Error opening file ", map_file_name
      stop 3
    end if
    do
      read( file_unit, *, IOSTAT=stat ) primary_name, secondary_name, scaling
      if( stat > 0 ) then
        write(*,*) "File IO error for ", map_file_name
        stop 3
      else if( stat < 0 ) then
        exit
      else
        map_size = map_size + 1
      end if
    end do

    allocate( new_map%primary_id(     map_size ) )
    allocate( new_map%secondary_id(   map_size ) )
    allocate( new_map%scaling_factor( map_size ) )

    map_id = 0
    rewind( file_unit )
    do
      read( file_unit, *, IOSTAT=stat ) primary_name, secondary_name, scaling
      if( stat > 0 ) then
        write(*,*) "File IO error for ", map_file_name
        stop 3
      else if( stat < 0 ) then
        exit
      else
        map_id = map_id + 1
        if( .not. lookup_id( primary_ids,                                     &
                             primary_name,                                    &
                             new_map%primary_id( map_id ) ) ) then
          write(*,*) "Cound not find '", trim( primary_name ), "' in file ",  &
                     trim( map_file_name )
          stop 3
        end if
        if( .not. lookup_id( secondary_ids,                                   &
                             secondary_name,                                  &
                             new_map%secondary_id( map_id ) ) ) then
          write(*,*) "Cound not find '", trim( secondary_name ), "' in file ",&
                     trim( map_file_name )
          stop 3
        end if
        new_map%scaling_factor( map_id ) = scaling
      end if
    end do
    close( file_unit )

  end function create_map

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Lookup an id by its associated name
  function lookup_id( name_id_pairs, name, id ) result( found )

    !> Flag indicating whether the id was found
    logical :: found
    !> Set of name-id pairs
    type(name_id_pair_t), intent(in) :: name_id_pairs(:)
    !> Name to lookup
    character(len=MAX_NAME_LENGTH), intent(in) :: name
    !> Returned index
    integer, intent(out) :: id

    integer :: i_pair

    id = -1
    found = .false.
    do i_pair = 1, size( name_id_pairs )
      if( name_id_pairs( i_pair )%name .eq. name ) then
        id = name_id_pairs( i_pair )%id
        found = .true.
        exit
      end if
    end do

  end function lookup_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Transfer values from the namespace of the secondary component to the
  !! namespace of the primary component. Values are added to the primary
  !! array
  subroutine to_primary( this, primary_array, secondary_array )

    !> Map
    class(component_map_t), intent(in) :: this
    !> Primary array to update
    real, intent(inout) :: primary_array(:)
    !> Secondary array
    real, intent(in) :: secondary_array(:)

    integer :: i_map

    do i_map = 1, size( this%primary_id )
      primary_array( this%primary_id( i_map ) ) =                             &
        secondary_array( this%secondary_id( i_map ) ) *                       &
        this%scaling_factor( i_map )
    end do

  end subroutine to_primary

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Print the map
  subroutine do_print( this )

    !> Map
    class(component_map_t), intent(in) :: this

    integer :: i_map

    write(*,*) "Primary Id  | Secondary Id | Scaling"
    do i_map = 1, size( this%primary_id )
      write(*,*) this%primary_id( i_map ), "|", this%secondary_id( i_map ),   &
                 " |", this%scaling_factor( i_map )
    end do

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module map
