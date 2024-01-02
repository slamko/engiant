module types
  use raylib
  implicit none
  
  integer, parameter :: SCREEN_HEIGHT = 680
  integer, parameter :: SCREEN_WIDTH = 1200
  integer, parameter :: MIDDLE_X = SCREEN_WIDTH / 2.0
  integer, parameter :: MIDDLE_Y = SCREEN_HEIGHT / 2.0
  type(vector2_type), parameter :: MIDDLE = vector2_type(MIDDLE_X, MIDDLE_Y)
  real, parameter :: PART_RADIUS = 3.0

  integer, parameter :: RADIUS = min(SCREEN_HEIGHT, SCREEN_WIDTH) / 2

  integer, parameter :: FPS = 240

  type, abstract :: render_body
     logical :: init
  end type render_body

  type, extends (render_body) :: verlet_body
     type (vector2_type) :: pos
     type (vector2_type) :: prev_pos
     type (vector2_type) :: force
     real :: mass
  end type verlet_body

  type, extends (verlet_body) :: point_particle
     type (vector2_type) :: verlet_velocity
     real :: radius
  end type point_particle

  type, extends (render_body) :: stick
     type (point_particle), pointer :: p1
     type (point_particle), pointer :: p2
     real :: length
     logical :: edge_stick
  end type stick

  type :: geom_point
     type (vector2_type) :: pos
  end type geom_point

  type, public, extends (render_body) :: object
     type (point_particle), dimension(:), allocatable :: particles
     type (stick), dimension(:), allocatable :: sticks

     type (geom_point), dimension(:), allocatable :: match_shape
  end type object

  type :: engine
     type (object), dimension(:), allocatable :: obj
     integer :: cur_obj_num
     integer :: cur_obj
  end type engine
 
end module types
