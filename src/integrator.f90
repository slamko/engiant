module integrator

  use types
  implicit none
  type, abstract :: base_integrator
   contains
     procedure (base_integrate), deferred :: integrate
     procedure (setter), deferred :: set_velocity
     procedure (getter), deferred :: get_velocity
  end type base_integrator

  abstract interface
     subroutine setter (this, obj, param)
       use types
       import base_integrator

       class (base_integrator) :: this
       type (point_particle), pointer :: obj
       type (vector2_type) :: param

     end subroutine setter
     
     type (vector2_type) function getter (this, obj)
       use types
       import base_integrator

       class (base_integrator) :: this
       type (point_particle), pointer :: obj

     end function getter
     
     subroutine base_integrate (this, obj)
       use types
       import base_integrator

       class (base_integrator) :: this
       type (object), pointer :: obj
     end subroutine base_integrate
  end interface

  real :: delta, prev_delta

end module integrator
