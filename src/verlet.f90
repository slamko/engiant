module verlet
  use integrator
  use math
  implicit none
  
  type, extends (base_integrator) :: verlet_integrator
   contains
     procedure :: set_velocity => verlet_set_velocity
     procedure :: get_velocity => verlet_get_velocity
     procedure :: integrate => verlet_integrate
  end type verlet_integrator

contains
  subroutine verlet_set_velocity (this, obj, param)
    class (verlet_integrator) :: this
    type (point_particle), pointer :: obj
    type (vector2_type) :: param

    obj%prev_pos = vadd(obj%pos, vinv(vscale(param, delta)))
  end subroutine verlet_set_velocity

  function verlet_velocity (point, prev_pos, avarage_delta) result (vel)
    type (point_particle) :: point
    type (vector2_type) :: prev_pos
    real :: avarage_delta

    type (vector2_type) :: vel

    vel = vscale(vsub(point%pos, prev_pos), 1.0 / (2.0 * avarage_delta))
  end function verlet_velocity

  subroutine verlet_integrate (this, obj)
    class (verlet_integrator) :: this
    type (object), pointer :: obj
    type (point_particle), pointer :: cur
    type (vector2_type) :: cur_pos
    integer :: ii
    
    do ii = 1, size (obj%particles)
       block
         type (vector2_type) :: acc
         cur => obj%particles(ii)
         cur_pos = cur%pos
         
         acc = vscale(cur%force, 1. / cur%mass)
         cur%force = vector2_type(0,0)
         cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(acc, (delta ** 2)))
         
         cur%velocity = verlet_velocity(cur, cur%prev_pos, delta)
         cur%prev_pos = cur_pos
       end block
    end do

  end subroutine verlet_integrate

  function verlet_get_velocity (this, obj) result (vel)
    class (verlet_integrator) :: this
    type (point_particle), pointer :: obj
    type (vector2_type) :: vel

    vel = obj%velocity
  end function verlet_get_velocity
  
end module verlet
