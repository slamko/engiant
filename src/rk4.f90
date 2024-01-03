module rk4
  use integrator
  use math
  implicit none
  
  type, extends (base_integrator) :: rk4_integrator
   contains
     procedure :: set_velocity => rk4_set_velocity
     procedure :: get_velocity => rk4_get_velocity
     procedure :: integrate => rk4_integrate
  end type rk4_integrator

contains
  subroutine rk4_set_velocity (this, obj, param)
    class (rk4_integrator) :: this
    type (point_particle), pointer :: obj
    type (vector2_type) :: param

    obj%velocity = param
  end subroutine rk4_set_velocity

  function rk4_get_velocity (this, obj) result (vel)
    class (rk4_integrator) :: this
    type (point_particle), pointer :: obj
    type (vector2_type) :: vel

    vel = obj%velocity
  end function rk4_get_velocity

  function derive(dval, val, time_step) result(res)
    type (vector2_type) :: dval, val, res
    real :: time_step 

    res = val
  end function derive

  function rk4_vector(dval, val, time_step) result(res)
     type (vector2_type) :: dval, val, res
     real :: time_step 
     type (vector2_type) :: k1, k2, k3, k4, m

     k1 = derive(dval, val, 0.0)
     k2 = derive(vadd(dval,  vscale (k1, (time_step / 2.0))),val, time_step / 2.0) 
     k3 = derive(vadd(dval,  vscale (k2, (time_step / 2.0))),val, time_step / 2.0) 
     k4 = derive(vadd(dval,  vscale (k3, time_step)),val, time_step)
     
     m = vscale(vadd(k1, (vadd(k4, vadd(vscale(k2, 2.), vscale(k3, 2.))))), 1./6.)
     
     res = vadd(dval, vscale(m, time_step))
     
  end function rk4_vector

  subroutine rk4_integrate (this, obj)
    class (rk4_integrator) :: this
    type (object), pointer :: obj
    type (point_particle), pointer :: cur
    type (vector2_type) :: cur_pos
    integer :: ii
    
    do ii = 1, size (obj%particles)
       block
         type (vector2_type) :: acc
         cur => obj%particles(ii)
         
         acc = vscale(cur%force, 1.0 / cur%mass)
         cur%velocity = rk4_vector(cur%velocity, acc, delta)
         cur%pos = rk4_vector(cur%pos, cur%velocity, delta)
         cur%force = vzero()

       end block
    end do

  end subroutine rk4_integrate

 
end module rk4
