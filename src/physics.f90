module physics
  use math
  use types

  implicit none

  real, parameter :: MASS = 1.0
  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 10.0)

  interface

     subroutine obj_iter(obj)
       use types
       type (object), pointer :: obj
     end subroutine obj_iter

     subroutine obj_iter2(obj, cur_obj)
       use types
       type (object), pointer :: obj, cur_obj
     end subroutine obj_iter2

  end interface

contains

  subroutine apply_gravity (cur_obj)
     type (object), pointer :: cur_obj
     type (point_particle), pointer :: cur
     integer :: i
     
     do i = 1, size (cur_obj%particles)
        cur => cur_obj%particles(i)
        cur%force = vadd(cur%force, vscale(G_ACC, cur%mass))
     end do
     
  end subroutine apply_gravity

  subroutine apply_springs (cur_obj)
     type (object), pointer :: cur_obj
     integer ::  i
       
     do i = 1, size (cur_obj%sticks)
        block
          type (stick), pointer :: cur
          type (vector2_type) :: diff
          real :: dist
          
          cur => cur_obj%sticks(i)
          
          diff = vsub(cur%p1%pos, cur%p2%pos)
          dist = vmag(diff)
          
          if (dist .ne. cur%length) then
             block
               type (vector2_type) :: normal_diff
               type (vector2_type) :: force_vec, pos_vec
               type (vector2_type) :: old_pos_p1, old_pos_p2
               real :: force_fact, pos_fact, k, p
               
               normal_diff = vnormalize (diff) !
               
               k = 4.0
               p = 100.
               
               force_fact = k * (dist - cur%length) / (2.0)
               pos_fact = (dist - cur%length) / (2.0 * p)
               
               force_vec = vscale (normal_diff, force_fact)
               pos_vec = vscale (normal_diff, pos_fact)
               
               cur%p1%force = vsub(cur%p1%force, force_vec)
               cur%p2%force = vadd(cur%p2%force, force_vec)
               
               cur%p1%pos = vsub(cur%p1%pos, pos_vec)
               cur%p2%pos = vadd(cur%p2%pos, pos_vec)
               
             end block
          end if
          
        end block
     end do
   end subroutine apply_springs
   
   subroutine apply_shape_match (obj)
     type (object), pointer :: obj
     type (vector2_type) :: center
     type (geom_point), dimension(size(obj%particles)) :: obj_shape
     integer :: i
     real :: sinb, cosb

     sinb = 0.0
     cosb = 0.0
     center = vzero()
     obj_shape = obj%match_shape
     
     do i = 1, size(obj%particles)
        center = vadd(center, obj%particles(i)%pos)
     end do
     center = vscale(center, 1.0 / real(size(obj_shape)))

     call draw_circle (int(center%x), int(center%y), 10.0, BLUE)

     do i = 1, size(obj%particles)
        block
          type (vector2_type) :: point_vec, shape_point_vec

          point_vec = vsub(obj%particles(i)%pos, center)
          sinb = sinb + (vcross(obj_shape(i)%pos, point_vec) / (vmag(obj_shape(i)%pos) * vmag(point_vec)))
          cosb = cosb + (vdot(obj_shape(i)%pos, point_vec) / (vmag(obj_shape(i)%pos) * vmag(point_vec)))
        end block
     end do

     cosb = cosb / real(size(obj%particles))
     sinb = sinb / real(size(obj%particles))

     do i = 1, size(obj_shape)
        block
          real :: force_fact, pos_fact, k, p
          type (point_particle), pointer :: cur
          type (vector2_type) :: diff
          real :: dist
          
          k = 50.0
          obj_shape(i)%pos = vadd(center, vrotate(obj_shape(i)%pos, cosb, sinb))

          cur => obj%particles(i)
          diff = vsub(obj_shape(i)%pos, cur%pos)
          dist = vmag(diff)

          cur%force = vadd(cur%force, vscale(diff, k))
           
        end block
     end do
     
   end subroutine apply_shape_match
    
   subroutine iter2 (f, objects, num)
     type (object), target, dimension(*) :: objects
     type (object), pointer :: obj
     procedure (obj_iter2) :: f
     integer :: num, i
     
     do i = 1, num
        block
          integer :: ii
          obj => objects(i)
          
          if (.not. obj%init) cycle
           
          do ii = 1, num
             block
               type (object), pointer :: cur_obj
               cur_obj => objects(ii)
     
               if (.not. cur_obj%init) cycle

               call f (obj, cur_obj)

             end block
          end do
        end block

     end do
  end subroutine iter2

   subroutine iter (f, objects, num)
     type (object), target, dimension(*) :: objects
     procedure (obj_iter) :: f
     integer :: num, i
     
     do i = 1, num
        block
          type (object), pointer :: cur_obj
          integer :: ii
          cur_obj => objects(i)
          
          if (.not. cur_obj%init) cycle
          
          call f (cur_obj)
        end block

     end do
  end subroutine iter

end module physics
