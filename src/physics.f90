module physics
  use math
  use types

  implicit none

  real, parameter :: MASS = 1.0
  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 10.0)

contains

  subroutine apply_gravity (objects, num)
     type (object), target, dimension(*) :: objects
     integer :: num, i
     
     do i = 1, num
        block
          type (object), pointer :: cur_obj
          type (point_particle), pointer :: cur
          integer :: ii
          cur_obj => objects(i)
          
          if (.not. cur_obj%init) cycle
          
          do ii = 1, size (cur_obj%particles)
             cur => cur_obj%particles(ii)
             cur%force = vadd(cur%force, vscale(G_ACC, cur%mass))
          end do
          
        end block
     end do
     
  end subroutine apply_gravity

  subroutine apply_springs (objects, num)
     type (object), target, dimension(*) :: objects
     integer :: num, i
     
     do i = 1, num
        block
          type (object), pointer :: cur_obj
          integer :: ii
          cur_obj => objects(i)
          
          if (.not. cur_obj%init) cycle
          
          do ii = 1, size (cur_obj%sticks)
             block
               type (stick), pointer :: cur
               type (vector2_type) :: diff
               real :: dist
               
               cur => cur_obj%sticks(ii)
               
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
          
        end block
     end do
     
  end subroutine apply_springs

end module physics
