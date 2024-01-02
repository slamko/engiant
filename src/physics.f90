module physics
  use math
  use types

  implicit none

  real, parameter :: MASS = 1.0
  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 10.0)
  real, parameter :: COEFF_ELASTIC = 0.85

  real :: delta, prev_delta

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

     cosb = cosb / real(size(obj_shape))
     sinb = sinb / real(size(obj_shape))

     do i = 1, size(obj_shape)
        block
          real :: force_fact, pos_fact, k, p
          type (vector2_type) :: force_vec, pos_vec
          type (point_particle), pointer :: cur
          type (vector2_type) :: diff
          real :: dist
          
          k = 40.0
          p = 64.0

          obj_shape(i)%pos = vadd(center, vrotate(obj_shape(i)%pos, cosb, sinb))

          ! call draw_circle (int(obj_shape(i)%pos%x), int((obj_shape(i)%pos%y)), 5.0, BLUE)

          cur => obj%particles(i)
          diff = vsub(obj_shape(i)%pos, cur%pos)
          pos_vec = vscale (diff, 1.0 / p)

          cur%force = vadd(cur%force, vscale(diff, k))
          cur%pos = vadd(cur%pos, pos_vec)
        end block
     end do
     
   end subroutine apply_shape_match
  function get_max_point (obj) result(max_point)
    type (object), pointer :: obj
    type (vector2_type) :: max_point
    real :: max_x, max_y
    integer :: i

    max_x = 0.0
    max_y = 0.0

    do i = 1, size(obj%particles)
       block
         type (vector2_type) :: pos

         pos = obj%particles(i)%pos

         if (pos%x > max_x) then
            max_x = pos%x
         end if

         if (pos%y > max_y) then
            max_y = pos%y
         end if

       end block
    end do

    max_point = vector2_type(max_x, max_y)

  end function get_max_point

  function get_min_point (obj) result(min_point)
    type (object), pointer :: obj
    type (vector2_type) :: min_point
    real :: min_x, min_y
    integer :: i

    min_x = 10000.0
    min_y = 10000.0

    do i = 1, size(obj%particles)
       block
         type (vector2_type) :: pos

         pos = obj%particles(i)%pos

         if (pos%x < min_x) then
            min_x = pos%x
         end if

         if (pos%y < min_y) then
            min_y = pos%y
         end if

       end block
    end do

    min_point = vector2_type(min_x, min_y)

  end function get_min_point

  function get_outside_point (obj) result(outside_point)
    type (object), pointer :: obj
    type (vector2_type) :: outside_point
    
    outside_point = vadd(get_max_point(obj), vector2_type(133.0, 56.0))

  end function get_outside_point

  subroutine impulse_response (point, st)
!!$    
    type (vector2_type) :: avg_velocity, intermid, intermid_vec, targ, diff_prev, edge_target
    type (vector2_type) :: intersect, pseudo_vel, rnorm, edge_vel, inv_edge_vel, inv_norm, middle_pos
    type (vector2_type) :: offset1, offset2, middle_vec, inv_middle, inv_edge_targ
    type (vector2_type) :: vel_orth, vel_par, st_vel_orth, st_vel_par, dir, new_vel, new_st_vel
    real :: common_vel, vel_after, st_vel_after, vlen, st_vlen
    real :: d1, d2, len, middle_mag, avg_mass

    type (stick), pointer :: st
    type (point_particle), pointer :: point
    
    dir = vnormalize(vsub(st%p2%pos, st%p1%pos))
    avg_mass = (st%p2%mass + st%p1%mass) / 2.0

    intersect = point_segment_intersect(point%pos, st%p1%pos, st%p2%pos)
    d1 = vmag(vsub(st%p1%pos, intersect))
    d2 = vmag(vsub(st%p2%pos, intersect))
    len = vmag(vsub(st%p2%pos, st%p1%pos))
    
    rnorm = vnormalize(vsub(intersect, point%pos))
    inv_norm = rnorm
    rnorm = vinv(rnorm)

    edge_vel = vadd(vsub(st%p1%pos, st%p1%prev_pos), vsub(st%p2%pos, st%p2%prev_pos))
    inv_edge_vel = vinv(edge_vel)

    pseudo_vel = vsub(point%pos, point%prev_pos)
    vel_orth = vscale(dir, (vdot(dir, pseudo_vel)))
    vel_par = vscale(rnorm, (vdot(rnorm, pseudo_vel)))
   
    st_vel_orth = vscale(dir, (vdot(dir, edge_vel)))
    st_vel_par = vscale(rnorm, (vdot(rnorm, edge_vel)))

    vlen = vmag(vel_par)
    st_vlen = vmag(st_vel_par)

    vel_after = (point%mass * vlen + avg_mass * st_vlen + avg_mass * COEFF_ELASTIC * (st_vlen - vlen)) / (point%mass + avg_mass)
    st_vel_after = (point%mass * vlen + avg_mass * st_vlen + point%mass * COEFF_ELASTIC * (vlen - st_vlen)) / (point%mass + avg_mass)

    new_vel = vadd(vel_orth, vscale(vel_par, vel_after / vlen))
    new_st_vel = vadd(st_vel_orth, vscale(st_vel_par, st_vel_after / st_vlen))

    middle_vec = vscale(vsub(intersect, point%pos), 0.5)
    inv_middle = vinv(middle_vec)
    middle_mag = vmag(middle_vec)
    middle_pos = vadd(middle_vec, point%pos)
    
    point%pos = middle_pos
    intermid = vadd(point%pos, pseudo_vel)
    intermid_vec = vsub(point%pos, intermid)

    offset1 = vscale(inv_middle, d2 / len)
    offset2 = vscale(inv_middle, d1 / len)
    
    if (.not. st%p1%response_applied) then
       st%p1%pos = vadd(st%p1%pos, offset1)
       st%p1%prev_pos = vadd(st%p1%pos, vscale(vinv(new_st_vel), d2 / len))
    end if
    
    if (.not. st%p2%response_applied) then
       st%p2%pos = vadd(st%p2%pos, offset2)
       st%p2%prev_pos = vadd(st%p2%pos, vscale(vinv(new_st_vel), d1 / len))
    end if
    
    point%prev_pos = vadd(point%pos, vinv(new_vel))
    point%response_applied = .TRUE.
  end subroutine impulse_response
    
!!$
!!$

  subroutine response (point, st)
    type (vector2_type) :: avg_velocity, intermid, intermid_vec, targ, diff_prev, edge_target
    type (vector2_type) :: intersect, pseudo_vel, rnorm, edge_vel, inv_edge_vel, inv_norm, middle_pos
    type (vector2_type) :: offset1, offset2, middle_vec, inv_middle, inv_edge_targ
    type (stick), pointer :: st
    type (point_particle), pointer :: point
    real :: d1, d2, len, middle_mag
    
    intersect = point_segment_intersect(point%pos, st%p1%pos, st%p2%pos)
    d1 = vmag(vsub(st%p1%pos, intersect))
    d2 = vmag(vsub(st%p2%pos, intersect))
    len = vmag(vsub(st%p2%pos, st%p1%pos))
    
    pseudo_vel = vsub(point%pos, point%prev_pos)
    
    rnorm = vnormalize(vsub(intersect, point%pos))
    inv_norm = rnorm
    rnorm = vinv(rnorm)
    
    middle_vec = vscale(vsub(intersect, point%pos), 0.5)
    inv_middle = vinv(middle_vec)
    middle_mag = vmag(middle_vec)
    middle_pos = vadd(middle_vec, point%pos)
    
    point%pos = middle_pos
    intermid = vadd(point%pos, pseudo_vel)
    intermid_vec = vsub(point%pos, intermid)
    
    edge_vel = vadd(vsub(st%p1%pos, st%p1%prev_pos), vsub(st%p2%pos, st%p2%prev_pos))
    inv_edge_vel = vinv(edge_vel)
    
    targ = vscale(vsub(intermid_vec, vscale(rnorm, 2 * vdot(intermid_vec, rnorm))), COEFF_ELASTIC)
    
    edge_target = vscale(vsub(inv_edge_vel, vscale(inv_norm, 2 * vdot(inv_edge_vel, inv_norm))), COEFF_ELASTIC)
    inv_edge_targ = vnormalize(vinv(edge_target))
    
    offset1 = vscale(inv_middle, d2 / len)
    offset2 = vscale(inv_middle, d1 / len)
    
    if (.not. st%p1%response_applied) then
       
       st%p1%pos = vadd(st%p1%pos, offset1)
       ! st%p1%prev_pos = vadd(st%p1%pos, vscale(edge_target, d1 / len))
    end if
    
    if (.not. st%p2%response_applied) then
       
       st%p2%pos = vadd(st%p2%pos, offset2)
       ! st%p2%prev_pos = vadd(st%p2%pos, vscale(edge_target, d2 / len))
    end if
    
    point%prev_pos = vadd(point%pos, targ)
    point%response_applied = .TRUE.


  end subroutine response

  subroutine collision (me, cur_obj)
    type (object), pointer :: me
    type (object), pointer :: cur_obj
       type (point_particle), pointer :: cur
       type (vector2_type) :: outside_point, double_outside, inv_outside, min_point, max_point
       integer :: ii

       if (associated(cur_obj, me)) return

       outside_point = get_outside_point(cur_obj)
       double_outside = vadd(outside_point, vector2_type(100.0, 0.0))
       inv_outside = vscale(outside_point, 1.5)

       min_point = get_min_point (cur_obj)
       max_point = get_max_point (cur_obj)

       do ii = 1, size (me%particles)
          block
            integer :: iii = 0, intersect_cnt = 0, nearest = 0
            real :: nearest_dist = 3e+8, max_vdot
            cur => me%particles(ii)

            if (.not. (cur%pos%x >= min_point%x .and. cur%pos%y >= min_point%y .and. cur%pos%x <= max_point%x .and. cur%pos%y <= max_point%y)) cycle

            nearest_dist = 3e+8
            max_vdot = 0.0
            iii = 0
            nearest = 0
            intersect_cnt = 0

            do iii = 1, size (cur_obj%sticks)
               block
                 real :: dist
                 type (vector2_type) :: norm, vel

               if (.not. cur_obj%sticks(iii)%edge_stick) cycle

               if (segment_intersect(me%particles(ii)%pos, outside_point, cur_obj%sticks(iii)%p1%pos, cur_obj%sticks(iii)%p2%pos)) then
                     intersect_cnt = intersect_cnt + 1
               end if

               norm = segment_norm(cur_obj%sticks(iii)%p1%pos, cur_obj%sticks(iii)%p2%pos)
               vel = vnormalize(vsub(cur%pos, cur%prev_pos))

               dist = point_segment_distance(me%particles(ii)%pos, cur_obj%sticks(iii)%p1%pos, cur_obj%sticks(iii)%p2%pos)

               if (dist < nearest_dist)  then
                  nearest_dist = dist
                  nearest = iii
               end if

             end block
            end do

            if (nearest .eq. 0) then
               print *, "No nearest"
               return
            end if

            if (modulo(intersect_cnt, 2) .eq. 1) then
              block 
                type (stick), pointer :: st
                type (point_particle), pointer :: point
                
                point => me%particles(ii)
                st => cur_obj%sticks(nearest)
                
                call response(point, st)

              end block
            end if

          end block

       end do
 
  end subroutine collision

  subroutine constraint (cur_obj)
    type (object), pointer :: cur_obj
    type (point_particle), pointer :: cur
    integer :: ii
    
    do ii = 1, size (cur_obj%particles)
       cur => cur_obj%particles(ii)
       
       cur%response_applied = .FALSE.
       if (cur%pos%y > SCREEN_HEIGHT - cur%radius) then
          cur%pos%y = SCREEN_HEIGHT - cur%radius
          cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
          cur%prev_pos%x = cur%pos%x - (cur%verlet_velocity%x * COEFF_ELASTIC) * delta
       end if
       
    end do
  end subroutine
    
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
