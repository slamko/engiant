
program main
  use, intrinsic :: iso_c_binding, only: c_null_char
  use raylib
  use types
  use math
  use physics
  use renderer

  implicit none

  integer, parameter :: SCREEN_HEIGHT = 680
  integer, parameter :: SCREEN_WIDTH = 1200
  integer, parameter :: MIDDLE_X = SCREEN_WIDTH / 2.0
  integer, parameter :: MIDDLE_Y = SCREEN_HEIGHT / 2.0
  type(vector2_type), parameter :: MIDDLE = vector2_type(MIDDLE_X, MIDDLE_Y)
  integer, parameter :: RADIUS = min(SCREEN_HEIGHT, SCREEN_WIDTH) / 2
  integer, parameter :: FPS = 240
  real, parameter :: COEFF_ELASTIC = 0.85
  real, parameter :: PART_RADIUS = 3.0

  real :: delta, prev_delta
  integer :: num_alloc
  integer :: counter
   
  type (engine), target :: eng
  type (engine), pointer :: eng_ptr
  integer :: j

  call random_seed()
  counter = 0

  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran raylib' // c_null_char)
  call set_target_fps(FPS)

  allocate (eng%obj(40))

  do j = 1, 40
     eng%obj(j)%init = .FALSE.
  end do
  
  eng%cur_obj = 0
  eng_ptr => eng

  ! call instantiate_full_rectangle (eng_ptr, MIDDLE, 160.0, 160., 160.0)
  do while (.not. window_should_close())
     block
       integer :: i

       delta = get_frame_time()
       
       call begin_drawing()
       call clear_background(BLACK)
       
       do i = 1, 2
          call iter(apply_springs, eng%obj, size(eng%obj))
          call iter(apply_shape_match, eng%obj, size(eng%obj))
       end do
       
       call iter(apply_gravity, eng%obj, size(eng%obj))
       call iter(constraint, eng%obj, size(eng%obj))
       call iter2(collision, eng%obj, size(eng%obj))
       
       call iter(verlet, eng%obj, size(eng%obj))
       
       call iter(render, eng%obj, size(eng%obj))
       call iter(render_sticks, eng%obj, size(eng%obj))
       
       if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
          call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 20.0)
          ! call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 120.0, 160., 40.0)
          ! call instantiate_polygon (eng_ptr, get_mouse_position(), 30.0, 8)
       end if
       
       if (is_mouse_button_released(MOUSE_BUTTON_RIGHT)) then
          call instantiate_polygon (eng_ptr, get_mouse_position(), 20.0, 4)
       end if

       call end_drawing()
     end block
  end do

  call close_window()

contains
  function verlet_velocity (point, prev_pos, avarage_delta) result (vel)
    type (point_particle) :: point
    type (vector2_type) :: prev_pos
    real :: avarage_delta

    type (vector2_type) :: vel

    vel = vscale(vsub(point%pos, prev_pos), 1.0 / (2.0 * avarage_delta))
  end function verlet_velocity

  function verlet_velocity_o2 (point, avarage_delta) result (vel)
    type (point_particle) :: point
    type (vector2_type) :: prev_pos
    real :: avarage_delta

    type (vector2_type) :: vel

    vel = vscale(vsub(point%pos, point%prev_pos), 1.0 / (avarage_delta))
  end function verlet_velocity_o2

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

               if (cur%pos%x >= min_point%x .and. cur%pos%y >= min_point%y .and. cur%pos%x <= max_point%x .and. cur%pos%y <= max_point%y) then
              block 
                type (vector2_type) :: avg_velocity, intermid, intermid_vec, targ, diff_prev, edge_target
                type (vector2_type) :: intersect, pseudo_vel, rnorm, edge_vel, inv_edge_vel, inv_norm, middle_pos
                type (vector2_type) :: offset1, offset2, middle_vec, inv_middle, inv_edge_targ
                type (stick), pointer :: st
                type (point_particle), pointer :: point
                real :: d1, d2, len, middle_mag

                counter = counter + 1
                print *, "Detected!"
                print *, counter

                point => me%particles(ii)
                st => cur_obj%sticks(nearest)

                intersect = point_segment_intersect(me%particles(ii)%pos, st%p1%pos, st%p2%pos)
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

                if(vdot(vsub(st%p1%pos, st%p1%prev_pos), inv_norm) >= 0) then
                   st%p1%pos = vadd(st%p1%pos, offset1)
                   ! st%p1%prev_pos = vadd(st%p1%pos, vscale(edge_target, d1 / len))
                end if

                if(vdot(vsub(st%p2%pos, st%p2%prev_pos), inv_norm) >= 0) then
                   st%p2%pos = vadd(st%p2%pos, offset2)
                   ! st%p2%prev_pos = vadd(st%p2%pos, vscale(edge_target, d2 / len))
                end if

                point%prev_pos = vadd(point%pos, targ)

              end block
              end if
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
       
       if (cur%pos%y > SCREEN_HEIGHT - cur%radius) then
          cur%pos%y = SCREEN_HEIGHT - cur%radius
          cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
          cur%prev_pos%x = cur%pos%x - (cur%verlet_velocity%x * COEFF_ELASTIC) * delta
       end if
       
    end do
  end subroutine

  subroutine verlet (cur_obj)
    type (object), pointer :: cur_obj
    type (point_particle), pointer :: cur
    type (vector2_type) :: cur_pos
    integer :: ii
    
    do ii = 1, size (cur_obj%particles)
       block
         type (vector2_type) :: acc
         cur => cur_obj%particles(ii)
         cur_pos = cur%pos
         
         acc = vscale(cur%force, 1. / cur%mass)
         cur%force = vector2_type(0,0)
         cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(acc, (delta ** 2)))
         
         cur%verlet_velocity = verlet_velocity(cur, cur%prev_pos, delta)
         cur%prev_pos = cur_pos
       end block
    end do
  end subroutine

  subroutine instantiate_polygon (eng, pos, radius, sector_num)
    type (engine), pointer :: eng
    real :: radius
    integer :: sector_num
    type (vector2_type), dimension(4) :: init_pos
    type (vector2_type) :: pos
    type (object), pointer :: ob
    integer :: s, o = 0
    integer :: i
    real :: mass

    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    allocate(ob%particles(sector_num + 1))
    allocate(ob%match_shape(sector_num + 1))
    allocate(ob%sticks(sector_num * 2))

    mass = 1. / float(sector_num)
    ob%particles(1) = point_particle(.TRUE., pos, pos, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS)
    ob%match_shape(1) = geom_point(pos)

    do i = 2, sector_num + 1
       block
         type (vector2_type) :: point
         real :: x, y

         x = pos%x + radius * cos(2 * PI * real(i - 1) / real(sector_num))
         y = pos%y + radius * sin(2 * PI * real(i - 1) / real(sector_num))
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS)
         ob%match_shape(i) = geom_point(point)
       end block
    end do

    do i = 1, sector_num
       block
         real :: length, length_center
         integer :: next_id

         next_id = i + 2

         if (i + 2 > sector_num + 1) then 
            next_id = 2
         end if
         
         length = vmag(vsub(ob%particles(i + 1)%pos, ob%particles(next_id)%pos))
         length_center = vmag(vsub(ob%particles(i + 1)%pos, ob%particles(1)%pos))

         ob%sticks(2*(i - 1) + 1) = stick(.TRUE., ob%particles(i + 1), ob%particles(next_id), length, .TRUE.)
         ob%sticks(2*i) = stick(.TRUE., ob%particles(i + 1), ob%particles(1), length_center, .FALSE.)
     end block
    end do

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_polygon

  subroutine instantiate_full_rectangle (eng, pos, height, width, space)
    type (engine), pointer :: eng
    real :: height, width, space
    integer :: point_num, point_num_x, point_num_y, vertical_sticks, horiz_sticks, diag_sticks
    type (vector2_type) :: pos, start_pos
    type (object), pointer :: ob
    integer :: s = 0, o = 0
    integer :: i
    real :: mass

    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    point_num_x = int(width / space) + 1
    point_num_y = int(height / space) + 1
    point_num = point_num_x * point_num_y

    vertical_sticks = (point_num_y - 1) * point_num_x
    horiz_sticks = (point_num_x - 1) * point_num_y
    diag_sticks = (point_num_x - 1) * (point_num_y - 1) * 2

    allocate(ob%particles(point_num))
    allocate(ob%match_shape(point_num))
    allocate(ob%sticks(vertical_sticks + horiz_sticks))

    start_pos = vector2_type(pos%x - width / 2, pos%y - height / 2)
    mass = 1.0 / float(point_num)

    do i = 1, point_num
       block
         type (vector2_type) :: point
         real :: x, y

         x = start_pos%x + space * modulo(i - 1, point_num_x) 
         y = start_pos%y + space * ((i - 1) / point_num_x)
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS)
         ob%match_shape(i) = geom_point(vsub(point, pos))
       end block
    end do

    do i = 1, vertical_sticks
       block
         real :: length
         integer p1_id, p2_id
         logical :: board = .FALSE.

         p1_id = i
         p2_id = i + point_num_x
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         if ((modulo(i, point_num_x) .eq. 1) .or. (modulo(i, point_num_x) .eq. 0)) then
            board = .TRUE.
         end if

         ob%sticks(i) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, board)
     end block
    end do

    do i = 1, point_num
       block
         real :: length
         integer p1_id, p2_id
         logical :: board = .FALSE.

         p1_id = i
         p2_id = i + 1

         if (modulo(i, point_num_x) .eq. 0) cycle

         if ((i <= point_num_x) .or. (i >= (point_num - point_num_x))) then
            board = .TRUE.
         end if
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(vertical_sticks + i - (i / point_num_x)) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, board)
     end block
     end do

!!$    do i = 1, (point_num - point_num_x)
!!$       block
!!$         real :: length
!!$         integer p1_id, p2_id
!!$         p1_id = i
!!$         p2_id = i + point_num_x + 1
!!$
!!$         if (modulo(i, point_num_x) .eq. 0) cycle
!!$        
!!$         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))
!!$
!!$         ob%sticks(vertical_sticks + horiz_sticks + i - (i / point_num_x)) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .FALSE.)
!!$     end block
!!$
!!$     end do
!!$         do i = 1, (point_num - point_num_x)
!!$       block
!!$         real :: length
!!$         integer p1_id, p2_id
!!$         p1_id = i
!!$         p2_id = i + point_num_x - 1
!!$
!!$         if (modulo(i, point_num_x) .eq. 1) cycle
!!$        
!!$         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))
!!$
!!$         ob%sticks(vertical_sticks + horiz_sticks + (diag_sticks / 2) + i - (((i - 1) / point_num_x) + 1)) = &
!!$              stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .FALSE.)
!!$     end block
!!$     end do
!!$

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_full_rectangle


end program
