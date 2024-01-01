
program main
  use, intrinsic :: iso_c_binding, only: c_null_char
  use raylib
  use types
  use math
  use physics

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
   
  type (engine), target :: eng
  type (engine), pointer :: eng_ptr
  integer :: j

  call random_seed()

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
     ! call draw_polygon (MIDDLE_X, MIDDLE_Y, float(RADIUS), WHITE)

     do i = 1, 5
     call apply_springs(eng%obj, size(eng%obj))
        ! call solve_sticks(eng%obj, size(eng%obj))
     end do

     call apply_gravity(eng%obj, size(eng%obj))
     call constraint(eng%obj, size(eng%obj))

     call verlet(eng%obj, size(eng%obj))
     ! call apply_pos(eng%obj, size(eng%obj))

      do i = 1, 3
        ! call solve_sticks(eng%obj, size(eng%obj))
     end do

     ! call draw_cube_v(vector3_type(100.0, 300.0, 150.0), vector3_type(200.0, 100.0, 150.0), RED)

     call render(eng%obj, size(eng%obj))
     call render_sticks(eng%obj, size(eng%obj))

     if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
        call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 80.0)
        ! call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 120.0, 160., 40.0)
        ! call instantiate_polygon (eng_ptr, get_mouse_position(), 20.0, 8)
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

  subroutine move_collision (point, line, col_line)
    type (point_particle), pointer :: point
    type (stick), intent(in) :: line, col_line
    type (vector2_type) :: vec_stick, norm, pseudo, targ, dir_vec, targ_vec, npoint
    real :: scale, mag, speed_fact
    real :: limit

    if (associated(point)) then
      block

         type (vector2_type) :: rnorm, intersect, pseudo_vel, intermid, intermid_vec, new_prev_pos, inv_norm, outside_point
         real :: d1, d2
         
       vec_stick = vnormalize(vsub(col_line%p2%pos, col_line%p1%pos))
       norm%x = vec_stick%y
       norm%y = -vec_stick%x
       npoint = vadd(point%pos, norm)
       intersect = intersect_point(col_line%p1%pos, col_line%p2%pos, point%pos, npoint)
       targ_vec = vsub(intersect, point%pos)

       rnorm = vnormalize(vsub(intersect, point%pos))
       rnorm%x = -rnorm%x
       rnorm%y = -rnorm%y

       inv_norm = vinv(rnorm)
       outside_point = vadd(intersect, inv_norm)

       ! if (.not. point_in_segment(intersect%x, intersect%y, point%pos, outside_point, col_line%p1%pos, col_line%p2%pos)) return
       
       d1 = vmag(vsub(col_line%p1%pos, intersect))
       d2 = vmag(vsub(col_line%p2%pos, intersect))

       pseudo_vel = vsub(point%pos, point%prev_pos)

       intermid = vadd(point%pos, pseudo_vel)
       intermid_vec = vsub(point%pos, intermid)

       targ = vscale(vsub(intermid_vec, vscale(rnorm, 2 * vdot(intermid_vec, rnorm))), COEFF_ELASTIC)
       new_prev_pos = vadd(point%pos, targ)

       mag = vmag(targ_vec)
      end block

       ! dir_vec = vscale(vsub(point%prev_pos, point%pos), 1.0 + COEFF_ELASTIC)
       ! point%apply_pos = dir_vec

    end if
  end subroutine move_collision

  subroutine collision (me, objects, num)
    type (object), pointer :: me
    type (object), target, dimension(*) :: objects
    integer :: num, i
    integer :: s1_cnt, s2_cnt
    type (stick), pointer :: s11, s12
    type (stick), pointer :: s21, s22

    do i = 1, num
       block
       type (object), pointer :: cur_obj
       type (point_particle), pointer :: cur
       integer :: ii
       cur_obj => objects(i)

       if (.not. cur_obj%init .or. associated(cur_obj, me)) cycle

       do ii = 1, size (me%sticks)
          block
            integer :: iii, last_inter
            last_inter = 0
           
            if (.not. me%sticks(ii)%edge_stick) cycle

            do iii = 1, size (cur_obj%sticks)

               if (.not. cur_obj%sticks(iii)%edge_stick) cycle

               if (segment_intersect(me%sticks(ii)%p1%pos, me%sticks(ii)%p2%pos, cur_obj%sticks(iii)%p1%pos, cur_obj%sticks(iii)%p2%pos)) then
               end if

            end do

          end block

       end do
       
       end block
    end do
 
  end subroutine collision

  subroutine constraint (objects, num)
    type (object), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (object), pointer :: cur_obj
       type (point_particle), pointer :: cur
       integer :: ii
       cur_obj => objects(i)

       if (.not. cur_obj%init) cycle

       ! call collision (cur_obj, objects, num)
       
       do ii = 1, size (cur_obj%particles)
          cur => cur_obj%particles(ii)
          
         if (cur%pos%y > SCREEN_HEIGHT - cur%radius) then
             cur%pos%y = SCREEN_HEIGHT - cur%radius
             cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
             cur%prev_pos%x = cur%pos%x - (cur%verlet_velocity%x * COEFF_ELASTIC) * delta
          end if
 
       end do
       end block
    end do
  end subroutine

  subroutine verlet (objects, num)
    type (object), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (object), pointer :: cur_obj
       type (point_particle), pointer :: cur
       type (vector2_type) :: cur_pos
       integer :: ii
       cur_obj => objects(i)

       if (.not. cur_obj%init) cycle

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
    allocate(ob%sticks(sector_num * 2))

    mass = 1. / float(sector_num)
    ob%particles(1) = point_particle(.TRUE., pos, pos, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS)

    do i = 2, sector_num + 1
       block
         type (vector2_type) :: point
         real :: x, y

         x = pos%x + radius * cos(2 * PI * real(i - 1) / real(sector_num))
         y = pos%y + radius * sin(2 * PI * real(i - 1) / real(sector_num))
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS)
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
    allocate(ob%sticks(vertical_sticks + horiz_sticks + diag_sticks))

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

    do i = 1, (point_num - point_num_x)
       block
         real :: length
         integer p1_id, p2_id
         p1_id = i
         p2_id = i + point_num_x + 1

         if (modulo(i, point_num_x) .eq. 0) cycle
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(vertical_sticks + horiz_sticks + i - (i / point_num_x)) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .FALSE.)
     end block

     end do
         do i = 1, (point_num - point_num_x)
       block
         real :: length
         integer p1_id, p2_id
         p1_id = i
         p2_id = i + point_num_x - 1

         if (modulo(i, point_num_x) .eq. 1) cycle
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(vertical_sticks + horiz_sticks + (diag_sticks / 2) + i - (((i - 1) / point_num_x) + 1)) = &
              stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .FALSE.)
     end block
     end do

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_full_rectangle

  subroutine render (objects, num)
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
          call draw_circle(int(cur%pos%x), int(cur%pos%y), cur%radius, RED)
       end do
       end block
    end do
  end subroutine

  subroutine render_sticks (objects, num)
    type (object), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (object), pointer :: cur_obj
       type (stick), pointer :: cur
       integer :: ii
       cur_obj => objects(i)

       if (.not. cur_obj%init) cycle

       do ii = 1, size (cur_obj%sticks)
          cur => cur_obj%sticks(ii)
          call draw_line (int(cur%p1%pos%x), int(cur%p1%pos%y), int(cur%p2%pos%x), int(cur%p2%pos%y), GREEN)
       end do
       end block
    end do
  end subroutine
 
end program
