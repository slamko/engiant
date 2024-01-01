
program main
  use, intrinsic :: iso_c_binding, only: c_null_char
  use :: raylib
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

  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 16.0)
  real :: delta, prev_delta
  integer :: num_alloc

  type, abstract :: render_body
     logical :: init
  end type render_body

  type, extends (render_body) :: verlet_body
     type (vector2_type) :: pos
     type (vector2_type) :: velocity
  end type verlet_body

  type, extends (verlet_body) :: point_particle
     type (vector2_type) :: verlet_velocity
     type (vector2_type) :: apply_pos
     type (vector2_type) :: apply_prev_pos
     real :: radius
     integer :: intersect_cnt
     real :: intersect_mag
  end type point_particle

  type, extends (render_body) :: stick
     type (point_particle), pointer :: p1
     type (point_particle), pointer :: p2
     real :: length
     logical :: edge_stick
  end type stick

  type, extends (render_body) :: object
     type (point_particle), dimension(:), allocatable :: particles
     type (stick), dimension(:), allocatable :: sticks
  end type object

  type :: engine
     type (object), dimension(:), allocatable :: obj
     integer :: cur_obj_num
     integer :: cur_obj
  end type engine
    
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

  if (segment_intersect(vector2_type(10.0, 10.0), vector2_type(10.0, 20.0), vector2_type(5.0, 15.0), vector2_type(15.0, 15.0))) then
     print *, "OK"
  end if

  ! call instantiate_full_rectangle (eng_ptr, MIDDLE, 160.0, 160., 160.0)
  do while (.not. window_should_close())
     block
       integer :: i
     delta = get_frame_time()

     call begin_drawing()
     call clear_background(BLACK)
     ! call draw_polygon (MIDDLE_X, MIDDLE_Y, float(RADIUS), WHITE)

     do i = 1, 1
        ! call solve_sticks(eng%obj, size(eng%obj))
     end do

     call constraint(eng%obj, size(eng%obj))
     ! call apply_pos(eng%obj, size(eng%obj))

      do i = 1, 1
        call solve_sticks(eng%obj, size(eng%obj))
     end do

     ! call draw_cube_v(vector3_type(100.0, 300.0, 150.0), vector3_type(200.0, 100.0, 150.0), RED)

     call rk4(eng%obj, size(eng%obj))
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
  function vsub (v1, v2) result(v3)
    type (vector2_type), intent(in) :: v1, v2
    type (vector2_type) :: v3

    v3%x = v1%x - v2%x
    v3%y = v1%y - v2%y
  end function 

  function vadd (v1, v2) result(v3)
    type (vector2_type), intent(in) :: v1, v2
    type (vector2_type) :: v3

    v3%x = v1%x + v2%x
    v3%y = v1%y + v2%y
  end function 

  function vdot (v1, v2) result(res)
    type (vector2_type), intent(in) :: v1, v2
    real :: res

    res = v1%x * v2%x + v1%y * v2%y
  end function 

  function vscale (v1, fact) result(vec)
    type (vector2_type), intent(in) :: v1
    real, intent(in) :: fact
    type (vector2_type) :: vec

    vec%x = v1%x * fact
    vec%y = v1%y * fact         !
  end function 

  function vmag (v1) result(mag)
    type (vector2_type), intent(in) :: v1
    real :: mag

    mag = sqrt((v1%x * v1%x) + (v1%y * v1%y))
  end function 

  function vnormalize (v1) result(vec)
    type (vector2_type), intent(in) :: v1
    real :: mag
    type (vector2_type) :: vec

    mag = sqrt((v1%x**2) + (v1%y**2))
    if (mag < 0.001) then
       vec = vector2_type(0.0, 0.0)
    else
       vec%x = v1%x / mag
       vec%y = v1%y / mag
    end if
  end function 

  function point_in_segment (x, y, s1p1, s1p2, s2p1, s2p2) result (inter)
    type (vector2_type) :: s1p1, s1p2, s2p1, s2p2
    real :: x, y
    logical :: inter
    inter =  .FALSE.

    if ((y < max(s1p1%y, s1p2%y) .and. y > min(s1p1%y, s1p2%y)) .and. (x < max(s2p1%x, s2p2%x) .and. x > min(s2p1%x, s2p2%x))) then
       inter = .TRUE.
    end if
    
    if ((x < max(s1p1%x, s1p2%x) .and. x > min(s1p1%x, s1p2%x)) .and. (y < max(s2p1%y, s2p2%y) .and. y > min(s2p1%y, s2p2%y))) then
       inter = .TRUE.
    end if

  end function point_in_segment

  function intersect_point (s1p1, s1p2, s2p1, s2p2) result (inter)
    type (vector2_type) :: s1p1, s1p2, s2p1, s2p2
    real :: a1, b1, a2, b2, x, y
    type (vector2_type) :: inter

    if (abs(s1p2%x - s1p1%x) < 0.001) then
       a1 = 1000.0
    else 
       a1 = (s1p2%y - s1p1%y)/(s1p2%x - s1p1%x)
    end if

    b1 = s1p1%y - a1 * s1p1%x

    if (abs(s2p2%x - s2p1%x) < 0.001) then
       a2 = 1000.0
    else 
       a2 = (s2p2%y - s2p1%y)/(s2p2%x - s2p1%x)
    end if

    b2 = s2p1%y - a2 * s2p1%x

    if (abs(a2 - a1) < 0.001) then
       inter = vector2_type(0.0, 0.0)
       print *, "Error: little\n"
    else
       x = (b1 - b2) / (a2 - a1)
       y = a1 * x + b1

       inter = vector2_type(x, y)
    end if

  end function intersect_point

  function segment_intersect(s1p1, s1p2, s2p1, s2p2) result (inter)
    type (vector2_type) :: s1p1, s1p2, s2p1, s2p2
    real :: a1, b1, a2, b2, x, y
    logical :: inter

    inter = .FALSE.

    if (abs(s1p2%x - s1p1%x) < 0.001) then
       a1 = 1000.0
    else 
       a1 = (s1p2%y - s1p1%y)/(s1p2%x - s1p1%x)
    end if

    b1 = s1p1%y - a1 * s1p1%x

    if (abs(s2p2%x - s2p1%x) < 0.001) then
       a2 = 1000.0
    else 
       a2 = (s2p2%y - s2p1%y)/(s2p2%x - s2p1%x)
    end if

    b2 = s2p1%y - a2 * s2p1%x

    if (abs(a2 - a1) < 0.001) then
       inter = .FALSE.
    else
       x = (b1 - b2) / (a2 - a1)
       y = a1 * x + b1

       if (point_in_segment(x, y, s1p1, s1p2, s2p1, s2p2)) then
          inter = .TRUE.
       end if

    end if
  end function segment_intersect

  subroutine move_collision (point, line, col_line)
    type (point_particle), pointer :: point
    type (stick), intent(in) :: line, col_line
    type (vector2_type) :: vec_stick, norm, pseudo, targ, dir_vec, targ_vec, npoint
    real :: scale, mag, speed_fact
    real :: limit

    if (associated(point)) then
      block

         type (vector2_type) :: rnorm, intersect, pseudo_vel, intermid, intermid_vec, new_prev_pos
         
       vec_stick = vnormalize(vsub(col_line%p2%pos, col_line%p1%pos))
       norm%x = vec_stick%y
       norm%y = -vec_stick%x
       npoint = vadd(point%pos, norm)
       intersect = intersect_point(col_line%p1%pos, col_line%p2%pos, point%pos, npoint)
       targ_vec = vsub(intersect, point%pos)

       rnorm = vnormalize(vsub(intersect, point%pos))
       rnorm%x = -rnorm%x
       rnorm%y = -rnorm%y

       ! pseudo_vel = vsub(point%pos, point%prev_pos)

       intermid = vadd(point%pos, pseudo_vel)
       intermid_vec = vsub(point%pos, intermid)

       targ = vscale(vsub(intermid_vec, vscale(rnorm, 2 * vdot(intermid_vec, rnorm))), COEFF_ELASTIC)
       new_prev_pos = vadd(point%pos, targ)

       mag = vmag(targ_vec)

       if (point%intersect_mag > mag) then
!!$          if (vdot(pseudo_vel, rnorm) < 0) then
!!$             block
!!$               type (vector2_type) :: inv_vel, addit
!!$               inv_vel%x = -rnorm%x
!!$               inv_vel%y = -rnorm%y
!!$
!!$               addit = vadd(point%pos, vscale(rnorm, 2*COEFF_ELASTIC))
!!$
!!$               point%apply_pos = vsub(addit, point%prev_pos)
!!$             end block
!!$          else
!!$             point%apply_pos = vsub(new_prev_pos, point%prev_pos)
!!$          end if
!!$

          point%intersect_mag = mag

       end if
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

                  if (last_inter .ne. ii) then
                     me%sticks(ii)%p1%intersect_cnt = me%sticks(ii)%p1%intersect_cnt + 1
                     me%sticks(ii)%p2%intersect_cnt = me%sticks(ii)%p2%intersect_cnt + 1
                     last_inter = ii
                  end if

                  call move_collision(me%sticks(ii)%p1, me%sticks(ii), cur_obj%sticks(iii))
                  call move_collision(me%sticks(ii)%p2, me%sticks(ii), cur_obj%sticks(iii))

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
            cur%velocity =  vscale(vsub(cur%velocity, vscale(vector2_type(0.0, -1.0), 2*vdot(cur%velocity, vector2_type(0.0, -1.0)))), COEFF_ELASTIC)
             ! cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
             ! cur%prev_pos%x = cur%pos%x - (cur%verlet_velocity%x * COEFF_ELASTIC) * delta
          end if
 
       end do
       end block
    end do
  end subroutine

  subroutine solve_sticks (objects, num)
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
              type (vector2_type) :: apply_vec
              real :: fact

              normal_diff = vnormalize (diff) !
              ! fact = (dist - cur%length) / (2.0 * 8.0)
              
              fact = (dist - cur%length) / (2.0 * 1.0)
              apply_vec = vscale (normal_diff, fact)

              if (cur%p1%pos%x > cur%p2%pos%x) then
                 cur%p1%pos%x = cur%p1%pos%x - apply_vec%x
                 cur%p2%pos%x = cur%p2%pos%x + apply_vec%x
              else
                 cur%p1%pos%x = cur%p1%pos%x - apply_vec%x
                 cur%p2%pos%x = cur%p2%pos%x + apply_vec%x
              end if

              if (cur%p1%pos%y > cur%p2%pos%y) then
                 cur%p1%pos%y = cur%p1%pos%y - apply_vec%y
                 cur%p2%pos%y = cur%p2%pos%y + apply_vec%y
              else
                 cur%p1%pos%y = cur%p1%pos%y - apply_vec%y
                 cur%p2%pos%y = cur%p2%pos%y + apply_vec%y
              end if

            end block
         end if
         
         end block
 
       end do
       end block
    end do
  end subroutine solve_sticks
 
  subroutine apply_pos (objects, num)
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

          if (cur%intersect_cnt >= 2) then
             if (.not. isnan(cur%apply_pos%x) .and. .not. isnan(cur%apply_pos%y)) then
                ! cur%pos = vadd(cur%pos, cur%apply_pos)
                ! cur%prev_pos = vadd(cur%prev_pos, cur%apply_prev_pos) !
             end if
          end if

          cur%intersect_cnt = 0
          cur%apply_pos = vector2_type(0.0, 0.0)
          cur%intersect_mag = 1000.0
       end do
       end block
    end do
  end subroutine

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
 
  subroutine rk4 (objects, num)
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

          cur%velocity = rk4_vector(cur%velocity, G_ACC, delta)
          cur%pos = rk4_vector(cur%pos, cur%velocity, delta)
       end do
       end block
    end do
  end subroutine

  subroutine instantiate_circle (eng, pos, radius, sector_num)
    type (engine), pointer :: eng
    real :: radius
    integer :: sector_num
    type (vector2_type), dimension(4) :: init_pos
    type (vector2_type) :: pos
    type (object), pointer :: ob
    integer :: s, o = 0
    integer :: i

    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    allocate(ob%particles(sector_num))
    allocate(ob%sticks(sector_num + sector_num / 2))

    ! ob%particles(1) = point_particle(.TRUE., pos, pos, vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0)

    do i = 1, sector_num
       block
         type (vector2_type) :: point
         real :: x, y

         x = pos%x + radius * cos(2 * PI * real(i - 1) / real(sector_num))
         y = pos%y + radius * sin(2 * PI * real(i - 1) / real(sector_num))
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)
       end block
    end do

    do i = 1, sector_num
       block
         real :: length
         integer :: next_id

         next_id = i + 1

         if (i + 2 > sector_num + 1) then 
            next_id = 1
         end if
         
         length = vmag(vsub(ob%particles(i)%pos, ob%particles(next_id)%pos))

         ob%sticks(i) = stick(.TRUE., ob%particles(i), ob%particles(next_id), length, .TRUE.)
     end block
    end do
    do i = 1, sector_num / 2
       block
         real :: length
        
         length = vmag(vsub(ob%particles(i)%pos, ob%particles(sector_num / 2 + i)%pos))

         ob%sticks(sector_num + i) = stick(.TRUE., ob%particles(i), ob%particles(sector_num / 2 + i), length, .FALSE.)
     end block
    end do

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_circle

  subroutine instantiate_polygon (eng, pos, radius, sector_num)
    type (engine), pointer :: eng
    real :: radius
    integer :: sector_num
    type (vector2_type), dimension(4) :: init_pos
    type (vector2_type) :: pos
    type (object), pointer :: ob
    integer :: s, o = 0
    integer :: i

    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    allocate(ob%particles(sector_num + 1))
    allocate(ob%sticks(sector_num * 2))

    ob%particles(1) = point_particle(.TRUE., pos, vector2_type(0,0), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)

    do i = 2, sector_num + 1
       block
         type (vector2_type) :: point
         real :: x, y

         x = pos%x + radius * cos(2 * PI * real(i - 1) / real(sector_num))
         y = pos%y + radius * sin(2 * PI * real(i - 1) / real(sector_num))
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, vector2_type(0,0), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)
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

    do i = 1, point_num
       block
         type (vector2_type) :: point
         real :: x, y

         x = start_pos%x + space * modulo(i - 1, point_num_x) 
         y = start_pos%y + space * ((i - 1) / point_num_x)
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, vector2_type(0,0), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)
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

  subroutine instantiate_rectangle(eng, pos, width, height)
    type (engine), pointer :: eng
    real :: width, height
    type (vector2_type), dimension(4) :: init_pos
    type (vector2_type) :: pos
    type (object), pointer :: ob
    integer :: s, o
    s = 0
    o = 0

    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.
    allocate(ob%particles(4))
    allocate(ob%sticks(6))

    init_pos(1) = vadd(pos, vector2_type(width / 2.0, - height / 2.0))
    init_pos(2) = vadd(pos, vector2_type(width / 2.0, height / 2.0))
    init_pos(3) = vadd(pos, vector2_type(-width / 2.0, height / 2.0))
    init_pos(4) = vadd(pos, vector2_type(-width / 2.0, - height / 2.0))
    
    ob%particles(o + 1) = point_particle(.TRUE., init_pos(1), init_pos(1), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0) !
    ob%particles(o + 2) = point_particle(.TRUE., init_pos(2), init_pos(2), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)
    ob%particles(o + 3) = point_particle(.TRUE., init_pos(3), init_pos(3), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)
    ob%particles(o + 4) = point_particle(.TRUE., init_pos(4), init_pos(4), vector2_type(0, 0), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS, 0, 1000.0)

    ob%sticks(s + 1) = stick(.TRUE., ob%particles(o + 1), ob%particles(o + 2), & 
         vmag(vsub(ob%particles(o + 1)%pos, ob%particles(o + 2)%pos)), .TRUE.)
    ob%sticks(s + 2) = stick(.TRUE., ob%particles(o + 2), ob%particles(o + 3), & 
         vmag(vsub(ob%particles(o + 2)%pos, ob%particles(o + 3)%pos)), .TRUE.)
    ob%sticks(s + 3) = stick(.TRUE., ob%particles(o + 3), ob%particles(o + 4), & 
         vmag(vsub(ob%particles(o + 3)%pos, ob%particles(o + 4)%pos)), .TRUE.)
    ob%sticks(s + 4) = stick(.TRUE., ob%particles(o + 4), ob%particles(o + 1), & 
         vmag(vsub(ob%particles(o + 4)%pos, ob%particles(o + 1)%pos)), .TRUE.)
    ob%sticks(s + 5) = stick(.TRUE., ob%particles(o + 2), ob%particles(o + 4), & 
         vmag(vsub(ob%particles(o + 2)%pos, ob%particles(o + 4)%pos)), .FALSE.)
    ob%sticks(s + 6) = stick(.TRUE., ob%particles(o + 1), ob%particles(o + 3), &
         vmag(vsub(ob%particles(o + 1)%pos, ob%particles(o + 3)%pos)), .FALSE.)

    eng%cur_obj = eng%cur_obj + 1

  end subroutine instantiate_rectangle

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