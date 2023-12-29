
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
  integer, parameter :: FPS = 60
  integer, parameter :: COEFF_ELASTIC = 1.0
  real, parameter :: PART_RADIUS = 3.0

  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 8.0)
  real :: delta, prev_delta
  integer :: num_alloc

  type, abstract :: render_body
     logical :: init
  end type render_body

  type, extends (render_body) :: verlet_body
     type (vector2_type) :: pos
     type (vector2_type) :: prev_pos
  end type verlet_body

  type, extends (verlet_body) :: circle
     real :: radius
  end type circle

  type, extends (verlet_body) :: point_particle
     type (vector2_type) :: verlet_velocity
     type (vector2_type) :: apply_pos
     real :: radius
  end type point_particle

  type, extends (render_body) :: stick
     type (point_particle), pointer :: p1
     type (point_particle), pointer :: p2
     real :: length
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

  call instantiate_rectangle (eng_ptr, MIDDLE, 100.0, 100.0)

  if (segment_intersect(vector2_type(10.0, 5.0), vector2_type(10.0, 5.0), vector2_type(25.0, 15.0), vector2_type(20.0, 5.0))) then
     print *, "Hello"
  end if

  do while (.not. window_should_close())
     delta = get_frame_time()

     call begin_drawing()
     call clear_background(BLACK)
     call draw_circle (MIDDLE_X, MIDDLE_Y, float(RADIUS), WHITE)

     call constraint(eng%obj, size(eng%obj))
     call verlet(eng%obj, size(eng%obj))
     call solve_sticks(eng%obj, size(eng%obj))
     call render(eng%obj, size(eng%obj))
     call render_sticks(eng%obj, size(eng%obj))

     if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
        call instantiate_rectangle (eng_ptr, get_mouse_position(), 100.0, 100.0)
     end if

     call end_drawing()
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
    vec%y = v1%y * fact
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

  function verlet_velocity (point, prev_pos, avarage_delta) result (vel)
    type (point_particle) :: point
    type (vector2_type) :: prev_pos
    real :: avarage_delta

    type (vector2_type) :: vel

    vel = vscale(vsub(point%pos, prev_pos), 1.0 / (2.0 * avarage_delta))
  end function verlet_velocity

  function intersect_point (s1p1, s1p2, s2p1, s2p2) result (inter)
    type (vector2_type) :: s1p1, s1p2, s2p1, s2p2
    real :: a1, b1, a2, b2, x, y
    type (vector2_type) :: inter

    a1 = (s1p2%y - s1p1%y)/(s1p2%x - s1p1%x)
    b1 = s1p1%y - a1 * s1p1%x

    a2 = (s2p2%y - s2p1%y)/(s2p2%x - s2p1%x)
    b2 = s2p1%y - a2 * s2p1%x

    if (abs(a2 - a1) < 0.001) then
       inter = vector2_type(0.0, 0.0)
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

    a1 = (s1p2%y - s1p1%y)/(s1p2%x - s1p1%x)
    b1 = s1p1%y - a1 * s1p1%x

    a2 = (s2p2%y - s2p1%y)/(s2p2%x - s2p1%x)
    b2 = s2p1%y - a2 * s2p1%x

    if (abs(a2 - a1) < 0.001) then
       inter = .FALSE.
    else
       x = (b1 - b2) / (a2 - a1)
       y = a1 * x + b1

       if (y < max(s1p1%y, s1p2%y) .and. y > min(s1p1%y, s1p2%y)) then
          if (y < max(s2p1%y, s2p2%y) .and. y > min(s2p1%y, s2p2%y)) then
             if (x < max(s1p1%x, s1p2%x) .and. x > min(s1p1%x, s1p2%x)) then
                if (x < max(s2p1%x, s2p2%x) .and. x > min(s2p1%x, s2p2%x)) then
                   inter = .TRUE.
                end if
             end if
          end if
       end if
    end if
  end function segment_intersect

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
            integer :: iii
            integer :: found
            integer, dimension (2) :: me_id, id
            me_id = 1
            id = 1
            found = 0
            
            do iii = 1, size (cur_obj%sticks)
               if (segment_intersect(me%sticks(ii)%p1%pos, me%sticks(ii)%p2%pos, cur_obj%sticks(iii)%p1%pos, cur_obj%sticks(iii)%p2%pos)) then
                  found = found + 1
                  id(found) = iii

                  if (found == 2) exit
               end if
            end do

            if (found == 1) then
               block
                 integer :: found_id
                 found_id = id(1)
               found = 0

               do iii = 1, size (me%sticks)
                  if (segment_intersect(me%sticks(iii)%p1%pos, me%sticks(iii)%p2%pos, cur_obj%sticks(found_id)%p1%pos, cur_obj%sticks(found_id)%p2%pos)) then
                     found = found + 1
                     id(found) = found_id
                     
                     if (found == 2) exit
                  end if
               end do

               if (found >= 2) then
                  block
                    type (point_particle), pointer :: point => NULL()
                    type (vector2_type) :: vec_stick, norm, pseudo, targ, dir_vec

                    if (associated(me%sticks(me_id(1))%p1, me%sticks(me_id(2))%p1)) then
                       point => me%sticks(me_id(1))%p1
                    else if (associated(me%sticks(me_id(1))%p1, me%sticks(me_id(2))%p2)) then
                       point => me%sticks(me_id(1))%p1
                    else if (associated(me%sticks(me_id(1))%p2, me%sticks(me_id(2))%p1)) then
                       point => me%sticks(me_id(1))%p2
                    else if (associated(me%sticks(me_id(1))%p2, me%sticks(me_id(2))%p2)) then
                       point => me%sticks(me_id(1))%p2
                    end if

                    if (associated(point)) then
                       vec_stick = vnormalize(vsub(cur_obj%sticks(found_id)%p1%pos, cur_obj%sticks(found_id)%p2%pos))
                       norm = vector2_type(vec_stick%y, -vec_stick%x)
                       pseudo = vadd(point%pos, norm)
                       targ = intersect_point(cur_obj%sticks(found_id)%p1%pos, cur_obj%sticks(found_id)%p2%pos, pseudo, point%pos)
                       dir_vec = vscale(vsub(targ, point%pos), 0.05)

                       ! point%apply_pos = vadd(point%apply_pos, dir_vec)
                       point%pos = vadd(point%pos, dir_vec)
 
                    end if
                    
                  end block

               end if

             end block
          else if (found >= 2) then
             block
               type (point_particle), pointer :: point => NULL()
               type (vector2_type) :: vec_stick, norm, pseudo, targ, dir_vec
               integer :: found_id
               found_id = me_id(1)

                    if (associated(cur_obj%sticks(id(1))%p1, cur_obj%sticks(id(1))%p2)) then
                       point => cur_obj%sticks(id(1))%p1
                    else if (associated(cur_obj%sticks(id(1))%p1, cur_obj%sticks(id(2))%p2)) then
                       point => cur_obj%sticks(id(1))%p1
                    else if (associated(cur_obj%sticks(id(1))%p2, cur_obj%sticks(id(2))%p1)) then
                       point => cur_obj%sticks(id(1))%p2
                    else if (associated(cur_obj%sticks(id(1))%p2, cur_obj%sticks(id(2))%p2)) then
                       point => cur_obj%sticks(id(1))%p2
                    end if

                    if (associated(point)) then
                    vec_stick = vnormalize(vsub(me%sticks(found_id)%p1%pos, me%sticks(found_id)%p2%pos))
                    norm = vector2_type(vec_stick%y, -vec_stick%x)
                    pseudo = vadd(point%pos, norm)
                    targ = intersect_point(me%sticks(found_id)%p1%pos, me%sticks(found_id)%p2%pos, pseudo, point%pos)
                    dir_vec = vscale(vsub(targ, point%pos), 0.05)

                    ! point%apply_pos = vadd(point%apply_pos, dir_vec)
                    point%pos = vadd(point%pos, dir_vec)
                    end if

                  end block
               end if

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

       call collision (cur_obj, objects, num)
       
       do ii = 1, size (cur_obj%particles)
          cur => cur_obj%particles(ii)
          
         if (cur%pos%y > SCREEN_HEIGHT - cur%radius) then
             cur%pos%y = SCREEN_HEIGHT - cur%radius
             cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
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

              normal_diff = vnormalize (diff)
              ! fact = (dist - cur%length) / (2.0 * 32.0)
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
          cur => cur_obj%particles(ii)
          cur%pos = vadd(cur%pos, cur%apply_pos)
          cur%apply_pos = vector2_type(0.0, 0.0)
          cur_pos = cur%pos
          
          cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(G_ACC, (delta ** 2)))
          cur%verlet_velocity = verlet_velocity(cur, cur%prev_pos, delta)
          cur%prev_pos = cur_pos
       end do
       end block
    end do
  end subroutine

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
    allocate(ob%sticks(5))

    init_pos(1) = vadd(pos, vector2_type(width / 2.0, - height / 2.0))
    init_pos(2) = vadd(pos, vector2_type(width / 2.0, height / 2.0))
    init_pos(3) = vadd(pos, vector2_type(-width / 2.0, height / 2.0))
    init_pos(4) = vadd(pos, vector2_type(-width / 2.0, - height / 2.0))
    
    ob%particles(o + 1) = point_particle(.TRUE., init_pos(1), init_pos(1), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 2) = point_particle(.TRUE., init_pos(2), init_pos(2), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 3) = point_particle(.TRUE., init_pos(3), init_pos(3), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 4) = point_particle(.TRUE., init_pos(4), init_pos(4), vector2_type(0, 0), vector2_type(0, 0), PART_RADIUS)

    ob%sticks(s + 1) = stick(.TRUE., ob%particles(o + 1), ob%particles(o + 2), & 
         vmag(vsub(ob%particles(o + 1)%pos, ob%particles(o + 2)%pos)))
    ob%sticks(s + 2) = stick(.TRUE., ob%particles(o + 2), ob%particles(o + 3), & 
         vmag(vsub(ob%particles(o + 2)%pos, ob%particles(o + 3)%pos)))
    ob%sticks(s + 3) = stick(.TRUE., ob%particles(o + 3), ob%particles(o + 4), & 
         vmag(vsub(ob%particles(o + 3)%pos, ob%particles(o + 4)%pos)))
    ob%sticks(s + 4) = stick(.TRUE., ob%particles(o + 4), ob%particles(o + 1), & 
         vmag(vsub(ob%particles(o + 4)%pos, ob%particles(o + 1)%pos)))
    ob%sticks(s + 5) = stick(.TRUE., ob%particles(o + 2), ob%particles(o + 4), & 
         vmag(vsub(ob%particles(o + 2)%pos, ob%particles(o + 4)%pos)))
    ! ob%sticks(s + 6) = stick(.TRUE., ob%particles(o + 1), ob%particles(o + 3), &
         ! vmag(vsub(ob%particles(o + 1)%pos, ob%particles(o + 3)%pos)))

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
          ! call draw_pixel(int(cur%pos%x), int(cur%pos%y), RED)
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
