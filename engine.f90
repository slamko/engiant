
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

  call random_seed()

  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran raylib' // c_null_char)
  call set_target_fps(FPS)

  allocate (eng%obj(40))

  eng%cur_obj = 0
  eng_ptr => eng

  call instantiate_rectangle (eng_ptr, MIDDLE, 100.0, 100.0)

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

       do ii = 1, size (cur_obj%particles)
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
              fact = (dist - cur%length) / (2.0 * 32.0)
              ! fact = (dist - cur%length) / (2.0 * 1.0)
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
    allocate(ob%sticks(6))

    init_pos(1) = vadd(pos, vector2_type(width / 2.0, - height / 2.0))
    init_pos(2) = vadd(pos, vector2_type(width / 2.0, height / 2.0))
    init_pos(3) = vadd(pos, vector2_type(-width / 2.0, height / 2.0))
    init_pos(4) = vadd(pos, vector2_type(-width / 2.0, - height / 2.0))
    
    ob%particles(o + 1) = point_particle(.TRUE., init_pos(1), init_pos(1), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 2) = point_particle(.TRUE., init_pos(2), init_pos(2), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 3) = point_particle(.TRUE., init_pos(3), init_pos(3), vector2_type(0, 0), PART_RADIUS)
    ob%particles(o + 4) = point_particle(.TRUE., init_pos(4), init_pos(4), vector2_type(0, 0), PART_RADIUS)

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
    ob%sticks(s + 6) = stick(.TRUE., ob%particles(o + 1), ob%particles(o + 3), &
         vmag(vsub(ob%particles(o + 1)%pos, ob%particles(o + 3)%pos)))

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
          call draw_circle(int(cur%pos%x), int(cur%pos%y), cur%radius, BLUE)
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
