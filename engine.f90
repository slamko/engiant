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

  type :: engine
     type (point_particle), dimension(:), allocatable :: obj
     type (stick), dimension(:), allocatable :: sticks

     integer :: cur_obj_num
     integer :: cur_stick_num

     integer :: cur_obj
     integer :: cur_stick
  end type engine
    
  type (point_particle), target, dimension(:), allocatable :: obj
  type (stick), dimension(:), allocatable :: sticks
  type (engine), target :: eng
  type (engine), pointer :: eng_ptr

  call random_seed()

  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran raylib' // c_null_char)
  call set_target_fps(FPS)

  allocate (eng%obj(40))
  allocate (eng%sticks(50))
  num_alloc = 2

  eng%cur_obj = 0
  eng%cur_stick = 0
  eng_ptr => eng

  call instantiate_rectangle (eng_ptr, MIDDLE, 100.0, 100.0)

  do while (.not. window_should_close())
     delta = get_frame_time()
     call begin_drawing()
     call clear_background(BLACK)
     call draw_circle (MIDDLE_X, MIDDLE_Y, float(RADIUS), WHITE)

     call constraint(eng%obj, size(eng%obj))
     call verlet(eng%obj, size(eng%obj))
     call solve_sticks(eng%sticks, size(eng%sticks))
     call render(eng%obj, size(eng%obj))
     call render_sticks(eng%sticks, size(eng%sticks))

     if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then

        call instantiate_rectangle (eng_ptr, get_mouse_position(), 100.0, 100.0)
!!$        block
!!$          type (vector2_type) start_pos
!!$          integer :: i
!!$
!!$          start_pos = vadd(get_mouse_position(), vector2_type(0, 0))
!!$          ! obj(num_alloc) = circle(start_pos, start_pos, 25.0, .TRUE.)
!!$          num_alloc = num_alloc + 1
!!$
!!$          if (num_alloc > size(obj)) then
!!$             block
!!$               type (point_particle), dimension(num_alloc) :: old_arr
!!$
!!$               old_arr = obj
!!$               if (allocated(obj)) then
!!$                  deallocate(obj)
!!$               end if
!!$               allocate (obj(num_alloc * 2))
!!$               
!!$               do i = 1, num_alloc
!!$                  obj(i) = old_arr(i)
!!$               end do
!!$             
!!$             end block
!!$          end if
!!$        end block
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
    type (point_particle), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (point_particle), pointer :: cur
       cur => objects(i)

       if (cur%init) then
          if (cur%pos%y > SCREEN_HEIGHT - cur%radius) then
             cur%pos%y = SCREEN_HEIGHT - cur%radius
             cur%prev_pos%y = cur%pos%y + (cur%verlet_velocity%y * COEFF_ELASTIC) * delta
          end if
       end if
     end block
  end do
  end subroutine

  subroutine solve_sticks (sticks, num)
    type (stick), target, dimension (*) :: sticks
    integer :: num, i

    do i = 1, num
       block
         type (stick), pointer :: cur
         type (vector2_type) :: diff
         real :: dist
         cur => sticks(i)

         if (.not. cur%init) cycle

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
  end subroutine solve_sticks
 
  subroutine verlet (objects, num)
    type (point_particle), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (point_particle), pointer :: cur
       type (vector2_type) :: cur_pos
       cur => objects(i)

       if (cur%init) then
          cur_pos = cur%pos

          cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(G_ACC, (delta ** 2)))
          cur%verlet_velocity = verlet_velocity(objects(i), cur%prev_pos, delta)
          cur%prev_pos = cur_pos
       end if
       
       end block
    end do
  end subroutine

  subroutine instantiate_rectangle(eng, pos, width, height)
    type (engine), pointer :: eng
    real :: width, height
    integer :: o, s
    integer i
    type (vector2_type), dimension(4) :: init_pos
    type (vector2_type) :: pos

    s = eng%cur_stick
    o = eng%cur_obj

    init_pos(1) = vadd(pos, vector2_type(width / 2.0, - height / 2.0))
    init_pos(2) = vadd(pos, vector2_type(width / 2.0, height / 2.0))
    init_pos(3) = vadd(pos, vector2_type(-width / 2.0, height / 2.0))
    init_pos(4) = vadd(pos, vector2_type(-width / 2.0, - height / 2.0))
    
    eng%obj(o + 1) = point_particle(.TRUE., init_pos(1), init_pos(1), vector2_type(0, 0), PART_RADIUS)
    eng%obj(o + 2) = point_particle(.TRUE., init_pos(2), init_pos(2), vector2_type(0, 0), PART_RADIUS)
    eng%obj(o + 3) = point_particle(.TRUE., init_pos(3), init_pos(3), vector2_type(0, 0), PART_RADIUS)
    eng%obj(o + 4) = point_particle(.TRUE., init_pos(4), init_pos(4), vector2_type(0, 0), PART_RADIUS)

    eng%sticks(s + 1) = stick(.TRUE., eng%obj(o + 1), eng%obj(o + 2), vmag(vsub(eng%obj(o + 1)%pos, eng%obj(o + 2)%pos)))
    eng%sticks(s + 2) = stick(.TRUE., eng%obj(o + 2), eng%obj(o + 3), vmag(vsub(eng%obj(o + 2)%pos, eng%obj(o + 3)%pos)))
    eng%sticks(s + 3) = stick(.TRUE., eng%obj(o + 3), eng%obj(o + 4), vmag(vsub(eng%obj(o + 3)%pos, eng%obj(o + 4)%pos)))
    eng%sticks(s + 4) = stick(.TRUE., eng%obj(o + 4), eng%obj(o + 1), vmag(vsub(eng%obj(o + 4)%pos, eng%obj(o + 1)%pos)))
    eng%sticks(s + 5) = stick(.TRUE., eng%obj(o + 2), eng%obj(o + 4), vmag(vsub(eng%obj(o + 2)%pos, eng%obj(o + 4)%pos)))
    eng%sticks(s + 6) = stick(.TRUE., eng%obj(o + 1), eng%obj(o + 3), vmag(vsub(eng%obj(o + 1)%pos, eng%obj(o + 3)%pos)))

    eng%cur_obj = eng%cur_obj + 4
    eng%cur_stick = eng%cur_stick + 6

  end subroutine instantiate_rectangle

  subroutine render (objects, num)
    type (point_particle), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (point_particle), pointer :: cur
       type (vector2_type) :: cur_pos
       cur => objects(i)

       if (cur%init) then
          call draw_circle(int(cur%pos%x), int(cur%pos%y), cur%radius, BLUE)
       end if
       
       end block
    end do
  end subroutine

  subroutine render_sticks (sticks, num)
    type (stick), target, dimension(*) :: sticks
    integer :: num, i

    do i = 1, num

       block
       type (stick), pointer :: cur
       cur => sticks(i)

       if (.not. cur%init) cycle

       call draw_line (int(cur%p1%pos%x), int(cur%p1%pos%y), int(cur%p2%pos%x), int(cur%p2%pos%y), GREEN)
      
       end block
    end do
  end subroutine
 
end program
