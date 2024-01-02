
program main
  use, intrinsic :: iso_c_binding, only: c_null_char
  use raylib
  use types
  use math
  use physics
  use renderer
  use generator

  implicit none

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
       
       do i = 1, 3
          call iter(apply_springs, eng%obj, size(eng%obj))
       end do

       call iter(apply_shape_match, eng%obj, size(eng%obj))
       
       call iter(apply_gravity, eng%obj, size(eng%obj))
       call iter(constraint, eng%obj, size(eng%obj))
       call iter2(collision, eng%obj, size(eng%obj))
       
       call iter(verlet, eng%obj, size(eng%obj))
       
       call iter(render, eng%obj, size(eng%obj))
       call iter(render_sticks, eng%obj, size(eng%obj))
       
       if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
          call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 80.0)
          ! call instantiate_rectangle (eng_ptr, get_mouse_position(), 40.0, 40., 20.0)
          ! call instantiate_rectangle (eng_ptr, get_mouse_position(), 100.0, 100., 20.0)
          ! call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 120.0, 160., 40.0)
          ! call instantiate_polygon (eng_ptr, get_mouse_position(), 35.0, 16)
       end if
       
       if (is_mouse_button_released(MOUSE_BUTTON_RIGHT)) then
          call instantiate_polygon (eng_ptr, get_mouse_position(), 30.0, 10)
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


end program
