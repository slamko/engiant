
program main
  use, intrinsic :: iso_c_binding, only: c_null_char

  use raylib
  use types
  use math
  use physics
  use renderer
  use generator
  use integrator
  use verlet

  implicit none

  type (engine), target :: eng
  type (engine), pointer :: eng_ptr
  integer :: j

  call random_seed()

  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran raylib' // c_null_char)
  call set_target_fps(FPS)

  allocate (eng%obj(2))

  do j = 1, 2
     eng%obj(j)%init = .FALSE.
  end do
  
  eng%cur_obj = 0
  eng_ptr => eng

  ! call instantiate_full_rectangle (eng_ptr, MIDDLE, 160.0, 160., 160.0)
  do while (.not. window_should_close())
     block
       integer :: i
       type (verlet_integrator), target :: verlet_integ
       class (base_integrator), pointer :: integ

       integ => verlet_integ

       delta = get_frame_time()
       
       call begin_drawing()
       call clear_background(BLACK)
       
       do i = 1, 3
          call iter(apply_springs, integ, eng%obj, size(eng%obj))
       end do

       call iter(apply_shape_match, integ, eng%obj, size(eng%obj))
       
       call iter(apply_gravity, integ, eng%obj, size(eng%obj))
       call iter(constraint, integ, eng%obj, size(eng%obj))
       call iter2(collision, integ, eng%obj, size(eng%obj))
       
       call iter(do_integrate, integ, eng%obj, size(eng%obj))
       
       call iter(render, integ, eng%obj, size(eng%obj))
       call iter(render_sticks, integ, eng%obj, size(eng%obj))
       
       if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
          ! call instantiate_full_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 80.0)
          ! call instantiate_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 20.0)
          call instantiate_rectangle (eng_ptr, get_mouse_position(), 80.0, 80., 80.0)
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
end program
