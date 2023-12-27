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
  type(vector2_type), parameter :: G_ACC = vector2_type(0.0, 9.86 * 8.0)
  real :: delta
  integer :: num_alloc

  type :: object
     type (vector2_type) :: pos
     type (vector2_type) :: prev_pos
     real :: radius
     logical :: init
  end type object

  type (object), dimension(:),allocatable :: obj

  call random_seed()

  call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran raylib' // c_null_char)
  call set_target_fps(FPS)

  allocate(obj(4))
  num_alloc = 2
  obj(1) = object(vadd(MIDDLE, vector2_type(0, 0)), vadd(MIDDLE, vector2_type(0, 0)), 25.0, .TRUE.)

  do while (.not. window_should_close())
     delta = get_frame_time()
     call begin_drawing()
     call clear_background(BLACK)
     call draw_circle (MIDDLE_X, MIDDLE_Y, float(RADIUS), WHITE)

     call constraint (obj, size(obj))
     call verlet(obj, size(obj))
     call render(obj, size(obj))

     if (is_mouse_button_released(MOUSE_BUTTON_LEFT)) then
        block
          type (vector2_type) start_pos
          integer :: i

          start_pos = vadd(get_mouse_position(), vector2_type(0, 0))
          obj(num_alloc) = object(start_pos, start_pos, 25.0, .TRUE.)
          num_alloc = num_alloc + 1

          if (num_alloc > size(obj)) then
             block
               type (object), dimension(num_alloc) :: old_arr

               old_arr = obj
               if (allocated(obj)) then
                  deallocate(obj)
               end if
               allocate (obj(num_alloc * 2))
               
               do i = 1, num_alloc
                  obj(i) = old_arr(i)
               end do
             
             end block
          end if
        end block
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

    mag = sqrt((v1%x**2)+ (v1%y**2))
    vec%x = v1%x / mag
    vec%y = v1%y / mag
  end function 

  subroutine render (objects, num)
    type (object), dimension(*), intent(in) :: objects
    integer :: num
    integer :: i

    do i = 1, num
       if (objects(i)%init) then
          call draw_circle_sector(objects(i)%pos, objects(i)%radius, 0.0, 330.0, 1, BLUE)
       end if
    end do
    
  end subroutine

  subroutine solve_collisions (i, particles, length) 
    type(object), dimension(*) :: particles
    integer :: length
    
    integer :: i
    integer :: ii
 
    type(vector2_type) :: pos1, pos2

    pos1 = particles(i)%pos

    do ii = 1, length
       block
         real :: dist_mag
         type (vector2_type) :: diff

       if (i < ii .and. particles(ii)%init) then

          pos2 = particles(ii)%pos
          diff = vsub (pos1, pos2)
          dist_mag = vmag(diff)
          if (dist_mag < (particles(i)%radius + particles(ii)%radius)) then
             block
               real :: fact

               fact = -(dist_mag - (particles(i)%radius + particles(ii)%radius)) * 0.2
               particles(i)%pos = vadd(particles(i)%pos, vscale(vnormalize(diff), fact))
               particles(ii)%pos = vsub(particles(ii)%pos, vscale(vnormalize(diff), fact))
             end block
          end if
       end if

       end block
    end do
    
  end subroutine solve_collisions

  subroutine constraint (objects, num)
    type (object), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (object), pointer :: cur
       type (vector2_type) :: cur_pos
       type (vector2_type) :: diff_pos
       cur => objects(i)
       diff_pos = vsub(cur%pos, MIDDLE)

       if (cur%init) then
       if (vmag(diff_pos) > RADIUS - cur%radius) then
          block
            type (vector2_type) :: new_rel_pos
            
            new_rel_pos = vscale(vnormalize(diff_pos), 1.6 * (vmag(diff_pos) - RADIUS + cur%radius))

            cur%pos = vadd(cur%pos, vector2_type(-new_rel_pos%x, -new_rel_pos%y))
          end block
       end if

       call solve_collisions(i, objects, num)
      
       end if
       end block
    end do
  end subroutine
  
  subroutine verlet (objects, num)
    type (object), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (object), pointer :: cur
       type (vector2_type) :: cur_pos
       cur => objects(i)

       cur_pos = cur%pos

       cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(G_ACC, (delta ** 2)))
       cur%prev_pos = cur_pos
       
       end block
    end do
  end subroutine
  
end program
