module renderer
  use raylib
  use types

contains

  subroutine render (cur_obj)
    type (object), pointer :: cur_obj
    type (point_particle), pointer :: cur
    integer :: ii

    do ii = 1, size (cur_obj%particles)
       cur => cur_obj%particles(ii)
       call draw_circle(int(cur%pos%x), int(cur%pos%y), cur%radius, RED)
    end do
  end subroutine render

  subroutine render_sticks (cur_obj)
    type (object), pointer :: cur_obj
    type (stick), pointer :: cur
    integer :: ii
    
    do ii = 1, size (cur_obj%sticks)
       cur => cur_obj%sticks(ii)
       call draw_line (int(cur%p1%pos%x), int(cur%p1%pos%y), int(cur%p2%pos%x), int(cur%p2%pos%y), GREEN)
    end do
  end subroutine
 
end module renderer
