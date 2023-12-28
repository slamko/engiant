  subroutine render (objects, num)
    type (circle), dimension(*), intent(in) :: objects
    integer :: num
    integer :: i

    do i = 1, num
       if (objects(i)%init) then
          call draw_circle_sector(objects(i)%pos, objects(i)%radius, 0.0, 330.0, 1, BLUE)
       end if
    end do
    
  end subroutine

  subroutine solve_collisions (i, particles, length) 
    type (circle), dimension(*) :: particles
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
    type (circle), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (circle), pointer :: cur
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
    type (circle), target, dimension(*) :: objects
    integer :: num, i

    do i = 1, num
       block
       type (circle), pointer :: cur
       type (vector2_type) :: cur_pos
       cur => objects(i)

       cur_pos = cur%pos

       cur%pos = vadd(vsub(vscale(cur%pos, 2.0), cur%prev_pos), vscale(G_ACC, (delta ** 2)))
       cur%prev_pos = cur_pos
       
       end block
    end do
  end subroutine
 
