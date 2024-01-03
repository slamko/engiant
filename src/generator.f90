module generator
  use types
  use math

  implicit none

contains
  subroutine insert_object (eng)
    type (engine), pointer :: eng
    type (object), pointer :: new_obj
    integer :: siz

    if (eng%cur_obj >= size(eng%obj)) then
       block
       type (object), dimension (size(eng%obj)) :: obj
       integer :: i

       siz = size(eng%obj)
       obj = eng%obj

       if (allocated (eng%obj)) deallocate (eng%obj)

       allocate(eng%obj(2 * siz))

       do i = 1, siz
          eng%obj(i) = obj(i) 
       end do
     end block
    end if
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

    call insert_object (eng)
    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    allocate(ob%particles(sector_num))
    allocate(ob%match_shape(sector_num))
    allocate(ob%sticks(sector_num))

    mass = 1. / float(sector_num)

    do i = 1, sector_num
       block
         type (vector2_type) :: point
         real :: x, y

         x = pos%x + radius * cos(2 * PI * real(i - 1) / real(sector_num))
         y = pos%y + radius * sin(2 * PI * real(i - 1) / real(sector_num))
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS, .FALSE.)
         ob%match_shape(i) = geom_point(vsub(point, pos))
       end block
    end do

    do i = 1, sector_num
       block
         real :: length
         integer :: next_id

         next_id = i + 1

         if (i + 1 > sector_num) then 
            next_id = 1
         end if
         
         length = vmag(vsub(ob%particles(i)%pos, ob%particles(next_id)%pos))

         ob%sticks(i) = stick(.TRUE., ob%particles(i), ob%particles(next_id), length, .TRUE.)
     end block
    end do

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_polygon

  subroutine instantiate_rectangle (eng, pos, height, width, space)
    type (engine), pointer :: eng
    real :: height, width, space
    integer :: point_num, point_num_x, point_num_y, vertical_sticks, horiz_sticks, diag_sticks
    type (vector2_type) :: pos, start_pos
    type (object), pointer :: ob
    integer :: s = 0, o = 0
    integer :: i, ii
    real :: mass

    ! call insert_object (eng)
    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    point_num_x = int(width / space) + 1
    point_num_y = int(height / space) + 1
    point_num = (2 * point_num_x) + (2 * (point_num_y - 2))

    vertical_sticks = 2 * (point_num_y - 1)
    horiz_sticks = 2 * (point_num_x - 1)

    allocate(ob%particles(point_num))
    allocate(ob%match_shape(point_num))
    allocate(ob%sticks(vertical_sticks + horiz_sticks))

    start_pos = vector2_type(pos%x - width / 2, pos%y - height / 2)
    mass = 1.0 / float(point_num)

    i = 1
    do ii = 1, (point_num_x) * (point_num_y)
       block
         type (vector2_type) :: point
         real :: x, y

         if (modulo(ii - 1, point_num_x) > 0 .and. modulo (ii - 1, point_num_x) < point_num_x - 1 &
              .and. ii > point_num_x .and. ii < (point_num_x * point_num_y) - point_num_x) then
            cycle
         end if
         
         x = start_pos%x + space * modulo(ii - 1, point_num_x) 
         y = start_pos%y + space * ((ii - 1) / point_num_x)
         point = vector2_type (x, y)
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS, .FALSE.)
         ob%match_shape(i) = geom_point(vsub(point, pos))

         i = i + 1
       end block
    end do

    do i = 1, point_num_x - 1
       block
         real :: length
         integer p1_id, p2_id

         p1_id = i
         p2_id = i + 1
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(i) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
     end block
    end do

    do i = point_num - point_num_x + 1, point_num - 1
       block
         real :: length
         integer p1_id, p2_id

         p1_id = i
         p2_id = i + 1

         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks((horiz_sticks / 2) + (i - (point_num - point_num_x))) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
     end block
     end do

    do i = 1, (vertical_sticks / 2) - 1
       block
         real :: length
         integer p1_id, p2_id

         p1_id = point_num_x + 1 + (i - 1) * 2
         p2_id = p1_id + 2
        
         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(horiz_sticks + i) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
     end block
    end do

    block
      real :: length
      integer p1_id, p2_id
      
      p1_id = 1
      p2_id = p1_id + point_num_x
      
      length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))
       
      ob%sticks(horiz_sticks + (vertical_sticks / 2)) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
    end block
    
    do i = 1, (vertical_sticks / 2) - 1
       block
         real :: length
         integer p1_id, p2_id

         p1_id = point_num_x + (i - 1) * 2
         p2_id = p1_id + 2

         length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))

         ob%sticks(horiz_sticks + (vertical_sticks / 2) + i) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
     end block
     end do

     block
       real :: length
       integer p1_id, p2_id
       
       p1_id = point_num - point_num_x
       p2_id = point_num
       
       length = vmag(vsub(ob%particles(p1_id)%pos, ob%particles(p2_id)%pos))
       
       ob%sticks(horiz_sticks + vertical_sticks) = stick(.TRUE., ob%particles(p1_id), ob%particles(p2_id), length, .TRUE.)
     end block

    eng%cur_obj = eng%cur_obj + 1
  end subroutine instantiate_rectangle

  subroutine instantiate_full_rectangle (eng, pos, height, width, space)
    type (engine), pointer :: eng
    real :: height, width, space
    integer :: point_num, point_num_x, point_num_y, vertical_sticks, horiz_sticks, diag_sticks
    type (vector2_type) :: pos, start_pos
    type (object), pointer :: ob
    integer :: s = 0, o = 0
    integer :: i
    real :: mass

    call insert_object (eng)
    ob => eng%obj(eng%cur_obj + 1)
    ob%init = .TRUE.

    point_num_x = int(width / space) + 1
    point_num_y = int(height / space) + 1
    point_num = point_num_x * point_num_y

    vertical_sticks = (point_num_y - 1) * point_num_x
    horiz_sticks = (point_num_x - 1) * point_num_y
    diag_sticks = (point_num_x - 1) * (point_num_y - 1) * 2

    allocate(ob%particles(point_num))
    allocate(ob%match_shape(point_num))
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
         
         ob%particles(i) = point_particle(.TRUE., point, point, vector2_type(0, 0), mass, vector2_type(0, 0), PART_RADIUS, .FALSE.)
         ob%match_shape(i) = geom_point(vsub(point, pos))
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

end module generator
