
module math
  use :: raylib
  implicit none

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

  function vinv (v1) result(vi)
    type (vector2_type), intent(in) :: v1
    type (vector2_type) :: vi

    vi = vector2_type(-v1%x, -v1%y)
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
       print *, "Error: little slope\n"
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

end module math