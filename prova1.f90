module funzioni
implicit none
contains
function vx(x,y) result(res)
	real,intent(in) :: x,y
	real :: res
	if(x*y<=0) then
		res=1
	elseif (abs(y)<=abs(x**3)) then
		res=(1+ 9 * (y**4)**(1./3))**(-.5)
	else
		res=(1+9*x**4)**(-.5)
	end if
end function

function vy(x,y) result(res)
	real,intent(in) :: x,y
	real :: res
	if(x*y<=0) then
		res=0
	else if (abs(y)<=abs(x**3)) then
		res=3*(y**2)**(1./3) * (1 + 9*(y**4)**(1./3))**(-.5)
	else
		res=3*x**2*(1+9*x**4)**(-.5)
	end if
end function
end module funzioni


program aaaa
use funzioni
implicit none
real :: x,y,dx=0.1,dy=0.1,v_x,v_y
integer :: i,j
do i=-100,100
	do j=-100,100
		x=i*dx
		y=j*dy
		v_x=vx(x,y)
		v_y=vy(x,y)
		write(1,*) x, y, v_x*dx, v_y*dx, v_x**2+v_y**2
	end do
end do

end program

