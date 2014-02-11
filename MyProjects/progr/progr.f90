program progr
use dfopngl
use dfwin
use dflib
implicit none

integer(4)      ret
integer(2)      pattern
real, dimension(-255:255, -255:255) :: a
real:: ranval, masmax, i, j
integer :: colors
    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
	call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Хрень какая-то"C)
    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(-1.0), DBLE(-1.0), DBLE(1.0))
	call fglBegin(GL_POINTS)

masmax=2.0
colors=256
do i = -255, 255
 do j=-255, 255
  call random(ranval)
  a(i, j)=ranval+1.0
 end do
end do
call vis(a, masmax, colors)
call sleep(2000)




contains
subroutine vis(mas, masmax, colors)
 real, dimension(-255:255, -255:255) :: mas
 integer, dimension(-255:255, -255:255) :: a
 real :: masmax, r, g, b, x, y
 integer :: i, j, colors, k, sec, co
 do i=-255, 255
  do j=-255, 255
   a(i, j)=int(mas(i, j)*colors/masmax)
  end do
 end do
 do i=-255, 255
  do j=-255, 255
   x=i/255
   y=j/255
   co=a(i, j)
   sec=0
   k=0
   do while (((co-mod(co, colors))/real(colors)) > 0)
    sec=sec+(mod(co, colors)*(10**k))
    co=((co-mod(co, colors))/real(colors))
    k=k+1
   end do
   sec=sec+(mod(co, colors)*(10**k))  
   sec=mod(sec, 1000)
   r=((sec-mod(sec,100))/100)
   g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
   b=(mod(sec, 10))
   call fglColor3f((r/real(colors)),(g/real(colors)),(b/real(colors)))
   call fglVertex2f(x, y)
  end do
 end do
end subroutine vis

end program progr