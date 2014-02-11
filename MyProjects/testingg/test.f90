program test
use dflib
use dfopngl
use dfwin
implicit none
external array
integer(4)      ret
integer(2)      pattern
real, dimension(500, 500):: a, b
real :: ranval
integer :: i, j, l
a(200, 200)=2

 call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
 call fauxInitPosition (0, 0, 500, 500)
 ret = fauxInitWindow ("Массив"C)
 call fglClearColor (0.0, 0.0, 0.0, 0.0)
 call fglClear(GL_COLOR_BUFFER_BIT)
 call fglColor3f(1.0, 1.0, 1.0)
 call fglMatrixMode (GL_PROJECTION)
 call fglLoadIdentity ()
 call fglOrtho(DBLE(0.0), DBLE(1.0), DBLE(0.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0))
 call fglBegin(GL_POINTS)



do l = 1, 1000

do i=1, 500
do j=1, 500
b(i, j)=a(i, j)
end do
end do

do i=2, 499
do j=2, 499
call random(ranval)
a(i, j) = int((b((i-1), (j-1))+ &
          b((i-1), j)+ &
		  b((i-1), (j+1))+ &
          b(i, (j-1))+ &
		  b(i, j)+ &
		  b(i, (j+1))+ &
		  b((i+1), (j-1))+ &
		  b((i+1), j)+ &
		  b((i+1), (j+1)))* &
          (ranval/10))
end do
end do
call array(a, 1, 500, 1, 500, -1000.0, 1000.0, 27)
end do
call fglEnd()
call fglFlush()
call sleep(1000)
end program test 