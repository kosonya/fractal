program spectre
use dfopngl
use dfwin

integer(4)      ret
integer(2)      pattern
real :: lambda, y

    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
    call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("spectre"C)

    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(400.0), DBLE(700.0), DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0))
    call fglBegin(GL_POINTS)

lambda=700.0
do while (lambda>400.0)
   call fglColor3f(red(1100.0-lambda), green(1100.0-lambda), blue(1100.0-lambda))
   y=-1.0
   do while (y<1.0)
    call fglVertex2f(lambda, y) 
    y=y+0.003
   end do
   lambda=lambda-0.05
end do
 call fglEnd()
 call fglFlush()
 call sleep(10000)

contains
real function red(lambda)
 real :: lambda
 real, parameter :: sigma = 700.0
 red=1.05**(-(abs((lambda-sigma)*0.032)**2))
end function red

real function green(lambda)
 real :: lambda
 real, parameter :: sigma = 550.0
 green=1.05**(-(abs((lambda-sigma)*0.032)**2))
end function green

real function blue(lambda)
 real :: lambda
 real, parameter :: sigma = 400.0
 blue=1.05**(-(abs((lambda-sigma)*0.032)**2))
end function blue

end program spectre