program fract
  use dfopngl
  use dfwin

  implicit none


  real :: rez, imz, absmax, step
  integer(4)      ret
  integer(2)      pattern
  real :: a, b, r, g, bl
  integer :: i, k, sec
  complex :: z


	rez=0.0
	imz=0.0
	absmax=20
	step=0.003
    
	   
	call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
    call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Поздравляю с 1000 (bin) марта!"C)
    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0))
	call fglBegin(GL_POINTS)

	a = -1.0
	do while( a < 1.0)
	 b = -1.0
	 do while( b < 1.0)
	  z = (0.0, 0.0)
iter: do i = 0, 75
       z = (z ** 1.5) + cmplx(abs(a), abs(b))
	   if (abs(z) > absmax) then
	    exit iter
	   end if
	  end do iter
	  sec=0
      k=0
      do while (((i-mod(i, 6))/6) > 0)
       sec=sec+(mod(i, 6)*(10**k))
       i=((i-mod(i, 6))/6)
       k=k+1
      end do
      sec=sec+(mod(i, 6)*(10**k))  
      r=((sec-mod(sec,100))/100)
	  g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
	  bl=(mod(sec, 10))
	  call fglColor3f((r/6),(g/6),(bl/6))
	  call fglVertex2f(a, b)
	  b = b + step
	 end do
	 a = a + step
	end do
	call fglEnd()
    call fglFlush()
    call fglClear(GL_COLOR_BUFFER_BIT)
	call sleep(5000)
end program fract