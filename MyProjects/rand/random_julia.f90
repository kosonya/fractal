program ran
use dfopngl
use dfwin
use dflib

integer(4)      ret
integer(2)      pattern
  real :: rez, imz, xmin, xmax, ymin, ymax, zmax, zmaxmin, zmaxmax, zmaxstep
  integer :: colors, dimm
  real :: a, b, r, g, bl, stepy, stepx
  integer :: i, k, sec
  complex :: z, c

xmin = -2.0
xmax = 2.0
ymin = -2.0
ymax = 2.0
zmaxmin = 0.0
zmaxmax = 5.0
zmaxstep = 0.03
colors = 3
dimm = 500


    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
	call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Множество Мандельброта"C)
    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(xmin), DBLE(xmax), DBLE(ymin), DBLE(ymax), DBLE(-1.0), DBLE(1.0))
	call fglBegin(GL_POINTS)
	
	stepx=(abs(xmax-xmin))/(real(dimm))
	stepy=(abs(ymax-ymin))/(real(dimm))
    
    zmax = zmaxmin
    do while (zmax < zmaxmax)
     a = xmin
	 do while( a < xmax)
	  b = ymin
	  do while( b < ymax)
	   c = cmplx(a, b)
	   call random(rez)
	   call random(imz)
	   z=cmplx(rez, imz)
 iter: do i = 0, (3**colors)
        z = (z * z) + c
	    if (abs(z) > zmax) then
	     exit iter
	    end if
	   end do iter
	   sec=0
       k=0
       do while (((i-mod(i, colors))/real(colors)) > 0)
        sec=sec+(mod(i, colors)*(10**k))
        i=((i-mod(i, colors))/real(colors))
        k=k+1
       end do
       sec=sec+(mod(i, colors)*(10**k))  
	   sec=mod(sec, 1000)
       r=((sec-mod(sec,100))/100)
	   g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
	   bl=(mod(sec, 10))
	   call fglColor3f((r/real(colors)),(g/real(colors)),(bl/real(colors)))
       call fglVertex2f(a, b)
	   b = b + stepx
	  end do
	  a = a + stepy
	 end do
     zmax = zmax + zmaxstep
	end do
	



	call sleep(2000)
	
end program ran



