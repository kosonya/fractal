!******************************************************************************
!*       This is a part of the Compaq Visual Fortran Source Code Samples.
!******************************************************************************
!/*
! *  simple.f90
! */
use dfopngl
use dfwin

integer(4)      ret
integer(2)      pattern

    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
    call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Simple OpengGL Sample"C)

    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0), DBLE(-1.0), DBLE(1.0))


     call fglColor3f (3.0, 2.0, 1.0)
	call fglBegin(GL_POLYGON)
        call fglVertex2f(-0.25, -0.25)
        call fglVertex2f(-0.25, 0.5)
        call fglVertex2f(0.5, 0.5)
        call fglVertex2f(0.5, -0.25)
    call fglEnd()
    call fglFlush()
    call sleep (2000)
    call fglClear(GL_COLOR_BUFFER_BIT)

    ! Triangles
   

	
end
