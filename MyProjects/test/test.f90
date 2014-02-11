!  test.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   testSub()  - Callback routine for the main dialog box
!   testApply()- Callback routine for the APPLY button
!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

integer*4 function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'WinMain' :: WinMain
!DEC$ ENDIF

    use user32
    use kernel32
    use dflogm
    use dfcom
    use dfauto
    use testGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external testSub
    external testApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL
    call COMINITIALIZE(ret)

    lret = DlgInit(IDD_TEST_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_TEST_DIALOG, testSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, testApply)
    lret = DlgModeless(gdlg, nCmdShow)
    if (lret == .FALSE.) goto 99999

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
       if ( DlgIsDlgMessage(mesg) .EQV. .FALSE. ) then
           lret = TranslateMessage( mesg )
           ret  = DispatchMessage( mesg )
       end if
    end do
    call DlgUninit(gdlg)
    call COMUNINITIALIZE()

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application test"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: testSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE testSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: testSub

  use user32
  use dflogm
  use dfcom
  use dfauto

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE testSub

!****************************************************************************
!
!  FUNCTION: testApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE testApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: testApply

  use dflogm
  use dfcom
  use dfauto
  use dfwin
  use dfopngl
  external red
  external green
  external blue



  type (dialog) dlg
  integer id, callbacktype
  character(256) :: value
  logical(4) :: retlog
  real :: rez, imz, cmin, cmax, absmax, step
  integer(4)      ret
  integer(2)      pattern
  real :: a, b, r, g, bl
  integer :: i, k, sec
  complex :: z

    if (callbacktype == dlg_clicked) then
    retlog=dlgget(dlg, 1006, value)
	read(value, *) rez
    retlog=dlgget(dlg, 1008, value)
	read(value, *) imz
	retlog=dlgget(dlg, 1009, value)
	read(value, *) cmin
	retlog=dlgget(dlg, 1010, value)
	read(value, *) cmax
	retlog=dlgget(dlg, 1012, value)
	read(value, *) absmax
	retlog=dlgget(dlg, 1013, value)
	read(value, *) step

	   
	call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
    call fauxInitPosition (0, 0, 650, 650)
    ret = fauxInitWindow ("Фрактал"C)
    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(cmin), DBLE(cmax), DBLE(cmin), DBLE(cmax), DBLE(cmin), DBLE(cmax))
	call fglBegin(GL_POINTS)

	a = cmin
	do while( a < cmax)
	 b = cmin
	 do while( b < cmax)
	  z = (0.0, 0.0)
iter: do i = 0, 75
       !z = (z ** z) + cmplx(a, b)
	   z = (z * z) + cmplx(a, b)
	   !z = (z ** 3) + (cmplx(a, b) * cmplx(a, b)) - (cmplx(a, b) * z)
	   if (abs(z) > absmax) then
	    exit iter
	   end if
	  end do iter
!	  sec=0
!      k=0
!      do while (((i-mod(i, 6))/6) > 0)
!       sec=sec+(mod(i, 6)*(10**k))
!       i=((i-mod(i, 6))/6)
!       k=k+1
!      end do
!      sec=sec+(mod(i, 6)*(10**k))  
!      r=((sec-mod(sec,100))/100)
!	  g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
!	  bl=(mod(sec, 10))
	 ! call fglColor3f((r/6),(g/6),(bl/6))
	   call fglColor3f(red((i*3)+400.0), green((i*3)+400.0), blue((i*3)+400.0))
	  call fglVertex2f(a, b)
	  b = b + step
	 end do
	 a = a + step
	end do
	call fglEnd()
    call fglFlush()
    call fglClear(GL_COLOR_BUFFER_BIT)
    endif

END SUBROUTINE testApply

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
