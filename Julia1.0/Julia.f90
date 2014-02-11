!  Julia.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   JuliaSub()  - Callback routine for the main dialog box
!   JuliaApply()- Callback routine for the APPLY button
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
    use JuliaGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external JuliaSub
    external JuliaApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL
    call COMINITIALIZE(ret)

    lret = DlgInit(IDD_JULIA_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_JULIA_DIALOG, JuliaSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, JuliaApply)
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

    ret = MessageBox(ghwndMain, "Error initializing application Julia"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: JuliaSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE JuliaSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: JuliaSub

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

  END SUBROUTINE JuliaSub

!****************************************************************************
!
!  FUNCTION: JuliaApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************



SUBROUTINE JuliaApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: JuliaApply

  use dflogm
  use dfcom
  use dfauto
  use dfwin
  use dfopngl

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  character(256) :: value
  logical(4) :: retlog
  real :: rec, imc, cmin, cmax, absmax, step
  integer(4)      ret
  integer(2)      pattern
  real :: a, b, r, g, bl
  integer :: i, k, sec
  complex :: z

  if (callbacktype == dlg_clicked) then
    retlog=dlgget(dlg, 1006, value)
	read(value, *) rec
    retlog=dlgget(dlg, 1007, value)
	read(value, *) imc
	retlog=dlgget(dlg, 1008, value)
	read(value, *) cmin
	retlog=dlgget(dlg, 1009, value)
	read(value, *) cmax
	retlog=dlgget(dlg, 1010, value)
	read(value, *) absmax
	retlog=dlgget(dlg, 1011, value)
	read(value, *) step
	call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
    call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Множество Жюлиа"C)
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
	  z = cmplx(a, b)
iter: do i = 0, 216
       z = (z * z) + cmplx(rec, imc)
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

	 




  endif

  END SUBROUTINE JuliaApply




