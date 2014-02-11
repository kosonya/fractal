!  Mranimation.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MranimationSub()  - Callback routine for the main dialog box
!   MranimationApply()- Callback routine for the APPLY button
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
    use MranimationGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external MranimationSub
    external MranimationApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_MRANIMATION_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_MRANIMATION_DIALOG, MranimationSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, MranimationApply)
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

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application Mranimation"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: MranimationSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE MranimationSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: MranimationSub

  use user32
  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE MranimationSub

!****************************************************************************
!
!  FUNCTION: MranimationApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE MranimationApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: MranimationApply

  use dflogm
  use dfcom
  use dfauto
  use dfwin
  use dfopngl
  external red
  external green
  external blue
  include "resource.fd"

  type (dialog) dlg
  integer id, callbacktype
  logical(8) :: retlog
  character(256) :: value
  integer(4) :: ret
  real :: rez, imz, xmin, xmax, ymin, ymax, zmax, zmaxmin, zmaxmax, zmaxstep
  integer :: colors, dimm
  real :: a, b, r, g, bl, stepy, stepx
  integer :: i, k, sec
  complex :: z, c

  if (callbacktype == dlg_clicked) then
	retlog=dlgget(dlg, IDC_EDIT3, value)
	read(value, *) xmin
	retlog=dlgget(dlg, IDC_EDIT4, value)
	read(value, *) xmax
	retlog=dlgget(dlg, IDC_EDIT5, value)
    read(value, *) ymin
	retlog=dlgget(dlg, IDC_EDIT6, value)
	read(value, *) ymax
	retlog=dlgget(dlg, IDC_EDIT7, value)
	read(value, *) zmaxmin
	retlog=dlgget(dlg, IDC_EDIT8, value)
	read(value, *) zmaxmax
	retlog=dlgget(dlg, IDC_EDIT9, value)
	read(value, *) zmaxstep
	retlog=dlgget(dlg, IDC_EDIT10, value)
	read(value, *) colors
	retlog=dlgget(dlg, IDC_EDIT11, value)
    read(value, *) dimm

    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
	call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Рандомизированный фрактал Мандельброта"C)
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
!	   sec=0
!       k=0
!       do while (((i-mod(i, colors))/real(colors)) > 0)
!        sec=sec+(mod(i, colors)*(10**k))
!        i=((i-mod(i, colors))/real(colors))
!        k=k+1
!       end do
!       sec=sec+(mod(i, colors)*(10**k))  
!	   sec=mod(sec, 1000)
!       r=((sec-mod(sec,100))/100)
!	   g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
!	   bl=(mod(sec, 10))
!	   call fglColor3f((r/real(colors)),(g/real(colors)),(bl/real(colors)))
call fglColor3f(red((i*(700.0/(3**colors)))+400.0), green((i*(700.0/(3**colors)))+400.0), blue((i*(700.0/(3**colors)))+400.0))

       call fglVertex2f(a, b)
	   b = b + stepx
	  end do
	  a = a + stepy
	 end do
     zmax = zmax + zmaxstep
	end do 
	
  endif

  END SUBROUTINE MranimationApply

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


