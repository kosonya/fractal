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
    use JuliaGlobals


    implicit none
    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external JuliaSub
    external JuliaApply
	external JuliaHelp

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret




    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_JULIA_DIALOG, gdlg)
	if (lret == .FALSE.) goto 99999
	lret = DlgSetSub(gdlg, IDD_JULIA_DIALOG, JuliaSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, JuliaApply)
    lret = DlgSetSub(gdlg, IDC_BUTTON2, JuliaHelp)
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

    ret = MessageBox(ghwndMain, "Error initializing application Julia"C, &
                     "Error"C, MB_OK)
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
    external red
  external green
  external blue
  include "resource.fd"


  

  

  type (dialog) dlg
  integer id, callbacktype
  logical(8) :: retlog
  character(256) :: value
  integer(4) :: ret
  real :: rec, imc, xmin, xmax, ymin, ymax, zmax
  integer :: colors, dimm
  logical :: grid, delay
  real :: a, b, r, g, bl, stepy, stepx, sgx, sgy
  integer :: i, k, sec
  complex :: z


  if (callbacktype == dlg_clicked) then
    retlog=dlgget(dlg, IDC_EDIT1, value)
	read(value, *) rec 
	retlog=dlgget(dlg, IDC_EDIT2, value)
	read(value, *) imc
	retlog=dlgget(dlg, IDC_EDIT3, value)
	read(value, *) xmin
	retlog=dlgget(dlg, IDC_EDIT4, value)
	read(value, *) xmax
	retlog=dlgget(dlg, IDC_EDIT5, value)
    read(value, *) ymin
	retlog=dlgget(dlg, IDC_EDIT6, value)
	read(value, *) ymax
	retlog=dlgget(dlg, IDC_EDIT7, value)
	read(value, *) colors
	retlog=dlgget(dlg, IDC_EDIT8, value)
	read(value, *) zmax
	retlog=dlgget(dlg, IDC_EDIT9, value)
	read(value, *) dimm
	retlog=dlggetlog(dlg, IDC_CHECK1, grid)

    call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
	call fauxInitPosition (0, 0, 500, 500)
    ret = fauxInitWindow ("Множество Жюлиа"C)
    call fglClearColor (0.0, 0.0, 0.0, 0.0)
    call fglClear(GL_COLOR_BUFFER_BIT)
    call fglColor3f(1.0, 1.0, 1.0)
    call fglMatrixMode (GL_PROJECTION)
    call fglLoadIdentity ()
    call fglOrtho(DBLE(xmin), DBLE(xmax), DBLE(ymin), DBLE(ymax), DBLE(-1.0), DBLE(1.0))
	call fglBegin(GL_POINTS)
	
	stepx=(abs(xmax-xmin))/(real(dimm))
	stepy=(abs(ymax-ymin))/(real(dimm))

	a = xmin
	do while( a < xmax)
	 b = ymin
	 do while( b < ymax)
	  z = cmplx(a, b)
iter: do i = 0, (colors)
      ! z = (z**20)+(z**90)+(z**80)+(z**70)+cmplx(rec, imc)
	   !z = (z ** z) + cmplx(rec, imc)
	   z = (z * z) + cmplx(rec, imc)

	   if (abs(z) > zmax) then
	    exit iter
	   end if
	  end do iter

	   call fglColor3f(red((i*(700.0/(colors)))+400.0), green((i*(700.0/(colors)))+400.0), blue((i*(700.0/(colors)))+400.0))

      call fglVertex2f(a, b)
	  b = b + stepx
	 end do
	 a = a + stepy
	end do
	call fglEnd()
    call fglFlush()

	if (grid) then
	 sgx=((abs(xmax-xmin))/8.0)
	 sgy=((abs(ymax-ymin))/8.0)
	 call fglDisable (GL_LINE_STIPPLE)                        
     call fglLineWidth (1.0)
     call fglBegin(GL_LINES)
     call fglColor3f (1.0, 1.0, 0.0)
     a=xmin
	 do while (a<=xmax)
	  call fglVertex2f(a, ymax)
      call fglVertex2f(a, ymin)
	  a=a+sgx
     end do
	 a=ymin
	 do while (a<=ymax)
      call fglVertex2f(xmax, a)
      call fglVertex2f(xmin, a)
	  a=a+sgy
     end do
	 call fglColor3f (1.0, 0.0, 0.0)
	 call fglVertex2f(xmax, 0)
     call fglVertex2f(xmin, 0)
	 call fglVertex2f(0, ymin)
     call fglVertex2f(0, ymax) 	 
	 call fglEnd()
     call fglFlush() 
	end if
  endif


  END SUBROUTINE JuliaApply

SUBROUTINE JuliaHelp( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: JuliaHelp

  use dflogm
  use dfwina

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  integer(4) :: iret
  if (callbacktype == dlg_clicked) then
    iret=MessageBox(NULL, "&
Ввод чисел. Числа ReC, ImC, X (min и max), Y (min и max), |Z max| являются действиетльными.                      &
Десятичным разделителем является точка, а не запятая. Число в любом случае записывается в                        &
формате [знак]<число>.<число>. Даже если число является целым, то необходимо записать                            &
<целая часть>.0. Числа Цвета и Разрешение являются натуральными.                                                 &                          
Описание чисел. Программа строит множество Жюлиа по формуле Z=Z^2+C. Аттрактор - это число                       &
C. Число ReC - это действительная часть  аттрактора, ImZ - мнимая часть. Эти числа могут быть                    &
любыми. Числа X (min и max) Y (min и max) - это границы, для которых строится множетсво. Маскимальное число      &
итераций - это чило итераций, после которого программа определяет, что число Z не уйдёт на бесконечность. Это    &
число может быть любым натуральным, но при очень больших                                                         &
числах программа работает медленно. |Z max| - это число, которое программа условно считает                       &
бесконечностью. То есть, если функция его превысит, то считается, что она уходит                                 & 
на бесконечность. Разрешение показывает, сколько пикселей должно быть на экране в длину (а так                   &
как экран квадратный - такое же количество и в ширину). Координатная сетка, выводить которую                     &
позволяет соотвествующий флажок, всегда является квадратной, на экране в люом случае размещается ровно           &
восемь линий. Координатная секта жёлтая, красным выделены оси координат.                                         &
Автор - Максим Ковалев (aka Maxikov)&
"C, "Справка"C, IOR(MB_OK, MB_ICONINFORMATION))

  endif

  END SUBROUTINE JuliaHelp



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





