!  desire.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   desireSub()  - Callback routine for the main dialog box
!   desireApply()- Callback routine for the APPLY button
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
    use desireGlobals
	use dflib

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external desireSub
    external desireApply


    ! Variables
    type(dialog)            dlg
	type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret
	logical(8)              retlog
    integer    nine



    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_DESIRE_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999

    lret = DlgSetSub(gdlg, IDD_DESIRE_DIALOG, desireSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, desireApply)
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

    ret = MessageBox(ghwndMain, "Error initializing application desire"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: desireSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE desireSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: desireSub

  use user32
  use dflogm
  use dflib

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE desireSub

!****************************************************************************
!
!  FUNCTION: desireApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE desireApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: desireApply

  use dflogm
  use dflib
  

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  integer nine
  real ranval
  integer i
  logical(8) ret


  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
 call random(ranval)
 nine=int(ranval*100) 
 do i= 1007, 1016
 call random(ranval)
  ! ret=dlgset(dlg, i, achar(int(ranval*127)))
  ret=dlgset(dlg, i, achar(int(i/1006)))
end do

  endif

  END SUBROUTINE desireApply




