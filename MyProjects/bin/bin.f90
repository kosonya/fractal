!  bin.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   binSub()  - Callback routine for the main dialog box
!   binApply()- Callback routine for the APPLY button
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
    use binGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external binSub
    external binApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_BIN_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_BIN_DIALOG, binSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, binApply)
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

    ret = MessageBox(ghwndMain, "Error initializing application bin"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: binSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE binSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: binSub

  use user32
  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE binSub

!****************************************************************************
!
!  FUNCTION: binApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE binApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: binApply

  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  logical(8) :: retlog
  character(256):: val, fname
  

  if (callbacktype == dlg_clicked) then
    retlog=dlgset(dlg, 1006, val)
  endif

  END SUBROUTINE binApply


