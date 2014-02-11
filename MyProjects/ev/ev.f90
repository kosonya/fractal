!  ev.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   evSub()  - Callback routine for the main dialog box
!   evApply()- Callback routine for the APPLY button
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
    use evGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external evSub
    external evApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_EV_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_EV_DIALOG, evSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, evApply)
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

    ret = MessageBox(ghwndMain, "Error initializing application ev"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: evSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE evSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: evSub

  use user32
  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE evSub

!****************************************************************************
!
!  FUNCTION: evApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE evApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: evApply

  use dflogm
    use user32
    use kernel32
    use dflogm
    use evGlobals
	use dfwin

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  integer*4 :: iret

  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
	iret=MessageBox(NULL, "Your computer will now kill himself, sorry."C, "Evanescence"C, IOR(MB_OK, MB_ICONERROR))
  endif

  END SUBROUTINE evApply


