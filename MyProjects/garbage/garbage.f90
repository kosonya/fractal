!  garbage.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   garbageSub()  - Callback routine for the main dialog box
!   garbageApply()- Callback routine for the APPLY button
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
    use garbageGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external garbageSub
    external garbageApply
	external garbageButton

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL
    call COMINITIALIZE(ret)

    lret = DlgInit(IDD_GARBAGE_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_GARBAGE_DIALOG, garbageSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, garbageApply)
	lret = DlgSetSub(gdlg, IDC_BUTTON1, garbageButton)
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

    ret = MessageBox(ghwndMain, "Error initializing application garbage"C, &
                     "Error"C, MB_OK)
    call COMUNINITIALIZE()
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: garbageSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE garbageSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: garbageSub

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

  END SUBROUTINE garbageSub

!****************************************************************************
!
!  FUNCTION: garbageApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE garbageApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: garbageApply

  use dflogm
  use dfcom
  use dfauto
  use dfwina

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  logical(4) :: iret

  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
    iret = MessageBox(NULL, "11"C, "история"C, IOR(MB_OK, MB_ICONINFORMATION))
  endif

  END SUBROUTINE garbageApply

SUBROUTINE garbageButton( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: garbageButton

  use dflogm
  use dfcom
  use dfauto
  use dfwina

  implicit none

  type (dialog) dlg
  integer id, callbacktype
  logical(4) :: iret
  character(10)::a

  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
	a = char(123)
    iret = MessageBox(NULL, a, "Рассказ"C, IOR(MB_OK, MB_ICONINFORMATION))
  endif

 END SUBROUTINE garbageButton


