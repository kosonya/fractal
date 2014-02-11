!  harmony.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   harmonySub()  - Callback routine for the main dialog box
!   harmonyApply()- Callback routine for the APPLY button
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
    use harmonyGlobals
	USE DFLIB

    implicit none
    type (dialog) dlg
    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow
	integer ::  value

    include 'resource.fd'

    external harmonySub
    external harmonyApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_HARMONY_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_HARMONY_DIALOG, harmonySub)
    lret = DlgSetSub(gdlg, IDM_APPLY, harmonyApply)
    lret = DlgModeless(gdlg, nCmdShow)
    if (lret == .FALSE.) goto 99999

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
      if ( DlgIsDlgMessage(mesg) .EQV. .FALSE. ) then
2           lret = TranslateMessage( mesg )
           ret  = DispatchMessage( mesg )
		   
	       lret=dlggetint(dlg, IDC_EDIT1, value)
		   if (value/=0) then
		    lret=MessageBox(NULL, "Я те щас понажимаю на кнопки!!!!! А ну руки убери!!! Делать что-ли нечего!!!!!????? Иди отсюда!!!!! И не трогай высший разум!!!!!!"C, "ИДИ НА!!!!!!"C, IOR(MB_OK, MB_ICONERROR))
           else 
		   ! call sleepqq(5000)

		    
			go to 2
		   end if
       
	   
	   
	   end if
    end do
    call DlgUninit(gdlg)

    WinMain = mesg.wParam
    return

99999 &

    ret = MessageBox(ghwndMain, "Error initializing application harmony"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: harmonySub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE harmonySub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: harmonySub

  use user32
  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE harmonySub

!****************************************************************************
!
!  FUNCTION: harmonyApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE harmonyApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: harmonyApply

  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
  endif

  END SUBROUTINE harmonyApply


