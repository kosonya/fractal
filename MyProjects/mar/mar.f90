!  mar.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   marSub()  - Callback routine for the main dialog box
!   marApply()- Callback routine for the APPLY button
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
    use marGlobals

    implicit none

    integer*4 hInstance
    integer*4 hPrevInstance
    integer*4 lpszCmdLine
    integer*4 nCmdShow

    include 'resource.fd'

    external marSub
    external marApply

    ! Variables
    type (T_MSG)            mesg
    integer*4               ret
    logical*4               lret


    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lret = DlgInit(IDD_MAR_DIALOG, gdlg)
    if (lret == .FALSE.) goto 99999
    lret = DlgSetSub(gdlg, IDD_MAR_DIALOG, marSub)
    lret = DlgSetSub(gdlg, IDM_APPLY, marApply)
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

    ret = MessageBox(ghwndMain, "Error initializing application mar"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: marSub ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for initialization and destroy
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE marSub( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: marSub

  use user32
  use dflogm

  implicit none

  type (dialog) dlg
  integer id, callbacktype

  if (callbacktype == dlg_destroy) then
    call PostQuitMessage(0)
  endif

  END SUBROUTINE marSub

!****************************************************************************
!
!  FUNCTION: marApply ( dlg, id, callbacktype )
!
!  PURPOSE:  Dialog box callback for APPLY button
!
!  COMMENTS:
!
!****************************************************************************

SUBROUTINE marApply( dlg, id, callbacktype )
!DEC$ ATTRIBUTES DEFAULT :: marApply

  use dflogm
  use dflib
  use dfwin

  implicit none 

  type (dialog) dlg
  integer id, callbacktype
  logical(4) :: res
  character(256) :: name, family 
  integer(2) :: iyr, imon, iday
  integer(4) :: iret
  integer :: yesno
  if (callbacktype == dlg_clicked) then
    ! TO DO; Add your APPLY logic here
	CALL GETDAT (iyr, imon, iday)

	if ((imon /= 3).or.(iday /= 8)) then
	 iret=MessageBox(NULL, "Сегодня не восьмое марта."C, "Извините"C, IOR(MB_OK, MB_ICONERROR))
      else	
	   res=dlggetchar(dlg, 1006, name)
	   res=dlggetchar(dlg, 1007, family)
	   if ((name /= "Елена").or.(family /= "Кручинина")) then
	    iret=MessageBox(NULL, "0312x324 Нераспознаваемый символ"C, "Ошибка ввода"C, IOR(MB_OK, MB_ICONERROR))
	   else
	   iret=MessageBox(NULL, " ЛенчеГ КручинчеГ!                                "C, " =))))) "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " Поздравляю тебя с восьмым марта!                 "C, " =))))) "C, IOR(MB_OK, MB_ICONINFORMATION))
       iret=MessageBox(NULL, " Ты неотразима!                                   "C, " 8-)    "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " Ты прекрасна!                                    "C, " О да!  "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " Ты самая-самая!                                  "C, " САМАЯ  "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " Желаю тебе всегда оставаться такой же прекрасной "C, " ВСЕГДА "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " Всегда радуйся жизни                             "C, " %-)))) "C, IOR(MB_OK, MB_ICONINFORMATION))
       iret=MessageBox(NULL, " Чтобы твоя любовь была только счастливой         "C, " ЛЮБВИ  "C, IOR(MB_OK, MB_ICONINFORMATION))
	   iret=MessageBox(NULL, " С восьмым марта тебя, Лена!                            "C, " 8      "C, IOR(MB_OK, MB_ICONINFORMATION))
      end if
	 
	end if
  endif

  END SUBROUTINE marApply


