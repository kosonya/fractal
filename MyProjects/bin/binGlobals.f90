!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module binGlobals
use dflogm

implicit none

!  Parameters

integer*4, parameter, public :: SIZEOFAPPNAME = 100

!  Global data

integer		ghInstance
integer		ghModule
integer		ghwndMain
type (dialog) gdlg


end module
