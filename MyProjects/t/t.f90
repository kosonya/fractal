program t
  USE DFLIB
  INTEGER :: d, i
 ! integer :: um, re, mi, fa, sol, la, si
  !frequency = 261
  d  = 190
  
!  um= 261  !261,6, вообще-то, это нота до
!  re= 294  !294,7
!  mi= 329  !329,6
!  fa= 349  !349,2
!  sol=392  !392
!  la= 440  !440, ля первой октавы, скрипичный ключ однако
!  si= 494  !494
 
!do i = -10, 2, 2
!call BEEPQQ(f(i), d)
 !print*, f(i)
!end do

!call BEEPQQ(f(4), 3*d)
!call BEEPQQ(f(-6), d)
!call BEEPQQ(f(-6), 4*d)
!call BEEPQQ(f(-6), 6*d)
!call BEEPQQ(f(2), d)
call BEEPQQ(f(-10), d*16)
!call BEEPQQ(f(2), 3*d)
!call BEEPQQ(f(-7), d)
!call BEEPQQ(f(-7), 4*d)

!print*, note("um#")
!print*, note("re")
!print*, note("mi")
!print*, note("fa")
!print*, note("sol")
!!print*, note("la")
!print*, note("si")


print*, f(-36)

 
  !CALL BEEPQQ(frequency, duration)
!  CALL BEEPQQ(um, d)
!  CALL BEEPQQ(re, d)
!  CALL BEEPQQ(mi, d)
!  CALL BEEPQQ(fa, d)
!  CALL BEEPQQ(sol, d)
!  CALL BEEPQQ(la, d)
!  CALL BEEPQQ(si, d)
!CALL BEEPQQ(note("um"), d)
!CALL BEEPQQ(note("re"), d)
!CALL BEEPQQ(note("mi"), d)
!CALL BEEPQQ(note("fa"), d)
!CALL BEEPQQ(note("sol"), d)
!CALL BEEPQQ(note("la"), d)
!CALL BEEPQQ(note("si"), d)
!print*, note("la")
!print*, f(-10)
contains 
integer function f(x) !f - частота, x - чило полутонов от ля первой октавы
 integer :: x
 integer :: camerton
 camerton = 440
 f = (real(camerton) * (2**(real(x)/12)))
end function f


integer function note(no)
 character(4) :: no
 select case (no)
  case("um")
  note=f(-10)
  case("um#")
  note=f(-9)
  case("re")
  note=f(-8)
  case("re#")
  note=f(-7)
  case("mi")
  note=f(-6)
  case("mi#")
  note=f(-5)
  case("fa")
  note=f(-4)
  case("fa#")
  note=f(-3)
  case("sol")
  note=f(-2)
  case("sol#")
  note=f(-1)
  case("la")
  note=f(0)
  case("la#")
  note=f(1)
  case("si")
  note=f(2)
 end select

end function note





end program t