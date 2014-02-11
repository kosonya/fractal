subroutine array(a, imin, imax, jmin, jmax, amax, amin, color)
 use dfopngl
 use dfwin
 implicit none
 integer :: imin, imax, jmin, jmax
 real, dimension(imin:imax, jmin:jmax) :: a
 real :: amax, amin
 integer :: color
 
 integer :: colors
 integer :: i, j
 integer :: k, l
 integer :: sec, n
 real :: num, x, y
 real :: r, g, b
 integer(4) :: ret
 integer(2)      pattern
 colors=int(log(real(color))/log(3.0))



 do i=imin, imax
  do j=jmin, jmax
   num=a(i, j)-amin  
   n=int(num*color)
   
   x=real(i-imin)/real(imax-imin)
   y=real(j-jmin)/real(jmax-jmin)

   sec=0
   k=0
   do while (((n-mod(n, colors))/real(colors)) > 0)
    sec=sec+(mod(n, colors)*(10**k))
    n=((n-mod(i, colors))/real(colors))
    k=k+1
   end do
   sec=sec+(mod(n, colors)*(10**k))  
   sec=mod(sec, 1000)
   r=((sec-mod(sec,100))/100)
   g=((sec-mod(sec, 10)-(sec-mod(sec,100)))/10)
   b=(mod(sec, 10))
   call fglColor3f((r/real(colors)),(g/real(colors)),(b/real(colors)))
   call fglVertex2f(x, y)
  end do
 end do

end subroutine array
