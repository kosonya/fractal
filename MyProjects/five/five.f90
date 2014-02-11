program five
implicit none
integer :: inp
read*, inp
inp=mod(inp, 1000)
print*, ((inp-mod(inp,100))/100)
!print*, (div(inp, 100))
!print*, ((inp-(inp-(div(inp, 10)*10))-(div(inp, 100)*100))/10)
print*, ((inp-mod(inp, 10)-(inp-mod(inp,100)))/10)
!print*, (inp-(div(inp, 10)*10))
print*, mod(inp, 10)

contains
integer function div(x, y)
 integer :: x, y
 div=int((x-mod(x,y))/y)
end function div
end program five