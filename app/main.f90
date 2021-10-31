program acomplextest
  use ACOMPLEXFUNC
  implicit none
  COMPLEX,ALLOCATABLE :: a(:,:),b(:,:),c(:,:),res(:)
  ! ALLOCATE(a(2,2))
  !>输入原始数据
  ALLOCATE(a(4,4)) 
  a = reshape([(1,3),(7,-2),(9,9),(-2,-2),&
          &(3,-2),(2,7),(15,-3),(-2,-2),&
          &(2,1),(1,5),(3,15),(11,7),&
          &(13,6),(-2,8),(-2,1),(5,6)],(/4,4/))      
  b = reshape([(2,1),(7,2),(3,-2),(9,3),&
              &(1,2),(2,7),(-2,3),(3,9)],(/4,2/)) 
  ALLOCATE(c(size(a,1),size(b,2)+size(a,2)))                !这里如果不用allocate，矩阵的维度会发生奇怪的改变
  c = reshape([a,b],(/size(a,1),size(b,2)+size(a,2)/))                         
  res = ACFUNC(c)
  WRITE(*,*) res
  ! READ(*,*)

STOP
end program acomplextest
