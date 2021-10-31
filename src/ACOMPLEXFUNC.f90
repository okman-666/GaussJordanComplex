module ACOMPLEXFUNC
  implicit none
contains
  function  ACFUNC(c)                             !complex function
      complex,ALLOCATABLE :: ACFUNC(:)          
      complex,ALLOCATABLE :: c(:,:)               
      COMPLEX :: sum  = 0                        
      COMPLEX :: zhuyuan                          !设置主元
      INTEGER :: c_hang                         
      INTEGER :: c_lie                           
      INTEGER :: i = 0,j = 0,k = 0                !i是轮数控制（或行控制），j是行控制（或行控制），k是列控制（或列控制）            
      c_hang = size(c,1)
      c_lie = size(c,2)
      ALLOCATE(ACFUNC(c_hang))
      ACFUNC = (0,0)
      !>1 进行第1次处理（预处理）
      !>1.1 首先进行第1次归一化
      do i = 1, c_hang, 1
        zhuyuan = c(i,1)
        do j = 1, c_lie,1
            c(i,j) = c(i,j)/zhuyuan
        end do        
      end do
      !>1.2 进行第1次处理的消去
      do i = 2, c_hang, 1                       !i是行控制
        do j = 1, c_lie, 1                      !j是列控制
          c(i,j) = c(i,j) - c(1,j)
        end do        
      end do

      !>2 进行主体消去
      !>2.1 控制消去的轮数
      !>2.2 开始进行归一化与消去
      do i = 2, c_hang-1, 1
      !>2.2.1 开始进行下归一化
        do j = i, c_hang, 1
          zhuyuan = c(j,i)
          do k = i, c_lie, 1
            c(j,k) = c(j,k)/zhuyuan              
          end do
          ! call outputMatrix(c)          
        end do
      !>2.2.2 开始下消去
        do j = i+1, c_hang, 1
          do k = i, c_lie, 1
            c(j,k) = c(j,k) - c(i,k)
          end do          
        end do
      !>2.2.3 开始上归一化
        do j = i, 1, -1
          zhuyuan = c(j,i)
          do k = 1,c_lie, 1
            c(j,k) = c(j,k)/zhuyuan
          end do          
        end do
      !>2.2.4 开始上消去
          do j = i-1, 1, -1
            do k = 1, c_lie, 1
              c(j,k) = c(j,k)-c(i,k)
            end do            
          end do
      end do

      !>3 开始残体处理
      !>3.1 开始残体上归一化
      do j= c_hang, 1, -1
        zhuyuan = c(j,c_hang)
        do k = 1, c_lie, 1
          c(j,k) = c(j,k)/zhuyuan
        end do        
      end do

      do j = 1, c_hang-1, 1
        do k = 1, c_lie, 1
          c(j,k) = c(j,k) - c(c_hang,c_hang)
        end do        
      end do
      
      call outputMatrix(c)


      !>开始下部残体消去
      ! do i = , end 
        
      ! end do


      ! call outputMatrix(c)                      !模块内部相互引用不需要在文件的一开头写use xxx

      ! READ(*,*)
      
      
    !   do i = 1, c_hang - 1
    !       do  j= i, c_hang
    !           zhuyuan  = c(j,i)
    !           do k = i, c_lie
    !               c(j,k) = c(j,k)/zhuyuan
    !           end do
    !       end do
    !       do j = i+1, c_hang
    !           do k = i, c_lie
    !               c(j,k) = c(j,k) - c(i,k)
    !           end do
    !       end do  
    !   end do
    !   ACFUNC(c_hang) = c(c_hang,c_lie)/c(c_hang,c_lie-1)

    !   do j = c_hang-1, 1, -1
    !       sum = c(j,c_lie)
    !       do k = c_lie - 1, j+1, -1
    !           sum = sum - ACFUNC(k)*c(j,k)
    !       end do
    !       ACFUNC(j) = sum
    !       sum = 0
    !   end do
    ! WRITE(*,*)    
  end function

  subroutine outputMatrix(c)                !该模块的作用是把矩阵输出到文件
    complex, ALLOCATABLE :: c(:,:)
    complex, ALLOCATABLE :: c1(:,:)
    LOGICAL :: alive
    INTEGER :: c1_hang,c1_lie                   !implicit none 的情况下定义必须在正式语句的的前面
    CHARACTER(len = 100) :: filename = 'E:\1SoftwareData\fortran\Fortran2\GaussJordanComplex\mo.txt'  !MatrixOutput文件名的简写
    INTEGER :: i = 0

    OPEN(unit = 10, file = filename)              
    
    c1 = c
    c1_hang = size(c1,1)
    c1_lie = size(c1,2)
    !>1. 开始文件操作
    INQUIRE(file = filename,exist = alive)

    ! WRITE(unit = 10,fmt = *) "123"
    if ( alive )  then 
      do i = 1, c1_hang, 1
        WRITE(10,*) c1(i,:)                   !这里的unit与fmt不太能省,省了文件有可能输出不了,*****这里输出的特别有特点
        ! WRITE(unit = 10,fmt = "(/)")          !这里进行换行，关了换行就超场宽了，就输出不了了 
        ! WRITE(*,*) "123"
      end do
    else 
      WRITE(*,*) filename,"don't exist."
    end if 

    
  end subroutine
end module
