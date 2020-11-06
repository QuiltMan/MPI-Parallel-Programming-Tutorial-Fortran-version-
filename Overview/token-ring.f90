program token_ring
  use mpi
  
  implicit none
  
  integer status(MPI_STATUS_SIZE)
  integer my_rank, p, source, dest, tag, data, ierr
  
  call MPI_INIT(ierr)
  !初始化应用程序，进入MPI系统，并初始化通信器MPI_COMM_WORLD
  !他将包含所有初始启动的进程，给各个进程编号，并允许这些进程之间
  !可以互相通信
  
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
  !从通信器MPI_COMM_WORLD中获取本进程的序号my_rank
  
  call MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)
  !获取通信器MPI_COMM_WORLD包含的进程个数p

  if(p .le. 1)then
    print *, "Error : number of spawned must be larger than 1"
    stop
  end if
  
  tag = 50 !消息号赋值
  
  if(my_rank .eq. 0) then
    data = 1
    dest = 1
    
    call MPI 
  end if

end program token_ring
