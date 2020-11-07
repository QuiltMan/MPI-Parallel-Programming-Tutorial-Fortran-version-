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
    
    call MPI_SEND(data, 1, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
    call MPI_Recv(data, 1, MPI_INTEGER, p-1, tag, MPI_COMM_WORLD, status, ierr)
  
  else
    source=my_rank-1
    call MPI_Recv(data, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, ierr)
    dest =my_rank+1
    
    if(dest .eq. p) then
      dest=0
      data=data+1
      call MPI_SEND(data, 1, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
    end if
    
    if(my_rank .eq. 0) then
      if(data .eq. 0) then
        print *, "Successfully Token-Ring MEssage-Passing with P=", p
      else
        print *, "Sorry, Token-Ring MEssage-Passing with P=", p, "Failed"
      end if
    end if
  end if

  call MPI_FINALIZE(ierr)

  stop
end program token_ring
