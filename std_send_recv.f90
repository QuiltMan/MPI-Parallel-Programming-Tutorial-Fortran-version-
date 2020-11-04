program std_send_recv
	use mpi

	implicit none
	
	integer i, ierr, myid, numprocs, other
	integer status(MPI_STATUS_SIZE);
	integer,allocatable,dimension(:) :: sb, rb
	integer,parameter :: buf_size = 10
	
	allocate(sb(buf_size), rb(buf_size))
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

	do i = 1,buf_size
		sb(i)=myid+i
	enddo
		
	if(myid == 0) then
		other=1
	end if
	if(myid == 1) then 
		other=0
	end if
	
	if(myid == 0) then
		write (*,100) myid, numprocs
		call MPI_SEND(sb, buf_size, MPI_INTEGER, other, 1, MPI_COMM_WORLD,ierr)
		write (*,101) myid, numprocs
		call MPI_RECV(rb,buf_size,MPI_INTEGER, other, 1, MPI_COMM_WORLD,status,ierr)
		
	end if

	if(myid == 1) then
		write (*,101) myid, numprocs
		call MPI_RECV(rb,buf_size,MPI_INTEGER, other, 1, MPI_COMM_WORLD,status,ierr)
		write (*,100) myid, numprocs
		call MPI_SEND(sb, buf_size, MPI_INTEGER, other, 1, MPI_COMM_WORLD,ierr)
	end if


	write (*,102) myid, numprocs
	
	
	100 FORMAT ('process : ', I6 ,'of', I6 , 'trying sending...')
	101 FORMAT ('process : ', I6 ,'of', I6 , 'trying receiving...')
	102 FORMAT ('Hello World by fortran! Process', I6 ,'of', I6)
	
	call MPI_FINALIZE(ierr)
	
	deallocate(sb, rb)
	stop
	
end program std_send_recv
