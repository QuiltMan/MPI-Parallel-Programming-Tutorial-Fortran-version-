program scatter
	use mpi

	implicit none

	
	integer i,j,rank, numtasks, scnt, rcnt, src, ierr
	integer status(MPI_STATUS_SIZE);
	integer,allocatable,dimension(:,:) :: sb
	integer,allocatable,dimension(:) :: rb
	integer,parameter :: buf_size = 4
	
	allocate(rb(buf_size), sb(buf_size,buf_size))
	
	do i=1,buf_size
		do j=1,buf_size
			sb(i,j)=i + (j-1)*buf_size
		enddo
	enddo
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
	
	if(numtasks == buf_size) then
		src = 1
		scnt = buf_size
		rcnt = buf_size
		
		call MPI_SCATTER(sb, scnt, MPI_INTEGER, rb, rcnt, MPI_INTEGER, src, MPI_COMM_WORLD, ierr)
		
		write (*, 100) rank, rb(1), rb(2), rb(3), rb(4)
		
	else
		print *, "Must specify ", buf_size , " processors. Terminating."
	end if
	
	call MPI_FINALIZE(ierr)
	deallocate(rb,sb)
	
	100 FORMAT ('rank = ', I6, ', Results: ', I6, I6, I6, I6)
	stop
end program scatter
