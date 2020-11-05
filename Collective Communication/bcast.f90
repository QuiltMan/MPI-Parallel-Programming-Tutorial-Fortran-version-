program bcast
	use mpi

	implicit none

	
	integer rank, value, size, ierr
	integer status(MPI_STATUS_SIZE);
	integer,allocatable,dimension(:) :: buffer
	integer,parameter :: buf_size = 10
	
	allocate(buffer(buf_size))
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
	
	do while(value .ge. 0)
		if(rank == 0) then
			print *, "Please enter a value, -1 means finish"		
			read *,value 
			call MPI_BCAST(value,1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
		else 
			call MPI_BCAST(value,1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
			write (*,100) rank, value
		end if
	enddo
	
	call MPI_FINALIZE(ierr)
	deallocate(buffer)
	
	100 FORMAT ('Process', I6, ' got ', I6 )
	
	stop
end program bcast
