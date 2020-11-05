program ssendrcv
	use mpi

	implicit none

	
	integer rank, size, i, ierr
	integer status(MPI_STATUS_SIZE);
	integer,allocatable,dimension(:) :: buffer
	integer,parameter :: buf_size = 10
	
	allocate(buffer(buf_size))
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
	
	if(size<2) then
		print *, "Please run with two processes."
		
		deallocate(buffer)
		call MPI_FINALIZE(ierr)
		stop
	end if
	
	if(rank == 0) then
		do i=1,10
			buffer(i)=i
		enddo
		
		print *, "Process", rank, " sending..."		

		call MPI_SSEND(buffer, buf_size, MPI_INTEGER, 1, 123, MPI_COMM_WORLD,ierr)	
	end if
	
	if(rank == 1)then
		do i=1,10
			buffer(i)=-1
		enddo
	
		print *, "Process", rank, " receving..."		
		call MPI_RECV(buffer, buf_size, MPI_INTEGER, 0, 123, MPI_COMM_WORLD,status, ierr)	
	
		do i=1,10
			if(buffer(i) /= i) then
				print *, "Error: buffer[", i, "]=", buffer(i)," but is expected to be", i		
			else
				print *, "Proc ", rank , ": buffer[", i, "]=", buffer(i)," is as expected be", i	
			end if
		enddo
	end if
	
	call MPI_FINALIZE(ierr)
	deallocate(buffer)
	stop
end program ssendrcv
