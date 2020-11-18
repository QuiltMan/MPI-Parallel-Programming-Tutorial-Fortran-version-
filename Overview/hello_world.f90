program main
	use mpi
	
	implicit none
	
	character *(MPI_MAX_PROCESSOR_NAME) processor_name
	integer myid, numprocs, namelen, rc, ierr
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs,ierr)
	call MPI_GET_PROCESSOR_NAME(processor_name, namelen, ierr)
	
	write(*,100) myid,numprocs,processor_name
	
	call MPI_FINALIZE(ierr)
	stop

100 FORMAT('Hello World! Process,', I2, ' of ', I1, ' on ', 20A)
end program main
