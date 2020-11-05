program bsendrecv
	use mpi

	implicit none
		
	integer, parameter :: msg1len = 7, msg2len = 2, msg3len = 17
	integer, parameter :: rmsg1len = 64, rmsg2len = 64, rmsg3len = 64
	integer status(MPI_STATUS_SIZE);
	integer :: dest = 1, src = 0, tag = 1, errs = 0
	integer rank, bufsize, bsize
	integer s1, s2, s3, ierr
	
	
	character(len=7) :: msg1 = '012345'
	character(len=17) :: msg3 = '0123401234012341' 
	character(len=64) :: rmsg1, rmsg3
	
	character, allocatable, dimension(:) :: buf, bbuf 
	real, allocatable, dimension(:) :: msg2, rmsg2
	
	allocate(msg2(2), rmsg2(64))
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	
	call MPI_PACK_SIZE(7, MPI_CHARACTER, MPI_COMM_WORLD, s1, ierr)
	call MPI_PACK_SIZE(2, MPI_REAL, MPI_COMM_WORLD, s2, ierr)
	call MPI_PACK_SIZE(17, MPI_CHARACTER, MPI_COMM_WORLD, s3, ierr)
	
	bufsize = 3 * MPI_BSEND_OVERHEAD + s1 + s2 +s3

	allocate(buf(bufsize))
	
	call MPI_BUFFER_ATTACH(buf, bufsize, ierr)
	
	msg2(1) = 1.23
	msg2(2) = 3.21
	
	if(rank == src) then
		call MPI_Bsend(msg1, 7, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
		call MPI_Bsend(msg2, 2, MPI_REAL, dest, tag, MPI_COMM_WORLD, ierr)
		call MPI_Bsend(msg3, 17, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
	end if
	
	if(rank == dest) then
		call MPI_RECV(rmsg1, 7, MPI_CHARACTER, src, tag, MPI_COMM_WORLD, status, ierr)
		write (*,100) rmsg1, msg1
		call MPI_RECV(rmsg2, 2, MPI_REAL, src, tag, MPI_COMM_WORLD, status, ierr)
		write (*,101) rmsg2(1), msg2(1)
		write (*,101) rmsg2(2), msg2(2)
		call MPI_RECV(rmsg3, 17, MPI_CHARACTER, src, tag, MPI_COMM_WORLD, status, ierr)		
		write (*,102) rmsg3, msg3
	end if
	
	call MPI_BUFFER_DETACH(buf, bufsize, ierr)
	
	call MPI_FINALIZE(ierr)
	
	deallocate(msg2, rmsg2)
	deallocate(buf)
	
	100 FORMAT ('message1  ', A20, '  should be  ', A20 )
	101 FORMAT ('message2  ', F6.2, '  should be', F6.2 )
	102 FORMAT ('message3  ', A20, '  should be  ', A20 )
	
	stop
end program bsendrecv
