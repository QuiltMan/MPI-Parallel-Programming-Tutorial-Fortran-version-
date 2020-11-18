program main
	use mpi
	implicit none
	
	integer,parameter :: totalsize = 16, steps = 10
	integer,parameter :: mysize = totalsize/4

	integer n, myid, numprocs, i, j, rc
	real a(totalsize, mysize+2), b(totalsize, mysize+2)
	
	integer begin_col, end_col, ierr
	integer status(MPI_STATUS_SIZE)
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

	print *, "Process ", myid, " of ", numprocs, " is alive"
	
	do j = 1,mysize+2
		do i = 1, totalsize
			a(i,j)=0.0
		enddo
	enddo
	
	if (myid .eq. 0) then
		do i=1,totalsize
			a(i,2)=8.0
		end do
	end if

	if (myid .eq. 3) then
		do i=1,totalsize
			a(i,mysize+1)=8.0
		end do
	end if

	do i=1,mysize+2
		a(1,i)=8.0
		a(totalsize,i)=8.0
	end do
	
	do n=1,steps
		
		if (myid .lt. 3) then
			call MPI_RECV(a(1,mysize+2),totalsize,MPI_REAL,myid+1,10,MPI_COMM_WORLD,status,ierr)
		end if
		
		if ((myid .gt. 0) ) then
			call MPI_SEND(a(1,2),totalsize,MPI_REAL,myid-1,10,MPI_COMM_WORLD,ierr)
		end if
		
		if (myid .lt. 3) then
			call MPI_SEND(a(1,mysize+1),totalsize,MPI_REAL,myid+1,10,MPI_COMM_WORLD,ierr)
		end if
		
		if (myid .gt. 0) then
			call MPI_RECV(a(1,1),totalsize,MPI_REAL,myid-1,10,MPI_COMM_WORLD,status,ierr)
		end if	
	
		begin_col=2
		end_col=mysize+1
		
		if (myid .eq. 0) then
			begin_col=3
		endif
		
		if (myid .eq. 3) then
			end_col=mysize
		endif
		
		do j=begin_col,end_col
			do i=2,totalsize-1
				b(i,j)=(a(i,j+1)+a(i,j-1)+a(i+1,j)+a(i-1,j))*0.25
			end do
		end do
	
		do j=begin_col,end_col
			do i=2,totalsize-1
				a(i,j)=b(i,j)
			end do
		end do
	end do
	
	do i=2,totalsize-1
		print *, myid,(a(i,j),j=begin_col,end_col)
	end do
	
	call MPI_Finalize(rc)
	stop
end program main
