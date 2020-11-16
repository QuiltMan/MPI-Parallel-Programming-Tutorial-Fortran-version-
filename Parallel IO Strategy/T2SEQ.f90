program T2SEQ

	parameter(ntotal = 200)
	integer i
	real*8 suma
	real*8 A(ntotal), B(ntotal), C(ntotal), D(ntotal)

	do i = 1, ntotal
		B(i) = 3.D0/DFLOAT(i)+1.0
		C(i) = 2.D0/DFLOAT(i)+1.0
		D(i) = 1.D0/DFLOAT(i)+1.0
	enddo
	
	open(7 ,file = 'input.dat', form ='unformatted')
	write(7) B
	write(7) C
	write(7) D
	close(7)
	
	open(10 ,file = 'input.dat', status = 'old', form ='unformatted')
	read(10) B
	read(10) C
	read(10) D
	suma=0.0
	
	do i=1,ntotal
		A(i)=B(i)+C(i)*D(i)
		suma=suma+A(i)
	enddo
	
	write(*, 100) (A(i), i=1, ntotal,5)
	write(*, 101) suma
	
	close(10)
	
100 FORMAT(10F8.3)	
101 FORMAT('SUM of array A =', E15.5)	
	
	stop
end program T2SEQ
