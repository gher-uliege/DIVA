! Sylvain Watelet - 09/12/2013 ccc
! resolution of Ax=b where A is a tridiagonal matrix ccc

! particular case : vertical filtering of correlation length ccc
! gfortran main_tomvec.f90 tomvec.f -o main_tomvec.a


      Program main_tomvec
      implicit none
      	
	integer::i,j,k,lay1,lay2,n,nlayer
      	real(kind=8),allocatable,dimension(:,:)::A,B,C,F,X
	character(len=100)::var
	real(kind=8),dimension(200)::cl,depth,quality,nsamp,alpha
	real(kind=8)::CL_mean,delta,test

!	call get_command_argument(1,var)

	open(11,file="./input/3Dinfo",status="old")
	read(11,*)
	read(11,*) var
	read(11,*)
	read(11,*) lay1
	read(11,*)
	read(11,*) lay2

	nlayer=(lay2-lay1)+1
!	write(*,*) nlayer

	open(12,file="./input/contour.depth",status="old")
	Do n=1,lay1-1
		read(12,*)
	Enddo
	Do n=lay1,lay2
		read(12,*) depth((n-lay1)+1)
		depth((n-lay1)+1)=(-1)*depth((n-lay1)+1) !
	Enddo
		
	open(13,file="./output/quality.fit",status="old")
	i=1
	Do while (.true.)
		read(13,*,end=1016) quality(i)
		i=i+1
	Enddo

1016 continue

	open(14,file="./output/nsamp.fit",status="old")
	i=1
	Do while (.true.)
		read(14,*,end=1017) nsamp(i)
		i=i+1
	Enddo

1017 continue

	open(10,file="./output/"//trim(adjustl(var))//".CL.dat",status="old")
	j=1
	Do while (.true.)
		if (j .gt. 200) then
		write(*,*) "SEVERE ERROR in main_tomvec.f90 : too many layers, please raise the dimension of cl"
		endif	

		read(10,*,end=1020) cl(j)
!		write(*,*) cl(j)
		j=j+1
	Enddo

1020 	continue
	j=j-1

        IF(nlayer .gt. 1) THEN

	allocate(A(1,nlayer),B(1,nlayer),C(1,nlayer),F(1,nlayer),X(1,nlayer))

!!! Computation of alphas,deltas,CL_mean !!!
	CL_mean=0.
	Do i=1,nlayer
		alpha(i)=(real(nsamp(i))/100.)*max(quality(i)-0.7,0.)  ! 100 and 0.7 are tuned parameters
!		write(*,*) nsamp(i),alpha(i),depth(i)
		CL_mean=CL_mean+cl(i)
	Enddo
	CL_mean=CL_mean/nlayer
	delta=(depth(1)-depth(nlayer))/(nlayer-1)
!	write(*,*) delta

!!! Computation of A,B,C,F !!!
	
	Do i=1,nlayer
		if (i==1) then
		A(1,i)=alpha(i)+(delta**2)/(depth(2)-depth(1))**2
		else if (i==nlayer) then
		A(1,i)=alpha(i)+(delta**2)/(depth(i)-depth(i-1))**2
		else
		A(1,i)=alpha(i)+(delta**2)/(depth(i)-depth(i-1))**2+(delta**2)/(depth(i+1)-depth(i))**2
		endif
	Enddo
	
	B(1,1)=0.
	Do i=2,nlayer
		B(1,i)=(-delta**2)/(depth(i)-depth(i-1))**2
	Enddo

	C(1,nlayer)=0.
	Do i=1,nlayer-1
		C(1,i)=(-delta**2)/(depth(i+1)-depth(i))**2
	Enddo

	Do i=1,nlayer
		F(1,i)=(alpha(i)*cl(i)+CL_mean)
	Enddo	

!	Do i=1,nlayer
!	write(*,*) A(1,i), B(1,i), C(1,i),F(1,i)
!	Enddo

	X(1,:)=0
	call tomvec(A,B,C,F,X,1,1,1,nlayer)

	open(15,file="bidon",status="replace")
	Do i=1,nlayer
	write(15,*) X(1,i) !, cl(i)
	Enddo

        ELSE

	open(15,file="bidon",status="replace")
	Do i=1,nlayer
	write(15,*) cl(i)
	Enddo

        ENDIF


	deallocate(A,B,C,F,X)
	close(10)
	close(11)
	close(12)
	close(13)
	close(14)
	close(15)

      End program
