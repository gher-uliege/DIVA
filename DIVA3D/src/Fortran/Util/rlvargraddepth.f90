!------------------------------------------------
! Sylvain Watelet - 09/05/2016
! Creation of RL fields depending on depth (=h1=h2 at the moment)
! -----------------------------------------------

Program rlvardepth
implicit none

integer,parameter::nm=5000000
real*4 h1(nm)
real*8 c8(nm)

integer::M,N,imax,jmax,iprecr,nbmotr,iua,kmax,ic,i,j,k,l,z,i_old,j_old
integer::Mdom,Ndom,counter,NTIMES,nn
real::x1,y1,dx,dy,valex,hmax,hmin,CL,valex2,minrl,theta
real::a1,b1,c1,d1,e1,f1,g1,h11,i1,j1
real::x1dom,y1dom,dxdom,dydom,deltalat,deltalon
real(kind=4)::h
real::pi=3.14159265,a, c, R=6371.,distance,distance_old,minh1,minh1_old
real(kind=4),dimension(:,:),allocatable::h1_2D,RL,lath1,lonh1,latdom,londom,h1_2Ddom,RN,gradh1_2Ddom,h2_2Ddom
!real(kind=4),dimension(nm)::h1sorted

!------------------------------------------------
! Reading h1
!------------------------------------------------
      
iua=10
read(iua,*) x1
read(iua,*) y1
read(iua,*) dx
read(iua,*) dy
read(iua,*) M
read(iua,*) N

iua=13
read(iua,*) CL
      
if ((M+2)*(N+2).GT.NM) stop 'increase NM'

write(6,*) 'into ureadc',M,N
write(6,*) 'reading'
imax=M
jmax=N
call ureadc(12,c8,h1,valex,iprecr,imax,jmax,kmax,nbmotr)
write(6,*) 'out of reading',imax,jmax
      
if ((M.NE.IMAX).OR.(N.NE.JMAX)) stop 'incoherent files'

!write(*,*) h1(10)

ALLOCATE(h1_2D(M,N),lath1(M,N),lonh1(M,N))

ic=0
Do j=1,jmax
	Do i=1,imax
		ic=ic+1
		h1_2D(i,j)=h1(ic)
!		write(*,*) h1_2D(i,j) 
	Enddo
Enddo

!------------------------------------------------
! Reading the domain of interest
!------------------------------------------------

iua=11
read(iua,*) x1dom
read(iua,*) y1dom
read(iua,*) dxdom
read(iua,*) dydom
read(iua,*) Mdom
read(iua,*) Ndom
read(iua,*) valex

allocate(RL(Mdom,Ndom),latdom(Mdom,Ndom),londom(Mdom,Ndom),h1_2Ddom(Mdom,Ndom),RN(Mdom,Ndom),&
&gradh1_2Ddom(Mdom,Ndom),h2_2Ddom(Mdom,Ndom))

write(*,*) x1dom,y1dom,dxdom,dydom,Mdom,Ndom

!------------------------------------------------
! Computing lat and lon (dom & h1)
!------------------------------------------------

Do j=1,jmax
	lath1(:,j)=y1+(j-1)*dy
Enddo

Do i=1,imax
	lonh1(i,:)=x1+(i-1)*dx
Enddo

Do j=1,Ndom
	latdom(:,j)=y1dom+(j-1)*dydom
Enddo

Do i=1,Mdom
	londom(i,:)=x1dom+(i-1)*dxdom
Enddo

!------------------------------------------------
! Interpolating topography on domain grid
!------------------------------------------------

RL(:,:)=valex
h1_2Ddom(:,:)=valex

Do i=1,Mdom
	Do j=1,Ndom
		distance_old=100000.
		Do k=1,M
			Do l=1,N
				deltalat = latdom(i,j)-lath1(k,l)
				deltalon = londom(i,j)-lonh1(k,l) 

				If ((abs(deltalat)>dydom).or.(abs(deltalon)>dxdom)) cycle

!				write(*,*) i,j,delta_lat,delta_lon,abs(delta_lat)+abs(delta_lon)
				deltalat = deltalat*(pi/180.)
				deltalon = deltalon*(pi/180.)

				a = (sin(deltalat/2.))**2 + cos(latdom(i,j)*(pi/180.))*cos(lath1(m,n)*(pi/180.))*(sin(deltalon/2.))**2  ! Haversine 
				c = 2*atan2(sqrt(a),sqrt(1-a))                                                              	          ! Formula
				distance = R*c

				If (distance<distance_old) then
!					write(*,*) distance_old,distance
					distance_old=distance
					h1_2Ddom(i,j)=-1.*h1_2D(k,l) ! h1 inverted
				Endif
			Enddo
		Enddo
	Enddo
!write(*,*) "x-step=",i
Enddo

h2_2Ddom(:,:)=h1_2Ddom(:,:) ! h1=h2=topo (for the moment)

!------------------------------------------------
! Avoid division by zero
!------------------------------------------------

Do i=1,Mdom
	Do j=1,Ndom
		If (abs(h2_2Ddom(i,j)).lt.0.001) h2_2Ddom(i,j)=0.001
	Enddo
Enddo

!------------------------------------------------
! Computing gradient of h1 (and RL)
!------------------------------------------------

gradh1_2Ddom(:,:)=valex
!write(*,*) gradh1_2Ddom(:,:)

! boudary conditions

i=1
Do j=2,Ndom-1
!	if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j-1))/(2.*dydom))**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=0.
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=0.
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=0.
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif
Enddo

i=Mdom
Do j=2,Ndom-1
!	if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j-1))/(2.*dydom))**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=0.
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=0.
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=abs((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif
Enddo

j=1
Do i=2,Mdom-1
!	if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i-1,j))/(2.*dxdom))**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=0.
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=0.
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif
Enddo

j=Ndom
Do i=2,Mdom-1
!	if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i-1,j))/(2.*dxdom))**2&
!&+((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=0.
		g1=abs((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)
		h11=0.
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif
Enddo

i=1
j=1
!if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=0.
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=0.
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=0.
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=0.
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif

i=1
j=Ndom
!if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)**2&
!&+((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=0.
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=0.
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=0.
		g1=abs((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)
		h11=0.
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif

i=Mdom
j=1
!if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=0.
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=0.
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=0.
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=0.
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif

i=Mdom
j=Ndom
!if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)**2&
!&+((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)**2)
	if (h1_2Ddom(i,j).le.0.) then
		a1=0.
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=0.
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=0.
		g1=abs((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)
		h11=0.
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
	endif

! general conditions

Do i=2,Mdom-1
	Do j=2,Ndom-1
!		if (h1_2Ddom(i,j).le.0.) gradh1_2Ddom(i,j)=sqrt(((h1_2Ddom(i+1,j)-h1_2Ddom(i-1,j))/(2.*dxdom))**2&
!&+((h1_2Ddom(i,j+1)-h1_2Ddom(i,j-1))/(2.*dydom))**2)
!!		write(*,*) gradh1_2Ddom(i,j),h1_2Ddom(i+1,j),h1_2Ddom(i-1,j),dxdom,h1_2Ddom(i,j+1),h1_2Ddom(i,j-1),dydom
!	if (h1_2Ddom(i,j).le.0.) then
		a1=abs((h1_2Ddom(i+1,j)-h1_2Ddom(i,j))/dxdom)
		b1=abs((h1_2Ddom(i,j)-h1_2Ddom(i-1,j))/dxdom)
		c1=abs((h1_2Ddom(i+1,j)+h1_2Ddom(i,j))/2.)
		d1=abs((h1_2Ddom(i,j)+h1_2Ddom(i-1,j))/2.)
		e1=((a1+b1)/2.)/((c1+d1)/2.)
		f1=abs((h1_2Ddom(i,j+1)-h1_2Ddom(i,j))/dydom)
		g1=abs((h1_2Ddom(i,j)-h1_2Ddom(i,j-1))/dydom)
		h11=abs((h1_2Ddom(i,j+1)+h1_2Ddom(i,j))/2.)
		i1=abs((h1_2Ddom(i,j)+h1_2Ddom(i,j-1))/2.)
		j1=((f1+g1)/2.)/((h11+i1)/2.)
		RL(i,j)=sqrt(e1**2+j1**2)
!	endif
	Enddo
Enddo

!------------------------------------------------
! Creating the RL field (=1/(1+((||gradh1_2Ddom||/(-h2_2Ddom))*L))
!------------------------------------------------

minrl=1.

Do i=1,Mdom
	Do j=1,Ndom
!!		If (h1_2Ddom(i,j).le.0) then
			RL(i,j)=(1./(1.+(RL(i,j)*CL)))
		If ((h1_2Ddom(i,j).le.0).and.(RL(i,j).lt.minrl)) then
			minrl=RL(i,j)
		Endif
!			RL(i,j)=(1./(1.+((abs(gradh1_2Ddom(i,j))/abs(h2_2Ddom(i,j)))*CL)))
!!			write(*,*) RL(i,j),gradh1_2Ddom(i,j),h2_2Ddom(i,j),CL
!!		Endif
!!		write(*,*) h1_2Ddom(57,1),RL(57,1)
!!		write(*,*) RL(i,j),gradh1_2Ddom(i,j),h2_2Ddom(i,j),CL
!!		write(*,*) topo2Ddom(i,j),latdom(i,j),londom(i,j),h,RL(i,j)
!!		call sleep(1)
	Enddo
Enddo

write(*,*) "min rl:",minrl

theta=(1./minrl)-1.

write(*,*) "advised theta for advection constraint:",theta

iua=15
write(iua,*) theta

! Take into account exclusion values and boundaries
! by filling first the grid.

call ufill(RL,valex,Mdom,Ndom,1)

!C After this, smooth the RL field by an application of a Laplacian filter at least
!C sqrt(sqrt(Mdom*Ndom)) times

!             NTIMES=sqrt(sqrt(Mdom*Ndom*1.)+1.)/3.+1.
!C             NTIMES=0
	NTIMES=sqrt(sqrt(Mdom*Ndom*1.)+1.)/2.+1.
!	NTIMES=100 ! to avoid "chessboard pattern"
             do nn=1,NTIMES
             
             
              do i=2,Mdom-1
               do j=2,Ndom-1
               RN(i,j)=(RL(I+1,j)+RL(I-1,j)+RL(i,j+1)+RL(i,j-1))/4.
               enddo
              enddo
!C zero gradient
             do i=1,Mdom
             RN(i,1)=RL(i,2)
             RN(i,Ndom)=RL(i,Ndom-1)
             enddo
             do j=1,Ndom
             RN(1,j)=RN(2,j)
             RN(Mdom,j)=RN(Mdom-1,j)
             enddo

             do i=1,Mdom
              do j=1,Ndom
              RL(i,j)=RN(i,j)
              enddo
             enddo

!C Make sure average is 1. Possibly average on 1/RL^2.
!C use only wet point for this average.
!             RRR=0
!             do i=1,NX
!             do j=1,NY
!              if(C(i,j).NE.VALEX) then
!              RRR=RRR+1./RL(i,j)/RL(i,j)
!              endif
!              enddo
!             enddo
!             RRR=RRR/RWET
!             write(6,*) 'RRR',RRR
!             do i=1,NX
!              do j=1,NY
!              if(C(i,j).NE.VALEX) then
!              RL(i,j)=RL(i,j)*sqrt(RRR)
!              endif
!              enddo
!             enddo
             
             enddo

!Do i=1,Mdom
!	Do j=1,Ndom
!		write(*,*) h1_2Ddom(i,j),RL(i,j),RL(57,1)
!	Enddo
!Enddo

call UWRITC(20,c8,RL,valex,4,Mdom,Ndom,1,Mdom*Ndom)

!-----------------------------------------------------------------------------------------------------------------------------------------------------
! From this point, subroutines ureadc and uwritc
! copied from datalengthscale.f (small modifs)
!-----------------------------------------------------------------------------------------------------------------------------------------------------

contains

	Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
	implicit none

!c23456                ======
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Reads the field C(I,J,K) from fortran unit iu 
!c returns the field in the array c4 if the returned iprecr=4
!c returns the field in the array c8 if the returned iprecr=8
!c returns the values if imaxr,jmaxr,kmaxr found in the file
!c
!c JMB 6/3/91 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c23456
       integer,PARAMETER::KBLANC=10
       real*4 c4(*)
       real*8 c8(*)
	integer::iu,iprecr,imaxr,jmaxr,kmaxr,nbmotr,kb,kl,ide,imaxc,jmaxc,kmaxc,iprec,nbmots,ir,kc,nl
	real::valexr,valexc
!c in the calling routin you can specify the following equivalence to
!c save memory space:
!c      equivalence(c,c4)
!c      equivalence(c,c8)
!c
!c skip KBLANC lines
       write(6,*) ' ureadc in'
       do 1 kb=1,KBLANC
        read(iu,end=99,ERR=99)
 1     continue
!c
        read(iu,end=99,err=99) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!c
!c pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc
!c
!c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!c
!c if pathological case, read only four values C0 and DCI,DCJ,DCK
!c and return
!c them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!c
!c
!c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
!c
!c double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,ir)
                       else
           goto 99
         endif
         endif
!c
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         imaxr=1
         jmaxr=1
         kmaxr=1
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1
         imaxr=0
         return
         End subroutine
         

       
	Subroutine UWRITC(iu,c8,c4,valex8,ipre8,imaxc,jmaxc,kmaxc,nbmots)
	implicit none
!c                ======
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c writes the field C(I,J,K)  into fortran unit iu 
!c writes the field in the array c4 if iprecr=4
!c writes the field in the array c8 if iprecr=8
!c
!c The KBLANC blank lines are at the disposal of the user
!c JMB 6/3/92
!c
!c IF c(i,j,k)=NaN or infinity, it is replaced by VALEX! 
!c
!c 
!c RS 12/1/93
!c
!c If nbmots = -1  then write only 1 data record
!c     (only for non-degenerated data)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      integer,PARAMETER::KBLANC=10
      real*4 c4(*)
      real*8 c8(*)
      real*4 valex8
      real*4 valexc
	integer::iu,ipre8,imaxc,jmaxc,kmaxc,nbmots,i,j,k,kb,kl,ich,ioff,ide,iprec,ir,kc,nl
	real::un,z
!c in the calling routin you can specify the following equivalence to
!c save memory space:
!c      equivalence(c,c4)
!c      equivalence(c,c8)
!c
!c Putting  Valex where not numbers
       z=0.
       un=1.
       ich=0
       ioff=1
       if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

       IF (NBMOTS.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

       do k=1,kmaxc
        do j=1,jmaxc
         do i=1,imaxc
!c         if( c4(ioff).eq.(z/z) ) goto 1010 
!c         if( c4(ioff).eq.(un/z) ) goto 1010 
!c         if( c4(ioff).eq.(-z/z) ) goto 1010 
!c         if( c4(ioff).eq.(-un/z) ) goto 1010 
         goto 1011
 1010     continue
          c4(ioff)=valex8
          ich=ich+1
 1011    continue 
         ioff=ioff+1
         enddo
        enddo
       enddo
       if(ich.gt.0) then
       write(6,*) ' WARNING:',ich,' Values are not numbers'
       write(6,*) '   Changing them into VALEX'
       endif
       endif
       valexc=valex8
       iprec=4
!c
!c skip KBLANC lines
!C        write(6,*) iu,imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
       do 1 kb=1,KBLANC
        write(iu,ERR=99)
 1     continue
!c
        write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!c
!c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!c
!c if pathological case, write only four values C0 and DCI,DCJ,DCK found 
!c as the two four elements of the array so that C(I,J,K) =
!c C0 + I * DCI + J * DCJ + K * DCK
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!c
!c
!c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,ir)
                       else
!c
!c double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          write (iu,ERR=99) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          write (iu,ERR=99) (c8(ide+kc),kc=1,ir)
                       else
           goto 99
         endif
         endif
!c
         return
 99      continue
         write(*,*) 'Data error in UWRITC, not a conform file'
        write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
        write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
         return
         End subroutine

      subroutine UFILL(c,valexc,imax,jmax,kmax)
	implicit none
!c                =====
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c Fills in the standart field C by interpolating the field into the
!c points where there was a exclusion value valexc.
!c The interpolation is continued until the complete field contains
!c regular values
!c
!c JMB 3/4/91
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	integer::imax,jmax,kmax       
	real*4 C(imax,jmax,kmax),valexc
       integer,parameter::iwork=1000000
       real*4 work(iwork),work2(iwork)
       integer*2 ic(iwork)
       integer*2 ic2(iwork)
       if( (imax+2)*(jmax+2)*(kmax+2).gt.iwork) then
         write(6,*) ' Sorry, UFILL needs more working space'
         return
       endif
!c
       call JM0000(c,valexc,imax,jmax,kmax,work,work2,ic,ic2)
       return
       End subroutine
!c
       subroutine JM0000(c,valexc,imax,jmax,kmax,work,work2,ic,ic2)
	implicit none
!c      parameter(A1=5,A2=2,A3=4)
       integer,parameter::A1=5,A2=0,A3=0
       real*4 C(imax,jmax,kmax),valexc
       real*4 work(0:imax+1,0:jmax+1,0:kmax+1)
       real*4 work2(0:imax+1,0:jmax+1,0:kmax+1)
       integer*2 ic(0:imax+1,0:jmax+1,0:kmax+1)
       integer*2 ic2(0:imax+1,0:jmax+1,0:kmax+1)
	integer::isom1,isom2,isom3,imax,jmax,kmax,isom,icount
	real::rsom1,rsom2,rsom3
!c fill the borders...
       write(6,*) ' Filling in',valexc,imax,jmax,kmax
       do 11 j=0,jmax+1
        do 11 i=0,imax+1
        work(i,j,0)=valexc
        ic(i,j,0)=0
        work(i,j,kmax+1)=valexc
        ic(i,j,kmax+1)=0
 11     continue
       do 12 k=0,kmax+1
        do 12 i=0,imax+1
        work(i,0,k)=valexc
        ic(i,0,k)=0
        work(i,jmax+1,k)=valexc
        ic(i,jmax+1,k)=0
 12     continue
       do 13 k=0,kmax+1
        do 13 j=0,jmax+1
        work(0,j,k)=valexc
        ic(0,j,k)=0
        work(imax+1,j,k)=valexc
        ic(imax+1,j,k)=0
 13     continue
!c
!c copy interior field
        do 100 k=1,kmax
         do 100 j=1,jmax
          do 100 i=1,imax
          work(i,j,k)=c(i,j,k)
          ic(i,j,k)=1
          if(work(i,j,k).eq.valexc) ic(i,j,k)=0
 100    continue
!c
 1000  continue 
       icount=0
       do 500 k=1,kmax
        do 500 j=1,jmax
         do 500 i=1,imax
         work2(i,j,k)=work(i,j,k)
         ic2(i,j,k)=ic(i,j,k)
         if(ic(i,j,k).eq.0 ) then
         work2(i,j,k)=valexc
          icount=icount+1
	isom1=&
!c    &      ic(i,j,k+1)+ic(i,j,k-1)
      	0.&      
	+ic(i+1,j,k)+ic(i-1,j,k)&
        +ic(i,j+1,k)+ic(i,j-1,k)
       	isom2=&
      	ic(i+1,j+1,k+1)+ic(i+1,j+1,k-1)&
      	+ic(i+1,j-1,k+1)+ic(i+1,j-1,k-1)&
      	+ic(i-1,j+1,k+1)+ic(i-1,j+1,k-1)&
      	+ic(i-1,j-1,k+1)+ic(i-1,j-1,k-1)
       	isom3=&
      	ic(i,j+1,k+1)+ic(i,j+1,k-1)&
      	+ic(i,j-1,k+1)+ic(i,j-1,k-1)&
      	+ic(i+1,j,k+1)+ic(i+1,j,k-1)&
     	+ic(i-1,j,k+1)+ic(i-1,j,k-1)&
      	+ic(i+1,j+1,k)+ic(i+1,j-1,k)&
     	+ic(i-1,j+1,k)+ic(i-1,j-1,k)
           isom=isom1*a1+isom2*a2+isom3*a3
           if(isom.ne.0) then
!c interpolate
           rsom1=& 
!c    &      ic(i,j,k+1)*work(i,j,k+1)
!c    &     +ic(i,j,k-1)*work(i,j,k-1)
           0.&
          +ic(i+1,j,k)*work(i+1,j,k)&
          +ic(i-1,j,k)*work(i-1,j,k)&
          +ic(i,j+1,k)*work(i,j+1,k)&
          +ic(i,j-1,k)*work(i,j-1,k)
          rsom2=&
           ic(i+1,j+1,k+1)*work(i+1,j+1,k+1)&
          +ic(i+1,j+1,k-1)*work(i+1,j+1,k-1)&
          +ic(i+1,j-1,k+1)*work(i+1,j-1,k+1)&
          +ic(i+1,j-1,k-1)*work(i+1,j-1,k-1)&
          +ic(i-1,j+1,k+1)*work(i-1,j+1,k+1)&
          +ic(i-1,j+1,k-1)*work(i-1,j+1,k-1)&
          +ic(i-1,j-1,k+1)*work(i-1,j-1,k+1)&
          +ic(i-1,j-1,k-1)*work(i-1,j-1,k-1)
          rsom3=&
           ic(i,j+1,k+1)*work(i,j+1,k+1)&
          +ic(i,j+1,k-1)*work(i,j+1,k-1)&
          +ic(i,j-1,k+1)*work(i,j-1,k+1)&
          +ic(i,j-1,k-1)*work(i,j-1,k-1)&
          +ic(i+1,j,k+1)*work(i+1,j,k+1)&
          +ic(i+1,j,k-1)*work(i+1,j,k-1)&
          +ic(i-1,j,k+1)*work(i-1,j,k+1)&
          +ic(i-1,j,k-1)*work(i-1,j,k-1)&
          +ic(i+1,j+1,k)*work(i+1,j+1,k)&
          +ic(i+1,j-1,k)*work(i+1,j-1,k)&
          +ic(i-1,j+1,k)*work(i-1,j+1,k)&
          +ic(i-1,j-1,k)*work(i-1,j-1,k)
          work2(i,j,k)=(a1*rsom1+a2*rsom2+a3*rsom3)/float(isom)
          ic2(i,j,k)=1
           endif
         endif
 500   continue
       do 501 k=1,kmax
        do 501 j=1,jmax
         do 501 i=1,imax
          work(i,j,k)=work2(i,j,k)
          ic(i,j,k)=ic2(i,j,k)
 501   continue
       if(icount.gt.0) goto 1000
!c
!c fini
       do 99 k=1,kmax
        do 99 j=1,jmax
         do 99 i=1,imax
         c(i,j,k)=work(i,j,k)
 99    continue

       return
       End subroutine

End program rlvardepth
