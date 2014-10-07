!------------------------------------------------
! Sylvain Watelet - 09/09/2014
! Creation of RL fields depending on depth
! -----------------------------------------------

Program rlvardepth
implicit none

integer,parameter::nm=5000000
real*4 topo(nm)
real*8 c8(nm)

integer::M,N,imax,jmax,iprecr,nbmotr,iua,kmax,ic,i,j,k,l,z,i_old,j_old
integer::Mdom,Ndom,counter,NTIMES,nn
real::x1,y1,dx,dy,valex,hmax,hmin
real::x1dom,y1dom,dxdom,dydom,deltalat,deltalon
real(kind=4)::h
real::pi=3.14159265,a, c, R=6371.,distance,distance_old,mintopo,mintopo_old
real(kind=4),dimension(:,:),allocatable::topo2D,RL,lattopo,lontopo,latdom,londom,topo2Ddom,topo2Ddomtmp,RN
real(kind=4),dimension(nm)::toposorted

!------------------------------------------------
! Reading topography
!------------------------------------------------
      
iua=10
read(iua,*) x1
read(iua,*) y1
read(iua,*) dx
read(iua,*) dy
read(iua,*) M
read(iua,*) N
      
if ((M+2)*(N+2).GT.NM) stop 'increase NM'

write(6,*) 'into ureadc',M,N
write(6,*) 'reading'
imax=M
jmax=N
call ureadc(12,c8,topo,valex,iprecr,imax,jmax,kmax,nbmotr)
write(6,*) 'out of reading',imax,jmax
      
if ((M.NE.IMAX).OR.(N.NE.JMAX)) stop 'incoherent files'

!write(*,*) topo(10)

ALLOCATE(topo2D(M,N),lattopo(M,N),lontopo(M,N))

ic=0
Do j=1,jmax
	Do i=1,imax
		ic=ic+1
		topo2D(i,j)=topo(ic)
!		write(*,*) topo2D(i,j) 
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

allocate(RL(Mdom,Ndom),latdom(Mdom,Ndom),londom(Mdom,Ndom),topo2Ddom(Mdom,Ndom),topo2Ddomtmp(Mdom,Ndom),RN(Mdom,Ndom))

write(*,*) x1dom,y1dom,dxdom,dydom,Mdom,Ndom

!------------------------------------------------
! Computing lat and lon (dom & topo)
!------------------------------------------------

Do j=1,jmax
	lattopo(:,j)=y1+(j-1)*dy
Enddo

Do i=1,imax
	lontopo(i,:)=x1+(i-1)*dx
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
topo2Ddom(:,:)=valex

Do i=1,Mdom
	Do j=1,Ndom
		distance_old=100000.
		Do k=1,M
			Do l=1,N
				deltalat = latdom(i,j)-lattopo(k,l)
				deltalon = londom(i,j)-lontopo(k,l) 

				If ((abs(deltalat)>dy).or.(abs(deltalon)>dx)) cycle

!				write(*,*) i,j,delta_lat,delta_lon,abs(delta_lat)+abs(delta_lon)
				deltalat = deltalat*(pi/180.)
				deltalon = deltalon*(pi/180.)

				a = (sin(deltalat/2.))**2 + cos(latdom(i,j)*(pi/180.))*cos(lattopo(m,n)*(pi/180.))*(sin(deltalon/2.))**2  ! Haversine 
				c = 2*atan2(sqrt(a),sqrt(1-a))                                                              	          ! Formula
				distance = R*c

				If (distance<distance_old) then
!					write(*,*) distance_old,distance
					distance_old=distance
					topo2Ddom(i,j)=-1.*topo2D(k,l) ! topo inverted
				Endif
			Enddo
		Enddo
	Enddo
write(*,*) "x-step=",i
Enddo

!------------------------------------------------
! Computing hmin and hmax via topography 
! quartiles
!------------------------------------------------

topo2Ddomtmp(:,:)=topo2Ddom(:,:)
counter=0

Do z=1,Mdom*Ndom
	mintopo_old=0.
	Do i=1,Mdom
		Do j=1,Ndom
			mintopo=topo2Ddomtmp(i,j)
			If (mintopo<mintopo_old) then
!				write(*,*) mintopo_old,mintopo
				mintopo_old=mintopo
				i_old=i
				j_old=j
			Endif
		Enddo
	Enddo
	toposorted(z)=mintopo_old
!	write(*,*) toposorted(z)
	topo2Ddomtmp(i_old,j_old)=0.
	If (mintopo_old<0.) counter=counter+1
Enddo

!write(*,*) counter

hmax=toposorted(int(1.*counter/4.))
hmin=toposorted(int(3.*counter/4.))

write(*,*) "hmax=",hmax,"hmin=",hmin

!------------------------------------------------
! Creating the RL field (RL=0.3 near the coast,
! RL=3. in open sea)
!------------------------------------------------

Do i=1,Mdom
	Do j=1,Ndom
		If ((topo2Ddom(i,j)/=valex).and.(topo2Ddom(i,j)<0.)) then
			h=topo2Ddom(i,j)
			if (h<hmax) h=hmax
			if (h>hmin) h=hmin
			RL(i,j)=(0.3*(sqrt(abs(hmax))-sqrt(abs(h)))+3.*(sqrt(abs(h))-sqrt(abs(hmin))))/(sqrt(abs(hmax))-sqrt(abs(hmin)))
		else
			RL(i,j)=0.3
!			write(*,*) topo2Ddom(i,j),RL(i,j)
!			call sleep(1)
		Endif
!		write(*,*) topo2Ddom(i,j),latdom(i,j),londom(i,j),h,RL(i,j)
!		call sleep(1)
	Enddo
Enddo

! Take into account exclusion values and boundaries
! by filling first the grid.

call ufill(RL,valex,Mdom,Ndom,1)

!C After this, smooth the RL field by an application of a Laplacian filter at least
!C sqrt(sqrt(Mdom*Ndom)) times

             NTIMES=sqrt(sqrt(Mdom*Ndom*1.)+1.)/3.+1.
!C             NTIMES=0
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
!		write(*,*) topo2Ddom(i,j),RL(i,j)
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
