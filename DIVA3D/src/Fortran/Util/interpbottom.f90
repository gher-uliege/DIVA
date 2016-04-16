Program interpbottom
implicit none

!------------------------------------------------
! Sylvain Watelet - 27/03/2016
! swatelelet@ulg.ac.be
! 
! Interpolation of the bottom depth from topo_fine.grd
!------------------------------------------------

      integer nmax,iw,i,j,n,ii,d
      parameter(nmax=10000,iw=100000000)
      REAL*4 U(iw)
      REAL*4 X(nmax)
      REAL*4 Y(nmax)

	real*8,allocatable,dimension(:)::lon,lat,botdepth
	real*8::deltalat,deltalon,a,c,R=6371.,pi=3.14159265,distance
	real*8,dimension(4)::dist_proche,U_proche

      integer NX, NY, KMAX, ipr, nw, IMAX, JMAX
      real VALEXU
      real*8 W8(1)
      real xorig, yorig, dx, dy, xend, yend, valex
      
      character(len=256)::gherfile,infofile,bigfile,bigfile2
	character(len=256)::c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,botdepthc

gherfile="topo_fine.grd"
infofile="TopoInfo_fine.dat"
bigfile="bigfile.nobotdepth"
bigfile2="bigfile.botdepth"

open (unit=10,file=trim(gherfile),form='unformatted')
CALL UREADC(10,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)

      if (KMAX.ne.1) stop "2D Field only, please."

      ipr=4
      nw=imax*jmax
      if(nw.gt.iw) then
      write(6,*) 'Severe error, increase iw interpbottom.f90'
      stop 'Severe error, increase iw interpbottom.f90'
      endif
      if(imax.gt.nmax.or.jmax.gt.nmax) then
      write(6,*) 'Severe error, increase iw interpbottom.f90'
      stop 'Severe error, increase nmax interpbottom.f90'
      endif


!    Reads the grid data from info file
!----------------------------------------------
      open(unit=90,file=trim(infofile))
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend

      NX=int((xend-xorig)/dx)
      NY=int((yend-yorig)/dy)

      close(90)
!      write(*,*) 'valex = ,', valex 
      close(10)

!------------------------------------------------
! Creation of dimensions
!------------------------------------------------

do i = 1, IMAX
   X(i) = xorig + (i - 1) * dx
end do
do j = 1, JMAX
   Y(j) = yorig + (j - 1) * dy
end do

!Do j=1,jmax
!	Do i=1,imax
!		write(0,*) X(i),Y(j),U(i+(j-1)*imax)
!	Enddo
!Enddo

!------------------------------------------------
! Reading coordinates of VAR.bigfile.nobotdepth
!------------------------------------------------

open(unit=11,file=trim(bigfile))

n=0
Do while (.true.)
	read(11,*,end=1009)
	n=n+1
Enddo

1009 continue

allocate(lon(n),lat(n),botdepth(n))

rewind(11)

i=0
Do while (.true.)
	i=i+1
	read(11,*,end=1010) lon(i),lat(i)
Enddo

1010 continue

 close(11)

!write(0,*) n,lon(n),lat(n)

!------------------------------------------------
! Interpolation of bot depth on each lon-lat 
!------------------------------------------------

Do ii=1,n
	dist_proche(:)=99999.
	U_proche(:)=99999.
	Do i=1,imax
		Do j=1,jmax
			d=d+1
			
			deltalon=lon(ii)-X(i)
			deltalat=lat(ii)-Y(j)

			If ((abs(deltalat)>dy).or.(abs(deltalon)>dx)) cycle 

			deltalat = deltalat*(pi/180.)
			deltalon = deltalon*(pi/180.)

			a = (sin(deltalat/2.))**2 + cos(lat(ii)*(pi/180.))*cos(Y(j)*(pi/180.))*(sin(deltalon/2.))**2  !! Haversine formula
			c = 2*atan2(sqrt(a),sqrt(1-a))
			distance = R*c

			If (distance<epsilon(0.)) distance=epsilon(0.) ! to avoid division by zero

			If (distance<dist_proche(1)) then
				dist_proche(4)=dist_proche(3)
				dist_proche(3)=dist_proche(2)
				dist_proche(2)=dist_proche(1)
				dist_proche(1)=distance
				U_proche(4)=U_proche(3)
				U_proche(3)=U_proche(2)
				U_proche(2)=U_proche(1)
				U_proche(1)=U(i+(j-1)*imax)
			else if (distance<dist_proche(2)) then
				dist_proche(4)=dist_proche(3)
				dist_proche(3)=dist_proche(2)
				dist_proche(2)=distance
				U_proche(4)=U_proche(3)
				U_proche(3)=U_proche(2)
				U_proche(2)=U(i+(j-1)*imax)
			else if (distance<dist_proche(3)) then
				dist_proche(4)=dist_proche(3)
				dist_proche(3)=distance
				U_proche(4)=U_proche(3)
				U_proche(3)=U(i+(j-1)*imax)
			else if (distance<dist_proche(4)) then
				dist_proche(4)=distance
				U_proche(4)=U(i+(j-1)*imax)
			Endif

		Enddo
	Enddo
	
	botdepth(ii)=sum((1./dist_proche(:))*U_proche(:))/sum(1./dist_proche(:))
!	write(0,*) U_proche,dist_proche,botdepth(ii)
!	call sleep(1)
!	write(0,*) ii,botdepth(ii) !,dist_proche(:),epsilon(0.)
Enddo

!------------------------------------------------
! Writing bigfile.botdepth
!------------------------------------------------

rewind(11)
open(unit=11,file=trim(bigfile))
open(unit=12,file=trim(bigfile2))

Do ii=1,n
	read(11,*) c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12
	write(botdepthc,'(i5)') int(botdepth(ii))
write(12,'(12(a,x))') trim(c1),trim(c2),trim(c3),trim(c4),trim(c5),trim(c6),trim(c7),trim(c8),trim(c9),trim(c10),trim(c11),&
&trim(adjustl(botdepthc))
Enddo

 close(11)
 close(12)

      stop
End program



      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
!                ======
!-----------------------------------------------------------------------
! Reads the field C(I,J,K) from fortran unit iu
! returns the field in the array c4 if the returned iprecr=4
! returns the field in the array c8 if the returned iprecr=8
! returns the values if imaxr,jmaxr,kmaxr found in the file
!
! JMB 6/3/91
!-----------------------------------------------------------------------
!
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      integer*4 imaxc,jmaxc,kmaxc,iprec,nbmots
      
! in the calling routin you can specify the following equivalence to
! save memory space:
!      equivalence(c,c4)

!      equivalence(c,c8)
!
! skip KBLANC lines
      do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1    continue
!
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!
! pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc

!      print *, 'iprecr=', iprec
!      print *, 'imaxr=', imaxc
!      print *, 'jmaxr=', jmaxc
!      print *, 'kmaxr=', kmaxc
!      print *, 'nbmotr=', nbmots
!      print *, 'valexr=', valexc


!
! compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!

! if pathological case, read only four values C0 and DCI,DCJ,DCK
! and return
! them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
        nl=0
        ir=4
        endif
!
!
! single precision
        if(iprec.eq.4) then
        do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                      else
!
! double precision
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
!
        return
 99      continue
        write(*,*) 'Data error in UREADC, not a conform file'
        return
100      continue
        write(*,*) 'Data error in UREADC, EOF reached'
        write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

        return
        end

