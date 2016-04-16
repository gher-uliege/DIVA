Program emobath2ghertopo
implicit none

!------------------------------------------------
! Sylvain Watelet - 16/04/2016
! swatelelet@ulg.ac.be
!------------------------------------------------

include "netcdf.inc"

integer::im4,jm4
integer::im,jm,km,ipar
real(kind=4)::hrss,time_val !,valexu
real(kind=8)::valex8
real(kind=4)::offset,scale_factor

        real*8 c8(1)

real(kind=4),dimension(:,:),allocatable::var

real(kind=4),dimension(:),allocatable::X
real(kind=4),dimension(:),allocatable::Y

 character(len=256)::ncfile,myfield
 character(len=256)::err_shname,err_lgname,string256
 character(len=5)::LAT_NAME
 character(len=7)::LON_NAME
!      character*(*) LAT_NAME, LON_NAME
!      parameter (LAT_NAME='lat', LON_NAME='lon')

integer,dimension(3)::dim
integer,dimension(4)::start, count
integer::id1,id2,i,j
integer::timeid,idtime,icdf

integer::lonid,latid,depthid,nvid
integer::idlon,idlat,iddepth
integer::ncid,status
integer::ierr
real(kind=4)::dx,dy

im = im4
jm = jm4
LAT_NAME='LINES'
LON_NAME='COLUMNS'
scale_factor=0.1
offset=-2590.3

call get_command_argument(1,ncfile)
write(*,*) "The netcdf to transform is ", ncfile

call get_command_argument(2,myfield)
!write(*,*) "The field to transform is ", myfield

!------------------------------------------------
! Reading ncfile
!------------------------------------------------
      !-----------------------
      ! Open the data file
      !-----------------------
!
      status = nf_open(TRIM(ncfile), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo open file'
      ENDIF
!
      !----------------------
      !  Inquire dimensions id
      !----------------------
!
      status=nf_inq_dimid(ncid,LON_NAME,lonid)
      IF (status .NE.nf_noerr) LON_NAME="LON"
      status=nf_inq_dimid(ncid,LON_NAME,lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo def lon'
      ENDIF

      status=nf_inq_dimid(ncid,LAT_NAME,latid)
      IF (status .NE.nf_noerr) LAT_NAME="LAT"
      status=nf_inq_dimid(ncid,LAT_NAME,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo def lat'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo dimlen lon'
      ENDIF
      write(6,*) ncid,lonid,IM
!      lonid=1

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo dimlen lat'
      ENDIF
      write(6,*) ncid,latid,JM
!      latid=5
      !-----------------------
      ! Inquire data variables
      !-----------------------

      status=nf_inq_varid(ncid,myfield,id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2topgrd def var'
      ENDIF
!


      ierr = 1

!
      icdf=1
!
      status = nf_sync(ncid)
!
      start(1)=1
      start(2)=1
      start(3)=icdf
      start(4)=1
      count(1)=JM !IM
      count(2)=JM
      count(3)=1
      count(4)=1
      
      allocate(var(IM,JM))
      allocate(X(IM))
      allocate(Y(JM))
!
      status=nf_get_vara_real(ncid,10, start, count,var) ! id1 => ? (hard-coded) !
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in ncdf2gher get var'
      ENDIF
      var=(var*scale_factor)+(offset)
      var=-var ! diva convention (depth positive in ocean)
!     write(6,*) var
!     stop
      start(1)=1
      start(2)=1
      start(3)=icdf
      start(4)=1
      count(1)=IM
      count(2)=1
      count(3)=1
      count(4)=1
      x=0
      status=nf_get_vara_real(ncid,9, start, count,x) ! lonid => 9 (hard-coded) !
!      IF (status .NE.nf_noerr) THEN
!         print *,nf_strerror(status)
!         STOP 'Stopped in ncdf2gher get lon'
!      ENDIF
      write(6,*) 'x',x
      start(1)=1
      start(2)=1
      start(3)=icdf
      start(4)=1
      count(1)=JM
      count(2)=1
      count(3)=1
      count(4)=1
      y=0
      status=nf_get_vara_real(ncid,8, start, count,y) ! latid => 8 (hard-coded) !
!      IF (status .NE.nf_noerr) THEN
!         print *,nf_strerror(status)
!         STOP 'Stopped in ncdf2gher get lat'
!      ENDIF
      write(6,*) 'y',y
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in emobath2ghertopo get var'
      ENDIF
!


!
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF

!------------------------------------------------

!------------------------------------------------
! Writing gher file + info file
!------------------------------------------------

! need to get exclusion value ...
       valex8=1E6

      open(unit=12,file="input/topo_fine.grd",form='unformatted')
      open(unit=13,file="input/TopoInfo_fine.dat",form='formatted')
! Check if uniform grid
      dx=x(2)-x(1)
      dy=y(2)-y(1)
      do i=1,IM-1
      if (abs(x(i+1)-x(i)-dx).gt.0.01*abs(dx)) then
!	write(6,*) x(i+1),x(i),dx,x(i+1)-x(i)-dx,0.01*abs(dx)
         stop 'NON UNIFORM GRID X'
      endif
      enddo
      do i=1,JM-1
      if (abs(y(i+1)-y(i)-dy).gt.0.01*abs(dy)) then
         stop 'NON UNIFORM GRID Y '
      endif
      enddo
! Writing      
      write(13,*)  x(1)
      write(13,*)  y(1)
      write(13,*) x(2)-x(1)
      write(13,*) y(2)-y(1)
      write(13,*) IM
      write(13,*) JM
      write(6,*) 'Finished writing info file'
      call uwritc(12,c8,var,valex8,4,IM,JM,1,IM*JM)
      write(6,*) 'Finished writing binary file'
      close(12)
      close(13)
      stop

End program

      Subroutine UWRITC(iu,c8,c4,valex8,ipre8,imaxc,jmaxc,kmaxc,nbmots)
!                ======
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! writes the field C(I,J,K)  into fortran unit iu 
! writes the field in the array c4 if iprecr=4
! writes the field in the array c8 if iprecr=8
!
! The KBLANC blank lines are at the disposal of the user
! JMB 6/3/92
!
! IF c(i,j,k)=NaN or infinity, it is replaced by VALEX! 
!
! 
! RS 12/1/93
!
! If nbmots = -1  then write only 1 data record
!     (only for non-degenerated data)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      real*8 valex8
      real*4 valexc
! in the calling routin you can specify the following equivalence to
! save memory space:
!      equivalence(c,c4)
!      equivalence(c,c8)
!
! Putting  Valex where not numbers
       z=0.
       un=1.
       ich=0
       ioff=1
       if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

       IF (NBMOTS.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

       do k=1,kmaxc
        do j=1,jmaxc
         do i=1,imaxc
!         if( c4(ioff).eq.(z/z) ) goto 1010 
!         if( c4(ioff).eq.(un/z) ) goto 1010
!         if( c4(ioff).eq.(-z/z) ) goto 1010 
!         if( c4(ioff).eq.(-un/z) ) goto 1010 
         goto 1011
 1010     continue
          c4(ioff)=sngl(valex8)
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
       valexc=SNGL(valex8)
       iprec=4
!
! skip KBLANC lines
!        write(6,*) iu,imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
       do 1 kb=1,KBLANC
        write(iu,ERR=99)
 1     continue
!
        write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!
! compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!
! if pathological case, write only four values C0 and DCI,DCJ,DCK found 
! as the two four elements of the array so that C(I,J,K) =
! C0 + I * DCI + J * DCJ + K * DCK
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!
!
! single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,ir)
                       else
!
! double precision
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
!
         return
 99      continue
         write(*,*) 'Data error in UWRITC, not a conform file'
        write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
        write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
         return
         end
         
