Program ncdf2gher
implicit none

!------------------------------------------------
! Sylvain Watelet - 02/12/2014
! swatelelet@ulg.ac.be
!------------------------------------------------

include "netcdf.inc"

integer::im4,jm4
integer::im,jm,km,ipar
real(kind=4)::hrss,time_val !,valexu
real(kind=8)::valex8

real(kind=4),dimension(:,:),allocatable::var

real(kind=4),dimension(:),allocatable::X
real(kind=4),dimension(:),allocatable::Y

 character(len=256)::ncfile,myfield
 character(len=256)::err_shname,err_lgname,string256
 character(len=3)::LAT_NAME, LON_NAME
!      character*(*) LAT_NAME, LON_NAME
!      parameter (LAT_NAME='lat', LON_NAME='lon')

integer,dimension(3)::dim
integer,dimension(3)::start, count
integer::id1,id2,i,j
integer::timeid,idtime,icdf

integer::lonid,latid,depthid,nvid
integer::idlon,idlat,iddepth
integer::ncid,status
integer::ierr
real(kind=4)::dx,dy

im = im4
jm = jm4
LON_NAME='lon'
LAT_NAME='lat'

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
         STOP 'Stopped in divabath2topogrd open file'
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
         STOP 'Stopped in divabath2topogrd def lon'
      ENDIF

      status=nf_inq_dimid(ncid,LAT_NAME,latid)
      IF (status .NE.nf_noerr) LAT_NAME="LAT"
      status=nf_inq_dimid(ncid,LAT_NAME,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd def lat'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd dimlen lat'
      ENDIF
      !-----------------------
      ! Inquire data variables
      !-----------------------

      status=nf_inq_varid(ncid,"bat",id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd def bat'
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
      count(1)=IM
      count(2)=JM
      count(3)=1
      
      allocate(var(IM,JM))
      allocate(X(IM))
      allocate(Y(JM))
!
      status=nf_get_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd get var'
      ENDIF
!
      start(1)=1
      start(2)=1
      start(3)=icdf
      count(1)=IM
      count(2)=1
      count(3)=1
      status=nf_get_vara_real(ncid,lonid, start, count,x)
      write(6,*) 'x',x
      start(1)=1
      start(2)=1
      start(3)=icdf
      count(1)=JM
      count(2)=1
      count(3)=1
      status=nf_get_vara_real(ncid,latid, start, count,y)
      write(6,*) 'y',y
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in divabath2topogrd get var'
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

      open(unit=12,file="./input/"//trim(myfield)//".grd",form='unformatted')
      open(unit=13,file="./input/"//trim(myfield)//"Info.dat",form='formatted')
! Check if uniform grid
      dx=x(2)-x(1)
      dy=y(2)-y(1)
      do i=1,IM-1
      if (abs(x(i+1)-x(i)-dx).gt.0.0001*abs(dx)) then
         stop 'NON UNIFORM GRID X'
      endif
      enddo
      do i=1,JM-1
      if (abs(y(i+1)-y(i)-dy).gt.0.0001*abs(dy)) then
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
      call uwritc(12,valex8,var,valex8,4,IM,JM,1,IM*JM)
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
         
