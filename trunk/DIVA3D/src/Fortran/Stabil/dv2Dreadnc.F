!
      SUBROUTINE NC_RD2DCL(im4,jm4,var,verr,file_name)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer                             :: im4,jm4
        integer                             :: im,jm,km,ipar
        real*4                              :: hrss,time_val !,valexu
!
        real*4  ,dimension(im4,jm4)        :: var, verr
!   
        real*4  ,dimension(im4)              :: X
        real*4  ,dimension(jm4)              :: Y
!
	character (len=*)     :: file_name !,var_shname
!
	character (len=256)   :: err_shname,err_lgname
     &                         , string256
!
      character*(*) LAT_NAME, LON_NAME
      parameter (LAT_NAME='y', LON_NAME='x')

      integer  ,dimension(3)              :: dim
      integer  ,dimension(3)              :: start, count
      integer                             :: id1,id2
      integer                             :: timeid,idtime, icdf
!
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: ierr
!
      im = im4
      jm = jm4
!!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D open file'
      ENDIF
!
      !----------------------
      !  Inquire dimensions id
      !----------------------
!
      status=nf_inq_dimid(ncid,LON_NAME,lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D def lon'
      ENDIF

      status=nf_inq_dimid(ncid,LAT_NAME,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D def lat'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D dimlen lat'
      ENDIF
      !-----------------------
      ! Inquire data variables
      !-----------------------

      status=nf_inq_varid(ncid,"analyzed_field",id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D def var'
      ENDIF
!
      ierr = 1
      status=nf_inq_varid(ncid,"error_field",id2)
      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         print *,'NC_READ2D no error field present in'
!x         print *,TRIM(file_name)
         ierr = 0
      ENDIF
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
! 
      status=nf_get_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D get var'
      ENDIF
!
      IF(ierr == 1) THEN
      status=nf_get_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ2D get verr'
      ENDIF
      ELSE
      verr = -9999.
      ENDIF
!
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF

      RETURN
      END
