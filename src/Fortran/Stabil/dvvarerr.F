!
      SUBROUTINE NC_RDVARER(im4,jm4,km,var,verr,clo,cla,dep,
     & var_min,var_max,ver_min,ver_max,VALEXC,l_units,
     & file_name,var_shname)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer                             :: im4,jm4
        integer                             :: im,jm,km,ipar
        real*4                              :: valexc, 
     &            var_min,var_max,ver_min,ver_max
        real*4                              :: hrss,time_val
!
        real*4  ,dimension(im4,jm4,km)        :: var, verr
!
!        real*4  ,dimension(2,1)              :: clbnds
!     &                                        ,climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep
!
	character (len=*)     :: file_name,var_shname
	character (len=255)   :: var_units, Ref_time
        character (len=20), DIMENSION(2)           :: l_units
!
	character (len=256)   :: err_shname,err_lgname
     &                         , string256
!
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                         :: id1,id1_1,id1_2,id2,id2_1
     &                                 , id3,id3_1,id4,id2d1,id2d2,id2d3
     &                                 , id0_1,id0_2,id0_3,idcb
      integer                             :: timeid,idtime, icdf
!
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      im = im4
      jm = jm4
!
      write(*,*)'file_name2 ',LEN_TRIM(file_name),TRIM(file_name)
      write(*,*)'**************************************************'
      write(*,*)'var_shname ',LEN_TRIM(var_shname),TRIM(var_shname)
      write(*,*)'**************************************************'
!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr open file'
      ENDIF
!
      !----------------------
      !  Inquire dimensions id
      !----------------------
!
      status=nf_inq_dimid(ncid,'lon',lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def lon'
      ENDIF

      status=nf_inq_dimid(ncid,'lat',latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def lat'
      ENDIF

      status=nf_inq_dimid(ncid,'depth',depthid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def depth'
      ENDIF
!
      status=nf_inq_dimid(ncid,'time',timeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr dimid time'
      ENDIF

      status=nf_inq_dimid(ncid, 'nv', nvid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def nvid'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr dimlen lat'
      ENDIF

      status=nf_inq_dimlen(ncid,depthid,KM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr dimlen depth'
      ENDIF
!
      !----------------------------
      ! Inquire coordinate variables
      !----------------------------
!
      status=nf_inq_varid(ncid,'lon',idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def  var lon'
      ENDIF

      status=nf_inq_varid(ncid,'lat',idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def var lat'
      ENDIF

      status=nf_inq_varid(ncid,'depth',iddepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def var depth'
      ENDIF

      status=nf_inq_varid(ncid,'time',idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def var time'
      ENDIF
!
      !-----------------------------------------     
      ! Get coordinate variables attributes
      !-----------------------------------------
!
      status=nf_get_att_text(ncid,idlon,'units',l_units(1))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att lon'
      ENDIF

      status=nf_get_att_text(ncid,idlat,'units',l_units(2))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att lat'
      ENDIF
!
!===================================================
!
      status=nf_inq_varid(ncid,TRIM(var_shname),id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def var'
      ENDIF
!
      status=nf_get_att_text(ncid,id1,'units',var_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att units'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_min',var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att varmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_max',var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att varmax'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'missing_value',valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att valexc'
      ENDIF
!
!----------------------------------------------------------------
!
      WRITE(err_shname,'(a,"_err")')TRIM(var_shname)
!--------------------

      status=nf_inq_varid(ncid,TRIM(err_shname),id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr def err3'
      ENDIF

!
      status=nf_get_att_real(ncid,id2,'valid_min',ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att vermin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2,'valid_max',ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr att vermax'
      ENDIF
!
!----------------------------------------------------------------
!
      status = nf_get_var_real(ncid,idlon,clo)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr get clo'
      ENDIF
!
      status = nf_get_var_real(ncid,idlat,cla)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr get cla'
      ENDIF
!
      status = nf_get_var_real(ncid,iddepth,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr get dep'
      ENDIF
!
      icdf=1
!
      status = nf_sync(ncid)
!
      status = nf_get_var1_real(ncid, idtime, icdf, hrss)
      time_val = hrss
!
      start(1)=1
      start(2)=1
      start(3)=1
      start(4)=icdf
      count(1)=IM
      count(2)=JM
      count(3)=KM
      count(4)=1
! 
      status=nf_get_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr get var'
      ENDIF

      status=nf_get_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_varverr get verr'
      ENDIF
!
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF

      RETURN
      END
