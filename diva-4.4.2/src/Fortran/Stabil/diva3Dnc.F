      SUBROUTINE NC_3DFILE
     &(var,verr,dbins,clo,cla,im4,jm4,km,dep,valexu,
     &var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &file_name,var_shname,var_lgname,
     &err_shname,err_lgname,var_units, title_string)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer*4                           :: im4,jm4
!!        integer                             :: im4,jm4
        integer                             :: im,jm,km
        real*4                              :: valexu, 
     &var_min,var_max,ver_min,ver_max,dbin_min,dbin_max
!
        real*4  ,dimension(im4,jm4,km)        :: var,verr,dbins
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)              :: dep
!
	character (len=*)     :: file_name, title_string
     &                         , var_shname,var_lgname,var_units
     &                         , err_shname,err_lgname
!
	integer  ,dimension(3)              :: dim
	integer                             :: id1,id2,id3
	integer  ,dimension(3)              :: start, count
!
	integer                      :: lonid,latid,levid
	integer                      :: idlon,idlat,idlev
        integer                      :: ncid,status
        integer                      :: OLDMOD
!
!-------------------------------------------------------------------
!
      im = im4
      jm = jm4
!
      !-----------------------
      ! create the data file
      !-----------------------
!
      status = nf_create(TRIM(file_name), nf_share,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE create file'
      ENDIF
!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_write,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE open file'
      ENDIF
!
      !----------------------
      ! Put in define mode
      !----------------------
!      
      status = nf_redef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE redef'
      ENDIF
!
      !----------------------
      ! Define (check Fillmode)
      !----------------------
!
      status = nf_set_fill(ncid, nf_fill, OLDMOD)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE fill mod'
      ENDIF
!
      !----------------------
      ! Define dimensions
      !----------------------
!
      status=nf_def_dim(ncid,'lon',IM,lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def lon'
      ENDIF

      status=nf_def_dim(ncid,'lat',JM,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def lat'
      ENDIF

      status=nf_def_dim(ncid,'lev',KM,levid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def lev'
      ENDIF

!
      !----------------------------
      ! Define coordinate variables
      !----------------------------
!
      status=nf_def_var(ncid,'lon',nf_float,1, lonid,idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def  var lon'
      ENDIF

      status=nf_def_var(ncid,'lat',nf_float,1,latid ,idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def var lat'
      ENDIF

      status=nf_def_var(ncid,'lev',nf_float,1,levid ,idlev)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def var lev'
      ENDIF
!
      !-----------------------------------------     
      ! Give attributes to coordinate variables 
      !-----------------------------------------
!
      status=nf_put_att_text(ncid,idlon,'units',
     &              LEN_TRIM('degrees_east'),'degrees_east')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lon'
      ENDIF

      status=nf_put_att_text(ncid,idlat,'units',
     &               LEN_TRIM('degrees_north'),'degrees_north')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lat'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'units',LEN_TRIM('meters'),
     &                                                   'meters')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lev units'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'positive',LEN_TRIM('down'),
     &                                                      'down')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lev positive'
      ENDIF
!
      !-----------------------
      ! Define data variables
      !-----------------------
!
      dim(1)=lonid
      dim(2)=latid
      dim(3)=levid
!
      status=nf_def_var(ncid,TRIM(var_shname),nf_float,3,dim,id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def var'
      ENDIF

      status=nf_put_att_text(ncid,id1,'long_name',
     &                 LEN_TRIM(var_lgname),TRIM(var_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',
     &                                   nf_float,1,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',
     &                                   nf_float,1,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
!
      status=nf_def_var(ncid,TRIM(err_shname),nf_float,3,dim,id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def err'
      ENDIF

      status=nf_put_att_text(ncid,id2,'long_name',
     &                 LEN_TRIM(err_lgname),TRIM(err_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att errlgname'
      ENDIF

!x      status=nf_put_att_text(ncid,id2,'units',
!x     &                 LEN_TRIM(var_units),TRIM(var_units))
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_3DFILE att errunits'
!x      ENDIF

!
      status=nf_put_att_real(ncid,id2,'valid_min',
     &                                   nf_float,1,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',
     &                                   nf_float,1,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valexerr'
      ENDIF

      status=nf_def_var(ncid,'dbins',nf_float,3,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def err'
      ENDIF

      status=nf_put_att_text(ncid,id3,'long_name',
     &              LEN_TRIM('data bins'),'data bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att errlgname'
      ENDIF

! 
      status=nf_put_att_real(ncid,id3,'valid_min',
     &                                   nf_float,1,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',
     &                                   nf_float,1,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!x      status=nf_put_att_text(ncid,id3,'units',
!x     &                 LEN_TRIM(var_units),TRIM(var_units))
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_3DFILE att errunits'
!x      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valexerr'
      ENDIF
!
      !----------------------
      !Put global attributes
      !----------------------
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'title',
     &                 LEN_TRIM(title_string),TRIM(title_string))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att title'
      ENDIF
!
      !--------------------
      !End define mode
      !--------------------
!
      status = nf_enddef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE enddef'
      ENDIF
!
      status = nf_put_var_real(ncid,idlon,clo)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put clo'
      ENDIF
!
      status = nf_put_var_real(ncid,idlat,cla)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put cla'
      ENDIF
!
      status = nf_put_var_real(ncid,idlev,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put dep'
      ENDIF
!
      status = nf_sync(ncid)
!
!        
      start(1)=1
      start(2)=1
      start(3)=1
      count(1)=IM
      count(2)=JM
      count(3)=KM
!
      status=nf_put_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put var'
      ENDIF
!
      status=nf_put_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put verr'
      ENDIF
!
      status=nf_put_vara_real(ncid,id3, start, count,dbins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE put dbins'
      ENDIF
! 
      RETURN
      END
