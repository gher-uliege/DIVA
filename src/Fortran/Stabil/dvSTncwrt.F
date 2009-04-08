!
      SUBROUTINE NC_4DCLIM(im4,jm4,km,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,
     &  clo,cla,dep,CORLEN,SN,VARBAK,valexu,
     &  var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &  var1_min,var1_max,var2_min,var2_max,verel_min,verel_max,
     &  obin_min,obin_max,file_name,var_shname,var_lgname,
     &  var_units,vrb_units,l_units,title_string,cellmeth,Ref_time)
! !
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer*4                           :: im4,jm4
!!        integer   :: im4,jm4 
        integer                             :: im,jm,km,time_len,ipar
!
        real*4                              :: valexu, 
     &               var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &               var1_min,var1_max,clen_min,clen_max,
     &               var2_min,var2_max,verel_min,verel_max,
     &               obin_min,obin_max,u_min,u_max,v_min,v_max
        real*4                              :: hrss,time_val
!
        real*4  ,dimension(im4,jm4,km)        :: var, var1, var2
     &                                       , Verr,reler,dbins,obins
!
        real*4  ,dimension(2,time_len)      :: clbnds !,climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)              :: dep
     &                                       , CORLEN, SN, VARBAK
!
	character (len=*)     :: file_name, title_string, Ref_time
     &                         , var_shname,var_lgname,var_units
     &                         , vrb_units,cellmeth
        character (len=*), DIMENSION(2)           :: l_units
	character (len=256)   :: err_shname,err_lgname
     &                         , var1_shname,var1_lgname
     &                         , var2_shname,var2_lgname
     &                         , rer_shname,rer_lgname
     &                         , string256
!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(4)              :: dim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(4)              :: start, count
      integer                             :: id1,id1_1,id1_2,id2,id2_1
     &                                     , id3,id3_1,id4,id5,id6
     &                                     , id0_1,id0_2,id0_3,idcb
      integer                             :: timeid,idtime, icdf
!
      integer                 :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
!x      save                    :: id1,id1_1,id1_2,id2,id2_1
!x     &                         , id3,id3_1,id4,id5,id6
!x     &                         , id0_1,id0_2,id0_3,idcb
!x      save                    :: timeid,idtime, icdf
!x      save                    :: lonid,latid,depthid,nvid
!x      save                    :: idlon,idlat,iddepth
!x      save                    :: ncid,status
!
!-------------------------------------------------------------------
!
      WRITE(err_shname,'(a,".err")')TRIM(var_shname)
      WRITE(var1_shname,'(a,".L1")')TRIM(var_shname)
      WRITE(var2_shname,'(a,".L2")')TRIM(var_shname)
      WRITE(rer_shname,'(a,".relerr")')TRIM(var_shname)
!
      WRITE(err_lgname,
     &       '("Standard deviation error of ",a)')TRIM(var_lgname)
!
      WRITE(rer_lgname,'("Relative error of ",a)')TRIM(var_lgname)
!
      WRITE(var1_lgname,
     &  '("First threshold masked fields of ",a)')TRIM(var_lgname)
!
      WRITE(var2_lgname,
     &  '("Second threshold masked fields of ",a)')TRIM(var_lgname)
!
!-------------------------------------------------------------------
!
      im = im4
      jm = jm4
!
!
      !-----------------------
      ! create the data file
      !-----------------------
!
      status = nf_create(TRIM(file_name), nf_share,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO create file'
      ENDIF
!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_write,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO open file'
      ENDIF
!
      !----------------------
      ! Put in define mode
      !----------------------
!      
      status = nf_redef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO redef'
      ENDIF
!
      !----------------------
      ! Define (check Fillmode)
      !----------------------
!
      status = nf_set_fill(ncid, nf_fill, OLDMOD)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO fillmod'
      ENDIF
!
      !----------------------
      ! Define dimensions
      !----------------------
!
      status=nf_def_dim(ncid,'lon',IM,lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def lon'
      ENDIF

      status=nf_def_dim(ncid,'lat',JM,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def lat'
      ENDIF

      status=nf_def_dim(ncid,'depth',KM,depthid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def depth'
      ENDIF
!NF_UNLIMITED
      status=nf_def_dim(ncid,'time',time_len,timeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def time'
      ENDIF

      status=nf_def_dim(ncid, 'nv', 2, nvid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def nvid'
      ENDIF

      !----------------------------
      ! Define coordinate variables
      !----------------------------
!
      status=nf_def_var(ncid,'lon',nf_float,1, lonid,idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def  var lon'
      ENDIF

      status=nf_def_var(ncid,'lat',nf_float,1,latid ,idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var lat'
      ENDIF

      status=nf_def_var(ncid,'depth',nf_float,1,depthid ,iddepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var depth'
      ENDIF

      status=nf_def_var(ncid,'time',nf_float,1,timeid ,idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var time'
      ENDIF
!
      if(ipar == 1) then

      cbdim(1)=nvid
      cbdim(2)=timeid
      status=nf_def_var(ncid,'climatology_bounds',nf_float,
     &                                           2,cbdim,idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def clbnds'
      ENDIF
!
      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
     &                            nf_float,2*time_len,clbnds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put clbnds'
      ENDIF

      ENDIF
!
      !-----------------------------------------     
      ! Give attributes to coordinate variables 
      !-----------------------------------------
!
      status=nf_put_att_text(ncid,idlon,'units',
     &                  LEN_TRIM(l_units(1)),TRIM(l_units(1)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lon'
      ENDIF

      status=nf_put_att_text(ncid,idlat,'units',
     &                  LEN_TRIM(l_units(2)),TRIM(l_units(2)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lat'
      ENDIF

      status=nf_put_att_text(ncid,iddepth,'units',LEN_TRIM('meters'),
     &                                                     'meters')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att depth units'
      ENDIF

      status=nf_put_att_text(ncid,iddepth,'positive',LEN_TRIM('down'),
     &                                                        'down')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att depth positive'
      ENDIF

      status = nf_put_att_text(ncid, timeid, 'units',
     &              LEN_TRIM(Ref_time),TRIM(Ref_time))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def timeunits'
      ENDIF

      status = nf_put_att_text(ncid, timeid, 'climatology',
     &        LEN_TRIM('climatology_bounds'),'climatology_bounds')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def climbnds'
      ENDIF

!
      !-----------------------
      ! Define data variables
      !-----------------------
!
      cbdim(1)=depthid
      cbdim(2)=timeid
!
      status=nf_def_var(ncid,'CORLEN',nf_float,2,cbdim,id0_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var0_1'
      ENDIF
      string256='Correlation Length'
      status=nf_put_att_text(ncid,id0_1,'long_name',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_1 lgname'
      ENDIF
      status=nf_put_att_text(ncid,id0_1,'units',
     &                  LEN_TRIM(l_units(2)),TRIM(l_units(2)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_1 units'
      ENDIF
!
      status=nf_def_var(ncid,'SN',nf_float,2,cbdim,id0_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var0_2'
      ENDIF
      string256='Signal to Noise'
      status=nf_put_att_text(ncid,id0_2,'long_name',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_2 lgname'
      ENDIF
      string256=' '
      status=nf_put_att_text(ncid,id0_2,'units',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_2 units'
      ENDIF
!
      status=nf_def_var(ncid,'VARBAK',nf_float,2,cbdim,id0_3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var0_3'
      ENDIF
      string256='Background Field Variance'
      status=nf_put_att_text(ncid,id0_3,'long_name',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_3 lgname'
      ENDIF
!
      status=nf_put_att_text(ncid,id0_3,'units',
     &                 LEN_TRIM(vrb_units),TRIM(vrb_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_3 units'
      ENDIF
!
!===================================================
!
      dim(1)=lonid
      dim(2)=latid
      dim(3)=depthid
      dim(4)=timeid
!
      status=nf_def_var(ncid,TRIM(var_shname),nf_float,4,dim,id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var'
      ENDIF
!
      status=nf_put_att_text(ncid,id1,'long_name',
     &                 LEN_TRIM(var_lgname),TRIM(var_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',
     &                                   nf_float,1,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',
     &                                   nf_float,1,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
      status=nf_put_att_real(ncid,id1,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 Fillvalue'
      ENDIF

      status= nf_put_att_text(ncid,id1 , 'cell_methods',
     &                 LEN_TRIM(cellmeth),TRIM(cellmeth))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 Fillvalue'
      ENDIF

!
      status=nf_put_att_real(ncid,id1,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_def_var(ncid,TRIM(var1_shname),nf_float,4,dim,id1_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var1'
      ENDIF

      status=nf_put_att_text(ncid,id1_1,'long_name',
     &                 LEN_TRIM(var1_lgname),TRIM(var1_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v1lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1_1,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att var1units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_min',
     &                                nf_float,1,var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_max',
     &                                nf_float,1,var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
      status=nf_put_att_real(ncid,id1_1,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'missing_value',
     &                                 nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv1'
      ENDIF
!--------------------

      status=nf_def_var(ncid,TRIM(var2_shname),nf_float,4,dim,id1_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err2'
      ENDIF

      status=nf_put_att_text(ncid,id1_2,'long_name',
     &                 LEN_TRIM(var2_lgname),TRIM(var2_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF

      status=nf_put_att_text(ncid,id1_2,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_min',
     &                                   nf_float,1,var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2min'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_max',
     &                                   nf_float,1,var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2max'
      ENDIF
      status=nf_put_att_real(ncid,id1_2,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv2'
      ENDIF
!--------------------

      status=nf_def_var(ncid,TRIM(err_shname),nf_float,4,dim,id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err3'
      ENDIF

      status=nf_put_att_text(ncid,id2,'long_name',
     &                 LEN_TRIM(err_lgname),TRIM(err_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF

      status=nf_put_att_text(ncid,id2,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errunits'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_min',
     &                                   nf_float,1,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',
     &                                   nf_float,1,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax'
      ENDIF
      status=nf_put_att_real(ncid,id2,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!-----------------------

      status=nf_def_var(ncid,TRIM(rer_shname),nf_float,4,dim,id2_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def relerr'
      ENDIF

      status=nf_put_att_text(ncid,id2_1,'long_name',
     &                 LEN_TRIM(rer_lgname),TRIM(rer_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF
      string256=' '
      status=nf_put_att_text(ncid,id2_1,'units',
     &                 LEN_TRIM(string256),TRIM(string256))

      status=nf_put_att_real(ncid,id2_1,'valid_min',
     &                                   nf_float,1,verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_max',
     &                                   nf_float,1,verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmax'
      ENDIF
      status=nf_put_att_real(ncid,id2_1,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexrer'
      ENDIF
!
!----------------------------------------------------------------
!

      status=nf_def_var(ncid,'databins',nf_float,4,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def dbins'
      ENDIF

      status=nf_put_att_text(ncid,id3,'long_name',
     &        LEN_TRIM('logarithm10 of number of data in bins'),
     &                 'logarithm10 of number of data in bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dblgname'
      ENDIF

! 
      status=nf_put_att_real(ncid,id3,'valid_min',
     &                                   nf_float,1,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',
     &                                   nf_float,1,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
      status=nf_put_att_real(ncid,id3,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexdb'
      ENDIF
!
      status=nf_def_var(ncid,'outlbins',nf_float,4,dim,id3_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def obins'
      ENDIF

      status=nf_put_att_text(ncid,id3_1,'long_name',
     &   LEN_TRIM('logarithm10 of number of outliers data in bins'),
     &            'logarithm10 of number of outliers data in bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att oblgname'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3_1,'valid_min',
     &                                   nf_float,1,obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'valid_max',
     &                                   nf_float,1,obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmax'
      ENDIF
      status=nf_put_att_real(ncid,id3_1,'_FillValue',
     &                                   nf_float,1,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'missing_value',
     &                                   nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexob'
      ENDIF
!
!----------------------------------------------------------------
!
!
!----------------------------------------------------------------
!
      !----------------------
      !Put global attributes
      !----------------------
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'title',
     &                 LEN_TRIM(title_string),TRIM(title_string))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att title'
      ENDIF
!
        string256='CF-1.0'
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Conventions',
     &	                    LEN_TRIM(string256),string256)
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'file_name',
     &                 LEN_TRIM(file_name),TRIM(file_name))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO glofilename'
      ENDIF
!
      string256='SeaDataNet'
      status=nf_put_att_text(ncid,NF_GLOBAL,'project',
     &                 LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO sdn'
      ENDIF
!
      string256='ULg_GHER'
      status=nf_put_att_text(ncid,NF_GLOBAL,'institution',
     &                 LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO ulg'
      ENDIF
!
       string256='DIVA group'
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'production',
     &	                 LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO diva'
      ENDIF
!
      !--------------------
      !End define mode
      !--------------------
!
      status = nf_enddef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO enddef'
      ENDIF
!
      status = nf_put_var_real(ncid,idlon,clo)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put clo'
      ENDIF
!
      status = nf_put_var_real(ncid,idlat,cla)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put cla'
      ENDIF
!
      status = nf_put_var_real(ncid,iddepth,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put dep'
      ENDIF
!
      status = nf_sync(ncid)
!
!===================================================
!
      icdf=1
!
      status = nf_sync(ncid)
!
      hrss = time_val
      status = nf_put_var1_real(ncid, idtime, icdf, hrss)
!
      stpar(1)=1
      stpar(2)=icdf
      prcnt(1)=KM
      prcnt(2)=1

      status = nf_put_vara_real(ncid,id0_1, stpar, prcnt,CORLEN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put CORLEN'
      ENDIF
!
      status = nf_put_vara_real(ncid,id0_2, stpar, prcnt,SN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put SN'
      ENDIF
!
      status = nf_put_vara_real(ncid,id0_3, stpar, prcnt,VARBAK)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put VARBAK'
      ENDIF
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
      status=nf_put_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var'
      ENDIF
!
      status=nf_put_vara_real(ncid,id1_1, start, count,var1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var1'
      ENDIF

      status=nf_put_vara_real(ncid,id1_2, start, count,var2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var2'
      ENDIF

      status=nf_put_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put verr'
      ENDIF

      status=nf_put_vara_real(ncid,id2_1, start, count,reler)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put reler'
      ENDIF
!
      status=nf_put_vara_real(ncid,id3, start, count,dbins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put dbins'
      ENDIF
!
      status=nf_put_vara_real(ncid,id3_1, start, count,obins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put obins'
      ENDIF
! 
      RETURN
      END
