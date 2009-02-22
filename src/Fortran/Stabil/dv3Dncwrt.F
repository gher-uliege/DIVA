!
      SUBROUTINE NC_4DCLIM(im4,jm4,km,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,
     &  clo,cla,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min,var_max,vbt_max,vbt_min,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,obin_min,obin_max,rl_min,rl_max,VALEXC,l_units,
     &  L_file_name,file_name,L_var_shname,var_shname,
     &  L_var_lgname,var_lgname,L_var_units,var_units,
     &  L_vrb_units,vrb_units,L_title_string,title_string,
     &  L_cellmeth,cellmeth,L_Ref_time,Ref_time,
     &  L_INSTITUT,INSTITUT,L_PRODUCTION,PRODUCTION,L_SOURCE,SOURCE,
     &  L_COMMENT,COMMENT)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer*4   :: im4,jm4 
!!        integer   :: im4,jm4 
        integer     :: one,two,foor,ttime_len,km ,time_len,ipar,im,jm
     &              , three, valexc 
        integer     :: L_file_name,L_var_shname,L_var_lgname,
     &  L_var_units,L_vrb_units,L_title_string,L_cellmeth,L_Ref_time,
     &  L_INSTITUT,L_PRODUCTION,L_SOURCE,L_COMMENT
!
        real*4                              :: 
     &            var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &            vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,
     &            var2_min,var2_max,verel_min,verel_max,
     &            obin_min,obin_max,rl_min,rl_max
        real*4                              :: hrss,time_val
!
        real*4  ,dimension(im4,jm4,km)        :: var, var1, var2
     &                                       , Verr,reler,dbins
     &                                       , obins,rlfield
        real*4  ,dimension(im4,jm4)          :: varbot
        real*4  ,dimension(2,time_len)       :: clbnds 
!     &                                      , climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep, valexu
     &                                       , CORLEN, SN, VARBAK
!
	character (len=L_file_name)        :: file_name
	character (len=L_title_string)     :: title_string
	character (len=L_Ref_time)         :: Ref_time
	character (len=L_var_shname)       :: var_shname
	character (len=L_var_lgname)       :: var_lgname
	character (len=L_var_units)        :: var_units
	character (len=L_INSTITUT)         :: INSTITUT
	character (len=L_PRODUCTION)       :: PRODUCTION
	character (len=L_SOURCE)           :: SOURCE
	character (len=L_COMMENT)          :: COMMENT
	character (len=L_vrb_units)        :: vrb_units
	character (len=L_cellmeth)         :: cellmeth
        character (len=*), DIMENSION(2)   :: l_units
	character (len=256)   :: err_shname,err_lgname
     &                         , var1_shname,var1_lgname
     &                         , var2_shname,var2_lgname
     &                         , rer_shname,rer_lgname
     &                         , vbt_shname,vbt_lgname
     &                         , string256
!
	character (len=12)   :: Real_clock
!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                        :: id1,id1_1,id1_2,id2,id2_1
     &                                , id3,id3_1,id4,id2d1
     &                                , id0_1,id0_2,id0_3,idcb
      integer                        :: timeid,idtime, icdf
!
      integer                      :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      save                    :: id1,id1_1,id1_2,id2,id2_1
     &                         , id3,id3_1,id4,id2d1
     &                         , id0_1,id0_2,id0_3,idcb
      save                    :: timeid,idtime, icdf
      save                    :: lonid,latid,depthid,nvid
      save                    :: idlon,idlat,iddepth
      save                    :: ncid,status
!
      data icdf /0/
!-------------------------------------------------------------------
!
      one = 1
      two = 2
      three= 3
      foor= 4
      im   = im4
      jm   = jm4
!
      if (icdf == 0) then
!
      WRITE(err_shname,'(a,".err")')TRIM(var_shname)
      WRITE(vbt_shname,'(a,".bottom")')TRIM(var_shname)
      WRITE(var1_shname,'(a,".L1")')TRIM(var_shname)
      WRITE(var2_shname,'(a,".L2")')TRIM(var_shname)
      WRITE(rer_shname,'(a,".relerr")')TRIM(var_shname)
!
      WRITE(err_lgname,
     &       '("Standard deviation error of ",a)')TRIM(var_lgname)
!
      WRITE(vbt_lgname,'("deepest values of ",a)')TRIM(var_lgname)
!
      WRITE(rer_lgname,'("Relative error of ",a)')TRIM(var_lgname)
!
      WRITE(var1_lgname,
     &  '("First threshold masked fields of ",a)')TRIM(var_lgname)
!
      WRITE(var2_lgname,
     &  '("Second threshold masked fields of ",a)')TRIM(var_lgname)
!
      valexu(:) = valexc
!-------------------------------------------------------------------
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
         print *,ncid,lonid,IM,status
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
      two=2
      status=nf_def_dim(ncid, 'nv', two, nvid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def nvid'
      ENDIF

      !----------------------------
      ! Define coordinate variables
      !----------------------------
!
      status=nf_def_var(ncid,'lon',nf_float,one, lonid,idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def  var lon'
      ENDIF

      status=nf_def_var(ncid,'lat',nf_float,one,latid ,idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var lat'
      ENDIF

      status=nf_def_var(ncid,'depth',nf_float,one,depthid ,iddepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var depth'
      ENDIF

      status=nf_def_var(ncid,'time',nf_float,one,timeid ,idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var time'
      ENDIF
!
      if(ipar == 1) then

      cbdim(1)=timeid
      cbdim(2)=nvid
      status=nf_def_var(ncid,'climatology_bounds',nf_float,
     &                                           two,cbdim,idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def clbnds'
      ENDIF
!
      ttime_len=2*time_len
      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
     &                            nf_float,ttime_len,clbnds)
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

      string256='climatology_bounds'
      status = nf_put_att_text(ncid, timeid, 'climatology',
     &                 LEN_TRIM(string256),TRIM(string256))
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
      status=nf_def_var(ncid,'CORLEN',nf_float,two,cbdim,id0_1)
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
      status=nf_def_var(ncid,'SN',nf_float,two,cbdim,id0_2)
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
      status=nf_def_var(ncid,'VARBAK',nf_float,two,cbdim,id0_3)
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
      dim2(1)=lonid
      dim2(2)=latid
      dim2(3)=timeid
!
      three = 3
      status=nf_def_var(ncid,TRIM(vbt_shname),nf_float,
     &                                             three,dim2,id2d1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def vbt'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d1,'long_name',
     &                 LEN_TRIM(vbt_lgname),TRIM(vbt_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d1,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_min',
     &                                   nf_float,one,vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_max',
     &                                   nf_float,one,vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt max'
      ENDIF
      status=nf_put_att_real(ncid,id2d1,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt Fillvalue'
      ENDIF

!x      status= nf_put_att_text(ncid,id2d1 , 'cell_methods',
!x     &                 LEN_TRIM(cellmeth),TRIM(cellmeth))
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_4DCLIMATO vbt clmt'
!x      ENDIF

!
!===============================================
!
      dim(1)=lonid
      dim(2)=latid
      dim(3)=depthid
      dim(4)=timeid
!
      foor = 4
      status=nf_def_var(ncid,TRIM(var_shname),nf_float,foor,dim,id1)
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
     &                                   nf_float,one,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',
     &                                   nf_float,one,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmax'
      ENDIF
      status=nf_put_att_real(ncid,id1,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
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
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
!
!===================================================
!
      status=nf_def_var(ncid,TRIM(var1_shname),nf_float,foor,dim,id1_1)
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
     &                                nf_float,one,var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_max',
     &                                nf_float,one,var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
      status=nf_put_att_real(ncid,id1_1,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'missing_value',
     &                                 nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv1'
      ENDIF


      status=nf_def_var(ncid,TRIM(var2_shname),nf_float,foor,dim,id1_2)
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
     &                                   nf_float,one,var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2min'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_max',
     &                                   nf_float,one,var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2max'
      ENDIF
      status=nf_put_att_real(ncid,id1_2,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv2'
      ENDIF
!--------------------

      status=nf_def_var(ncid,TRIM(err_shname),nf_float,foor,dim,id2)
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
     &                                   nf_float,one,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',
     &                                   nf_float,one,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax'
      ENDIF
      status=nf_put_att_real(ncid,id2,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!-----------------------

      status=nf_def_var(ncid,TRIM(rer_shname),nf_float,foor,dim,id2_1)
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
     &                                   nf_float,one,verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_max',
     &                                   nf_float,one,verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmax'
      ENDIF
      status=nf_put_att_real(ncid,id2_1,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexrer'
      ENDIF
!
!----------------------------------------------------------------
!

      status=nf_def_var(ncid,'databins',nf_float,foor,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def dbins'
      ENDIF

      status=nf_put_att_text(ncid,id3,'long_name',
     &     LEN_TRIM('logarithm10 of number of data in bins'),
     &              'logarithm10 of number of data in bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dblgname'
      ENDIF

! 
      status=nf_put_att_real(ncid,id3,'valid_min',
     &                                   nf_float,one,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',
     &                                   nf_float,one,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
      status=nf_put_att_real(ncid,id3,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexdb'
      ENDIF
!
      status=nf_def_var(ncid,'outlbins',nf_float,foor,dim,id3_1)
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
     &                                   nf_float,one,obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'valid_max',
     &                                   nf_float,one,obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmax'
      ENDIF
      status=nf_put_att_real(ncid,id3_1,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexob'
      ENDIF
!
      status=nf_def_var(ncid,'CLfield',nf_float,foor,dim,id4)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def rlfield'
      ENDIF

      status=nf_put_att_text(ncid,id4,'long_name',
     &     LEN_TRIM('Correlation lenght field'),
     &              'Correlation lenght field')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rllgname'
      ENDIF
! 
      status=nf_put_att_real(ncid,id4,'valid_min',
     &                                   nf_float,one,rl_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_max',
     &                                   nf_float,one,rl_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlbmax'
      ENDIF
      status=nf_put_att_real(ncid,id4,'_FillValue',
     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id4 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'missing_value',
     &                                   nf_float,one,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexdb'
      ENDIF
!


!----------------------------------------------------------------
!
      !----------------------
      !Put global attributes
      !----------------------
!
        string256='CF-1.0'
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Conventions',
     &	                    LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO conv'
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
      status=nf_put_att_text(ncid,NF_GLOBAL,'institution',
     &                 LEN_TRIM(INSTITUT),INSTITUT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO ulg'
      ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'production',
     &	                 LEN_TRIM(PRODUCTION),PRODUCTION)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att prod'
      ENDIF
!
      CALL DATE_AND_TIME(Real_clock) 
      STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'date',
     &	                 LEN_TRIM(Real_clock),Real_clock)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att date'
      ENDIF
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'title',
     &                 LEN_TRIM(title_string),TRIM(title_string))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att title'
      ENDIF
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'file_name',
     &                 LEN_TRIM(file_name),TRIM(file_name))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO glofilename'
      ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'source',
     &	                 LEN_TRIM(SOURCE),SOURCE)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att source'
      ENDIF

        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'comment',
     &	                 LEN_TRIM(COMMENT),COMMENT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att comment'
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
      else                  !   icdf=0
!
      if (icdf == time_len-1) then
!
!
      status=nf_put_att_real(ncid,id2d1,'valid_min',
     &                                   nf_float,one,vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbtmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_max',
     &                                   nf_float,one,vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbtmax 2'
      ENDIF

      status=nf_put_att_real(ncid,id1,'valid_min',
     &                                   nf_float,one,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',
     &                                   nf_float,one,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_min',
     &                                nf_float,one,var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_max',
     &                                nf_float,one,var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_min',
     &                                   nf_float,one,var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2min 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_max',
     &                                   nf_float,one,var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2max 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_min',
     &                                   nf_float,one,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',
     &                                   nf_float,one,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_min',
     &                                   nf_float,one,verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_max',
     &                                   nf_float,one,verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3,'valid_min',
     &                                   nf_float,one,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',
     &                                   nf_float,one,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3_1,'valid_min',
     &                                   nf_float,one,obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'valid_max',
     &                                   nf_float,one,obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id4,'valid_min',
     &                                   nf_float,one,rl_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_max',
     &                                   nf_float,one,rl_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmax 2'
      ENDIF
!
      ttime_len=2*time_len
      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
     &                            nf_float,ttime_len,clbnds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put clbnds'
      ENDIF
!
      endif                  !   icdf=time_len-1
!
      endif                  !   icdf=0
!
      icdf=icdf+1
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
      star2(1)=1
      star2(2)=1
      star2(3)=icdf
      coun2(1)=IM
      coun2(2)=JM
      coun2(3)=1
! 
      status=nf_put_vara_real(ncid,id2d1, star2, coun2,varbot)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var'
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
      status=nf_put_vara_real(ncid,id4, start, count,rlfield)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put rlfield'
      ENDIF
! 
      RETURN
      END
