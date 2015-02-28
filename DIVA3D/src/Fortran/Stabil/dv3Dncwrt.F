!time_Yval,xclbnds,
      SUBROUTINE NC_4DCLIM(
     &  im4,jm4,km,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  clo,cla,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &  obin_min,obin_max,rl_min,rl_max,VALEXC,l_units,
     &  L_file_name,file_name,L_var_shname,var_shname,
     &  L_var_lgname,var_lgname,L_var_units,var_units,
     &  L_vrb_units,vrb_units,L_title_string,title_string,
     &  L_cellmeth,cellmeth,L_Ref_time,Ref_time,
     &  L_INSTITUT,INSTITUT,L_PRODUCTION,PRODUCTION,L_SOURCE,SOURCE,
     &  L_Email,Email,L_COMMENT,COMMENT)
!
       use divaio
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer*4   :: im4,jm4 
!!        integer   :: im4,jm4 
        integer     :: one,two,four,ttime_len,km ,time_len,ipar,im,jm
     &              , three
        integer     :: L_file_name,L_var_shname,L_var_lgname,
     &  L_var_units,L_vrb_units,L_title_string,L_cellmeth,L_Ref_time,
     &  L_INSTITUT,L_PRODUCTION,L_SOURCE,L_Email,L_COMMENT
!
        real*4                              :: 
     &            var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &            vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,
     &            var2_min,var2_max,verel_min,verel_max,
     &            vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &            obin_min,obin_max,rl_min,rl_max
        real*4                       :: hrss,time_val,time_Yval, valexc
!
        real*4  ,dimension(im4,jm4,km)        :: var, var1, var2
     &                                       , Verr,reler,dbins
     &                                       , obins,rlfield
        real*4  ,dimension(im4,jm4)          :: varbot,varb1,varb2
        real*4  ,dimension(2,time_len)       :: clbnds ,xclbnds 
        real*4  ,dimension(2)       :: climatology_bounds
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
	character (len=L_Email)            :: Email
	character (len=L_COMMENT)          :: COMMENT
	character (len=L_vrb_units)        :: vrb_units
	character (len=L_cellmeth)         :: cellmeth
        character (len=*), DIMENSION(2)   :: l_units
	character (len=256)   :: err_shname,err_lgname
     &                         , var1_shname,var1_lgname
     &                         , var2_shname,var2_lgname
     &                         , varb1_shname,varb1_lgname
     &                         , varb2_shname,varb2_lgname
     &                         , rer_shname,rer_lgname
     &                         , vbt_shname,vbt_lgname
     &                         , string256
!

!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                        :: id1,id1_1,id1_2,id2,id2_1
     &                                , id3,id3_1,id4,id2d1,id2d2,id2d3
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
     &                         , id3,id3_1,id4,id2d1,id2d2,id2d3
     &                         , id0_1,id0_2,id0_3,idcb
      save                    :: timeid,idtime, icdf
      save                    :: lonid,latid,depthid,nvid
      save                    :: idlon,idlat,iddepth
      save                    :: ncid,status
!
      LOGICAL                 :: exist
!
      data icdf /0/
!-------------------------------------------------------------------
!
      one = 1
      two = 2
      three= 3
      four= 4
      im   = im4
      jm   = jm4
!
      if (icdf == 0) then
!
      WRITE(err_shname,'(a,"_err")')TRIM(var_shname)
      WRITE(vbt_shname,'(a,"_deepest")')TRIM(var_shname)
!
      WRITE(var1_shname,'(a,"_L1")')TRIM(var_shname)
      WRITE(var2_shname,'(a,"_L2")')TRIM(var_shname)
!
      WRITE(varb1_shname,'(a,"_L1")')TRIM(vbt_shname)
      WRITE(varb2_shname,'(a,"_L2")')TRIM(vbt_shname)

      WRITE(rer_shname,'(a,"_relerr")')TRIM(var_shname)
!
      WRITE(err_lgname,
     &       '("Error standard deviation of ",a)')TRIM(var_lgname)
!
      WRITE(vbt_lgname,'("Deepest values of ",a)')TRIM(var_lgname)
!
      WRITE(rer_lgname,'("Relative error of ",a)')TRIM(var_lgname)
!
      WRITE(var1_lgname,
     &'(a," masked using relative error threshold 0.3 ")')
     &TRIM(var_lgname)
!
      WRITE(var2_lgname,
     &'(a," masked using relative error threshold 0.5 ")')
     &TRIM(var_lgname)
!
      WRITE(varb1_lgname,
     &'(a," masked using relative error threshold 0.3 ")')
     &TRIM(vbt_lgname)
!
      WRITE(varb2_lgname,
     &'(a," masked using relative error threshold 0.5 ")')
     &TRIM(vbt_lgname)
!
      valexu(:) = valexc
!-------------------------------------------------------------------
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
!      status = nf_open(TRIM(file_name), nf_64bit_offset,ncid)
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
!
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

      cbdim(2)=timeid
      cbdim(1)=nvid
      status=nf_def_var(ncid,'climatology_bounds',nf_float,
     &                                           two,cbdim,idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def clbnds'
      ENDIF
!
      ttime_len=2*time_len
!x      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
!x     &                            nf_float,ttime_len,clbnds)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_4DCLIMATO put clbnds'
!x      ENDIF

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

      status = nf_put_att_text(ncid,idtime, 'units',
     &              LEN_TRIM(Ref_time),TRIM(Ref_time))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def timeunits'
      ENDIF

      string256='climatology_bounds'
      status = nf_put_att_text(ncid,idtime, 'climatology',
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
      status=nf_def_var(ncid,'CORRLEN',nf_float,two,cbdim,id0_1)
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
      status=nf_def_var(ncid,'SNR',nf_float,two,cbdim,id0_2)
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
!x      string256=' '
!x      status=nf_put_att_text(ncid,id0_2,'units',
!x     &                 LEN_TRIM(string256),TRIM(string256))
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_4DCLIMATO att0_2 units'
!x      ENDIF
!
      status=nf_def_var(ncid,'VARBACK',nf_float,two,cbdim,id0_3)
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
      four = 4
      status=nf_def_var(ncid,TRIM(var_shname),nf_float,four,dim,id1)
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 Fillvalue'
      ENDIF

      status= nf_put_att_text(ncid,id1 , 'cell_methods',
     &                 LEN_TRIM(cellmeth),TRIM(cellmeth))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 cellmeth'
      ENDIF

!
      status=nf_put_att_real(ncid,id1,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
!
!===================================================

      status=nf_def_var(ncid,TRIM(err_shname),nf_float,four,dim,id2)
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!-----------------------
!
      status=nf_def_var(ncid,TRIM(var1_shname),nf_float,four,dim,id1_1)
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'missing_value',
     &                                 nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv1'
      ENDIF


      status=nf_def_var(ncid,TRIM(var2_shname),nf_float,four,dim,id1_2)
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv2'
      ENDIF
!--------------------

      status=nf_def_var(ncid,TRIM(rer_shname),nf_float,four,dim,id2_1)
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
!x      string256=' '
!x      status=nf_put_att_text(ncid,id2_1,'units',
!x     &                 LEN_TRIM(string256),TRIM(string256))
!x
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexrer'
      ENDIF
!
!----------------------------------------------------------------
!

      status=nf_def_var(ncid,'databins',nf_float,four,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def dbins'
      ENDIF

      status=nf_put_att_text(ncid,id3,'long_name',
     &     LEN_TRIM('Logarithm10 of number of data in bins'),
     &              'Logarithm10 of number of data in bins')
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexdb'
      ENDIF
!
      status=nf_def_var(ncid,'outlbins',nf_float,four,dim,id3_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def obins'
      ENDIF

      status=nf_put_att_text(ncid,id3_1,'long_name',
     &   LEN_TRIM('Logarithm10 of number of outliers data in bins'),
     &            'Logarithm10 of number of outliers data in bins')
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexob'
      ENDIF
!
      status=nf_def_var(ncid,'CLfield',nf_float,four,dim,id4)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def rlfield'
      ENDIF

      status=nf_put_att_text(ncid,id4,'long_name',
     &     LEN_TRIM('Correlation length field'),
     &              'Correlation length field')
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
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id4 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexdb'
      ENDIF
!
!
      dim2(1)=lonid
      dim2(2)=latid
      dim2(3)=timeid
!
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
     &                                   nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
!x      status= nf_put_att_text(ncid,id2d1 , 'cell_methods',
!x     &                 LEN_TRIM(cellmeth),TRIM(cellmeth))
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_4DCLIMATO vbt clmt'
!x      ENDIF
!==================================================================
      status=nf_def_var(ncid,TRIM(varb1_shname),nf_float,
     &                                             three,dim2,id2d2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def varb1'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d2,'long_name',
     &                 LEN_TRIM(varb1_lgname),TRIM(varb1_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb1 lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d2,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb1 units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_min',
     &                                   nf_float,one,vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1 min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_max',
     &                                   nf_float,one,vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1 max'
      ENDIF
      status=nf_put_att_real(ncid,id2d2,'_FillValue',
     &                                   nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!==========================================================
!
!
      status=nf_def_var(ncid,TRIM(varb2_shname),nf_float,
     &                                             three,dim2,id2d3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def varb2'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d3,'long_name',
     &                 LEN_TRIM(varb2_lgname),TRIM(varb2_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb2 lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d3,'units',
     &                 LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb2 units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'valid_min',
     &                                   nf_float,one,vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2 min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'valid_max',
     &                                   nf_float,one,vbt2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2 max'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'_FillValue',
     &                                   nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2valex'
      ENDIF
!
!==========================================================


!----------------------------------------------------------------
!
      !----------------------
      !Put global attributes
      !----------------------
!
        string256='CF-1.0'
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Conventions',
     &	                    LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO conv'
      ENDIF
!
      string256='SeaDataNetII: http://www.seadatanet.org/ '
      status=nf_put_att_text(ncid,NF_GLOBAL,'project',
     &                 LEN_TRIM(string256),TRIM(string256))
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
      string256='GHER OPENDAB: http://gher-diva.phys.ulg.ac.be/data/'
      status=nf_put_att_text(ncid,NF_GLOBAL,'data_access',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO sdn'
      ENDIF
!
      string256=
     &'http://gher-diva.phys.ulg.ac.be/web-vis/clim.html '
      status=nf_put_att_text(ncid,NF_GLOBAL,'WEB_visualisation',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO sdn'
      ENDIF
!

      write(*,*) 'going to look for sextantcatalog_id'

      INQUIRE(FILE='sextantcatalog_id',EXIST=exist)
      IF(exist) THEN
          CLOSE(12)
          OPEN(UNIT=12,FILE='sextantcatalog_id')
          READ(12,*) string256
      write(*,*) ' sextantcatalog_id: ',string256
          CLOSE(12)
          status=nf_put_att_text
     &         (ncid,NF_GLOBAL,'product_id',
     &         LEN_TRIM(string256),TRIM(string256))
          IF (status .NE.nf_noerr) THEN
             print *,nf_strerror(status)
             STOP 'Stopped in NC_4DCLIMATO sdn'
          ENDIF
       ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Author_e-mail',
     &	                 LEN_TRIM(Email),TRIM(Email))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att email'
      ENDIF
!
      STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'date',
     &                   len('YYYY-MM-DD hh:mm:ss.ss'),
     &                   mjdstr(mjdnow(),'YYYY-MM-DD hh:mm:ss.ss'))
      
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
      
      status = nf_redef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO redef2'
      ENDIF
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
      status=nf_put_att_real(ncid,id2d2,'valid_min',
     &                                   nf_float,one,vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1min 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_max',
     &                                   nf_float,one,vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1max 2'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'valid_min',
     &                                   nf_float,one,vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2min 2'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'valid_max',
     &                                   nf_float,one,vbt2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2max 2'
      ENDIF
!
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
!
!x      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
!x     &                            nf_float,ttime_len,clbnds)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_4DCLIMATO put clbnds'
!x      ENDIF
!
      status = nf_enddef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO enddef'
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
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO hrss'
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
      status=nf_put_vara_real(ncid,id2d2, star2, coun2,varb1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var'
      ENDIF
!
      status=nf_put_vara_real(ncid,id2d3, star2, coun2,varb2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var'
      ENDIF
!
      stpar(1)=1
      stpar(2)=icdf
      prcnt(1)=2
      prcnt(2)=1
!
      climatology_bounds(1) = clbnds(1,icdf)
      climatology_bounds(2) = clbnds(2,icdf)
!

	If (ipar == 1) then

      write(*,*) 'NCWRT   climatology_bounds '
      write(*,*) 'climatology_bounds ',climatology_bounds


      status = 
     &nf_put_vara_real(ncid,idcb, stpar, prcnt,climatology_bounds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put xclbnds'
      ENDIF

	Endif
!
      stpar(1)=1
      stpar(2)=icdf
      prcnt(1)=KM
      prcnt(2)=1
!
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
      status = nf_sync(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped last sync'
      ENDIF
!
      RETURN
      END
