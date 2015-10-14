!
      SUBROUTINE NC_RD4Dfinal(im4,jm4,km,ipar,time_len,time_val,clbnds,
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
     &  L_Email,Email,L_COMMENT,COMMENT,obs,idlen,obsid,obsid_lgname,
     &  obsid_coord,obslon,obslon_units,obslat,obslat_units,obsdepth,
     &  obsdepth_units,obsdepth_positive,obstime,obstime_units,
     &  obsid1,obsid2)

!
       use divaio
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer                             :: im4,jm4,obs,idlen
        integer                             :: im,jm,km,ipar,time_len
        integer     :: L_file_name,L_var_shname,L_var_lgname,
     &  L_var_units,L_vrb_units,L_title_string,L_cellmeth,L_Ref_time,
     &  L_INSTITUT,L_PRODUCTION,L_SOURCE,L_Email,L_COMMENT
        real*4                              :: valexc, 
     &            var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &            vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,
     &            var2_min,var2_max,verel_min,verel_max,
     &            vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &            obin_min,obin_max,rl_min,rl_max
        real*4,  dimension(time_len)          :: hrss,time_val
!
        real*4  ,dimension(im4,jm4,km,time_len)  :: var, var1, var2
     &                                       , Verr,reler,dbins
     &                                       , obins,rlfield
        real*4  ,dimension(im4,jm4)          :: varbot,varb1,varb2
!
        real*4  ,dimension(2,time_len)              :: clbnds
	real*4	,dimension(2)		     :: climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep, valexu
        real*4  ,dimension(km,time_len)      :: CORLEN, SN, VARBAK
	
	real*4,dimension(obs)		:: obslon,obslat,obsdepth,obstime
!
	character (len=L_file_name)        :: file_name
!	character (len=L_title_string)     :: title_string
!	character (len=L_Ref_time)         :: Ref_time
	character (len=L_var_shname)       :: var_shname
!	character (len=L_var_lgname)       :: var_lgname
!	character (len=L_var_units)        :: var_units
!	character (len=L_INSTITUT)         :: INSTITUT
!	character (len=L_PRODUCTION)       :: PRODUCTION
!	character (len=L_SOURCE)           :: SOURCE
!	character (len=L_Email)            :: Email
!	character (len=L_COMMENT)          :: COMMENT
!	character (len=L_vrb_units)        :: vrb_units
!	character (len=L_cellmeth)         :: cellmeth

!	character (len=255)    		   :: file_name
	character (len=255)  		   :: title_string
	character (len=30)		   :: Ref_time
!	character (len=99)   		   :: var_shname
	character (len=99)       	   :: var_lgname
	character (len=20)        	   :: var_units
	character (len=255)         	   :: INSTITUT
	character (len=255)       	   :: PRODUCTION
	character (len=255)           	   :: SOURCE
	character (len=255)                :: Email
	character (len=255)                :: COMMENT
	character (len=20)                 :: vrb_units
	character (len=255)                :: cellmeth

	character(30),dimension(obs,idlen)::obsid,obsid1,obsid2

        character (len=20), DIMENSION(2)           :: l_units
!
	character (len=256)   :: err_shname,err_lgname
     &                         , var1_shname,var1_lgname
     &                         , var2_shname,var2_lgname
     &                         , rer_shname,rer_lgname
     &                         , varb1_shname,varb1_lgname
     &                         , varb2_shname,varb2_lgname
     &                         , vbt_shname,vbt_lgname
     &                         , string256

	character(len=256)	:: obsid_lgname,obsid_coord
	character(len=256)	:: obslon_units,obslat_units
	character(len=256)	:: obsdepth_units,obsdepth_positive
	character(len=256)	:: obstime_units
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

	integer				:: obsidid,obslonid,obslatid
	integer				:: obsdepthid,obstimeid
!
      integer                 :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer		      :: observid,idlenid
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
      data icdf /0/
!-------------------------------------------------------------------
!

      im = im4
      jm = jm4
!!
!      write(*,*)'file_name2 ',LEN_TRIM(file_name),TRIM(file_name)
!      write(*,*)'**************************************************'
!      write(*,*)'var_shname ',LEN_TRIM(var_shname),TRIM(var_shname)
!      write(*,*)'**************************************************'
!!

      if (icdf == 0) then
!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL open file'
      ENDIF
!
      !----------------------
      !  Get Global Attributes
      !----------------------
!
      STATUS = NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'title',title_string)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL title'
      ENDIF

      STATUS =NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'production',PRODUCTION)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL production'
      ENDIF

      STATUS=NF_GET_ATT_TEXT(NCID,NF_GLOBAL,'institution',INSTITUT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL institution'
      ENDIF

      STATUS = NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'source',SOURCE)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL source'
      ENDIF

      STATUS = NF_GET_ATT_TEXT(NCID,NF_GLOBAL,'comment',COMMENT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL comment'
      ENDIF

      STATUS = NF_GET_ATT_TEXT(NCID,NF_GLOBAL,'Author_e-mail',Email)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL email'
      ENDIF
!
      !----------------------
      !  Inquire dimensions id
      !----------------------
!
      status=nf_inq_dimid(ncid,'lon',lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def lon'
      ENDIF

      status=nf_inq_dimid(ncid,'lat',latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def lat'
      ENDIF

      status=nf_inq_dimid(ncid,'depth',depthid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def depth'
      ENDIF
!
      status=nf_inq_dimid(ncid,'time',timeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimid time'
      ENDIF

      status=nf_inq_dimid(ncid, 'nv', nvid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def nvid'
      ENDIF
!
      status=nf_inq_dimid(ncid, 'observations', observid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def observations'
      ENDIF

      status=nf_inq_dimid(ncid, 'idlen', idlenid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def idlen'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen lat'
      ENDIF

      status=nf_inq_dimlen(ncid,depthid,KM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen depth'
      ENDIF
!
      status=nf_inq_dimlen(ncid,observid,obs)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,idlenid,idlen)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen lon'
      ENDIF
!
      !----------------------------
      ! Inquire coordinate variables
      !----------------------------
!
      status=nf_inq_varid(ncid,'lon',idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def  var lon'
      ENDIF

      status=nf_inq_varid(ncid,'lat',idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var lat'
      ENDIF

      status=nf_inq_varid(ncid,'depth',iddepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var depth'
      ENDIF

      status=nf_inq_varid(ncid,'time',idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var time'
      ENDIF
!
!!      IF(ipar == 1) then

      status=nf_inq_varid(ncid,'climatology_bounds',idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def clbnds'
      ENDIF
!
      status = nf_get_att_real(ncid,idcb,'climatology_bounds',
     &                            clbnds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL put clbnds'
      ENDIF

!!      ENDIF
!
      !-----------------------------------------     
      ! Get coordinate variables attributes
      !-----------------------------------------
!
      status=nf_get_att_text(ncid,idlon,'units',l_units(1))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lon'
      ENDIF

      status=nf_get_att_text(ncid,idlat,'units',l_units(2))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lat'
      ENDIF

      status = nf_get_att_text(ncid, timeid, 'units',Ref_time)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def timeunits'
      ENDIF

      !-----------------------
      ! Inquire data variables
      !-----------------------
!
      status=nf_inq_varid(ncid,'CORRLEN',id0_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var0_1'
      ENDIF

      status=nf_inq_varid(ncid,'SNR',id0_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var0_2'
      ENDIF
!
      status=nf_inq_varid(ncid,'VARBACK',id0_3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var0_3'
      ENDIF

      status=nf_get_att_text(ncid,id0_3,'units',vrb_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att units'
      ENDIF
!
!===================================================
!
      status=nf_inq_varid(ncid,TRIM(var_shname),id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var'
      ENDIF
!
      status=nf_get_att_text(ncid,id1,'long_name',var_lgname)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lgname'
      ENDIF

      status=nf_get_att_text(ncid,id1,'units',var_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att units'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_min',var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att varmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_max',var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att varmax'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'missing_value',valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att valexc'
      ENDIF
!
      status= nf_get_att_text(ncid,id1 , 'cell_methods',cellmeth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL cellmeth'
      ENDIF
!
!----------------------------------------------------------------
!

      WRITE(err_shname,'(a,"_err")')TRIM(var_shname)
!x      WRITE(err_lgname,
!x     &       '(a," standard deviation error")')TRIM(var_lgname)
!

      WRITE(rer_shname,'(a,"_relerr")')TRIM(var_shname)
!x      WRITE(rer_lgname,'(a," relative error")')TRIM(var_lgname)
!
      WRITE(var1_shname,'(a,"_L1")')TRIM(var_shname)
!x      WRITE(var1_lgname,
!x     & '(a,": first threshold masked fields")')TRIM(var_lgname)
!
      WRITE(var2_shname,'(a,"_L2")')TRIM(var_shname)
!x      WRITE(var2_lgname,
!x     & '(a,": second threshold masked fields")')TRIM(var_lgname)
!
      WRITE(vbt_shname,'(a,"_deepest")')TRIM(var_shname)
!x      WRITE(vbt_lgname,'("deepest values of ",a)')TRIM(var_lgname)
!
      WRITE(varb1_shname,'(a,"_L1")')TRIM(vbt_shname)
      WRITE(varb2_shname,'(a,"_L2")')TRIM(vbt_shname)
!
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(vbt_shname),id2d1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def vardeep'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d1,'long_name',vbt_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ4DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d1,'valid_min',vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d1,'valid_max',vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmax'
      ENDIF
!
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(varb1_shname),id2d2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def vardeep'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d2,'long_name',varb1_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ4DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d2,'valid_min',vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d2,'valid_max',vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmax'
      ENDIF
!
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(varb2_shname),id2d3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def vardeep'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d3,'long_name',varb2_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ4DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d3,'valid_min',vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d3,'valid_max',vbt2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vbtmax'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(var1_shname),id1_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def var1'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_1,'valid_min',var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att valex'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_1,'valid_max',var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att valex'
      ENDIF
!
!--------------------
!
      status=nf_inq_varid(ncid,TRIM(var2_shname),id1_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def err2'
      ENDIF

!
      status=nf_get_att_real(ncid,id1_2,'valid_min',var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att v2min'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_2,'valid_max',var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att v2max'
      ENDIF
!--------------------

      status=nf_inq_varid(ncid,TRIM(err_shname),id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def err3'
      ENDIF

!
      status=nf_get_att_real(ncid,id2,'valid_min',ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vermin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2,'valid_max',ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att vermax'
      ENDIF
!
!-----------------------

      status=nf_inq_varid(ncid,TRIM(rer_shname),id2_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def relerr'
      ENDIF
!
      status=nf_get_att_real(ncid,id2_1,'valid_min',verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att verelmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2_1,'valid_max',verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att verelmax'
      ENDIF
!
!----------------------------------------------------------------
!

      status=nf_inq_varid(ncid,'databins',id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def dbins'
      ENDIF
! 
      status=nf_get_att_real(ncid,id3,'valid_min',dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att dbmax'
      ENDIF
!
      status=nf_get_att_real(ncid,id3,'valid_max',dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att dbmax'
      ENDIF
!
      status=nf_inq_varid(ncid,'outlbins',id3_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obins'
      ENDIF
! 
      status=nf_get_att_real(ncid,id3_1,'valid_min',obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id3_1,'valid_max',obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obmax'
      ENDIF

      status=nf_inq_varid(ncid,'CLfield',id4)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO inq rlfield'
      ENDIF
! 
      status=nf_get_att_real(ncid,id4,'valid_min',rl_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO get att rlmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id4,'valid_max',rl_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO get att rlbmax'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,'obsid',obsidid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obsid'
      ENDIF
! 
      status=nf_get_att_text(ncid,obsidid,'long_name',obsid_lgname)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obsid_lgname'
      ENDIF
! 
      status=nf_get_att_text(ncid,obsidid,'coordinates',obsid_coord)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obsid_coord'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,'obslon',obslonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obslon'
      ENDIF
! 
      status=nf_get_att_text(ncid,obslonid,'units',obslon_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obslon_units'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,'obslat',obslatid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obslat'
      ENDIF
! 
      status=nf_get_att_text(ncid,obslatid,'units',obslat_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obslat_units'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,'obsdepth',obsdepthid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obsdepth'
      ENDIF
! 
      status=nf_get_att_text(ncid,obsdepthid,'units',obsdepth_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obsdepth_units'
      ENDIF
! 
      status=nf_get_att_text(ncid,obsdepthid,'positive',
     & obsdepth_positive)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obsdepth_units'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,'obstime',obstimeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL def obstime'
      ENDIF
! 
      status=nf_get_att_text(ncid,obstimeid,'units',obstime_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att obstime_units'
      ENDIF
!
!----------------------------------------------------------------
!
!
!----------------------------------------------------------------
!
      status = nf_get_var_real(ncid,idlon,clo)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get clo'
      ENDIF
!
      status = nf_get_var_real(ncid,idlat,cla)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get cla'
      ENDIF
!
      status = nf_get_var_real(ncid,iddepth,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get dep'
      ENDIF
!
      status = nf_get_var_real(ncid,idtime,time_val)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get time'
      ENDIF
!
      ENDIF
!
      icdf=icdf+1
!
      status = nf_sync(ncid)
!
!      status = nf_get_var1_real(ncid, idtime, icdf, hrss)
!      time_val = hrss
!
      start(1)=1
      start(2)=1
      count(1)=2
      count(2)=time_len

      status=nf_get_vara_real(ncid,idcb, start, count,clbnds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get var'
      ENDIF
!
      start(1)=1
      start(2)=1
      start(3)=1
      start(4)=icdf
      count(1)=IM
      count(2)=JM
      count(3)=KM
      count(4)=time_len
! 
      status=nf_get_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get var'
      ENDIF
!
      status=nf_get_vara_real(ncid,id1_1, start, count,var1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get var1'
      ENDIF

      status=nf_get_vara_real(ncid,id1_2, start, count,var2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get var2'
      ENDIF

      status=nf_get_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get verr'
      ENDIF

      status=nf_get_vara_real(ncid,id2_1, start, count,reler)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get reler'
      ENDIF
!
      status=nf_get_vara_real(ncid,id3, start, count,dbins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get dbins'
      ENDIF
!
      status=nf_get_vara_real(ncid,id3_1, start, count,obins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get obins'
      ENDIF
!
      status=nf_get_vara_real(ncid,id4, start, count,rlfield)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get rlfield'
      ENDIF
!
      star2(1)=1
      star2(2)=1
      star2(3)=icdf
      coun2(1)=IM
      coun2(2)=JM
      coun2(3)=1
! 
      status=nf_get_vara_real(ncid,id2d1, star2, coun2,varbot)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get varbot'
      ENDIF
!
      status=nf_get_vara_real(ncid,id2d2, star2, coun2,varb1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get varbot'
      ENDIF
!
      status=nf_get_vara_real(ncid,id2d3, star2, coun2,varb2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get varbot'
      ENDIF
!
      stpar(1)=1
      stpar(2)=icdf
      prcnt(1)=KM
      prcnt(2)=time_len

      status = nf_get_vara_real(ncid,id0_1, stpar, prcnt,CORLEN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get CORLEN'
      ENDIF
!      write(*,*)' dvncread CORLEN ',CORLEN
!      write(*,*)' dvncread sterror ',nf_strerror(status)
!
      status = nf_get_vara_real(ncid,id0_2, stpar, prcnt,SN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get SN'
      ENDIF
!
      status = nf_get_vara_real(ncid,id0_3, stpar, prcnt,VARBAK)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get VARBAK'
      ENDIF
!
!      stpar(1)=1
!      stpar(2)=1
!      prcnt(1)=idlen
!      prcnt(2)=int(obs/2)
!
!      status = nf_get_vara_text(ncid,obsidid, stpar, prcnt,obsid1)
!      IF (status .NE.nf_noerr) THEN
!         print *,nf_strerror(status)
!         STOP 'Stopped in NC_READ4DCL get obsid1'
!      ENDIF
!
!      stpar(1)=1
!      stpar(2)=1+int(obs/2)
!      prcnt(1)=idlen
!      prcnt(2)=obs-int(obs/2)
!
!      status = nf_get_vara_text(ncid,obsidid, stpar, prcnt,obsid2)
!      IF (status .NE.nf_noerr) THEN
!         print *,nf_strerror(status)
!         STOP 'Stopped in NC_4DCLIMATO get obsid2'
!      ENDIF
!
!      obsid(1:int(obs/2),:)=obsid1(1:int(obs/2),:)
!      obsid(int(obs/2)+1:obs,:)=obsid2(1:(obs-int(obs/2)),:)
!
      stpar(1)=1
      stpar(2)=1
      prcnt(1)=obs
      prcnt(2)=1

      status = nf_get_vara_real(ncid,obslonid, stpar, prcnt,obslon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get obslon'
      ENDIF
!
      stpar(1)=1
      stpar(2)=1
      prcnt(1)=obs
      prcnt(2)=1

      status = nf_get_vara_real(ncid,obslatid, stpar, prcnt,obslat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get obslat'
      ENDIF
!
      stpar(1)=1
      stpar(2)=1
      prcnt(1)=obs
      prcnt(2)=1

      status = nf_get_vara_real(ncid,obsdepthid, stpar, prcnt,obsdepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get obsdepth'
      ENDIF
!
      stpar(1)=1
      stpar(2)=1
      prcnt(1)=obs
      prcnt(2)=1

      status = nf_get_vara_real(ncid,obstimeid, stpar, prcnt,obstime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL get obstime'
      ENDIF
!
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF

      RETURN
      END
