!
      SUBROUTINE NC_RD3DCL(im4,jm4,km,ipar,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,
     &  clo,cla,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min,var_max,vbt_max,vbt_min,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,obin_min,obin_max,rl_min,rl_max,VALEXC,
     &  file_name,var_shname)
!     &  cellmeth,var_units,vrb_units,l_units,Ref_time,
!     &  title_string,INSTITUTION,PRODUCTION,SOURCE,COMMENT)
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
!
        real*4  ,dimension(2,1)              :: clbnds
     &                                        ,climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep, valexu
     &                                       , CORLEN, SN, VARBAK
!
	character (len=*)     :: file_name,var_shname
	character (len=255)     :: vrb_units,var_lgname,var_units
	character (len=255)     :: title_string, Ref_time,cellmeth
     &                         , INSTITUTION,PRODUCTION,SOURCE,COMMENT
        character (len=20), DIMENSION(2)           :: l_units
!
	character (len=256)   :: err_shname,err_lgname
     &                         , var1_shname,var1_lgname
     &                         , var2_shname,var2_lgname
     &                         , rer_shname,rer_lgname
     &                         , vbt_shname,vbt_lgname
     &                         , string256
!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                             :: id1,id1_1,id1_2,id2,id2_1
     &                                     , id3,id3_1,id4,id2d1,id2d2
     &                                     , id0_1,id0_2,id0_3,idcb
      integer                             :: timeid,idtime, icdf
!
      integer                 :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      im = im4
      jm = jm4
!!
      write(*,*)'file_name2 ',LEN_TRIM(file_name),TRIM(file_name)
      write(*,*)'**************************************************'
      write(*,*)'var_shname ',LEN_TRIM(var_shname),TRIM(var_shname)
      write(*,*)'**************************************************'
!!
      !-----------------------
      ! Open the data file       
      !-----------------------
!
      status = nf_open(TRIM(file_name), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL open file'
      ENDIF
!
      !----------------------
      !  Get Global Attributes
      !----------------------
!
      STATUS = NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'title',title_string)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL title'
      ENDIF

      STATUS =NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'production',PRODUCTION)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL production'
      ENDIF

      STATUS=NF_GET_ATT_TEXT(NCID,NF_GLOBAL,'institution',INSTITUTION)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL institution'
      ENDIF

      STATUS = NF_GET_ATT_TEXT (NCID,NF_GLOBAL,'source',SOURCE)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL source'
      ENDIF

      STATUS = NF_GET_ATT_TEXT(NCID,NF_GLOBAL,'comment',COMMENT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL comment'
      ENDIF
!
      !----------------------
      !  Inquire dimensions id
      !----------------------
!
      status=nf_inq_dimid(ncid,'lon',lonid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def lon'
      ENDIF

      status=nf_inq_dimid(ncid,'lat',latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def lat'
      ENDIF

      status=nf_inq_dimid(ncid,'depth',depthid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def depth'
      ENDIF
!
      status=nf_inq_dimid(ncid,'time',timeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL dimid time'
      ENDIF

      status=nf_inq_dimid(ncid, 'nv', nvid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def nvid'
      ENDIF
!
      status=nf_inq_dimlen(ncid,lonid,IM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL dimlen lon'
      ENDIF

      status=nf_inq_dimlen(ncid,latid,JM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL dimlen lat'
      ENDIF

      status=nf_inq_dimlen(ncid,depthid,KM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL dimlen depth'
      ENDIF
!
      !----------------------------
      ! Inquire coordinate variables
      !----------------------------
!
      status=nf_inq_varid(ncid,'lon',idlon)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def  var lon'
      ENDIF

      status=nf_inq_varid(ncid,'lat',idlat)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var lat'
      ENDIF

      status=nf_inq_varid(ncid,'depth',iddepth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var depth'
      ENDIF

      status=nf_inq_varid(ncid,'time',idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var time'
      ENDIF
!
      IF(ipar == 1) then

      cbdim(1)=nvid
      cbdim(2)=timeid
      status=nf_inq_varid(ncid,'climatology_bounds',idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def clbnds'
      ENDIF
!
      status = nf_get_att_real(ncid,idcb,'climatology_bounds',
     &                            clbnds)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL put clbnds'
      ENDIF

      ENDIF
!
      !-----------------------------------------     
      ! Get coordinate variables attributes
      !-----------------------------------------
!
      status=nf_get_att_text(ncid,idlon,'units',l_units(1))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att lon'
      ENDIF

      status=nf_get_att_text(ncid,idlat,'units',l_units(2))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att lat'
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
      cbdim(1)=depthid
      cbdim(2)=timeid

      status=nf_inq_varid(ncid,'CORLEN',id0_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var0_1'
      ENDIF

      status=nf_inq_varid(ncid,'SN',id0_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var0_2'
      ENDIF
!
      status=nf_inq_varid(ncid,'VARBAK',id0_3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var0_3'
      ENDIF

      status=nf_get_att_text(ncid,id0_3,'units',vrb_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att units'
      ENDIF
!
!===================================================
!
      status=nf_inq_varid(ncid,TRIM(var_shname),id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var'
      ENDIF
!
      status=nf_get_att_text(ncid,id1,'long_name',var_lgname)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att lgname'
      ENDIF

      status=nf_get_att_text(ncid,id1,'units',var_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att units'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_min',var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att varmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id1,'valid_max',var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att varmax'
      ENDIF

      status= nf_get_att_text(ncid,id1 , 'cell_methods',cellmeth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL cellmeth'
      ENDIF
!
!----------------------------------------------------------------
!

      WRITE(err_shname,'(a,".err")')TRIM(var_shname)
!x      WRITE(err_lgname,
!x     &       '(a," standard deviation error")')TRIM(var_lgname)
!

      WRITE(rer_shname,'(a,".relerr")')TRIM(var_shname)
!x      WRITE(rer_lgname,'(a," relative error")')TRIM(var_lgname)
!
      WRITE(var1_shname,'(a,".L1")')TRIM(var_shname)
!x      WRITE(var1_lgname,
!x     & '(a,": first threshold masked fields")')TRIM(var_lgname)
!
      WRITE(var2_shname,'(a,".L2")')TRIM(var_shname)
!x      WRITE(var2_lgname,
!x     & '(a,": second threshold masked fields")')TRIM(var_lgname)
!
      WRITE(vbt_shname,'(a,".bottom")')TRIM(var_shname)
!x      WRITE(vbt_lgname,'("deepest values of ",a)')TRIM(var_lgname)
!
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(vbt_shname),id2d1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d1,'long_name',vbt_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ3DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d1,'valid_min',vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d1,'valid_max',vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vbtmax'
      ENDIF
!
!----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(var1_shname),id1_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def var1'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_1,'valid_min',var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att valex'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_1,'valid_max',var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att valex'
      ENDIF
!
!--------------------
!
      status=nf_inq_varid(ncid,TRIM(var2_shname),id1_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def err2'
      ENDIF

!
      status=nf_get_att_real(ncid,id1_2,'valid_min',var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att v2min'
      ENDIF
!
      status=nf_get_att_real(ncid,id1_2,'valid_max',var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att v2max'
      ENDIF
!--------------------

      status=nf_inq_varid(ncid,TRIM(err_shname),id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def err3'
      ENDIF

!
      status=nf_get_att_real(ncid,id2,'valid_min',ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vermin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2,'valid_max',ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vermax'
      ENDIF
!
!-----------------------

      status=nf_inq_varid(ncid,TRIM(rer_shname),id2_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def relerr'
      ENDIF
!
      status=nf_get_att_real(ncid,id2_1,'valid_min',verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att verelmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2_1,'valid_max',verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att verelmax'
      ENDIF
!
!----------------------------------------------------------------
!

      status=nf_inq_varid(ncid,'databins',id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def dbins'
      ENDIF
! 
      status=nf_get_att_real(ncid,id3,'valid_min',dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att dbmax'
      ENDIF
!
      status=nf_get_att_real(ncid,id3,'valid_max',dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att dbmax'
      ENDIF
!
      status=nf_inq_varid(ncid,'outlbins',id3_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def obins'
      ENDIF
! 
      status=nf_get_att_real(ncid,id3_1,'valid_min',obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att obmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id3_1,'valid_max',obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att obmax'
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
!
!----------------------------------------------------------------
!
      status = nf_get_var_real(ncid,idlon,clo)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get clo'
      ENDIF
!
      status = nf_get_var_real(ncid,idlat,cla)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get cla'
      ENDIF
!
      status = nf_get_var_real(ncid,iddepth,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get dep'
      ENDIF
!
      icdf=1
!
      status = nf_sync(ncid)
!
      status = nf_get_var1_real(ncid, idtime, icdf, hrss)
      time_val = hrss
!
      stpar(1)=1
      stpar(2)=icdf
      prcnt(1)=KM
      prcnt(2)=1

      status = nf_get_vara_real(ncid,id0_1, stpar, prcnt,CORLEN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get CORLEN'
      ENDIF
!
      status = nf_get_vara_real(ncid,id0_2, stpar, prcnt,SN)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get SN'
      ENDIF
!
      status = nf_get_vara_real(ncid,id0_3, stpar, prcnt,VARBAK)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get VARBAK'
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
      status=nf_get_vara_real(ncid,id1, start, count,var)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get var'
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
         STOP 'Stopped in NC_READ3DCL get varbot'
      ENDIF
!
      status=nf_get_vara_real(ncid,id1_1, start, count,var1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get var1'
      ENDIF

      status=nf_get_vara_real(ncid,id1_2, start, count,var2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get var2'
      ENDIF

      status=nf_get_vara_real(ncid,id2, start, count,verr)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get verr'
      ENDIF

      status=nf_get_vara_real(ncid,id2_1, start, count,reler)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get reler'
      ENDIF
!
      status=nf_get_vara_real(ncid,id3, start, count,dbins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get dbins'
      ENDIF
!
      status=nf_get_vara_real(ncid,id3_1, start, count,obins)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get obins'
      ENDIF
!
      status=nf_get_vara_real(ncid,id4, start, count,rlfield)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get rlfield'
      ENDIF
!
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF

      RETURN
      END
