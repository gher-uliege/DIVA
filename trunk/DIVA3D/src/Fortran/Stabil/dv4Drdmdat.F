!
      SUBROUTINE NC_RD4DMD(im,jm,km,nm,irewrt,
     &  WestBL,SouthBL,EastBL,NorthBL,dx,dy,
     &  var_min,var_max,var_units,s_units,
     &  L_INSTITUT,INSTITUT,L_PRODUCTION,PRODUCTION,
     &  L_Email,Email,
     &  var_shname,file_name)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer                             :: i,j,k,attlen
        integer                             :: im,jm,km,irewrt,nm,chlen

        integer     :: 
     &  L_INSTITUT,L_PRODUCTION,L_Email

        real*4                              :: valexc,
     &            var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &            vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,
     &            var2_min,var2_max,verel_min,verel_max,
     &            vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &            obin_min,obin_max,rl_min,rl_max
!
        real*4                              :: hrss,time_val
!
        real*4  ,dimension(2,1)              :: clbnds
     &                                        ,climatology_bounds
!   
        real*4  ,dimension(:)  ,  ALLOCATABLE        :: clo
        real*4  ,dimension(:)  ,  ALLOCATABLE        :: cla
        real*4  ,dimension(:)  ,  ALLOCATABLE        :: dep
     &                                       , CORLEN, SN, VARBAK
!
!
      real*4                    :: xorig, yorig, dx, dy, xend, yend
     &                            , WestBL,SouthBL,EastBL,NorthBL
      character (len=8)          :: yy1yy2
      character (len=99)          :: latunits,lonunits,sdn_units
!
      character (len=L_INSTITUT)         :: INSTITUT
      character (len=L_PRODUCTION)       :: PRODUCTION
      character (len=L_Email)            :: Email
!
      character (len=15) , DIMENSION(2)           :: l_units
!
      LOGICAL                          :: exist
!
	character (len=*)     :: file_name,var_shname
     &                          ,var_units,s_units
	character (len=255)     :: vrb_units,var_lgname
     &                            , file_name2
!
	character (len=256)   :: string256
!
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                         :: id1
     &                                 , id0_1,id0_2,id0_3,idcb
      integer                             :: timeid,idtime, icdf
!
      integer                 :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      save                    :: id1
     &                         , id0_1,id0_2,id0_3,idcb
      save                    :: timeid,idtime, icdf
      save                    :: lonid,latid,depthid,nvid
      save                    :: idlon,idlat,iddepth
      save                    :: ncid,status
!
      !-----------------------
      ! Open the data file       
      !-----------------------
!


      write(*,*)' 1 file_name = ',TRIM(file_name)



      IF (irewrt == 0) THEN



      write(*,*)' 2 file_name = ',TRIM(file_name)



      status = nf_open(TRIM(file_name), nf_nowrite,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL open file'
      ENDIF
!
      ELSE


      write(*,*)' 3 file_name = ',TRIM(file_name)


      status = nf_open(TRIM(file_name), nf_write,ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL open2 file'
      ENDIF
!
      !----------------------
      ! Put in define mode
      !----------------------
!      
      status = nf_redef(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NCREAD4DCL_ redef'
      ENDIF
!
      !----------------------
      !  Put new Global Attributes
      !----------------------

      write(*,*)' 2 iwewrt   = ',irewrt
      write(*,*)' 2 INSTITUT   = ',TRIM(INSTITUT)
      write(*,*)' 2 PRODUCTION   = ',TRIM(PRODUCTION)
      write(*,*)' 2 Email   = ',TRIM(Email)

!
      string256='SeaDataNetII: http://www.seadatanet.org/ '
      status=nf_put_att_text(ncid,NF_GLOBAL,'project',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL sdn'
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
     & 'http://gher-diva.phys.ulg.ac.be/web-vis/clim.html '
      status=nf_put_att_text(ncid,NF_GLOBAL,'data_visualisation',
     &                 LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO sdn'
      ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Author_e-mail',
     &	                 LEN_TRIM(Email),TRIM(Email))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att email'
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

      status=nf_inq_dimlen(ncid,timeid,NM)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL dimlen depth'
      ENDIF

!xx      RETURN
!xx      ENDIF
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
!
      !-----------------------------------------     
      ! Get coordinate variables attributes
      !-----------------------------------------
!
      status=nf_inq_attlen(ncid,idlon,'units',attlen)
      IF (status .NE. nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL attlen lon'
      ENDIF

      status=nf_get_att_text(ncid,idlon,'units',lonunits)
!l_units(1))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lon'
      ENDIF

      l_units(1)=lonunits(1:attlen)
      status=nf_inq_attlen(ncid,idlat,'units',attlen)
      IF (status .NE. nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL attlen lat'
      ENDIF

      status=nf_get_att_text(ncid,idlat,'units',latunits )
!l_units(2))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lat'
      ENDIF

      l_units(2) = latunits(1:attlen)

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
      status=nf_inq_attlen(ncid,id1,'long_name',attlen)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL attlen lgname'
      ENDIF

      status=nf_get_att_text(ncid,id1,'long_name',string256)
!var_lgname)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att lgname'
      ENDIF

      var_lgname = string256(1:attlen)

      status=nf_inq_attlen(ncid,id1,'units',attlen)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL attlen units'
      ENDIF


      status=nf_get_att_text(ncid,id1,'units',string256)
!var_units)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ4DCL att units'
      ENDIF
      var_units = string256(1:attlen)
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
!----------------------------------------------------------------
!
      ALLOCATE(clo(IM))
      ALLOCATE(cla(JM))
      ALLOCATE(dep(km))
      ALLOCATE(CORLEN(km))
      ALLOCATE(SN(km))
      ALLOCATE(VARBAK(km))
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
      status=nf_close(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped when closing' !,TRIM(file_name)
      ENDIF
!
      WestBL = clo(1) !!xorig
      SouthBL = cla(1) !!yorig
      dx = clo(2) - clo(1)
      dy = cla(2) - cla(1)
      EastBL = xorig + dx*IM
      NorthBL =yorig + dy*JM
!
! Writing an info text file for matadata files:
      WRITE(file_name2,'("../output/",
     &a,".4DNCMetainfo")')TRIM(var_shname)
      close(12)
!
      OPEN(UNIT=12,FILE=file_name2)
!
      chlen = LEN_TRIM(l_units(1))
      if (chlen == 2) then
         s_units = l_units(1)
         WRITE(sdn_units,'("SDN:P061:38:ULKM = Kilometres (km)")')
      else
         s_units = l_units(1)(1:chlen-5)
         WRITE(sdn_units,'("SDN:P061:38:UAAA = Degrees (deg)")')
      endif
!
      write(12,*) IM
      write(12,*) JM
      write(12,*) TRIM(sdn_units)
      write(12,*) TRIM(var_units)
!
      write(12,1111)SouthBL
      write(12,1111)NorthBL
      write(12,1111)WestBL
      write(12,1111)EastBL
      write(12,1112)dx
      write(12,1112)dy
      write(12,*)(dep(k),k=1,KM)
      write(12,*)dep(KM)
      write(12,*)dep(1)
!
 1111 FORMAT(1X,F7.3)
 1112 FORMAT(1X,F5.3)

      RETURN
      END
