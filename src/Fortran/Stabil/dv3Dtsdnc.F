!
      SUBROUTINE NC_TSDN2(im4,jm4,km,time_val,
     &  psal,temp,ers,ert,RHO,N2,clo,cla,dep,
     &  psal_min,psal_max,temp_min,temp_max,ers_min,ers_max,ert_min,
     &  ert_max,RHO_min,RHO_max,N2_min,N2_max,
     &  VALEXC,l_units,
     &  L_file_name,file_name,
     &  L_sal_shname,sal_shname,
     &  L_sal_lgname,sal_lgname,
     &  L_sal_units,sal_units,
     &  L_tmp_shname,tmp_shname,
     &  L_tmp_lgname,tmp_lgname,
     &  L_tmp_units,tmp_units,
     &  L_title_string,title_string,
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
        integer     :: one,two,four,ttime_len,km ,time_len,ipar,im,jm
     &              , three
        integer     :: L_file_name,
     &  L_tmp_shname,L_tmp_lgname,L_tmp_units,
     &  L_sal_shname,L_sal_lgname,L_sal_units,
     &  L_title_string,L_cellmeth,L_Ref_time,
     &  L_INSTITUT,L_PRODUCTION,L_SOURCE,L_COMMENT
!
        real*4                              :: 
     &  psal_min,psal_max,temp_min,temp_max,ers_min,ers_max,ert_min,
     &  ert_max,RHO_min,RHO_max,N2_min,N2_max
        real*4                              :: hrss,time_val, valexc
!
        real*4  ,dimension(im4,jm4,km)      :: psal,temp,ers,ert,RHO,N2
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep !, valexu
!
	character (len=L_file_name)        :: file_name
	character (len=L_title_string)     :: title_string
	character (len=L_Ref_time)         :: Ref_time
	character (len=L_tmp_shname)       :: tmp_shname
	character (len=L_tmp_lgname)       :: tmp_lgname
	character (len=L_tmp_units)        :: tmp_units
	character (len=L_sal_shname)       :: sal_shname
	character (len=L_sal_lgname)       :: sal_lgname
	character (len=L_sal_units)        :: sal_units
	character (len=L_INSTITUT)         :: INSTITUT
	character (len=L_PRODUCTION)       :: PRODUCTION
	character (len=L_SOURCE)           :: SOURCE
	character (len=L_COMMENT)          :: COMMENT
	character (len=L_cellmeth)         :: cellmeth
        character (len=20), DIMENSION(2)   :: l_units
	character (len=256)   :: ert_shname,ert_lgname
     &                         , ers_shname,ers_lgname
     &                         , sig_shname,sig_lgname
     &                         , n2_shname,n2_lgname
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
      integer                        :: id1,id2,id3,id4,
     &                                  id5,id6,idcb
      integer                        :: timeid,idtime, icdf
!
      integer                      :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,levid,nvid
      integer                 :: idlon,idlat,idlev
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      save                    :: id1,id2,id3,id4,id5,id6,idcb
      save                    :: timeid,idtime, icdf
      save                    :: lonid,latid,levid,nvid
      save                    :: idlon,idlat,idlev
      save                    :: ncid,status
!
      data icdf /0/
!-------------------------------------------------------------------
!
      write(*,*)'  L_file_name,file_name ',L_file_name,file_name
      write(*,*)'  L_sal_shname,sal_shname ',L_sal_shname,sal_shname
      write(*,*)'  L_sal_lgname,sal_lgname ',L_sal_lgname,sal_lgname
      write(*,*)'  L_sal_units,sal_units ',L_sal_units,sal_units
      write(*,*)'  L_tmp_shname,tmp_shname ',L_tmp_shname,tmp_shname
      write(*,*)'  L_tmp_lgname,tmp_lgname ',L_tmp_lgname,tmp_lgname
      write(*,*)'  L_tmp_units,tmp_units ',L_tmp_units,tmp_units





      one = 1
      two = 2
      three= 3
      four= 4
      im   = im4
      jm   = jm4
!
      if (icdf == 0) then
!
      WRITE(ert_shname,'(a,"_err")')TRIM(tmp_shname)
      WRITE(ers_shname,'(a,"_err")')TRIM(sal_shname)
!
      WRITE(ert_lgname,'(a,"_err")')TRIM(tmp_lgname)
      WRITE(ers_lgname,'(a,"_err")')TRIM(sal_lgname)

      WRITE(sig_shname,'("SIG")')
      WRITE(n2_shname,'("N2")')
!
      WRITE(sig_lgname,'("Sea water density")')
      WRITE(n2_lgname,'("Brunt-Vaisala Frequency")')
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
!
      status=nf_def_dim(ncid,'lat',JM,latid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def lat'
      ENDIF

      status=nf_def_dim(ncid,'lev',KM,levid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def lev'
      ENDIF

      status=nf_def_dim(ncid,'time',nf_unlimited,timeid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def time'
      ENDIF
!!      two=2
!!      status=nf_def_dim(ncid, 'nv', two, nvid)
!!      IF (status .NE.nf_noerr) THEN
!!         print *,nf_strerror(status)
!!         STOP 'Stopped in NC_4DCLIMATO def nvid'
!!      ENDIF

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

      status=nf_def_var(ncid,'lev',nf_float,one,levid ,idlev)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var lev'
      ENDIF

      status=nf_def_var(ncid,'time',nf_float,one,timeid ,idtime)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var time'
      ENDIF
!
!xx      if(ipar == 1) then
!xx
!xx      cbdim(1)=timeid
!xx      cbdim(2)=nvid
!xx      status=nf_def_var(ncid,'climatology_bounds',nf_float,
!xx     &                                           two,cbdim,idcb)
!xx      IF (status .NE.nf_noerr) THEN
!xx         print *,nf_strerror(status)
!xx         STOP 'Stopped in NC_4DCLIMATO def clbnds'
!xx      ENDIF
!
!xx      ttime_len=2*time_len
!xx      status = nf_put_att_real(ncid,idcb,'climatology_bounds',
!xx     &                            nf_float,ttime_len,clbnds)
!xx      IF (status .NE.nf_noerr) THEN
!xx         print *,nf_strerror(status)
!xx         STOP 'Stopped in NC_4DCLIMATO put clbnds'
!xx      ENDIF
!xx      endif
!
      !-----------------------------------------     
      ! Give attributes to coordinate variables 
      !-----------------------------------------
!
      status=nf_put_att_text(ncid,idlon,'units',12,'degrees_east')
!!     &                  LEN_TRIM(l_units(1)),TRIM(l_units(1)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lon'
      ENDIF

      status=nf_put_att_text(ncid,idlat,'units',13,'degrees_north')
!!     &                  LEN_TRIM(l_units(2)),TRIM(l_units(2)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lat'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'units',LEN_TRIM('meter'),
     &                                                     'meter')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lev units'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'positive',LEN_TRIM('down'),
     &                                                        'down')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lev positive'
      ENDIF

      status = nf_put_att_text(ncid,idtime, 'units',
     &              LEN_TRIM(Ref_time),TRIM(Ref_time))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def timeunits'
      ENDIF

!!      string256='climatology_bounds'
!!      status = nf_put_att_text(ncid,idtime, 'climatology',
!!     &                 LEN_TRIM(string256),TRIM(string256))
!!      IF (status .NE.nf_noerr) THEN
!!         print *,nf_strerror(status)
!!         STOP 'Stopped in NC_4DCLIMATO def climbnds'
!!      ENDIF

!
      !-----------------------
      ! Define data variables
      !-----------------------
!
!===================================================
!
      dim(1)=lonid
      dim(2)=latid
      dim(3)=levid
      dim(4)=timeid
!
      four = 3
      status=nf_def_var(ncid,TRIM(sal_shname),nf_float,four,dim,id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var'
      ENDIF
!
      status=nf_put_att_text(ncid,id1,'long_name',
     &                 LEN_TRIM(sal_lgname),TRIM(sal_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lgname'
      ENDIF
!
      status=nf_put_att_text(ncid,id1,'units',
     &                 LEN_TRIM(sal_units),TRIM(sal_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',
     &                                   nf_float,one,psal_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',
     &                                   nf_float,one,psal_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'_FillValue',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 Fillvalue'
      ENDIF
!
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
!===================================================

      status=nf_def_var(ncid,TRIM(ers_shname),nf_float,four,dim,id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err3'
      ENDIF

      status=nf_put_att_text(ncid,id2,'long_name',
     &                 LEN_TRIM(ers_lgname),TRIM(ers_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_min',
     &                                   nf_float,one,ers_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',
     &                                   nf_float,one,ers_max)
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
!==========================================================
      status=nf_def_var(ncid,TRIM(tmp_shname),nf_float,four,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var'
      ENDIF
!
      status=nf_put_att_text(ncid,id3,'long_name',
     &                 LEN_TRIM(tmp_lgname),TRIM(tmp_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lgname'
      ENDIF
!
      status=nf_put_att_text(ncid,id3,'units',
     &                 LEN_TRIM(tmp_units),TRIM(tmp_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_min',
     &                                   nf_float,one,temp_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',
     &                                   nf_float,one,temp_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'_FillValue',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 Fillvalue'
      ENDIF
!
      status= nf_put_att_text(ncid,id3 , 'cell_methods',
     &                 LEN_TRIM(cellmeth),TRIM(cellmeth))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 cellmeth'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_def_var(ncid,TRIM(ert_shname),nf_float,four,dim,id4)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err3'
      ENDIF

      status=nf_put_att_text(ncid,id4,'long_name',
     &                 LEN_TRIM(ers_lgname),TRIM(ert_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_min',
     &                                   nf_float,one,ert_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_max',
     &                                   nf_float,one,ert_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax'
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
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!==========================================================
      status=nf_def_var(ncid,TRIM(sig_shname),nf_float,four,dim,id5)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var'
      ENDIF
!
      status=nf_put_att_text(ncid,id5,'long_name',
     &                 LEN_TRIM(sig_lgname),TRIM(sig_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lgname'
      ENDIF
!
      status=nf_put_att_real(ncid,id5,'valid_min',
     &                                   nf_float,one,RHO_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id5,'valid_max',
     &                                   nf_float,one,RHO_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id5,'_FillValue',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id5 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id5,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_def_var(ncid,TRIM(n2_shname),nf_float,four,dim,id6)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err3'
      ENDIF

      status=nf_put_att_text(ncid,id6,'long_name',
     &                 LEN_TRIM(n2_lgname),TRIM(n2_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF
!
      status=nf_put_att_real(ncid,id6,'valid_min',
     &                                   nf_float,one,n2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id6,'valid_max',
     &                                   nf_float,one,n2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax'
      ENDIF
      status=nf_put_att_real(ncid,id6,'_FillValue',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id6 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id6,'missing_value',
     &                                   nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!
!==========================================================
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
      status = nf_put_var_real(ncid,idlev,dep)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put dep'
      ENDIF
!
      status = nf_sync(ncid)
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
      start(1)=1
      start(2)=1
      start(3)=1
      start(4)=icdf
      count(1)=IM
      count(2)=JM
      count(3)=KM
      count(4)=1
! 

       write(*,*)'%%%%%% 1', id1

      status=nf_put_vara_real(ncid,id1, start, count,psal)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var'
      ENDIF
!
       write(*,*)'%%%%%% 2', id2

      status=nf_put_vara_real(ncid,id2, start, count,ers)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var1'
      ENDIF

       write(*,*)'%%%%%% 3', id3

      status=nf_put_vara_real(ncid,id3, start, count,temp)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put var2'
      ENDIF

       write(*,*)'%%%%%% 4', id4

      status=nf_put_vara_real(ncid,id4, start, count,ert)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put verr'
      ENDIF

       write(*,*)'%%%%%% 5', id5

      status=nf_put_vara_real(ncid,id5, start, count,RHO)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put reler'
      ENDIF
!
       write(*,*)'%%%%%% 6', id6

      status=nf_put_vara_real(ncid,id6, start, count,N2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put dbins'
      ENDIF
! 
       write(*,*)'%%%%%% fin'

      status = nf_sync(ncid)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped last sync'
      ENDIF
!
!     CLose the file up
      status=NF_CLOSE(ncid)

 
      RETURN
      END
