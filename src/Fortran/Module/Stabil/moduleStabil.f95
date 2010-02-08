MODULE moduleStabil

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module specifications               ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1
! -----------
SUBROUTINE albesi(valexu,nx,ny,nk,mask,dep,psal,temp,alpha,beta,RHO)
      IMPLICIT NONE

      INTEGER                        :: nx,ny,nk,i,j,k,ISIG,wpnb
      REAL*4  , DIMENSION(nx,ny,nk)  :: psal,temp,SIG
      REAL*4                         :: SA,TP,SIG0,SSA,CR,TPP,SAP
      REAL*4  , DIMENSION(nx,ny,nk)  :: RHO
      REAL*4  , DIMENSION(nx,ny)     :: DRHOS,DRHOT
      INTEGER , DIMENSION(nx,ny,nk)  :: mask
      REAL*4  , DIMENSION(nx,ny)     :: alpha,beta
      REAL*4  , DIMENSION(nk)        :: DEP
      REAL                           :: valexu,PR,RSS
      REAL                           :: RHOG,VW,VW0,VSTP
      REAL                           :: T0,S0,ST2,ST1,SS1,SS2,RHOZERO,sig1,sig2,prsdb

      REAL, DIMENSION (15)           :: C
      REAL, DIMENSION (5)            :: C1
      REAL, DIMENSION (7)            :: C2
      REAL, DIMENSION (14)           :: C3

      DATA C/999.842594d0, .6793952e-1,-.9095290e-2, .1001685e-3, -.1120083e-5,.6536332e-8, .8244930e+0,-.4089900e-2, &
       .7643800e-4,-.8246700e-6, .5387500e-8,-.5724660e-2,.1022700e-3,-.1654600e-5, .4831400e-3/
      DATA C1/19652.21e0,148.4206e0,-2.327105e0,1.360477e-02,-5.155288e-05/
      DATA C2/54.6746e0,-0.603459e0,1.09987e-02,-6.1670e-05,7.944e-02,1.6483e-02,5.3009e-04/
      DATA C3/3.239908e0,1.43713e-03,1.16092e-04,-5.77905e-07,2.2838e-03,-1.0981e-05,-1.6078e-06,1.91075e-04,8.50935e-05,&
      -6.12293e-06,5.2787e-08,-9.9348e-07,2.0816e-08,9.1697e-10/
      DATA RHOG /0.09926209279/

       prsdb=0.
!!       PR=RHOG*DEP(1)
       call  pzcon(1,9.81,0.,pr,dep(1))

       RHO(:,:,2) =  valexu
       DO j = 1,ny 
       DO i = 1,nx 
         if (mask(i,j,1) == 1) then

            SA = psal(i,j,1)
            RSS=SQRT(SA)
            CALL POTMP(PR,temp(i,j,1),SA,prsdb,TP)

                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)        &
                      +TP*(C(5)+TP*C(6)))))                   &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)    &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)       &
                      +TP*(C(13)+TP*C(14))))

                  VW=C1(1)+                                    &
                      TP*C1(2)+                                &
                      TP*TP*C1(3)+                             &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)

                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))

                  VSTP=VW0+PR*(                                         &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)         &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))      &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                  &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))              &
                      )

                  SIG0 = SIG0/(1.-PR/VSTP)

                  RHO(i,j,1) =  SIG0
           else
                  RHO(i,j,1) =  valexu
          end if
       ENDDO
       ENDDO

       DO j = 1,ny 
       DO i = 1,nx 
         if (mask(i,j,1) == 1) then

                 SA = psal(i,j,1)
                 TPP = temp(i,j,1) +5.E-2
                 RSS=SQRT(SA)
                 CALL POTMP(PR,TPP,SA,prsdb,TP)

                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)        &
                      +TP*(C(5)+TP*C(6)))))                   &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)    &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)       &
                      +TP*(C(13)+TP*C(14))))


                  VW=C1(1)+                                    &
                      TP*C1(2)+                                &
                      TP*TP*C1(3)+                             &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)

                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))

                  VSTP=VW0+PR*(                                         &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)         &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))      &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                  &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))              &
                      )

                  SIG0 = SIG0/(1.-PR/VSTP)

                  DRHOT(i,j) =  SIG0

                 SA = psal(i,j,1)+1.E-2
                 RSS=SQRT(SA)
                 CALL POTMP(PR,temp(i,j,1),SA,prsdb,TP)

                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)       &
                      +TP*(C(5)+TP*C(6)))))                  &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)   &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)      &
                      +TP*(C(13)+TP*C(14))))


                  VW=C1(1)+                                   &
                      TP*C1(2)+                               &
                      TP*TP*C1(3)+                            &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)

                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))

                  VSTP=VW0+PR*(                                       &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)       &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))    &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))            &
                      )

                  SIG0 = SIG0/(1.-PR/VSTP)

                  DRHOS(i,j) =  SIG0

                  alpha(i,j) = (-1./RHO(i,j,1))*(DRHOT(i,j)-RHO(i,j,1))  &
                               /5.E-2

                   beta(i,j) = ( 1./RHO(i,j,1))*(DRHOS(i,j)-RHO(i,j,1))  &
                               /1.E-2

         else

                  DRHOT(i,j) =  valexu
                  DRHOS(i,j) =  valexu
                  alpha(i,j) = 0.0
                  beta(i,j)  = 0.0

         end if
       ENDDO
       ENDDO

      RETURN
END SUBROUTINE 

! Procedure 2
! -----------

SUBROUTINE NC_3DFILE(var,verr,dbins,clo,cla,im4,jm4,km,dep,valexu, &
     var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,                  &
     file_name,var_shname,var_lgname,                                    &
     err_shname,err_lgname,var_units, title_string)
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
        real*4                              :: valexu,var_min,var_max,ver_min,ver_max,dbin_min,dbin_max
!
        real*4  ,dimension(im4,jm4,km)        :: var,verr,dbins
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)              :: dep
!
	character (len=*)     :: file_name, title_string, var_shname,var_lgname,var_units, err_shname,err_lgname
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
      status=nf_put_att_text(ncid,idlon,'units',LEN_TRIM('degrees_east'),'degrees_east')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lon'
      ENDIF

      status=nf_put_att_text(ncid,idlat,'units',LEN_TRIM('degrees_north'),'degrees_north')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lat'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'units',LEN_TRIM('meters'),'meters')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lev units'
      ENDIF

      status=nf_put_att_text(ncid,idlev,'positive',LEN_TRIM('down'),'down')
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

      status=nf_put_att_text(ncid,id1,'long_name',LEN_TRIM(var_lgname),TRIM(var_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',nf_float,1,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',nf_float,1,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'missing_value',nf_float,1,valexu)
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

      status=nf_put_att_text(ncid,id2,'long_name',LEN_TRIM(err_lgname),TRIM(err_lgname))
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
      status=nf_put_att_real(ncid,id2,'valid_min',nf_float,1,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',nf_float,1,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valexerr'
      ENDIF

      status=nf_def_var(ncid,'dbins',nf_float,3,dim,id3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE def err'
      ENDIF

      status=nf_put_att_text(ncid,id3,'long_name',LEN_TRIM('data bins'),'data bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att errlgname'
      ENDIF

! 
      status=nf_put_att_real(ncid,id3,'valid_min',nf_float,1,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',nf_float,1,dbin_max)
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
      status=nf_put_att_real(ncid,id3,'missing_value',nf_float,1,valexu)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_3DFILE att valexerr'
      ENDIF
!
      !----------------------
      !Put global attributes
      !----------------------
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'title',LEN_TRIM(title_string),TRIM(title_string))
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
END SUBROUTINE

! Procedure 3
! ------------
SUBROUTINE NC_4DCLIM(im4,jm4,km,ipar,time_len,time_val,clbnds,var,   &
       var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,        &
       clo,cla,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,                      &
       var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,           &
       dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,             &
       verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,                      &
       obin_min,obin_max,rl_min,rl_max,VALEXC,l_units,                     &
       L_file_name,file_name,L_var_shname,var_shname,                      &
       L_var_lgname,var_lgname,L_var_units,var_units,                      &
       L_vrb_units,vrb_units,L_title_string,title_string,                  &
       L_cellmeth,cellmeth,L_Ref_time,Ref_time,                            &
       L_INSTITUT,INSTITUT,L_PRODUCTION,PRODUCTION,L_SOURCE,SOURCE,        &
       L_COMMENT,COMMENT)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
        integer*4   :: im4,jm4 
!!        integer   :: im4,jm4 
        integer     :: one,two,foor,ttime_len,km ,time_len,ipar,im,jm, three
        integer     :: L_file_name,L_var_shname,L_var_lgname,L_var_units,L_vrb_units,L_title_string,L_cellmeth,L_Ref_time, &
       L_INSTITUT,L_PRODUCTION,L_SOURCE,L_COMMENT
!
        real*4                              ::                           &
                 var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,      &
                 vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,    &
                 var2_min,var2_max,verel_min,verel_max,                  &
                 vbt1_min,vbt1_max,vbt2_min,vbt2_max,                    &
                 obin_min,obin_max,rl_min,rl_max
        real*4                              :: hrss,time_val, valexc
!
        real*4  ,dimension(im4,jm4,km)        :: var, var1, var2          &
                                            , Verr,reler,dbins            &
                                            , obins,rlfield
        real*4  ,dimension(im4,jm4)          :: varbot,varb1,varb2
        real*4  ,dimension(2,time_len)       :: clbnds
!     &                                      , climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep, valexu               &
                                            , CORLEN, SN, VARBAK
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
	character (len=256)   :: err_shname,err_lgname        &
                              , var1_shname,var1_lgname       &
                              , var2_shname,var2_lgname       &
                              , varb1_shname,varb1_lgname     &
                              , varb2_shname,varb2_lgname     &
                              , rer_shname,rer_lgname         &
                              , vbt_shname,vbt_lgname         &
                              , string256
!
	character (len=12)   :: Real_clock
!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                        :: id1,id1_1,id1_2,id2,id2_1        &
                                     , id3,id3_1,id4,id2d1,id2d2,id2d3   &
                                     , id0_1,id0_2,id0_3,idcb
      integer                        :: timeid,idtime, icdf
!
      integer                      :: IREG,ISPEC,ICOOC
      integer                 :: lonid,latid,depthid,nvid
      integer                 :: idlon,idlat,iddepth
      integer                 :: ncid,status
      integer                 :: OLDMOD
!
      save                    :: id1,id1_1,id1_2,id2,id2_1         &
                              , id3,id3_1,id4,id2d1,id2d2,id2d3    &
                              , id0_1,id0_2,id0_3,idcb
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
      WRITE(err_lgname,'("Error standard deviation of ",a)')TRIM(var_lgname)
!
      WRITE(vbt_lgname,'("Deepest values of ",a)')TRIM(var_lgname)
!
      WRITE(rer_lgname,'("Relative error of ",a)')TRIM(var_lgname)
!
      WRITE(var1_lgname,'(a," masked using relative error threshold 0.3 ")')TRIM(var_lgname)
!
      WRITE(var2_lgname,'(a," masked using relative error threshold 0.5 ")')TRIM(var_lgname)
!
      WRITE(varb1_lgname,'(a," masked using relative error threshold 0.3 ")')TRIM(vbt_lgname)
!
      WRITE(varb2_lgname,'(a," masked using relative error threshold 0.5 ")')TRIM(vbt_lgname)
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
      status=nf_def_var(ncid,'climatology_bounds',nf_float,two,cbdim,idcb)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def clbnds'
      ENDIF
!
      ttime_len=2*time_len
      status = nf_put_att_real(ncid,idcb,'climatology_bounds',nf_float,ttime_len,clbnds)
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
      status=nf_put_att_text(ncid,idlon,'units',LEN_TRIM(l_units(1)),TRIM(l_units(1)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lon'
      ENDIF

      status=nf_put_att_text(ncid,idlat,'units',LEN_TRIM(l_units(2)),TRIM(l_units(2)))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lat'
      ENDIF

      status=nf_put_att_text(ncid,iddepth,'units',LEN_TRIM('meters'),'meters')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att depth units'
      ENDIF

      status=nf_put_att_text(ncid,iddepth,'positive',LEN_TRIM('down'),'down')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att depth positive'
      ENDIF

      status = nf_put_att_text(ncid, timeid, 'units',LEN_TRIM(Ref_time),TRIM(Ref_time))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def timeunits'
      ENDIF

      string256='climatology_bounds'
      status = nf_put_att_text(ncid, timeid, 'climatology',LEN_TRIM(string256),TRIM(string256))
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
      status=nf_put_att_text(ncid,id0_1,'long_name',LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_1 lgname'
      ENDIF
      status=nf_put_att_text(ncid,id0_1,'units',LEN_TRIM(l_units(2)),TRIM(l_units(2)))
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
      status=nf_put_att_text(ncid,id0_2,'long_name',LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_2 lgname'
      ENDIF
      string256=' '
      status=nf_put_att_text(ncid,id0_2,'units',LEN_TRIM(string256),TRIM(string256))
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
      status=nf_put_att_text(ncid,id0_3,'long_name',LEN_TRIM(string256),TRIM(string256))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att0_3 lgname'
      ENDIF
!
      status=nf_put_att_text(ncid,id0_3,'units',LEN_TRIM(vrb_units),TRIM(vrb_units))
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
      foor = 4
      status=nf_def_var(ncid,TRIM(var_shname),nf_float,foor,dim,id1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var'
      ENDIF
!
      status=nf_put_att_text(ncid,id1,'long_name',LEN_TRIM(var_lgname),TRIM(var_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',nf_float,one,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',nf_float,one,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varmax'
      ENDIF
      status=nf_put_att_real(ncid,id1,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 Fillvalue'
      ENDIF

      status= nf_put_att_text(ncid,id1 , 'cell_methods',LEN_TRIM(cellmeth),TRIM(cellmeth))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1 cellmeth'
      ENDIF

!
      status=nf_put_att_real(ncid,id1,'missing_value',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
!
!===================================================

      status=nf_def_var(ncid,TRIM(err_shname),nf_float,foor,dim,id2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err3'
      ENDIF

      status=nf_put_att_text(ncid,id2,'long_name',LEN_TRIM(err_lgname),TRIM(err_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF

      status=nf_put_att_text(ncid,id2,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errunits'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_min',nf_float,one,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',nf_float,one,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax'
      ENDIF
      status=nf_put_att_real(ncid,id2,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'missing_value',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexerr'
      ENDIF
!-----------------------
!
      status=nf_def_var(ncid,TRIM(var1_shname),nf_float,foor,dim,id1_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def var1'
      ENDIF

      status=nf_put_att_text(ncid,id1_1,'long_name',LEN_TRIM(var1_lgname),TRIM(var1_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v1lgname'
      ENDIF

      status=nf_put_att_text(ncid,id1_1,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att var1units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_min',nf_float,one,var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_max',nf_float,one,var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
      status=nf_put_att_real(ncid,id1_1,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'missing_value',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv1'
      ENDIF


      status=nf_def_var(ncid,TRIM(var2_shname),nf_float,foor,dim,id1_2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def err2'
      ENDIF

      status=nf_put_att_text(ncid,id1_2,'long_name',LEN_TRIM(var2_lgname),TRIM(var2_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF

      status=nf_put_att_text(ncid,id1_2,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2units'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_min',nf_float,one,var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2min'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_max',nf_float,one,var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2max'
      ENDIF
      status=nf_put_att_real(ncid,id1_2,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id1_2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'missing_value',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valexv2'
      ENDIF
!--------------------

      status=nf_def_var(ncid,TRIM(rer_shname),nf_float,foor,dim,id2_1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def relerr'
      ENDIF

      status=nf_put_att_text(ncid,id2_1,'long_name',LEN_TRIM(rer_lgname),TRIM(rer_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att errlgname'
      ENDIF
      string256=' '
      status=nf_put_att_text(ncid,id2_1,'units',LEN_TRIM(string256),TRIM(string256))

      status=nf_put_att_real(ncid,id2_1,'valid_min',nf_float,one,verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_max',nf_float,one,verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmax'
      ENDIF
      status=nf_put_att_real(ncid,id2_1,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id2_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'missing_value',nf_float,one,valexc)
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

      status=nf_put_att_text(ncid,id3,'long_name',LEN_TRIM('Logarithm10 of number of data in bins'),  &
                   'Logarithm10 of number of data in bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dblgname'
      ENDIF

! 
      status=nf_put_att_real(ncid,id3,'valid_min',nf_float,one,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',nf_float,one,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax'
      ENDIF
      status=nf_put_att_real(ncid,id3,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'missing_value',nf_float,one,valexc)
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

      status=nf_put_att_text(ncid,id3_1,'long_name',                   &
        LEN_TRIM('Logarithm10 of number of outliers data in bins'),    &
                 'Logarithm10 of number of outliers data in bins')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att oblgname'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3_1,'valid_min',nf_float,one,obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'valid_max',nf_float,one,obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmax'
      ENDIF
      status=nf_put_att_real(ncid,id3_1,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id3_1 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'missing_value',nf_float,one,valexc)
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

      status=nf_put_att_text(ncid,id4,'long_name',  &
          LEN_TRIM('Correlation length field'),     &
                   'Correlation length field')
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rllgname'
      ENDIF
! 
      status=nf_put_att_real(ncid,id4,'valid_min',nf_float,one,rl_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmin'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_max',nf_float,one,rl_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlbmax'
      ENDIF
      status=nf_put_att_real(ncid,id4,'_FillValue',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO id4 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'missing_value',nf_float,one,valexc)
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
      three = 3
      status=nf_def_var(ncid,TRIM(vbt_shname),nf_float,three,dim2,id2d1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def vbt'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d1,'long_name',LEN_TRIM(vbt_lgname),TRIM(vbt_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d1,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_min',nf_float,one,vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_max',nf_float,one,vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt max'
      ENDIF
      status=nf_put_att_real(ncid,id2d1,'_FillValue',nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'missing_value',nf_float,one,valexc)
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
      three = 3
      status=nf_def_var(ncid,TRIM(varb1_shname),nf_float,three,dim2,id2d2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def varb1'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d2,'long_name',LEN_TRIM(varb1_lgname),TRIM(varb1_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb1 lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d2,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb1 units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_min',nf_float,one,vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1 min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_max',nf_float,one,vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1 max'
      ENDIF
      status=nf_put_att_real(ncid,id2d2,'_FillValue',nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'missing_value',nf_float,one,valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valex'
      ENDIF
!==========================================================
!
!
      three = 3
      status=nf_def_var(ncid,TRIM(varb2_shname),nf_float,three,dim2,id2d3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO def varb2'
      ENDIF
!
      status=nf_put_att_text(ncid,id2d3,'long_name',LEN_TRIM(varb2_lgname),TRIM(varb2_lgname))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb2 lgname'
      ENDIF

      status=nf_put_att_text(ncid,id2d3,'units',LEN_TRIM(var_units),TRIM(var_units))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att varb2 units'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'valid_min',nf_float,one,vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2 min'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'valid_max',nf_float,one,vbt2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2 max'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'_FillValue',nf_float,one,valexc)
!     &                                   nf_float,one,nf_fill_real)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO vbt2 Fillvalue'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d3,'missing_value',nf_float,one,valexc)
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
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'Conventions',LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO conv'
      ENDIF
!
      string256='SeaDataNet'
      status=nf_put_att_text(ncid,NF_GLOBAL,'project',LEN_TRIM(string256),string256)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO sdn'
      ENDIF
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'institution',LEN_TRIM(INSTITUT),INSTITUT)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO ulg'
      ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'production',LEN_TRIM(PRODUCTION),PRODUCTION)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att prod'
      ENDIF
!
      CALL DATE_AND_TIME(Real_clock) 
      STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'date',LEN_TRIM(Real_clock),Real_clock)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att date'
      ENDIF
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'title',LEN_TRIM(title_string),TRIM(title_string))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att title'
      ENDIF
!
      status=nf_put_att_text(ncid,NF_GLOBAL,'file_name',LEN_TRIM(file_name),TRIM(file_name))
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO glofilename'
      ENDIF
!
        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'source',LEN_TRIM(SOURCE),SOURCE)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO put att source'
      ENDIF

        STATUS=NF_PUT_ATT_TEXT(NCID,NF_GLOBAL,'comment',LEN_TRIM(COMMENT),COMMENT)
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
      status=nf_put_att_real(ncid,id2d1,'valid_min',nf_float,one,vbt_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbtmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d1,'valid_max',nf_float,one,vbt_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbtmax 2'
      ENDIF
      status=nf_put_att_real(ncid,id2d2,'valid_min',nf_float,one,vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1min 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2d2,'valid_max',nf_float,one,vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt1max 2'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'valid_min',nf_float,one,vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2min 2'
      ENDIF
      status=nf_put_att_real(ncid,id2d3,'valid_max',nf_float,one,vbt2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vbt2max 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_min',nf_float,one,var_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1,'valid_max',nf_float,one,var_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att valmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_min',nf_float,one,var1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_1,'valid_max',nf_float,one,var1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_min',nf_float,one,var2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2min 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id1_2,'valid_max',nf_float,one,var2_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att v2max 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_min',nf_float,one,ver_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermin'
      ENDIF
!
      status=nf_put_att_real(ncid,id2,'valid_max',nf_float,one,ver_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att vermax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_min',nf_float,one,verel_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id2_1,'valid_max',nf_float,one,verel_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att verelmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3,'valid_min',nf_float,one,dbin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id3,'valid_max',nf_float,one,dbin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att dbmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id3_1,'valid_min',nf_float,one,obin_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id3_1,'valid_max',nf_float,one,obin_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att obmax 2'
      ENDIF
! 
      status=nf_put_att_real(ncid,id4,'valid_min',nf_float,one,rl_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmin 2'
      ENDIF
!
      status=nf_put_att_real(ncid,id4,'valid_max',nf_float,one,rl_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_4DCLIMATO att rlmax 2'
      ENDIF
!
      ttime_len=2*time_len
      status = nf_put_att_real(ncid,idcb,'climatology_bounds',nf_float,ttime_len,clbnds)
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
      RETURN
END SUBROUTINE

! Procedure 4
! -----------
SUBROUTINE NC_RD3DCL(im4,jm4,km,ipar,time_val,clbnds,                  &
       var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,      &
       clo,cla,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,                        &
       var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,             &
       dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,               &
       verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,                        &
       obin_min,obin_max,rl_min,rl_max,VALEXC,                               &
       file_name,var_shname)
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
        real*4                              :: valexc,                   &
                 var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,      &
                 vbt_min,vbt_max,var1_min,var1_max,clen_min,clen_max,    &
                 var2_min,var2_max,verel_min,verel_max,                  &
                 vbt1_min,vbt1_max,vbt2_min,vbt2_max,                    &
                 obin_min,obin_max,rl_min,rl_max
        real*4                              :: hrss,time_val
!
        real*4  ,dimension(im4,jm4,km)        :: var, var1, var2         &
                                            , Verr,reler,dbins           &
                                            , obins,rlfield
        real*4  ,dimension(im4,jm4)          :: varbot,varb1,varb2
!
        real*4  ,dimension(2,1)              :: clbnds                   &
                                             ,climatology_bounds
!   
        real*4  ,dimension(im4)              :: clo
        real*4  ,dimension(jm4)              :: cla
        real*4  ,dimension(km)               :: dep, valexu              &
                                            , CORLEN, SN, VARBAK
!
	character (len=*)     :: file_name,var_shname
	character (len=255)     :: vrb_units,var_lgname,var_units
	character (len=255)     :: title_string, Ref_time,cellmeth       &
                              , INSTITUTION,PRODUCTION,SOURCE,COMMENT
        character (len=20), DIMENSION(2)           :: l_units
!                                                                        &
	character (len=256)   :: err_shname,err_lgname                    &
                              , var1_shname,var1_lgname                  &
                              , var2_shname,var2_lgname                  &
                              , rer_shname,rer_lgname                    &
                              , varb1_shname,varb1_lgname                &
                              , varb2_shname,varb2_lgname                &
                              , vbt_shname,vbt_lgname                    &
                              , string256
!
      integer  ,dimension(2)              :: cbdim, pardim
      integer  ,dimension(2)              :: stpar, prcnt
      integer  ,dimension(3)              :: dim2
      integer  ,dimension(3)              :: star2, coun2
      integer  ,dimension(4)              :: dim
      integer  ,dimension(4)              :: start, count
      integer                         :: id1,id1_1,id1_2,id2,id2_1         &
                                      , id3,id3_1,id4,id2d1,id2d2,id2d3    &
                                      , id0_1,id0_2,id0_3,idcb
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
      status = nf_get_att_real(ncid,idcb,'climatology_bounds',clbnds)
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
!
      status=nf_get_att_real(ncid,id1,'missing_value',valexc)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att valexc'
      ENDIF
!
      status= nf_get_att_text(ncid,id1 , 'cell_methods',cellmeth)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL cellmeth'
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
         STOP 'Stopped in NC_READ3DCL def vardeep'
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
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(varb1_shname),id2d2)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def vardeep'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d2,'long_name',varb1_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ3DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d2,'valid_min',vbt1_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d2,'valid_max',vbt1_max)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vbtmax'
      ENDIF
!
!-----------------------------------------------------------------
!
      status=nf_inq_varid(ncid,TRIM(varb2_shname),id2d3)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL def vardeep'
      ENDIF
!
!x      status=nf_get_att_text(ncid,id2d3,'long_name',varb2_lgname)
!x      IF (status .NE.nf_noerr) THEN
!x         print *,nf_strerror(status)
!x         STOP 'Stopped in NC_READ3DCL att lgname'
!x      ENDIF
!
      status=nf_get_att_real(ncid,id2d3,'valid_min',vbt2_min)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL att vbtmin'
      ENDIF
!
      status=nf_get_att_real(ncid,id2d3,'valid_max',vbt2_max)
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
      status=nf_get_vara_real(ncid,id2d2, star2, coun2,varb1)
      IF (status .NE.nf_noerr) THEN
         print *,nf_strerror(status)
         STOP 'Stopped in NC_READ3DCL get varbot'
      ENDIF
!
      status=nf_get_vara_real(ncid,id2d3, star2, coun2,varb2)
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
END SUBROUTINE

! Procedure 5
! -----------
      subroutine N2brunt(valex,imax,jmax,maxt,Z,ict,alpha,beta,T,S,N2,rmsp,rmsn)
      implicit none
!
! in : z, T, S fields
! out: N2 = db/dz field, where b is the buoyency
!      rmsn = rms of all the negative N2 in the field
!      rmsp =                positive
!
      integer  :: imax,jmax,maxt
      real, dimension(imax,jmax,maxt), intent(in) :: S,T
      real, dimension(imax,jmax)                  :: alpha,beta
      integer ,dimension(imax,jmax,maxt), intent(in) :: ict
      real, dimension(imax,jmax,maxt-1), intent(out) :: N2
      real, dimension(maxt), intent(in) :: Z
      real, intent(out) :: rmsp,rmsn

      real, dimension(imax,jmax,maxt) :: B
      real :: DZ,G,FACA,FACB
!
      real :: valex
      integer :: i,j,k, nv,np,nn,istat

! inplace bouyancy in place
!
      G = 9.81
      do j=1,jmax
      do i=1,imax
        IF (ict(i,j,1) == 1) then
           B(i,j,1) = G*alpha(i,j)*(T(i,j,2)-T(i,j,1))
           B(i,j,2) = G*beta(i,j) *(S(i,j,2)-S(i,j,1))
        else
          B(i,j,1:2) = valex
        endif
      enddo
      enddo
!
      nn=0
      np=0
      nv=0
      rmsn=0.
      rmsp=0.

      k=1
       do j=1,jmax
        do i=1,imax
           if (ict(i,j,k) == 1 ) then
              N2(i,j,k)=(B(i,j,1)-B(i,j,2))/(Z(k+1)-Z(k))
              if (N2(i,j,k).lt.0.) then
                rmsn=rmsn+(N2(i,j,k)*N2(i,j,k))
                nn=nn+1
              else
                rmsp=rmsp+(N2(i,j,k)*N2(i,j,k))
                np=np+1
              end if
           else
              N2(i,j,k)=valex
              nv=nv+1
           end if
        end do
       end do
      !=========

      if (nn.gt.0) then
        rmsn=sqrt(rmsn/nn)
      else
        rmsn=0.0
      end if
      if (np.gt.0) then
        rmsp=sqrt(rmsp/np)
      else
        rmsp=0.0
      end if
!
!xx      write(*,*)'  rmsp = ',rmsp
!xx      write(*,*)'  rmsn = ',rmsn

end subroutine

! Procedure 6
! -----------
      SUBROUTINE POTMP(PRESS,TEMP,S,RP,POTEMP)
!C
!C     TITLE:
!C     *****
!C
!C       POTMP  -- CALCULATE POTENTIAL TEMPERATURE FOR AN ARBITRARY
!C                 REFERENCE PRESSURE
!C
!C     PURPOSE:
!C     *******
!C
!C       TO CALCULATE POTENTIAL TEMPERATURE
!C
!C       REF: N.P. FOFONOFF
!C            DEEP SEA RESEARCH
!C            IN PRESS NOV 1976
!C
!C     PARAMETERS:
!C     **********
!C
!C       PRESS  -> PRESSURE IN DECIBARS
!C       TEMP   -> TEMPERATURE IN CELSIUS DEGREES
!C       S      -> SALINITY PSS 78
!C       RP     -> REFERENCE PRESSURE IN DECIBARS
!C                 (0.0 FOR THE QUANTITY THETA)
!C       POTEMP <- POTENTIAL TEMPERATURE (DEG C)
!C
        REAL PRESS,TEMP,S,RP,POTEMP
!C
!C     VARIABLES:
!C     *********
!C
         INTEGER I,J,N
         REAL*4 DP,P,Q,R1,R2,R3,R4,R5,S1,T,X
!C
!C     CODE:
!C     ****
!C
      S1 = S-35.0
      P  = PRESS
      T  = TEMP
!C
      DP = RP - P
      N  = IFIX(ABS(DP)/1000.) + 1
      DP = DP/FLOAT(N)
!C
      DO 10 I=1,N
         DO 20 J=1,4
!C
            R1 = ((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P
            R2 = (2.7759E-12*T-1.1351E-10)*S1
            R3 = ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T
            R4 = (R1+(R2+R3+1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*S1
            R5 = R4+((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5
!C
            X  = DP*R5
!C
            GO TO (100,200,300,400),J
!C
  100       CONTINUE
            T = T+.5*X
            Q = X
            P = P + .5*DP
            GO TO 20
!C
  200       CONTINUE
            T = T + .29298322*(X-Q)
            Q = .58578644*X + .121320344*Q
            GO TO 20
!C
  300       CONTINUE
            T = T + 1.707106781*(X-Q)
            Q = 3.414213562*X - 4.121320344*Q
            P = P + .5*DP
            GO TO 20
!C
  400       CONTINUE
            T = T + (X-2.0*Q)/6.0
  20      CONTINUE
  10    CONTINUE
!C
        POTEMP = T
        RETURN
!C
!C       END POTMP
!C
END SUBROUTINE

! Procedure 7
! -----------
      subroutine pzcon(cnvrsn,grav,dynz,prsdb,depth)
!c
!c     title:
!c     *****
!c
!c       pzcon  -- convert pressure in decibars to depth in meters
!c                 (or visa versa)
!c
!c     system:
!c     ******
!c
!c       pacodf hydrographic data library
!c
!c     purpose:
!c     *******
!c
!c       to calculate depth in meters (mantyla,saunders)
!c       from pressure in decibars (or pressure from depth).
!c
!c       ref: journ. physical ocean.,vol 11 no. 4, april, 1981
!c            (saunders)
!c            private correspondence 1982-1983
!c            (mantyla)
!c
!c     method:
!c     ******
!c
!c       a standard ocean (salinity=35.0,t=0.0) is used plus a dynamic
!c       height correction to account for deviations from the standard
!c       ocean. pressure to depth conversion is effected as:
!c
!c       z = p/(b*c) + dynz/b
!c
!c       where:
!c
!c          p    = insitu pressure (decibars)
!c          b    = insitu gravity as a function of latitude and pressure
!c                 (decameters/sec/sec)
!c          c    = insitu mean density rho(35.0,0.0,p)
!c                 (grams/centimeter**3)
!c          dynz = dynamic height in dynamic meters
!c          z    = depth in meters
!c
!c     parameters:
!c     **********
!c
!c       cnvrsn  -> conversion to be performed:
!c                  0 = pressure to depth
!c                  1 = depth to pressure
!c       grav    -> acceleration of gravity meters/sec/sec
!c                  at station latitude, pressure =0.0db
!c       dynz    -> dynamic height in dynamic meters
!c       prsdb   -> pressure in decibars (cnvrsn=0)
!c               <- pressure in decibars (cnvrsn=1)
!c       depth   <- depth in meters (cnvrsn=0)
!c               -> depth in meters (cnvrsn=1)
!c
        integer cnvrsn
        real grav,prsdb,dynz,depth
!c
!c     variables:
!c     *********
!c
        real*4 a,b,c
        real*8 dd,dg,dh,da,db,dc
!c
!c     external functions:
!c     ******** *********
!c
        integer isnan
!c
!c     code:
!c     ****
!c
!c     select
!c       (convert pressure to depth):
          if(cnvrsn.ne.0) go to 10
          a = 2.2e-6*prsdb
!c             /* pressure correction to gravity */
          b = 0.1*(grav+0.5*a)
!c             /* insitu gravity decameters/sec/sec */
!c
          c = 1.0285+a
!c             /* insitu mean density rho(35.0,0.0,p) */
!c
          depth = prsdb/(b*c)
!c             /* pressure to depth conversion in standard ocean */
!c
!c         select
!c           (dynamic height):
!c
        	if(dynz.eq.9.9e37) then
		isnan = 1
	        else
		isnan = 0
	        endif
              if(isnan.eq.0) then
                depth = depth + dynz/b
              endif
!c           (otherwise):
!c         end select
          go to 999
!c       (convert depth to pressure):
10     continue
          dd = dble(depth)
          dg = dble(grav*0.1)
          dh = dble(dynz)
          da = 2.42d-13*dd
          db = dd*(1.13135d-7+2.2e-6*dg)-1.0
          dc = dg*dd
!c         select
!c           (dynamic height):


        	if(dynz.eq.9.9e37) then
		isnan = 1
	        else
		isnan = 0
	        endif
              if(isnan.ne.0) then
                 goto 20
              endif
              
              db = db - 2.2d-6*dh
              dc = dc-dh
!c           (otherwise):
20         continue
!c         end select
!c
          dc = 1.0285d0*dc
!c
          prsdb  = 0.0
          if(da.ne.0.0) then
             prsdb  = sngl((-db-dsqrt(db*db-4.0d0*da*dc))/(da+da))
          endif
!c     end select
!c
!c     /* return */
!c
  999 continue
!c
      return
!c
!c     end pzcon
!c
end subroutine

! Procedure 8
! -----------
SUBROUTINE NC_OUTDAT2D(klev,iter,rec,TP,SA,N2,SIG,clo,cla,dep,im4,jm4,valexu)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
       CHARACTER (len=19)                     :: ANADAT

        integer*4                             :: im4,jm4
        integer                               :: im,jm
        real*4  ,dimension(im4,jm4,2)           :: TP,SA
        real*4  ,dimension(im4,jm4)             :: SIG,N2
        real*4  ,dimension(im4)                :: clo
        real*4  ,dimension(jm4)                :: cla
        real*4  ,dimension(2)                 :: dep
        real*4                                :: fmis, valexu, hrss
!
        CHARACTER (len=11) , dimension(4)     :: var1, vard1, u1
!
	integer  ,dimension(3)                :: dim1
	integer  ,dimension(4)                :: id1,dim
	integer  ,dimension(3)                :: start1, count1
	integer  ,dimension(4)                :: start, count
!
	integer                         :: lonid,latid,levid,timeid
	integer                         :: idlon,idlat,idlev,idtime
        integer                         :: ncid,status
        integer                         :: icdf, klev, num
        integer                         :: iter, rec
!
	save                            :: icdf
	save                            :: ncid
	save                            :: id1
	save                            :: lonid,latid,levid,timeid
	save                            :: idlon,idlat,idlev,idtime
!
!----------------------------------------------------------
!
	data fmis /-1.e22/

	data var1 /'TP','SA','N2','NVF'/

	data vard1 /'Temperature','Salinity','B.V.-freq','LOCATIONS'/

	data u1 /'c','parts/thnd',' ',' '/
!
!---------------------------------------------------------------
!
      IM = IM4
      JM = JM4
!
      ANADAT='nsquare_T1.1xxxx.nc'            !  file name
!
      WRITE(ANADAT(13:16),'(I4.4)') klev
!
      if (iter == 1 .and. rec == 1) then
          icdf = 0
	!-----------------------
	! create the data file
	!-----------------------

	status = nf_create(ANADAT, nf_share,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Put in define mode
	!----------------------
 	
	status = nf_redef(ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Define dimensions
	!----------------------

	status=nf_def_dim(ncid,'lon',IM,lonid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'lat',JM,latid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'lev',2,levid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'time',nf_unlimited,timeid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------------
        ! Define coordinate variables
	!----------------------------
 
	status=nf_def_var(ncid,'lon',nf_float,1, lonid,idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'lat',nf_float,1,latid ,idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'lev',nf_float,1,levid ,idlev)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'time',nf_float,1,timeid ,idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------------------------     
        ! Give attributes to coordinate variables 
	!-----------------------------------------

	status=nf_put_att_text(ncid,idlon,'units',12,'degrees_east')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idlat,'units',13,'degrees_north')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idlev,'units',13,'level')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idtime,'units',5,'hours')
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Define data variables
	!-----------------------

	dim(1)=lonid
	dim(2)=latid
	dim(3)=levid
	dim(4)=timeid

	do num=1,2

	status=nf_def_var(ncid,var1(num),nf_float,4,dim,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value',nf_float,1,valexu)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo


	dim1(1)=lonid
	dim1(2)=latid
	dim1(3)=timeid

	do num=3,4

	status=nf_def_var(ncid,var1(num),nf_float,3,dim1,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value',nf_float,1,valexu)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo
 
	!----------------------
	!Put global attributes
	!----------------------

	status=nf_put_att_text(ncid,NF_GLOBAL,'title',35,		' DIVA: Hydrostatic stabilisation ')

	!--------------------
	!End define mode
	!--------------------
      
	status = nf_enddef(ncid)
       if (status .ne. nf_noerr) call handle_err(status)


       status = nf_put_var_real(ncid,idlon,clo)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlat,cla)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlev,dep)
       if (status .ne. nf_noerr) call handle_err(status)

      else

       if(rec == 1) then
	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lon',idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lat',idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lev',idlev)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'time',idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	do num=1,4

          status=nf_inq_varid(ncid,var1(num),id1(num))
          if (status .ne. nf_noerr) call handle_err(status)

	enddo

      endif

	status=nf_inq_dimlen(ncid,idtime,icdf)
	if (status .ne. nf_noerr) call handle_err(status)
!
      endif


        status = nf_sync(ncid)

	icdf=icdf+1

	hrss = (icdf-1)*1. 
	status = nf_put_var1_real(ncid, idtime, icdf, hrss)

	start1(1)=1
	start1(2)=1
	start1(3)=icdf
	count1(1)=IM
	count1(2)=JM
	count1(3)=1

	status=nf_put_vara_real(ncid,id1(3), start1, count1,N2)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_vara_real(ncid,id1(4), start1, count1,SIG)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	start(1)=1
	start(2)=1
	start(3)=1
	start(4)=icdf
	count(1)=IM
	count(2)=JM
	count(3)=2
	count(4)=1

	status=nf_put_vara_real(ncid,id1(1), start, count,TP)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

	status=nf_put_vara_real(ncid,id1(2), start, count,SA)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
 
    	RETURN
END SUBROUTINE

END MODULE moduleStabil
