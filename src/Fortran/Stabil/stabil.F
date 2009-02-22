!
      PROGRAM ADDVAR
!
      implicit none
!
!----------------------------------------------------------------------
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
!
      DOUBLE PRECISION :: lon,lat,val
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U
!
! Depth distribution
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: z_watercolumn
!
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: temp,psal,N2
     &                                           , sigma,RHO,ert,ers
      REAL*4 , DIMENSION(:,:),    ALLOCATABLE   :: alpha,beta
      REAL*4 , DIMENSION(:)    ,    ALLOCATABLE :: XLON,YLAT
!
      REAL*4     , DIMENSION(:,:), ALLOCATABLE :: dsig, dsigma
      REAL*4     , DIMENSION(:,:,:), ALLOCATABLE :: newtemp_lon
     &                                          , newtemp_lat
     &                                          , newtemp_val
     &                                          , newtemp_wht
     &                                          , newpsal_lon
     &                                          , newpsal_lat
     &                                          , newpsal_val
     &                                          , newpsal_wht
!
!
      REAL*4, DIMENSION(2)                 ::  sig
     &                                      ,  dep, Z
      REAL*4                               :: T1,T2,S1,S2,rmsp,rmsn
     &                                      , TO,SO,TM,SM
     &                                      , max_inst,EPST,EPSS,FACT
     &                                      , FACS,FACA,FACB,ETA,KSI
     &                                      , vlon, vlat, vval, wght
     &                                      , NN2_max,RWN
      REAL*4                               :: n2tild, relerr1, relerr2
     &                                      , N2R, epsilon3, epsilon4
     &                                      , KSI_mix, zz
     &                                      , CorLenT, CorLenS, CorLen
     &                                      , erc1,erc2
!
      INTEGER,DIMENSION(:,:,:),  ALLOCATABLE :: it_newdata,is_newdata
     &                                        , mask
      integer*4, DIMENSION(:,:), ALLOCATABLE :: ik_instabl
     &                                        , ij_instabl,ij_lay
      integer                              :: i,j,k,klev,ic,icl, nl
     &                                      , ii, jj, l,m,i1,i2,j1,j2
     &                                      , nb_wetpoint
     &                                      , tflag1,sflag1,iflag,ttdat
     &                                      , datacount,layer,tsflag
     &                                      , iter,maxit,io,ndata
     &                                      , tflag2,sflag2
      integer  , DIMENSION(2)    :: psaldata_nmb,tempdata_nmb,data_nmb
!
      integer                   :: surface_lev,top_lev,icdf
      integer                   :: NX, NY, NK
      integer*4                   :: KMAX, ipr, nw, IMAX, JMAX
      integer                   :: CL_i,CL_j
      integer                   :: isq, jsq, itrack
      real                      :: VALEXU, M_width
      real                      :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=72)                :: divafile, newdata, comment
      CHARACTER (len=32)                :: comments
      CHARACTER (len=4)                 :: lnum
      CHARACTER (len=22)                :: DEPTHS
      CHARACTER (len=16)                :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: DATAFILE
      CHARACTER (len=99), DIMENSION(2)  :: var_name
!
      CHARACTER (len=16)                :: REPORT
      LOGICAL                           :: exist
!
!--------------------------------------------------------------
!
!
! reading procedure information parameters
      EXECIN='../input/STBinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)! comments
      READ(2,*) var_name(1)
      READ(2,*)! comments
      READ(2,*) var_name(2)
      READ(2,*)! comments
      READ(2,*) MINLEV
      READ(2,*)! comments
      READ(2,*) MAXLEV
      READ(2,*)! comments
      READ(2,*) surface_lev
      ALLOCATE(z_watercolumn(surface_lev))
      READ(2,*)! comments
      READ(2,*) relerr1
      READ(2,*)! comments
      READ(2,*) relerr2
      READ(2,*)! comments
      READ(2,*) M_width
      READ(2,*)! comments
      READ(2,*) KSI_mix
      READ(2,*)! comments
      READ(2,*) maxit
      CLOSE(2)
!
      WRITE(VARFILEIN,
     &'(a,".1",i4.4,".analysis")')TRIM(var_name(1)),MINLEV
      WRITE(DATAFILE,
     &'(a,".1",i4.4)')TRIM(var_name(1)),MINLEV
      REPORT    = 'info.1xxxx.1xxxx'
!
! reading grid data
      file_name = '../output/3Danalysis/STABOUT/analysis/GridInfo.dat'
      open(unit=90,file=file_name)
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend
      close(90)
!
!arrays memory allocation
      NX = INT(xend)
      NY = INT(yend)
!
      ALLOCATE(newtemp_lon(NX,NY,2))
      ALLOCATE(newtemp_lat(NX,NY,2))
      ALLOCATE(newtemp_val(NX,NY,2))
      ALLOCATE(newtemp_wht(NX,NY,2))
      ALLOCATE(newpsal_lon(NX,NY,2))
      ALLOCATE(newpsal_lat(NX,NY,2))
      ALLOCATE(newpsal_val(NX,NY,2))
      ALLOCATE(newpsal_wht(NX,NY,2))
      ALLOCATE(is_newdata(NX,NY,2))
      ALLOCATE(it_newdata(NX,NY,2))
      ALLOCATE(ik_instabl(NX,NY))
      ALLOCATE(ij_instabl(NX,NY))
      ALLOCATE(ij_lay(NX,NY))
      ALLOCATE(dsigma(NX,NY))
      ALLOCATE(dsig(NX,NY))
      ALLOCATE(N2(NX,NY,1:1))
      ALLOCATE(U(NX*NY))
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(mask(1:NX,1:NY,1:2))
      ALLOCATE(sigma(NX,NY,2))
      ALLOCATE(RHO(NX,NY,2))
      ALLOCATE(ert(NX,NY,2))
      ALLOCATE(ers(NX,NY,2))
      ALLOCATE(alpha(NX,NY))
      ALLOCATE(beta(NX,NY))
!
!
      DEPTHS='../input/contour.depth'
      OPEN(2,FILE=DEPTHS,STATUS='OLD')
      nl = 1
 1    continue
      read(2,*,err=99,end=99) z_watercolumn(nl)
      nl=nl+1
      goto 1
 99   continue
      CLOSE(2)
!
! reading loop state prameters
      EXECIN='infoloop'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) itrack
      READ(2,*) layer
      READ(2,*) iter
      READ(2,*) iflag
      CLOSE(2)
!
      WRITE(REPORT,'("info.1",i4.4,".1",i4.4)')layer,layer+1
!
      istep = layer + 1
!
      WRITE(lnum(1:4),'(I4.4)') istep
!
      WRITE(VARFILEIN,
     &'("param.par.",a,".1",i4.4)')TRIM(var_name(1)),istep
      WRITE(file_name,'("../input/divaparam/",a)')TRIM(VARFILEIN)
!
      INQUIRE(FILE=file_name,EXIST=exist)
      IF(exist) THEN
         open(unit=2,file=file_name)
      ELSE
         divafile = '../input/divaparam/param.par.1'//lnum
         INQUIRE(FILE=divafile,EXIST=exist)
         IF(exist) THEN
           open(unit=2,file=divafile)
         ELSE
           divafile = '../input/divaparam/param.par'
           open(unit=2,file=divafile)
         ENDIF
      ENDIF
      READ(2,*)! comments
      READ(2,*) CorLenT
      CLOSE(2)
!
      WRITE(lnum(1:4),'(I4.4)') istep
!
      WRITE(VARFILEIN,
     &'("param.par.",a,".1",i4.4)')TRIM(var_name(2)),istep
      WRITE(file_name,'("../input/divaparam/",a)')TRIM(VARFILEIN)
!
      INQUIRE(FILE=file_name,EXIST=exist)
      IF(exist) THEN
         open(unit=2,file=file_name)
      ELSE
         divafile = '../input/divaparam/param.par.1'//lnum
         INQUIRE(FILE=divafile,EXIST=exist)
         IF(exist) THEN
           open(unit=2,file=divafile)
         ELSE
           divafile = '../input/divaparam/param.par'
           open(unit=2,file=divafile)
         ENDIF
      ENDIF
      READ(2,*)! comments
      READ(2,*) CorLenS
      CLOSE(2)
!
      CorLen = AMIN1(CorLenS,CorLenT)
!
      CL_i = INT(M_width*CorLen/dx)
      CL_j = INT(M_width*CorLen/dy)
!
      ALLOCATE(temp(NX,NY,2))
      ALLOCATE(psal(NX,NY,2))
!
!retreiving grid points longitude and latitude
      DO i = 1, NX
         XLON(i) = xorig+(i-1)*dx
      ENDDO
      DO j = 1, NY
         YLAT(j) = yorig+(j-1)*dy
      ENDDO
!
! Reading analysis files
      DO k = 1,2
        istep = k + layer - 1
!
        DO ivar = 1,2
!
        WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
        WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
            OPEN(12,FILE=file_name)
             read(12,*)! comments
             read(12,*)ndata
            CLOSE(12)
            IF(ndata == 0) THEN
              write(11,*)3
              stop
            ENDIF
!
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".analysis")')TRIM(var_name(ivar)),istep
        WRITE(file_name,
     &'("../output/3Danalysis/STABOUT/analysis/",a)')TRIM(VARFILEIN)
!
           close(84)
           open (unit=84,file=file_name,form='unformatted')
           CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
           if (KMAX.ne.1) stop "Not a 2D Field analysis"
           IF(ivar == 1) THEN
              psaldata_nmb(k)=ndata
              ic = 0
              DO j = 1,jmax
                 DO i = 1,imax
                    ic = ic + 1
                    psal(i,j,k) = U(ic)
                 ENDDO
              ENDDO
            ENDIF
            IF(ivar == 2) THEN
              tempdata_nmb(k)=ndata
              ic = 0
              DO j = 1,jmax
                 DO i = 1,imax
                    ic = ic + 1
                    temp(i,j,k) = U(ic)
                 ENDDO
              ENDDO
           ENDIF
        ENDDO
      ENDDO
!
! retreiving masks from data and levels dept
      mask = 1
      DO k = 1,2
         DO j = 1,jmax
           DO i = 1,imax
             if(psal(i,j,k) == valexu) mask(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
!
      DO j = 1,jmax
      DO i = 1,imax
      if ((mask(i,j,1) - mask(i,j,2)) > 0 )
     & mask(i,j,1) = mask(i,j,2)
      ENDDO
      ENDDO
!
! total considered wet points
      nb_wetpoint = 0
      DO j = 1,jmax
         DO i = 1,imax
           if( mask(i,j,1) == 1) nb_wetpoint = nb_wetpoint + 1
         ENDDO
      ENDDO
!
      k=0
      DO i = layer,layer+1
         k = k+1
        Z(k)  = -1.*z_watercolumn(i)  ! negatif values for depts
        dep(k)  = z_watercolumn(i)  !posetif values for depts
      ENDDO
!
      if(iter == 1) then
       ttdat = 0
       open (unit=17,file='fort.17')
           write(17,*) ttdat
       close(17)
!
       CALL albesi
     &  (valexu,nx,ny,nk,mask,dep,psal,temp,alpha,beta,RHO)
!
       open (unit=21,file='fort.21')
           write(21,*) alpha
           write(21,*) beta
       close(21)

        if(itrack .eq. 1) then 
          OPEN(UNIT=12,FILE=REPORT,STATUS='NEW')
          write(12,*)' ssssssssssssssssssssssssssssssssssssssssssssss'
          write(12,*)' sssssssssssssssssss STABILIZE ssssssssssssssss'
          write(12,*)' Stabilizing layer: ',layer,'  to',layer+1 
          write(12,*)' psal data number : ',psaldata_nmb
          write(12,*)' temp data number : ',tempdata_nmb
          write(12,*)' total wet grid points : ',nb_wetpoint
          write(12,*)' Layer thikness :  ',Z(2)-Z(1),'m'
          write(12,*)'Relative width of footprint mes: ',M_width
          write(12,*)'Relative error parameter eps1: ',relerr1
          write(12,*)'Relative error parameter eps2: ',relerr2
          CLOSE(12)
        else
          OPEN(UNIT=12,FILE=REPORT,STATUS='OLD',POSITION='APPEND')
          write(12,*)' ssssssssssssssssssssssssssssssssssssssssssssss'
          write(12,*)' ssss STABILIZE SECOND PROCESSING sssssssssssss'
          write(12,*)' psal data number : ',psaldata_nmb
          write(12,*)' temp data number : ',tempdata_nmb
          CLOSE(12)
        endif
      endif
!
      OPEN(UNIT=12,FILE=REPORT,STATUS='OLD',POSITION='APPEND')
!
      ik_instabl = 0
      ij_instabl = 0
      ij_lay = 0
      datacount  = 0
!
      dsig = 0.
!
! Sigma calculation
       sigma = valexu
      dsigma = valexu
!
! N2 calculation
      open (unit=21,file='fort.21')
         read(21,*) alpha
         read(21,*) beta
      close(21)
!
      CALL N2brunt
     &     (valexu,nx,ny,2,Z,mask,alpha,beta,temp,psal,N2,rmsp,rmsn)
!
      DO j = 1,jmax
         DO i = 1,imax
           if(mask(i,j,1) == 1) then
              dsigma(i,j) = 1.
              if(N2(i,j,1) < 0.0) then
                dsig(i,j) = N2(i,j,1)
                dsigma(i,j) = -1.  !N2(i,j,1)
!
                datacount = datacount + 1  ! instabilities number
                ik_instabl(i,j)  = 1       ! instabilities points
!
              endif
           endif
         ENDDO
      ENDDO
!
      IF(iter > maxit .OR. iflag >= 2) THEN
!
       if(itrack == 1) then
          CALL NC_OUTLEV1(layer,iter,itrack,temp(:,:,1),psal(:,:,1)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
       endif
          CALL NC_OUTLEV2(layer+1,iter,itrack,temp(:,:,2),psal(:,:,2)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
!
          write(12,*)' adddata finished performing for processing ',
     &itrack
          write(12,*)'positive N2 rms = ',rmsp
          write(12,*)'negative N2 rms = ',rmsn
          write(12,*)' psal data number : ',psaldata_nmb
          write(12,*)' temp data number : ',tempdata_nmb
          write(12,*)' '
!
          write(11,*)3
!
          stop
      ENDIF
!
      write(12,*)' sssssssssssssssssss'
      write(12,*)'  Iteration: ',iter
      write(12,*)' sssssssssssssssssss'
      write(12,*)'Total N2 instabilities : ',datacount
      write(12,*)'Posetive N2 rms : ',rmsp
      write(12,*)'Negative N2 rms : ',rmsn
      write(12,*)'Reference for N2 rms  ; ',max(rmsp,rmsn)
!!
      IF(datacount == 0) THEN
         write(11,*)3
         stop
      ELSE
! Reading analysis error files
      if(itrack .eq. 1) then 
       DO k = 1,2
        istep = k + layer - 1
        DO ivar = 1,2
!
          WRITE(VARFILEIN,
     &'(a,".1",i4.4,".error")')TRIM(var_name(ivar)),istep
          WRITE(file_name,
     &'("../output/3Danalysis/STABOUT/analysis/",a)')TRIM(VARFILEIN)
!
           close(84)
           open (unit=84,file=file_name,form='unformatted')
           CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
           if (KMAX.ne.1) stop "Not a 2D Field analysis"
           IF(ivar == 1) THEN
              ic = 0
              DO j = 1,jmax
                 DO i = 1,imax
                    ic = ic + 1
                    ers(i,j,k) = U(ic)
                 ENDDO
              ENDDO
            ENDIF
            IF(ivar == 2) THEN
              ic = 0
              DO j = 1,jmax
                 DO i = 1,imax
                    ic = ic + 1
                    ert(i,j,k) = U(ic)
                 ENDDO
              ENDDO
           ENDIF
        ENDDO
      ENDDO
      ENDIF
      endif
!
      isq = cl_i
      jsq = cl_j
!
      NN2_max = 0.
!
      DO j = 1,jmax
         DO i = 1,imax
           if(mask(i,j,1) == 1 .AND. dsig(i,j) < NN2_max) then
              ii = i
              jj = j
              NN2_max = dsig(i,j)
           endif
         ENDDO
      ENDDO
!
      write(12,*)' N2 maximum negative value = ',NN2_max
!
        if(iter == 1) then
           N2R = max(rmsp,rmsn)
           write(15,*) N2R
        else
           read(15,*) N2R
        endif
!
      IF(rmsn <= relerr1*N2R .AND. ABS(NN2_max) <= relerr2*N2R
     &                      .OR. iflag >= 2) THEN
!
       if(itrack == 1) then
          CALL NC_OUTLEV1(layer,iter,itrack,temp(:,:,1),psal(:,:,1)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
       endif
          CALL NC_OUTLEV2(layer+1,iter,itrack,temp(:,:,2),psal(:,:,2)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
!

!
          write(12,*)'positive N2 rms = ',rmsp
          write(12,*)'negative N2 rms = ',rmsn
          write(12,*)' psal data number : ',psaldata_nmb
          write(12,*)' temp data number : ',tempdata_nmb
!
          write(12,*)' adddata finished performing .... '
!
          write(11,*)3
          stop
      ENDIF
!
      i1 = max(ii-isq/2,1)
      i2 = min(ii+isq/2,NX)
      j1 = max(jj-jsq/2,1)
      j2 = min(jj+jsq/2,NY)
      ik_instabl(i1:i2,j1:j2)  = 0
      dsig(i1:i2,j1:j2)  = 0.
      ij_instabl(ii,jj)  = 1
!
      ij_lay(ii,jj) = 2
      if(itrack .eq. 1) then 
        erc1 = 0.5*(SUM(ert(i1:i2,j1:j2,1))+SUM(ers(i1:i2,j1:j2,1)))
        erc2 = 0.5*(SUM(ert(i1:i2,j1:j2,2))+SUM(ers(i1:i2,j1:j2,2)))
        if( erc2 .lt. erc1) ij_lay(ii,jj) = 1
      endif
!
      dsig(ii,jj)  = NN2_max
!
      epsilon3 = relerr1
!
      DO j = 1,jmax
         DO i = 1,imax
         if(ik_instabl(i,j) == 1) then
!
            max_inst = -1.*epsilon3*rmsp 
!
            DO m=1,NY
               DO l=1,NX
                  if(ik_instabl(l,m) == 1) then
                     if(dsig(l,m) <= max_inst) then
                        ii = l
                        jj = m
                        max_inst = dsig(l,m)
                     endif
                  endif
               ENDDO
            ENDDO
!
            i1 = max(ii-isq/2,1)
            i2 = min(ii+isq/2,NX)
            j1 = max(jj-jsq/2,1)
            j2 = min(jj+jsq/2,NY)
            ik_instabl(i1:i2,j1:j2)  = 0
            dsig(i1:i2,j1:j2)  = 0.
            ij_instabl(ii,jj)  = 1
            dsig(ii,jj)  = max_inst
!
      ij_lay(ii,jj) = 2
      if(itrack .eq. 1) then 
        erc1 = 0.5*(SUM(ert(i1:i2,j1:j2,1))+SUM(ers(i1:i2,j1:j2,1)))
        erc2 = 0.5*(SUM(ert(i1:i2,j1:j2,2))+SUM(ers(i1:i2,j1:j2,2)))
        if( erc2 .lt. erc1) ij_lay(ii,jj) = 1

        write(*,*) 'level iter ',layer,iter
        write(*,*) 'erc2  erc1', ij_lay(ii,jj)

      endif
!
        endif
        ENDDO
      ENDDO
      ik_instabl = ij_instabl
!
       if(itrack == 1) then
          CALL NC_OUTLEV1(layer,iter,itrack,temp(:,:,1),psal(:,:,1)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
       endif
          CALL NC_OUTLEV2(layer+1,iter,itrack,temp(:,:,2),psal(:,:,2)
     &                 ,N2(:,:,1),dsigma,xlon,ylat,IMAX,JMAX,valexu)
!
      epsilon4 = relerr1
!
      if(iter == 1) then
         tsflag = 1
         write(13,*) tsflag
!
         n2tild = epsilon4*N2R
         write(13,*) n2tild
      else
           read(13,*) tsflag
           read(13,*) n2tild
      endif
!
        iflag = 0
        is_newdata = 0
        it_newdata = 0
!
      DO j = 1,jmax
         DO i = 1,imax
!
          if(ik_instabl(i,j) == 1) then
          if(ij_lay(i,j) == 2) then
!
               iflag=iflag+1
               T1 = temp(i,j,1)
               T2 = temp(i,j,2)
               S1 = psal(i,j,1)
               S2 = psal(i,j,2)
!
! Calculation of minimal correction
!
               EPST = T2-T1
               EPSS = S2-S1
!
               FACS = 1./(1+psaldata_nmb(2))  
               FACT = 1./(1+tempdata_nmb(2))  
               RWN = FACT/(FACT+FACS)
!
               FACA = 9.81*alpha(i,j)/(Z(2)-Z(1))
               FACB = 9.81*beta(i,j)/(Z(2)-Z(1))
!
!x                ETA = -((n2tild-dsig(i,j))/(FACA))*FACS
!x     &                                    /(FACT + FACS)
!x                KSI =  ((n2tild-dsig(i,j))/FACB)*(1.-FACS
!x     &                                    /(FACT + FACS))
!x                TO   = T2 - ETA
!x                SO   = S2 - KSI
!
                TO   = T2 + (1-RWN)*((n2tild-dsig(i,j))/(FACA))
                SO   = S2 - RWN*((n2tild-dsig(i,j))/(FACB))
!
! Linear combination with mixing
!
                TM = T1 +(n2tild/N2(i,j,1))*EPST
                SM = S1 +(n2tild/N2(i,j,1))*EPSS
!
                temp(i,j,2)   = (1.-KSI_mix)*TO+ KSI_mix*TM
                psal(i,j,2)   = (1.-KSI_mix)*SO+ KSI_mix*SM
!
! New pseudo-data values
!
                it_newdata(i,j,2) = 1
                is_newdata(i,j,2) = 1
                newpsal_val(i,j,2) = psal(i,j,2)
                newpsal_lon(i,j,2) = XLON(i)
                newpsal_lat(i,j,2) = YLAT(j)
                newpsal_wht(i,j,2) = 10.
                newtemp_val(i,j,2) = temp(i,j,2)
                newtemp_lon(i,j,2) = XLON(i)
                newtemp_lat(i,j,2) = YLAT(j)
                newtemp_wht(i,j,2) = 10.
!
          else
!
               iflag=iflag+1
               T1 = temp(i,j,1)
               T2 = temp(i,j,2)
               S1 = psal(i,j,1)
               S2 = psal(i,j,2)
!
! Calculation of minimal correction
!

               EPST = T2-T1
               FACT = 1./(1+tempdata_nmb(1))  !1.-1./tempdata_nmb !
!
               EPSS = S2-S1
               FACS = 1./(1+psaldata_nmb(1))  !1.-1./psaldata_nmb !
!
               RWN = FACT/(FACT+FACS)
!
               FACA = 9.81*alpha(i,j)/(Z(2)-Z(1))
               FACB = 9.81*beta(i,j)/(Z(2)-Z(1))
!
!x                ETA = -((n2tild-dsig(i,j))/(FACA))*FACS
!x     &                                    /(FACT + FACS)
!x
!x                KSI =  ((n2tild-dsig(i,j))/FACB)*(1.-FACS
!x     &                                    /(FACT + FACS))
!x                TO   = T2 + ETA
!
!x                SO   = S2 + KSI
!
!
                TO   = T1 - (1-RWN)*((n2tild-dsig(i,j))/(FACA))
                SO   = S1 + RWN*((n2tild-dsig(i,j))/(FACB))
!
! Linear combination with mixing
!
                TM = T1 -(n2tild/N2(i,j,1))*EPST
                SM = S1 -(n2tild/N2(i,j,1))*EPSS
!
                temp(i,j,1)   = (1.-KSI_mix)*TO+ KSI_mix*TM
                psal(i,j,1)   = (1.-KSI_mix)*SO+ KSI_mix*SM
!
! New pseudo-data values
!
                it_newdata(i,j,1) = 1
                is_newdata(i,j,1) = 1
                newpsal_val(i,j,1) = psal(i,j,1)
                newpsal_lon(i,j,1) = XLON(i)
                newpsal_lat(i,j,1) = YLAT(j)
                newpsal_wht(i,j,1) = 10.
                newtemp_val(i,j,1) = temp(i,j,1)
                newtemp_lon(i,j,1) = XLON(i)
                newtemp_lat(i,j,1) = YLAT(j)
                newtemp_wht(i,j,1) = 10.

          endif
          endif
         ENDDO
      ENDDO
!
      tflag1 = SUM(it_newdata(:,:,1)) 
      sflag1 = SUM(is_newdata(:,:,1))
      tflag2 = SUM(it_newdata(:,:,2)) 
      sflag2 = SUM(is_newdata(:,:,2))
!
      open (unit=17,file='fort.17')
        read(17,*) ttdat
      close(17)
!
      ttdat =  ttdat + tflag1+tflag2
      open (unit=17,file='fort.17')
        write(17,*) ttdat
      close(17)
!
      IF(iflag < tsflag) THEN
        write(11,*)2
      ELSE
        write(11,*)1
      ENDIF

!xx      write(12,*)'total added data number : ',ttdat

      if(tflag2 .ge. 1 .OR. sflag2 .ge. 1) then
!
      ii = sflag2 + psaldata_nmb(2)
      jj = tflag2 + tempdata_nmb(2)
!
      write(12,*)'nbr of salinity data to be added for level',
     &                                            layer+1,': ',sflag2
      write(12,*)'nbr of temperature data to be added for level',
     &                                            layer+1, ': ',tflag2
!
      write(12,*)' new psal data number  for level',
     &                                            layer+1,': ',ii
      write(12,*)' new temp data number  for level',
     &                                            layer+1,': ',jj
      write(12,*)' '
!
      istep = layer + 1 
      ivar = 1
!
      WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
      WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
      close(84)
      open (unit=84,file=file_name,STATUS='old')
      write(84,*)' New data number'
      write(84,*)ii
      close(84)
!
      ivar = 2
!
      WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
      WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
      close(84)
      open (unit=84,file=file_name,STATUS='old')
      write(84,*)' New data number'
      write(84,*)jj
      close(84)
!
! Writing new data files
!
      ivar = 1
!
      WRITE(DATAFILE,
     &'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
      WRITE(file_name,'("./data/"
     &,a,".1",i4.4)')TRIM(var_name(ivar)),istep
!
      close(84)
      open (unit=84,file=file_name,STATUS='old',POSITION='APPEND')
      DO j = 1,jmax
         DO i = 1,imax
          if(is_newdata(i,j,2) == 1) then
             vval = newpsal_val(i,j,2)
             vlon = newpsal_lon(i,j,2)
             vlat = newpsal_lat(i,j,2)
             wght = newpsal_wht(i,j,2)
             write(84,*) vlon,vlat,vval,wght
          endif
         ENDDO
      ENDDO
!
      ivar = 2
!
      WRITE(DATAFILE,
     &'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
      WRITE(file_name,'("./data/"
     &,a,".1",i4.4)')TRIM(var_name(ivar)),istep
!
      close(84)
      open (unit=84,file=file_name,STATUS='old',POSITION='APPEND')
      DO j = 1,jmax
         DO i = 1,imax
          if(it_newdata(i,j,2) == 1) then
             vval = newtemp_val(i,j,2)
             vlon = newtemp_lon(i,j,2)
             vlat = newtemp_lat(i,j,2)
             wght = newtemp_wht(i,j,2)
             write(84,*) vlon,vlat,vval,wght
          endif
         ENDDO
      ENDDO
!
      endif
!
      if(tflag1 .ge. 1 .OR. sflag1 .ge. 1) then
!
      ii = sflag1 + psaldata_nmb(1)
      jj = tflag1 + tempdata_nmb(1)
!
      write(12,*)'nbr of salinity data to be added  for level',
     &                                              layer,': ',sflag1
      write(12,*)'nbr of temperature data to be added for level',
     &                                              layer,': ',tflag1
!
      write(12,*)' new psal data number  for level',
     &                                            layer,': ',ii
      write(12,*)' new temp data number  for level',
     &                                            layer,': ',jj
      write(12,*)' '
!
      istep = layer
      ivar = 1
!
      WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
      WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
      close(84)
      open (unit=84,file=file_name,STATUS='old')
      write(84,*)' New data number'
      write(84,*)ii
      close(84)
!
      ivar = 2
!
      WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
      WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
      close(84)
      open (unit=84,file=file_name,STATUS='old')
      write(84,*)' New data number'
      write(84,*)jj
      close(84)
!
! Writing new data files
!
      ivar = 1
!
      WRITE(DATAFILE,
     &'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
      WRITE(file_name,'("./data/"
     &,a,".1",i4.4)')TRIM(var_name(ivar)),istep
!
      open (unit=84,file=file_name,STATUS='old',POSITION='APPEND')
      DO j = 1,jmax
         DO i = 1,imax
          if(is_newdata(i,j,1) == 1) then
             vval = newpsal_val(i,j,1)
             vlon = newpsal_lon(i,j,1)
             vlat = newpsal_lat(i,j,1)
             wght = newpsal_wht(i,j,1)
             write(84,*) vlon,vlat,vval,wght
          endif
         ENDDO
      ENDDO
!
      ivar = 2
!
      WRITE(DATAFILE,
     &'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
      WRITE(file_name,'("./data/"
     &,a,".1",i4.4)')TRIM(var_name(ivar)),istep
!
      open (unit=84,file=file_name,STATUS='old',POSITION='APPEND')
      DO j = 1,jmax
         DO i = 1,imax
          if(it_newdata(i,j,1) == 1) then
             vval = newtemp_val(i,j,1)
             vlon = newtemp_lon(i,j,1)
             vlat = newtemp_lat(i,j,1)
             wght = newtemp_wht(i,j,1)
             write(84,*) vlon,vlat,vval,wght
          endif
         ENDDO
      ENDDO
!
      endif
!
      stop
      end
