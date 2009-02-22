!
      PROGRAM REPORTAD
!
!
      implicit none
!
!---------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step
!
      DOUBLE PRECISION :: lon,lat,val
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U,WS,WT
!
! Depth distribution
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: z_watercolumn
     &                                      ,  Z, dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT
!
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: temp,psal, N2,
     &                                          vart1,vart2,vars1,vars2
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: errt,errs,rert,rers
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: dbns,dbnt,obns,obnt
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1
      REAL   , DIMENSION(:)  ,  ALLOCATABLE :: Tlev,Slev
!
      INTEGER,DIMENSION(:,:,:), ALLOCATABLE :: mask,mast1,mast2
     &                                       , mass1,mass2
!
      integer                   :: i,j,k,klev,ic,top_lev,surface_lev
      integer                   :: NX, NY, NK, ndata, nl,kuws,kuwt
      integer*4                   :: KMAX, ipr, nw, IMAX, JMAX
!!      integer                   :: KMAX, ipr, nw, IMAX, JMAX
      integer                   :: IREG, ICOOC
      real                      :: VALEXU, zz, T0, S0, alpha, beta,
     &     vart_min,vart_max,vert_min,vert_max,dbint_min,dbint_max,
     &     vars_min,vars_max,vers_min,vers_max,dbins_min,dbins_max,
     &     vart1_min,vart1_max,vars1_min,vars1_max,
     &     vart2_min,vart2_max,vars2_min,vars2_max,
     &     obins_min,obins_max,obint_min,obint_max,
     &     rert_min,rert_max,rers_min,rers_max
      real                      :: xorig, yorig, dx, dy, xend, yend
     &                           , x,y,xr,yr
      REAL*4, DIMENSION(2)      :: time_val !,clbnds
      REAL*4, DIMENSION(2,1)      :: clbnds
!
      real                      :: nb_wetpoint
      real, DIMENSION(:) ,ALLOCATABLE :: ispcs,ispct,VARBAS,VARBAT
     &                                 , CorLenT, CorLenS
     &                           , SNS, SNT
!
      CHARACTER (len=4)                 :: lnum
      CHARACTER (len=40)               :: comments
      CHARACTER (len=70)               :: divafile
      CHARACTER (len=20)               :: EXECIN
      CHARACTER (len=22)               :: DEPTHS
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: DATAFILE
      CHARACTER (len=99), DIMENSION(2)  :: var_name, err_name
      character (len=255), DIMENSION(2) :: title_string,cellmeth
      character (len=99), DIMENSION(2)  :: var_lgname, err_lgname
     &                                   , REF_TIME
      character (len=20), DIMENSION(2)  ::var_units,err_units,vrb_units
      character (len=20), DIMENSION(2)           :: l_units
!
      CHARACTER (len=9)                :: NCFILE
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      NCFILE = 'ANALYS.nc'
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
      IF(.NOT. ALLOCATED(z_watercolumn)) 
     &                          ALLOCATE(z_watercolumn(surface_lev))
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
!
      READ(2,*) title_string(1)
      READ(2,*)! comments
      READ(2,*) var_lgname(1)
      READ(2,*)! comments
      READ(2,*) var_units(1)
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*) Ref_time(1)
      READ(2,*)! comments
      READ(2,*) time_val(1)
      READ(2,*)! comments
      READ(2,*) cellmeth(1)
      clbnds = 0.
!
      READ(2,*)! comments
      READ(2,*) title_string(2)
      READ(2,*)! comments
      READ(2,*) var_lgname(2)
      READ(2,*)! comments
      READ(2,*) var_units(2)
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*) Ref_time(2)
      READ(2,*)! comments
      READ(2,*) time_val(2)
      READ(2,*)! comments
      READ(2,*) cellmeth(2)
      clbnds = 0.
      CLOSE(2)
      WRITE(vrb_units(1),'(a,"^2")')TRIM(var_units(1))
      WRITE(vrb_units(2),'(a,"^2")')TRIM(var_units(2))
!
      DO ivar = 1,2
      WRITE(err_name(ivar),'(a,".error")')TRIM(var_name(ivar))
      WRITE(err_lgname(ivar),'(a,".error")')TRIM(var_lgname(ivar))
      ENDDO
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
      top_lev = MAXLEV - MINLEV + 1
       ALLOCATE(Z(top_lev))
      ALLOCATE(dep(top_lev))
      ALLOCATE(Tlev(top_lev))
      ALLOCATE(Slev(top_lev))

      ALLOCATE(ispcs(top_lev))
      ALLOCATE(ispct(top_lev))
      ALLOCATE(VARBAS(top_lev))
      ALLOCATE(VARBAT(top_lev))
      ALLOCATE(SNS(top_lev))
      ALLOCATE(SNT(top_lev))
      ALLOCATE(CorLenS(top_lev))
      ALLOCATE(CorLenT(top_lev))
!
!     Read the grid data from GridInfo.dat
!--------------------------------------------------------------
      divafile = '../output/3Danalysis/STABOUT/analysis/GridInfo.dat'
      open(unit=90,file=divafile)
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend
      read(90,*) valexu
      CLOSE(90)

!
!--------------------------------------------------------------
!
      NX = INT(xend)
      NY = INT(yend)
      NK = top_lev
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(mask(1:NX,1:NY,1:NK))
      ALLOCATE(mast1(1:NX,1:NY,1:NK))
      ALLOCATE(mast2(1:NX,1:NY,1:NK))
      ALLOCATE(mass1(1:NX,1:NY,1:NK))
      ALLOCATE(mass2(1:NX,1:NY,1:NK))
!
      ALLOCATE(temp(NX,NY,NK))
      ALLOCATE(errt(NX,NY,NK))
      ALLOCATE(rert(NX,NY,NK))
      ALLOCATE(dbnt(NX,NY,NK))
      ALLOCATE(obnt(NX,NY,NK))
!
      ALLOCATE(vart1(NX,NY,NK))
      ALLOCATE(vart2(NX,NY,NK))
      ALLOCATE(vars1(NX,NY,NK))
      ALLOCATE(vars2(NX,NY,NK))
!
      ALLOCATE(psal(NX,NY,NK))
      ALLOCATE(errs(NX,NY,NK))
      ALLOCATE(rers(NX,NY,NK))
      ALLOCATE(dbns(NX,NY,NK))
      ALLOCATE(obns(NX,NY,NK))
!
      ALLOCATE(N2(NX,NY,NK))
      ALLOCATE(U(NX*NY))
      ALLOCATE(WS(NX*NY*NK))
      ALLOCATE(WT(NX*NY*NK))
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
!
!retreiving grid points longitude and latitude
      DO i = 1, NX
         XLON(i) = xorig+(i-1)*dx
      ENDDO
      DO j = 1, NY
         YLAT(j) = yorig+(j-1)*dy
      ENDDO
!
! Reading analysis
      DO ivar = 1,2
         klev = 0
         kuws = 0
         kuwt = 0
        DO istep = MINLEV,MAXLEV
            klev = klev + 1
!
           WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
           WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
            OPEN(12, FILE=file_name)
             read(12,*)comments
             read(12,*)ndata
            CLOSE(12)
           IF(ndata >= 1) THEN
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
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   psal(i,j,klev) = U(ic)
                   kuws = kuws + 1
                   WS(kuws) = psal(i,j,klev)
                ENDDO
              ENDDO
            ENDIF
            IF(ivar == 2) THEN
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   temp(i,j,klev) = U(ic)
                   kuwt = kuwt + 1
                   WT(kuwt) = temp(i,j,klev)
                ENDDO
              ENDDO
            ENDIF
           ELSE
            IF(ivar == 2) temp(:,:,klev) = VALEXU
            IF(ivar == 1) psal(:,:,klev) = VALEXU
           ENDIF
!
        ENDDO
      ENDDO
! Reading error fiels
      DO ivar = 1,2
         klev = 0
         DO istep = MINLEV,MAXLEV
            klev = klev + 1
!
            WRITE(VARFILEIN,
     &'("param.par.",a,".1",i4.4)')TRIM(var_name(ivar)),istep
            WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILEIN)
            INQUIRE(FILE=file_name,EXIST=exist)
            IF(exist) then
              open(unit=2,file=file_name)
            ELSE
              WRITE(VARFILEIN,'("param.par")')
              WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILEIN)
              open(unit=2,file=file_name)
            ENDIF
            READ(2,*)! comments
            if(ivar == 1)  READ(2,*) CorLenS(klev)
            if(ivar == 2)  READ(2,*) CorLenT(klev)
            READ(2,*)! comments
            READ(2,*) ICOOC
            READ(2,*)! comments
            if(ivar == 1)  READ(2,*) ispcs(klev)
            if(ivar == 2)  READ(2,*) ispct(klev)
            READ(2,*)! comments
            READ(2,*) IREG
            DO i = 1,6
               READ(2,*)! comments
               READ(2,*)! comments
            ENDDO
            READ(2,*)! comments
            READ(2,*) VALEXU
            READ(2,*)! comments
            if(ivar == 1) READ(2,*) SNS(klev)
            if(ivar == 2) READ(2,*) SNT(klev)
            READ(2,*)! comments
            if(ivar == 1) READ(2,*) VARBAS(klev)
            if(ivar == 2) READ(2,*) VARBAT(klev)
            CLOSE(2)
!
         IF(ICOOC == 0 ) THEN
            l_units(1) = 'km'
            l_units(2) = 'km'
         ELSE
            l_units(1) = 'degrees_east'
            l_units(2) = 'degrees_north'
         ENDIF
!
           WRITE(VARFINFO,
     &'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
           WRITE(file_name,
     &'("./data/",a)')TRIM(VARFINFO)
!
            OPEN(12, FILE=file_name)
             read(12,*)comments
             read(12,*)ndata
            CLOSE(12)
           IF(ndata >= 1 .AND.
     &       (ispcs(klev) /= 0 .AND. ispct(klev) /= 0)) THEN
!
              WRITE(VARFILEIN,
     &'(a,".1",i4.4,".error")')TRIM(var_name(ivar)),istep
             WRITE(file_name,
     &'("../output/3Danalysis/STABOUT/analysis/",a)')TRIM(VARFILEIN)
!
            close(84)
            open (unit=84,file=TRIM(file_name),form='unformatted')
            CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
            if (KMAX.ne.1) stop "Not a 2D Field analysis"
            IF(ivar == 1) THEN
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   errs(i,j,klev) = U(ic)
                ENDDO
              ENDDO
              IF(VARBAS(klev) > 0.) THEN
                DO j = 1,jmax
                  DO i = 1,imax
                   rers(i,j,klev) = errs(i,j,klev)/SQRT(VARBAS(klev))
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
            IF(ivar == 2) THEN
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   errt(i,j,klev) = U(ic)
                ENDDO
              ENDDO
              IF(VARBAT(klev) > 0.) THEN
                DO j = 1,jmax
                  DO i = 1,imax
                   rert(i,j,klev) = errt(i,j,klev)/SQRT(VARBAT(klev))
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
!
              WRITE(VARFILEIN,
     &'(a,".1",i4.4,".DATABINS")')TRIM(var_name(ivar)),istep
             WRITE(file_name,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
!
            close(84)
            open (unit=84,file=TRIM(file_name),form='unformatted')
            CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
            if (KMAX.ne.1) stop "Not a 2D Field analysis"
            IF(ivar == 1) THEN
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   dbns(i,j,klev) = U(ic)
                ENDDO
              ENDDO
            ENDIF
            IF(ivar == 2) THEN
              ic = 0
              DO j = 1,jmax
                DO i = 1,imax
                   ic = ic + 1
                   dbnt(i,j,klev) = U(ic)
                ENDDO
              ENDDO
            ENDIF
!
           ELSE
            IF(ivar == 2) errt(:,:,klev) = VALEXU
            IF(ivar == 1) errs(:,:,klev) = VALEXU
            IF(ivar == 2) dbnt(:,:,klev) = VALEXU
            IF(ivar == 1) dbns(:,:,klev) = VALEXU
           ENDIF

           obns =VALEXU
           obnt =VALEXU
            WRITE(VARFILEIN,
     &'(a,".1",i4.4,".OUTLIERBINS")')TRIM(var_name(ivar)),istep
            file_name = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
            INQUIRE(FILE=TRIM(file_name),EXIST=exist)
            IF(exist) then
              close(84)
              open (unit=84,file=TRIM(file_name),form='unformatted')
              CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
              if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
              IF(ivar == 1) THEN
                ic = 0
                DO j = 1,jmax
                  DO i = 1,imax
                     ic = ic + 1
                    IF(U(ic) .gt. 0.) THEN
                     obns(i,j,klev) =log10(U(ic))
                    ELSE
                     obns(i,j,klev) =VALEXU
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
              IF(ivar == 2) THEN
                ic = 0
                DO j = 1,jmax
                  DO i = 1,imax
                     ic = ic + 1
                    IF(U(ic) .gt. 0.) THEN
                     obnt(i,j,klev) =log10(U(ic))
                    ELSE
                     obnt(i,j,klev) =VALEXU
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
            ENDIF
!
         ENDDO
      ENDDO
!
      k=0
      DO i = MINLEV,MAXLEV
         k = k+1
        Z(k)  = -1.*z_watercolumn(i)  ! negatif values for depts
        dep(k)  = z_watercolumn(i)
      ENDDO
!
! retreiving masks from data
      mask = 1
      DO k = 1,NK
         DO j = 1,jmax
           DO i = 1,imax
             if(psal(i,j,k) == valexu) mask(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
!
        vart1 = temp
        vart2 = temp
        mast1 = mask
        mast2 = mask
!
! Masking of fields
!
        klev = 0
        DO istep = MINLEV,MAXLEV
          klev = klev + 1
            DO j = 1,jmax
              DO i = 1,imax
                IF(errt(i,j,klev) == VALEXU .OR. errt(i,j,klev) > 0.3)
     &THEN
                   vart1(i,j,klev) = VALEXU
                   mast1(i,j,klev) = 0
                ENDIF
                IF(errt(i,j,klev) == VALEXU .OR. errt(i,j,klev) > 0.5)
     &THEN
                   vart2(i,j,klev) = VALEXU
                   mast2(i,j,klev) = 0
                ENDIF
              ENDDO
            ENDDO
        ENDDO
!
        vars1 = psal
        vars2 = psal
        mass1 = mask
        mass2 = mask
!
        klev = 0
        DO istep = MINLEV,MAXLEV
          klev = klev + 1
            DO j = 1,jmax
              DO i = 1,imax
                IF(errs(i,j,klev) == VALEXU .OR. errs(i,j,klev) > 0.3)
     &THEN
                   vars1(i,j,klev) = VALEXU
                   mass1(i,j,klev) = 0
                ENDIF
                IF(errs(i,j,klev) == VALEXU .OR. errs(i,j,klev) > 0.5)
     &THEN
                   vars2(i,j,klev) = VALEXU
                   mass2(i,j,klev) = 0
                ENDIF
              ENDDO
            ENDDO
        ENDDO
!
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(temp(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(temp(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vart_max = yr !MAXVAL(resmax1)
      vart_min = xr !MINVAL(resmin1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(errt(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(errt(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vert_max = yr !MAXVAL(resmax1)
      vert_min = xr !MINVAL(resmin1)

      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(dbnt(1:imax,1:jmax,k),dim=1,
     & MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0.
      call jmbround(x,y,xr,yr)
      dbint_max = yr !MAXVAL(resmax1)
      dbint_min = 0.
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(psal(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(psal(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vars_max = yr !MAXVAL(resmax1)
      vars_min = xr !MINVAL(resmin1)

      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(errs(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(errs(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vers_max = yr !MAXVAL(resmax1)
      vers_min = xr !MINVAL(resmin1)

      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(dbns(1:imax,1:jmax,k),dim=1,
     & MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0.
      call jmbround(x,y,xr,yr)
      dbins_max = yr !MAXVAL(resmax1)
      dbins_min = 0.
!
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(vars1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mass1(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(vars1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mass1(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vars1_min = xr !MINVAL(resmin1)
      vars1_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(vars2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mass2(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(vars2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mass2(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vars2_min = xr !MINVAL(resmin1)
      vars2_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(vart1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mast1(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(vart1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mast1(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vart1_min = xr !MINVAL(resmin1)
      vart1_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(vart2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mast2(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(vart2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mast2(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      vart2_min = xr !MINVAL(resmin1)
      vart2_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(rers(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(rers(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      rers_min = xr !MINVAL(resmin1)
      rers_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(rert(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(rert(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      rert_min = xr !MINVAL(resmin1)
      rert_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(obns(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0. !MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      obins_min = xr !MINVAL(resmin1)
      obins_max = yr !MAXVAL(resmax1)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(obnt(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0. !MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      obint_min = xr !MINVAL(resmin1)
      obint_max = yr !MAXVAL(resmax1)
!
      OPEN(44, FILE='fort.44')
      READ(44,*) step
      CLOSE(44)
!
      IF(step == 1) THEN
!
        ivar = 1

        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".nc")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV

        INQUIRE(FILE=TRIM(file_name),EXIST=exist)
        IF(exist) then
           write(*,*)TRIM(file_name),' before stabilization exists'
        ELSE

      write(*,*)' ====== ',step,TRIM(var_name(ivar))

      CALL NC_4DCLIM(imax,jmax,nk,0,1,time_val(1),clbnds,
     &  psal,vars1,vars2,errs,rers,dbns,obns,xlon,ylat,dep,CorLenS,SNS,
     &  VARBAS,IREG,ispcs,ICOOC,valexu,vars_min,vars_max,vers_min,
     &  vers_max,dbins_min,dbins_max,vars1_min,vars1_max,vars2_min,
     &  vars2_max,rers_min,rers_max,obins_min,obint_max,
     &  TRIM(file_name),TRIM(var_name(ivar)),TRIM(var_lgname(ivar)),
     &  TRIM(var_units(ivar)),TRIM(vrb_units(ivar)),l_units,
     &  TRIM(title_string(ivar)),TRIM(cellmeth(ivar)),
     &  TRIM(Ref_time(ivar)))




          WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,"analysis")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
          KMAX = MAXLEV-MINLEV+1
          ipr=4
          close(84)
          open (unit=84,file=TRIM(file_name),form='unformatted')
          CALL UWRITC(84,W8,WT,VALEXU,IPR,IMAX,JMAX,KMAX,1)
        ENDIF
!
        ivar = 2
!
        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".nc")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV

        INQUIRE(FILE=TRIM(file_name),EXIST=exist)
        IF(exist) then
           write(*,*)TRIM(file_name),' before stabilization exists'
        ELSE
!
      write(*,*)' ====== ',step,TRIM(var_name(ivar))

      CALL NC_4DCLIM(imax,jmax,nk,0,1,time_val(2),clbnds,
     &  temp,vart1,vart2,errt,rert,dbnt,obnt,xlon,ylat,dep,CorLenT,SNT,
     &  VARBAT,IREG,ispcs,ICOOC,valexu,vart_min,vart_max,vert_min,
     &  vert_max,dbint_min,dbint_max,vart1_min,vart1_max,vart2_min,
     &  vart2_max,rert_min,rert_max,obint_min,obint_max,
     &  TRIM(file_name),TRIM(var_name(ivar)),TRIM(var_lgname(ivar)),
     &  TRIM(var_units(ivar)),TRIM(vrb_units(ivar)),l_units,
     &  TRIM(title_string(ivar)),TRIM(cellmeth(ivar)),
     &  TRIM(Ref_time(ivar)))

          WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,"analysis")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
          WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".STB.analysis")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
          KMAX = MAXLEV-MINLEV+1
          ipr=4
          close(84)
          open (unit=84,file=file_name,form='unformatted')
          CALL UWRITC(84,W8,WT,VALEXU,IPR,IMAX,JMAX,KMAX,1)
        ENDIF
      ENDIF
      IF(step == 2) THEN
!
        ivar = 1
        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".STB.nc")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
!

      write(*,*)' ====== ',step,TRIM(var_name(ivar))

      CALL NC_4DCLIM(imax,jmax,nk,0,1,time_val(1),clbnds,
     &  psal,vars1,vars2,errs,rers,dbns,obns,xlon,ylat,dep,CorLenS,SNS,
     &  VARBAS,IREG,ispcs,ICOOC,valexu,vars_min,vars_max,vers_min,
     &  vers_max,dbins_min,dbins_max,vars1_min,vars1_max,vars2_min,
     &  vars2_max,rers_min,rers_max,obins_min,obint_max,
     &  TRIM(file_name),TRIM(var_name(ivar)),TRIM(var_lgname(ivar)),
     &  TRIM(var_units(ivar)),TRIM(vrb_units(ivar)),l_units,
     &  TRIM(title_string(ivar)),TRIM(cellmeth(ivar)),
     &  TRIM(Ref_time(ivar)))
!
        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".STB.analysis")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
        KMAX = MAXLEV-MINLEV+1
        ipr=4
        close(84)
        open (unit=84,file=file_name,form='unformatted')
        CALL UWRITC(84,W8,WS,VALEXU,IPR,IMAX,JMAX,KMAX,1)
!
        ivar = 2
        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".STB.nc")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
!
      write(*,*)' ====== ',step,TRIM(var_name(ivar))

      CALL NC_4DCLIM(imax,jmax,nk,0,1,time_val(2),clbnds,
     &  temp,vart1,vart2,errt,rert,dbnt,obnt,xlon,ylat,dep,CorLenT,SNT,
     &  VARBAT,IREG,ispcs,ICOOC,valexu,vart_min,vart_max,vert_min,
     &  vert_max,dbint_min,dbint_max,vart1_min,vart1_max,vart2_min,
     &  vart2_max,rert_min,rert_max,obint_min,obint_max,
     &  TRIM(file_name),TRIM(var_name(ivar)),TRIM(var_lgname(ivar)),
     &  TRIM(var_units(ivar)),TRIM(vrb_units(ivar)),l_units,
     &  TRIM(title_string(ivar)),TRIM(cellmeth(ivar)),
     &  TRIM(Ref_time(ivar)))
!
        WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".STB.analysis")')TRIM(var_name(ivar)),
     &MINLEV,MAXLEV
        KMAX = MAXLEV-MINLEV+1
        ipr=4
        close(84)
        open (unit=84,file=file_name,form='unformatted')
        CALL UWRITC(84,W8,WT,VALEXU,IPR,IMAX,JMAX,KMAX,1)
      ENDIF
!
      STOP
      END

        subroutine jmbround(x1,x2,x1r,x2r)
        if(x1.ge.x2) then
        write(6,*) 'Sorry only for range'
        x1r=x1
        x2r=x2
        return
        endif
        ip=log10(x2-x1)-2
        i1neg=0
        if (x1.lt.0) then
        i1neg=1
        x1=-x1
        x1=x1*0.99999
                     else
        x1=x1*1.00001
        endif

        i2neg=0
        if (x2.lt.0) then
        i2neg=1
        x2=-x2
        x2=x2*1.00001
                     else
        x2=x2*0.99999
        endif

        ip=-ip
        if (i1neg.eq.1) then
        i1s=INT(x1 * (10**(ip)) + 1)
        else 
        i1s=INT(x1 * (10**(ip)) + 0)
        endif
        x1r=i1s/10.**(ip)
        if (i1neg.eq.1) x1r=-x1r
        
        
        if (i2neg.eq.1) then
        i2s=INT(x2 * (10**(ip)) + 0)
        else 
        i2s=INT(x2 * (10**(ip)) + 1)
        endif
        x2r=i2s/10.**(ip)
        if (i2neg.eq.1) x2r=-x2r
        return
        end
