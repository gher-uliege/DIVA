!
      PROGRAM DIVASTORE
!
      implicit none
!
!---------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,ichge
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U,U3,W3
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: z_watercolumn
     &                                      ,  Z, dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT
     &                                     , VALEXU,CORLEN,SN,VARBAK
!
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1
     &                                        , varbot, verbot
     &                                        , varb1, varb2
     &                                     , mskbot,mskb1,mskb2,mskber
      REAL*4 ,DIMENSION(2,1)                 :: clbnds
!
      REAL*4 , DIMENSION(:,:,:),  ALLOCATABLE :: var, var1, var2
     &                                         , verr,reler,dbins
     &                                         , obins,rlfield
!
      integer,DIMENSION(:,:,:),  ALLOCATABLE :: mask,mask1,mask2,masker
!
      integer            :: i,j,k,klev,ic,top_lev,kuw,kwu,ipar,iparpar
      integer                   :: NX, NY, NK, ndata, nl,chlen,iun
      integer*4                 :: KMAX, ipr, nw, IMAX, JMAX, flag
!!      integer                 :: KMAX, ipr, nw, IMAX, JMAX
      integer                   :: ICOOC, IREG, ISPEC
      integer                   :: nm,xm1,xm2,xy1,xy2
!
      real*4                     :: zz, time_val,VALEXC,
     &              var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &              vbt_min,vbt_max,var1_min,var1_max,
     &              vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &              var2_min,var2_max,verel_min,verel_max,
     &              obin_min,obin_max,rl_min,rl_max,x,y,xr,yr
!
      real*4                     :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=16)         :: EXECIN
      CHARACTER (len=22)         :: DEPTHS
      CHARACTER (len=99)         :: VARFILEIN
      CHARACTER (len=255)        :: file_name,Ref_time, ncliste,prmliste
      character (len=255)        :: title_string, cellmeth
     &                            , INSTITUT,PRODUCTION,SOURCE,COMMENT
      CHARACTER (len=99)         :: var_shname,var_name,var_cfname
      character (len=99)         :: var_lgname
      character (len=20)         :: var_units,vrb_units
      character (len=4)          :: yy1,yy2
      character (len=2)          :: mm1,mm2
      character (len=20), DIMENSION(2)           :: l_units
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./fort.44'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) iparpar
      CLOSE(2)
!
      EXECIN='../input/3Dinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)! comments
      READ(2,*) var_shname
      READ(2,*)! comments
      READ(2,*) MINLEV
      READ(2,*)! comments
      READ(2,*) MAXLEV
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) title_string
      READ(2,*) !comments
      READ(2,*) var_lgname
      READ(2,*) !comments
      READ(2,*) var_units
      IF(iparpar == 34) then
        time_val = 0.
        xm1 = 0
        xm2 = 0
        xy1 = 0
        xy2 = 0
        WRITE(Ref_time,'("Months since xxxx-01-01 ")')
        WRITE(cellmeth,'(
     &"time: mean within month time: mean over years")')
        close(2)
        INSTITUT = 'Not specified'
        PRODUCTION = 'Not specified'
        SOURCE = 'Not specified'
        COMMENT = 'Not specified'
      ELSE
        READ(2,*)! comments
        READ(2,*) ipar
          READ(2,*)! comments
          READ(2,*) Ref_time
          READ(2,*)! comments
          READ(2,*) time_val
          READ(2,*)! comments
          READ(2,*) cellmeth
          READ(2,*)! comments
          READ(2,*) INSTITUT
          READ(2,*)! comments
          READ(2,*) PRODUCTION
          READ(2,*)! comments
          READ(2,*) SOURCE
          READ(2,*)! comments
          READ(2,*) COMMENT
          clbnds = 0.
          CLOSE(2)
      ENDIF
!
      IF(ipar == 1) then
        chlen = 1
        DO while(var_shname(chlen:chlen) .ne. ' ') 
           chlen = chlen + 1
        ENDDO
        chlen = chlen - 1
        mm2 = var_shname(chlen-1:chlen)
        mm1 = var_shname(chlen-3:chlen-2)
        yy2 = var_shname(chlen-8:chlen-5)
        yy1 = var_shname(chlen-12:chlen-9)
        if(yy1(1:4) .EQ. 'xxxx') yy1='1850'
        if(yy2(1:4) .EQ. 'xxxx') yy2='2050'
        WRITE(Ref_time,
     &'("Months since ",a,"-01-01")'),yy1(1:4) ! ,yy2(1:4) 
!x     &'("Months since ",a," until ",a)'),yy1(1:4) ,yy2(1:4) 
        read (mm1,'(I2.2)') xm1
        read (mm2,'(I2.2)') xm2
        read (yy1,'(I4.4)') xy1
        read (yy2,'(I4.4)') xy2
!x        time_val = (xy2-xy1+1)*12.+float(xm1-1)+float((xm2-xm1+1))/2
        if(xm1 > xm2) then
           nm = xm2 + (12 - xm1) + 1
        else
           nm = xm2 - xm1 + 1
        endif
        time_val = 12*(nint(float(xy2)-float(xy1))/2)
     &                        +float(xm1-1)+float(nm)/2
        clbnds(1,1)= 1.*xm1 !float(xm1)
        clbnds(2,1)= 1.*xm2 !float(xm2)
        WRITE(cellmeth,'(
     &"time: mean within month time: mean over years")')
!     &" over years ",I4," to ",I4)')xy1,xy2
!     &"time: mean data within months ",I2," to "
!     &,I2," over years ",I4," to ",I4)'),xm1,xm2,xy1,xy2
      ENDIF
!
      WRITE(vrb_units,'(a,"^2")')TRIM(var_units)
      WRITE(VARFILEIN,
     &'(a,".1",i4.4,".anl")')TRIM(var_shname),MINLEV
      WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".anl.nc")')TRIM(var_shname),MINLEV,MAXLEV
!
      IF(.NOT. ALLOCATED(z_watercolumn)) 
     &    ALLOCATE(z_watercolumn(MAXLEV))
!
      DEPTHS='../input/contour.depth'
      OPEN(2,FILE=DEPTHS,STATUS='OLD')
      do i=1,MAXLEV
      read(2,*,err=99,end=99) z_watercolumn(i) 
      enddo
 99   continue
      CLOSE(2)
!
      top_lev = MAXLEV - MINLEV + 1
      ALLOCATE(dep(top_lev))
!
      ALLOCATE(CORLEN(top_lev))
      ALLOCATE(VALEXU(top_lev))
      ALLOCATE(SN(top_lev))
      ALLOCATE(VARBAK(top_lev))
!
!     Read the grid data from GridInfo.dat
!--------------------------------------------------------------
      divafile = '../output/3Danalysis/Fields/GridInfo.dat'
      open(unit=90,file=divafile)
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend
      CLOSE(90)
!
!--------------------------------------------------------------
!
! Reading param.par
!
      flag=0
      WRITE(prmliste,'("../output/3Danalysis/",
     &a,".3Dparam")')TRIM(var_shname)
      INQUIRE(FILE=TRIM(ncliste),EXIST=exist)
      if(exist) then
        flag=1
      else
        close(12)
        OPEN(UNIT=12,FILE=prmliste)
      endif
!
         klev = 0
         DO istep = MINLEV,MAXLEV
            klev = klev + 1
!
            WRITE(VARFILEIN,
     &'("param.par.",a,".1",i4.4)')TRIM(var_shname),istep
            WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILEIN)
            INQUIRE(FILE=file_name,EXIST=exist)
            IF(exist) then
              open(unit=2,file=file_name)
            ELSE
              WRITE(VARFILEIN,
     &'("param.par.1",i4.4)')istep
              WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILEIN)
              INQUIRE(FILE=file_name,EXIST=exist)
              IF(exist) then
                open(unit=2,file=file_name)
              ELSE
                WRITE(VARFILEIN,'("param.par")')
                WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILEIN)
              INQUIRE(FILE=file_name,EXIST=exist)
                IF(exist) then
                  open(unit=2,file=file_name)
                ELSE
                  WRITE(VARFILEIN,'("param.par")')
                  WRITE(file_name,
     &'("../input/",a)')TRIM(VARFILEIN)
                  open(unit=2,file=file_name)
                ENDIF
              ENDIF
            ENDIF
            READ(2,*)! comments
            READ(2,*) CORLEN(klev)
            READ(2,*)! comments
            READ(2,*) ICOOC
            READ(2,*)! comments
            READ(2,*) ISPEC
            READ(2,*)! comments
            READ(2,*) IREG
            DO i = 1,6
               READ(2,*)! comments
               READ(2,*)! comments
            ENDDO
            READ(2,*)! comments
            READ(2,*) VALEXU(klev)
            READ(2,*)! comments
            READ(2,*) SN(klev)
            READ(2,*)! comments
            READ(2,*) VARBAK(klev)
            if(flag.eq.0) write(12,*)CORLEN(klev),SN(klev),VARBAK(klev)
            CLOSE(2)
         ENDDO
!
         VALEXC = VALEXU(top_lev)
!
         IF(ICOOC == 0 ) THEN
            l_units(1) = 'km'
            l_units(2) = 'km'
         ELSE
            l_units(1) = 'degrees_east'
            l_units(2) = 'degrees_north'
         ENDIF
!--------------------------------------------------------------
!
      NX = INT(xend)
      NY = INT(yend)
      NK = top_lev
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(mask(1:NX,1:NY,1:NK))
      ALLOCATE(mask1(1:NX,1:NY,1:NK))
      ALLOCATE(mask2(1:NX,1:NY,1:NK))
      ALLOCATE(masker(1:NX,1:NY,1:NK))
      ALLOCATE(var(NX,NY,NK))
      ALLOCATE(var1(NX,NY,NK))
      ALLOCATE(var2(NX,NY,NK))
      ALLOCATE(verr(NX,NY,NK))
      ALLOCATE(reler(NX,NY,NK))
      ALLOCATE(dbins(NX,NY,NK))
      ALLOCATE(obins(NX,NY,NK))
      ALLOCATE(rlfield(NX,NY,NK))
!
      ALLOCATE(U(NX*NY))
      ALLOCATE(U3(NX*NY*NK))
      ALLOCATE(W3(NX*NY*NK))
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
      ALLOCATE(varbot(1:NX,1:NY))
      ALLOCATE(verbot(1:NX,1:NY))
      ALLOCATE(varb1(1:NX,1:NY))
      ALLOCATE(varb2(1:NX,1:NY))
      ALLOCATE(mskbot(1:NX,1:NY))
      ALLOCATE(mskb1(1:NX,1:NY))
      ALLOCATE(mskb2(1:NX,1:NY))
      ALLOCATE(mskber(1:NX,1:NY))
!
!retreiving grid points longitude and latitude
      DO i = 1, NX
         XLON(i) = xorig+(i-1)*dx
      ENDDO
      DO j = 1, NY
         YLAT(j) = yorig+(j-1)*dy
      ENDDO
!
!
! Reading analysis
      klev = 0
      kuw = 0
      ichge = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        valexu(klev) = valexc
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".anl")')TRIM(var_shname),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) THEN
        close(84)
        open (unit=84,file=TRIM(divafile),form='unformatted')
        CALL UREADC(84,W8,U,VALEXU(klev),IPR,IMAX,JMAX,KMAX,NW)
!
        if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
             var(i,j,klev) = U(ic)
             kuw = kuw + 1
             U3(kuw) = var(i,j,klev)
          ENDDO
        ENDDO
       ELSE
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
             var(i,j,klev) = valexu(klev)
             kuw = kuw + 1
             U3(kuw) = valexu(klev)
          ENDDO
        ENDDO
       ENDIF          
      if(valexu(klev) /= valexc ) ichge = 1
      ENDDO
!
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,VAR)
!
!
! Reading error field
      klev = 0
      kwu = 0
      ichge = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        valexu(klev) = valexc
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".error")')TRIM(var_shname),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
!
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) then
          close(84)        
          open (unit=84,file=TRIM(divafile),form='unformatted')
          CALL UREADC(84,W8,U,VALEXU(klev),IPR,IMAX,JMAX,KMAX,NW)
          if (KMAX.ne.1) stop "Not a 2D Field analysis"
          ic = 0
          DO j = 1,jmax
            DO i = 1,imax
               ic = ic + 1
               verr(i,j,klev) = U(ic)
               kwu = kwu + 1
               W3(kwu) = verr(i,j,klev)
            ENDDO
          ENDDO
          IF(VARBAK(klev) > 0.) THEN
            DO j = 1,jmax
              DO i = 1,imax
               if (verr(i,j,klev) .ne. VALEXU(klev)) then
               reler(i,j,klev) = verr(i,j,klev)/SQRT(VARBAK(klev))
               else
               reler(i,j,klev) = VALEXU(klev)
               endif
              ENDDO
            ENDDO
          ENDIF
        ELSE
          DO j = 1,jmax
            DO i = 1,imax
               reler(i,j,klev) = VALEXU(klev)
               verr(i,j,klev) = VALEXU(klev)
            ENDDO
          ENDDO
        ENDIF
      if(valexu(klev) /= valexc ) ichge = 1
      ENDDO
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,verr)
!
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,reler)
!
!
! Reading databins file
      klev = 0
      ichge = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        valexu(klev) = valexc
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".DATABINS")')TRIM(var_shname),istep
        divafile = '../input/divadata/'//TRIM(VARFILEIN)
        dbins(:,:,klev) = valexc
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) then
          close(84)
          open (unit=84,file=TRIM(divafile),form='unformatted')
          CALL UREADC(84,W8,U,VALEXU(klev),IPR,IMAX,JMAX,KMAX,NW)
          if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
          ic = 0
          DO j = 1,jmax
            DO i = 1,imax
               ic = ic + 1
              IF(U(ic) .gt. 0. .AND. U(ic) .ne. valexu(klev)) THEN
               dbins(i,j,klev) =log10(U(ic))
              ELSE
               dbins(i,j,klev) =VALEXU(klev)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      if(valexu(klev) /= valexc ) ichge = 1
      ENDDO
!
!
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,dbins)
!
! Reading outlayersbins file
!
      obins =VALEXC
!
      klev = 0
      ichge = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        valexu(klev) = valexc
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".OUTLIERBINS")')TRIM(var_shname),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
        obins(:,:,klev) = valexc
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) then
          close(84)
          open (unit=84,file=TRIM(divafile),form='unformatted')
          CALL UREADC(84,W8,U,VALEXU(klev),IPR,IMAX,JMAX,KMAX,NW)
          if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
          ic = 0
          DO j = 1,jmax
            DO i = 1,imax
               ic = ic + 1
              IF(U(ic) .gt. 0. .AND. U(ic) .ne. VALEXU(klev)) THEN
               obins(i,j,klev) =log10(U(ic))
              ELSE
               obins(i,j,klev) =VALEXU(klev)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      if(valexu(klev) /= valexc ) ichge = 1
      ENDDO
!
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,obins)
!
      klev = 0
      ichge = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        valexu(klev) = valexc
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".RL")')TRIM(var_shname),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) then
          close(84)
          open (unit=84,file=TRIM(divafile),form='unformatted')
          CALL UREADC(84,W8,U,VALEXU(klev),IPR,IMAX,JMAX,KMAX,NW)
          if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
          ic = 0
          DO j = 1,jmax
            DO i = 1,imax
               ic = ic + 1
               if(U(ic) .ne. VALEXU(klev)) then
               rlfield(i,j,klev) = U(ic)*CORLEN(klev)
               else
               rlfield(i,j,klev) = U(ic)
               endif
            ENDDO
          ENDDO
        ELSE
          DO j = 1,jmax
            DO i = 1,imax
              if(var(i,j,klev) .ne. valexc) then
                rlfield(i,j,klev) = CORLEN(klev)
              else
                rlfield(i,j,klev) = valexc
              endif
            ENDDO
          ENDDO
        ENDIF
        if(valexu(klev) /= valexc ) ichge = 1
      ENDDO
!
      if(ichge == 1) 
     & CALL CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,rlfield)
!
!
      varbot =  valexc
      verbot =  valexc
      DO k = NK,1,-1
        DO j = 1,jmax
           DO i = 1,imax
             IF(var(i,j,k) /= valexc)   varbot(i,j) = var(i,j,k)
             IF(verr(i,j,k) /= valexc)   verbot(i,j) = verr(i,j,k)
           ENDDO
         ENDDO
      ENDDO
!
      k=0
      DO i = MINLEV,MAXLEV
         k = k+1
        dep(k)  = z_watercolumn(i)
      ENDDO
!
! retreiving masks from data
      mask = 1
      DO k = 1,NK
         DO j = 1,jmax
           DO i = 1,imax
             if(var(i,j,k) == valexc) mask(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
      masker = 1
      DO k = 1,NK
         DO j = 1,jmax
           DO i = 1,imax
             if(verr(i,j,k) == valexc) masker(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
!
        var1 = var
        var2 = var
        mask1 = mask
        mask2 = mask
!
! Masking of fields
!
        klev = 0
        DO istep = MINLEV,MAXLEV
          klev = klev + 1
            DO j = 1,jmax
              DO i = 1,imax
                IF(verr(i,j,klev) == VALEXC .OR. verr(i,j,klev) > 0.3)
     &THEN
                   var1(i,j,klev) = VALEXC
                   mask1(i,j,klev) = 0
                ENDIF
                IF(verr(i,j,klev) == VALEXC .OR. verr(i,j,klev) > 0.5)
     &THEN
                   var2(i,j,klev) = VALEXC
                   mask2(i,j,klev) = 0
                ENDIF
              ENDDO
            ENDDO
        ENDDO
!
! retreiving masks from data
      mskbot = 1
      DO j = 1,jmax
        DO i = 1,imax
          if(varbot(i,j) == valexc) mskbot(i,j) = 0
        ENDDO
      ENDDO
      mskber = 1
      DO j = 1,jmax
        DO i = 1,imax
          if(verbot(i,j) == valexc) mskber(i,j) = 0
        ENDDO
      ENDDO
!
       varb1 = varbot
       varb2 = varbot
       mskb1 = mskbot
       mskb2 = mskbot
!
! Masking of fields
!
       DO j = 1,jmax
         DO i = 1,imax
           IF(verbot(i,j) == VALEXC .OR. verbot(i,j) > 0.3)THEN
               varb1(i,j) = VALEXC
               mskb1(i,j) = 0
           ENDIF
           IF(verbot(i,j) == VALEXC .OR. verbot(i,j) > 0.5)THEN
               varb2(i,j) = VALEXC
               mskb2(i,j) = 0
           ENDIF
         ENDDO
       ENDDO
!
      y=MAXVAL(varbot(1:imax,1:jmax),
     &                        MASK= (mask(:,:,nk) .eq. 1))
      x=MINVAL(varbot(1:imax,1:jmax),
     &                        MASK= (mask(:,:,nk) .eq. 1))
      call jmbround(x,y,xr,yr)
      vbt_max = yr
      vbt_min = xr
!
      y=MAXVAL(varb1(1:imax,1:jmax),MASK= (mskb1(:,:) .eq. 1))
      x=MINVAL(varb1(1:imax,1:jmax),MASK= (mskb1(:,:) .eq. 1))
      call jmbround(x,y,xr,yr)
      vbt1_max = yr
      vbt1_min = xr
!
      y=MAXVAL(varb2(1:imax,1:jmax),MASK= (mskb2(:,:) .eq. 1))
      x=MINVAL(varb2(1:imax,1:jmax),MASK= (mskb2(:,:) .eq. 1))
      call jmbround(x,y,xr,yr)
      vbt2_max = yr
      vbt2_min = xr
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      var_max = yr
      var_min = xr
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(verr(1:imax,1:jmax,k),dim=1,
     &                        MASK= (masker(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(verr(1:imax,1:jmax,k),dim=1,
     &                        MASK= (masker(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      ver_max = yr
      ver_min = xr
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(dbins(1:imax,1:jmax,k),dim=1,
     & MASK= (dbins(:,:,k) .ge. 0.))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0.
      call jmbround(x,y,xr,yr)
      dbin_max = yr
      dbin_min = 0.
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(var1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask1(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(var1(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask1(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      var1_min = xr
      var1_max = yr
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(var2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask2(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(var2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask2(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      var2_min = xr
      var2_max = yr
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(reler(1:imax,1:jmax,k),dim=1,
     &                        MASK= (masker(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(reler(1:imax,1:jmax,k),dim=1,
     &                        MASK= (masker(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      verel_min = xr
      verel_max = yr
!


      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(obins(1:imax,1:jmax,k),dim=1,
     & MASK= (obins(:,:,k) .eq. valexc))
      ENDDO
      x = 0.
      call jmbround(x,y,xr,yr)
      obin_max = yr
      obin_min = 0.
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(rlfield(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = 0.
      call jmbround(x,y,xr,yr)
      rl_min = xr
      rl_max = yr
!
      WRITE(file_name,'(
     &a,".1",i4.4,".1",i4.4,".anl.nc")')TRIM(var_shname),MINLEV,MAXLEV

!xx      IF(ipar == 1) then
!xx      chlen = 1
!xx      DO while(var_shname(chlen:chlen) .ne. ' ') 
!xx         chlen = chlen + 1
!xx      ENDDO
!xx      chlen = chlen - 1
!xx      var_name = var_shname(1:chlen-5)
!xx      ELSE
!xx      var_name = TRIM(var_shname)
!xx      ENDIF
!
!xx      var_cfname = var_shname
      IF(ipar == 1) then
      chlen = 1
      DO while(var_shname(chlen:chlen) .ne. ' ')
!xx         IF(var_shname(chlen:chlen) .eq. '.') 
!xx     &             WRITE(var_cfname(chlen:chlen),'("_")')
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      var_name = var_shname(1:chlen-5)
      var_cfname = var_shname(1:chlen-14)
      ELSE
      var_cfname = var_shname
      var_name = TRIM(var_shname)
      ENDIF
!
      WRITE(ncliste,'("../output/3Danalysis/",
     &a,".3DNCliste")')TRIM(var_name)
        INQUIRE(FILE=TRIM(ncliste),EXIST=exist)
        if(exist) then
          close(12)
          OPEN(UNIT=12,FILE=ncliste,STATUS='OLD',POSITION='APPEND')
          write(12,*) TRIM(file_name),1
        else
          close(12)
          OPEN(UNIT=12,FILE=ncliste,STATUS='NEW')
          write(12,*) TRIM(file_name),1
        endif
!
      WRITE(ncliste,'("../output/3Danalysis/",
     &a,".3DNCinfo")')TRIM(var_name)
        INQUIRE(FILE=TRIM(ncliste),EXIST=exist)
        if(exist) then
          close(12)
          OPEN(UNIT=12,FILE=ncliste,STATUS='OLD',POSITION='APPEND')
          write(12,*) "'",TRIM(var_cfname),"'"
          write(12,*) "'",TRIM(cellmeth),"'"
          write(12,*) "'",TRIM(Ref_time),"'"
        else
          close(12)
          OPEN(UNIT=12,FILE=ncliste,STATUS='NEW')
          write(12,*) "'",TRIM(INSTITUT),"'"
          write(12,*) "'",TRIM(PRODUCTION),"'"
          write(12,*) "'",TRIM(SOURCE),"'"
          write(12,*) "'",TRIM(COMMENT),"'"
          write(12,*) "'",TRIM(var_lgname),"'"
          write(12,*) "'",TRIM(var_units),"'"
          write(12,*) "'",TRIM(vrb_units),"'"
          write(12,*) "'",TRIM(title_string),"'"
          write(12,*) "'",TRIM(l_units(1)),"'"
          write(12,*) "'",TRIM(l_units(2)),"'"
          write(12,*) "'",TRIM(var_cfname),"'"
          write(12,*) "'",TRIM(cellmeth),"'"
          write(12,*) "'",TRIM(Ref_time),"'"
        endif
!
      WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".anl.nc")')TRIM(var_shname),MINLEV,MAXLEV
!


      write(*,*)'TRIM(var_cfname)',TRIM(var_cfname)


      iun=1
      CALL NC_4DCLIM(IMAX, JMAX,nk,ipar,iun,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &  obin_min,obin_max,rl_min,rl_max,VALEXC,l_units,
     &  LEN_TRIM(file_name),TRIM(file_name),
     &  LEN_TRIM(var_cfname),TRIM(var_cfname),
     &  LEN_TRIM(var_lgname),TRIM(var_lgname),
     &  LEN_TRIM(var_units),TRIM(var_units),
     &  LEN_TRIM(vrb_units),TRIM(vrb_units),
     &  LEN_TRIM(title_string),TRIM(title_string),
     &  LEN_TRIM(cellmeth),TRIM(cellmeth),
     &  LEN_TRIM(Ref_time),TRIM(Ref_time),
     &  LEN_TRIM(INSTITUT),TRIM(INSTITUT),
     &  LEN_TRIM(PRODUCTION),TRIM(PRODUCTION),
     &  LEN_TRIM(SOURCE),TRIM(SOURCE),
     &  LEN_TRIM(COMMENT),TRIM(COMMENT))
!
      WRITE(divafile,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".fieldgher.anl")')TRIM(var_shname),
     &MINLEV,MAXLEV
      KMAX = MAXLEV-MINLEV+1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,U3,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX)
!
      WRITE(divafile,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".errorfieldgher.anl")')TRIM(var_shname),
     &MINLEV,MAXLEV
      KMAX = MAXLEV-MINLEV+1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,W3,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX)
!
      stop
      end

!
       SUBROUTINE CHGVLX(IMAX,JMAX,NK,VALEXC,VALEXU,VAR)
       INTEGER                         :: IMAX,JMAX,NK
       INTEGER                         :: I,J,K
       REAL*4                          :: VALEXC
       REAL*4 ,DIMENSION(NK)           :: VALEXU
       REAL*4 ,DIMENSION(IMAX,JMAX,NK) :: VAR
       DO k = 1,NK
          DO j = 1,jmax
            DO i = 1,imax
              if(var(i,j,k) == valexu(k)) var(i,j,k) = valexc
            ENDDO
          ENDDO
       ENDDO
       RETURN
       END
!

        subroutine jmbround(x1,x2,x1r,x2r)
        if(x1.ge.x2) then
!        write(6,*) 'Sorry only for range'
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
