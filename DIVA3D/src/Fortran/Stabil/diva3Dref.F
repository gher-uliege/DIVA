!
      PROGRAM DIVA3Dref
!
      implicit none
!
!------------------------------------------------------------------
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV,filop
      INTEGER :: step
!
      DOUBLE PRECISION :: lon,lat,val
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U,W
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: z_watercolumn
     &                                      ,  Z, dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT
!
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1
!      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax2,resmin2
!
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: var, verr, dbins
!
      INTEGER,DIMENSION(:,:,:),    ALLOCATABLE :: mask
!
      integer                   :: i,j,k,klev,ic,top_lev,kuw
      integer                   :: NX, NY, NK, ndata, nl
      integer*4                   :: KMAX, ipr, nw, IMAX, JMAX
!!      integer                   :: KMAX, ipr, nw, IMAX, JMAX
      real*4                      :: VALEXU, zz,
     &var_min,var_max,ver_min,ver_max,dbin_min,dbin_max
      real*4                      :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=16)         :: EXECIN
      CHARACTER (len=22)         :: DEPTHS
      CHARACTER (len=99)         :: VARFILEIN
      CHARACTER (len=99)         :: var_name, err_name
      CHARACTER (len=255)        :: file_name
      character (len=255)        :: title_string
      character (len=99)         :: var_lgname, err_lgname
      character (len=20)         :: var_units
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='../input/3Dinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) comments
      READ(2,*) var_name
      READ(2,*) comments
      READ(2,*) MINLEV
      READ(2,*) comments
      READ(2,*) MAXLEV
      READ(2,*) comments
      READ(2,*) ! comments
      READ(2,*) comments
      READ(2,*) ! comments
      READ(2,*) comments
      READ(2,*) ! comments
      READ(2,*) comments
      READ(2,*) filop
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
      READ(2,*) comments
      READ(2,*) title_string
      READ(2,*) comments
      READ(2,*) var_lgname
      READ(2,*) comments
      READ(2,*) var_units
      CLOSE(2)
!
      WRITE(err_name,'("err",a)')TRIM(var_name)
      WRITE(err_lgname,'(a,".error.reference")')TRIM(var_lgname)
!
      WRITE(VARFILEIN,
     &'(a,".1",i4.4,".ref")')TRIM(var_name),MINLEV
      WRITE(file_name,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".ref.nc")')TRIM(var_name),MINLEV,MAXLEV
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
      IF(.NOT. ALLOCATED(dep)) ALLOCATE(dep(top_lev))
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
      NX = INT(xend)
      NY = INT(yend)
      NK = top_lev
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(mask(1:NX,1:NY,1:NK))
      ALLOCATE(var(NX,NY,NK))
      ALLOCATE(verr(NX,NY,NK))
      ALLOCATE(dbins(NX,NY,NK))
      ALLOCATE(U(NX*NY))
      ALLOCATE(W(NX*NY*NK))
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
      klev = 0
      kuw = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".ref")')TRIM(var_name),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
        close(84)
        open (unit=84,file=TRIM(divafile),form='unformatted')
        CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
             var(i,j,klev) = U(ic)
             kuw = kuw + 1
             W(kuw) = var(i,j,klev)
          ENDDO
        ENDDO
      ENDDO
!
!
! Reading error field
      klev = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".error.ref")')TRIM(var_name),istep
        divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
!
        INQUIRE(FILE=TRIM(divafile),EXIST=exist)
        IF(exist) then
        close(84)        
        open (unit=84,file=TRIM(divafile),form='unformatted')
        CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        if (KMAX.ne.1) stop "Not a 2D Field analysis"
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
             verr(i,j,klev) = U(ic)
          ENDDO
        ENDDO
        ELSE
        verr(:,:,klev) = VALEXU
        ENDIF
      ENDDO
!
! Reading databins file
      klev = 0
      DO istep = MINLEV,MAXLEV
        klev = klev + 1
        WRITE(VARFILEIN,
     &'(a,".1",i4.4,".DATABINS")')TRIM(var_name),istep
        divafile = '../input/divadata/'//TRIM(VARFILEIN)
        close(84)
        open (unit=84,file=TRIM(divafile),form='unformatted')
        CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        if (KMAX.ne.1) stop "Not a 2D Field analysis"
!
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
            IF(U(ic) .ge. 1) THEN
             dbins(i,j,klev) =log10(U(ic))
            ELSE
             dbins(i,j,klev) =VALEXU ! U(ic)
            ENDIF
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
             if(var(i,j,k) == valexu) mask(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
!
      DO k = 1,NK
         DO j = 1,jmax
           DO i = 1,imax
            IF((mask(i,j,k).eq.1) .AND. var(i,j,k) .eq. 0.) then
            write(75,*) k,i,j,var(i,j,k)
            ENDIF
           ENDDO
         ENDDO
            write(75,*) 
            write(75,*) '==================================='
      ENDDO
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      var_max = MAXVAL(resmax1)
      var_min = MINVAL(resmin1)

      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(verr(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(verr(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      ver_max = MAXVAL(resmax1)
      ver_min = MINVAL(resmin1)

      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(dbins(1:imax,1:jmax,k),dim=1,
     & MASK= (dbins(:,:,k) .ge. 0.))
      ENDDO
      dbin_max = MAXVAL(resmax1)
      dbin_min = 0.

!
      CALL NC_3DFILE
     &  (var,verr,dbins,xlon,ylat,imax,jmax,nk,dep,valexu,
     &  var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &  TRIM(file_name),TRIM(var_name),TRIM(var_lgname),
     &  TRIM(err_name),TRIM(err_lgname),
     &  TRIM(var_units), TRIM(title_string))
!
      WRITE(divafile,'("../output/3Danalysis/",
     &a,".1",i4.4,".1",i4.4,".fieldgher.ref")')TRIM(var_name),
     &MINLEV,MAXLEV
      KMAX = MAXLEV-MINLEV+1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,W,VALEXU,IPR,IMAX,JMAX,KMAX,1)

      stop
      end
