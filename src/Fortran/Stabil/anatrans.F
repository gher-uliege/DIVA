!
      PROGRAM ANAINVTRAN
!
      implicit none
!
!---------------------------------------------------------------------!
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT, VECFLD
!
      REAL*4 , DIMENSION(:,:),  ALLOCATABLE :: var, verr
      integer, DIMENSION(:,:),  ALLOCATABLE :: mask
!
      integer                   :: i,j,ii,jj,nbval,nbval2,nblninv,n
      integer                   :: NX, NY, NK, istep, itrans
      integer*4                 :: IMAX, JMAX
      real*4                    :: VALEXC,val
      real*4                    :: xorig, yorig, dx, dy, xend, yend,y,x
!
      CHARACTER (len=255)        :: file_name,divafile,comments
      CHARACTER (len=99)         :: EXECIN,INVERFILE
      CHARACTER (len=99)         :: VARFILEIN,VARFILEOU,VARFILE
      CHARACTER (len=99)         :: var_name
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./ncname'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)VARFILEIN
      READ(2,*)var_name
      READ(2,*)istep
      READ(2,*)itrans
      CLOSE(2)
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
      WRITE(VARFILE,
     &'("param.par.",a,".1",i4.4)')TRIM(var_name),istep
      WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILE)
      INQUIRE(FILE=file_name,EXIST=exist)
      IF(exist) then
        open(unit=2,file=file_name)
      ELSE
        WRITE(VARFILE,
     &'("param.par.1",i4.4)')istep
        WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILE)
        INQUIRE(FILE=file_name,EXIST=exist)
        IF(exist) then
          open(unit=2,file=file_name)
        ELSE
          WRITE(VARFILE,'("param.par")')
          WRITE(file_name,
     &'("../input/divaparam/",a)')TRIM(VARFILE)
          INQUIRE(FILE=file_name,EXIST=exist)
          IF(exist) then
            open(unit=2,file=file_name)
          ELSE
            WRITE(VARFILE,'("param.par")')
            WRITE(file_name,
     &'("../input/",a)')TRIM(VARFILE)
            open(unit=2,file=file_name)
          ENDIF
        ENDIF
      ENDIF
      DO i = 1,10
         READ(2,*)! comments
         READ(2,*)! comments
       ENDDO
      READ(2,*)! comments
      READ(2,*) VALEXC
      CLOSE(2)
!
      NX = INT(xend)
      NY = INT(yend)
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(VECFLD(NX*NY))
      ALLOCATE(var(NX,NY))
      ALLOCATE(verr(NX,NY))
      ALLOCATE(mask(NX,NY))
!
!retreiving grid points longitude and latitude
      DO i = 1, NX
         XLON(i) = xorig+(i-1)*dx
      ENDDO
      DO j = 1, NY
         YLAT(j) = yorig+(j-1)*dy
      ENDDO
!
      imax = NX
      jmax = NY
      divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
      INQUIRE(FILE=TRIM(divafile),EXIST=exist)
      IF(exist) THEN
        CALL NC_RD2DCL(imax,jmax,var(:,:),verr(:,:),TRIM(divafile))
      ELSE
        WRITE(*,*)'Anamorphose transform: did not find ',TRIM(divafile)
        STOP
      ENDIF
!            
      nbval = 0
      DO j=1,NY
        DO i=1,NX
          IF(var(i,j) .ne. VALEXC) THEN
           nbval = nbval+1
           VECFLD(nbval) = var(i,j)
          ENDIF
        ENDDO
      ENDDO
!
      IF(itrans .eq. 13 .or. itrans .eq. 23) then
        OPEN(2,FILE='nblinv',STATUS='OLD')
        READ(2,*) nblninv
        CLOSE(2)
        WRITE(VARFILE,
     &'(a,"_inverf")')TRIM(var_name)
        WRITE(INVERFILE,
     &'("../input/divadata/",a)')TRIM(VARFILE)
        open(unit=12,file=INVERFILE)
        CALL ANAMINV(nblninv,nbval,VECFLD)
      ELSE
        CALL TRANSINV(itrans,nbval,VECFLD)
      ENDIF

      mask = 0
      nbval2 = 0
      DO j=1,NY
        DO i=1,NX
         IF(var(i,j) .ne. VALEXC) THEN
           nbval2 = nbval2+1
           var(i,j) = VECFLD(nbval2)
           mask(i,j) = 1
          ENDIF
        ENDDO
      ENDDO

      divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
      WRITE(*,*) 'found ',TRIM(divafile)
      INQUIRE(FILE=TRIM(divafile),EXIST=exist)
      IF(exist) THEN
       CALL NC_RWD2DCL(imax,jmax,var(:,:),TRIM(divafile))
      ELSE
        WRITE(*,*) 'did not find ',TRIM(divafile)
        STOP
      ENDIF
!     
      STOP
      END

!***********************************************************************
      SUBROUTINE ANAMINV(nblninv,nblines,VARVAL)
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,nblninv
      REAL    :: VAL, A, B
      REAL, DIMENSION(nblines) :: VARVAL,newVARVAL
      REAL, DIMENSION(nblninv) :: varinitmat,inverfmat
!
      DO n= 1,nblninv
        read(12,*)varinitmat(n),inverfmat(n)
      ENDDO
!
      DO n= 1,nblines
       i=1
       IF(VARVAL(n).lt.inverfmat(i)) THEN
          newVARVAL(n) = varinitmat(i)
       ELSE
          IF(VARVAL(n).ge.inverfmat(nblninv)) THEN
             newVARVAL(n) = varinitmat(nblninv)
          ELSE
             DO WHILE (VARVAL(n).ge.inverfmat(i))
               i=i+1
             ENDDO
             IF(VARVAL(n).eq.inverfmat(i-1)) THEN
                newVARVAL(n) = varinitmat(i-1)
             ELSE
                A = (varinitmat(i)-varinitmat(i-1))/
     &                                  (inverfmat(i)-inverfmat(i-1))
                B = varinitmat(i) - A*inverfmat(i)
                newVARVAL(n) = A*VARVAL(n)+B
             ENDIF
          ENDIF
       ENDIF
      ENDDO
      VARVAL=newVARVAL
      RETURN
      END
