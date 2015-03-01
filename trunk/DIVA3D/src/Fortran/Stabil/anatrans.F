!
      PROGRAM ANAINVTRAN
!
      implicit none
!
!---------------------------------------------------------------------!
!
      real*8                           :: W8
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT, VECFLD, U
      REAL*4, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL
!
      REAL*4 , DIMENSION(:,:),  ALLOCATABLE :: var, verr
      integer, DIMENSION(:,:),  ALLOCATABLE :: mask
!
      integer                   :: i,j,ii,jj,nbval,nbval2,nblninv,n
      integer                   :: NX, NY, NK, istep, itrans, isanalysis
      integer*4                 :: IMAX, JMAX, nblines,kmax,IPR
      real*4                    :: VALEXC,val,field_min,field_max
      real*4                    :: xorig, yorig, dx, dy, xend, yend,y,x
!
      CHARACTER (len=255)        :: file_name,divafile,datafile,comments
      CHARACTER (len=99)         :: EXECIN,INVERFILE
      CHARACTER (len=99)         :: VARFILEIN,VARFILEOU,VARFILE
      CHARACTER (len=99)         :: VARFILENCD,VARFILEASC,VARFILEBIN
      CHARACTER (len=99)         :: var_name
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./ncname'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,'(a)')VARFILENCD
      READ(2,'(a)')VARFILEBIN
      READ(2,'(a)')VARFILEASC
      READ(2,'(a)')var_name
      READ(2,*)istep
      READ(2,*)isanalysis
      READ(2,*)itrans
      READ(2,*) nblines
      CLOSE(2)
!--------------------------------------------------------------
!
      IF(nblines /= 0) then
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ENDIF
!
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
      NX = INT(xend)
      NY = INT(yend)
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(VECFLD(NX*NY))
      ALLOCATE(var(NX,NY))
      ALLOCATE(verr(NX,NY))
      ALLOCATE(mask(NX,NY))
      ALLOCATE(U(NX*NY))
!
!retreiving grid points longitude and latitude
      DO i = 1, NX
         XLON(i) = xorig+(i-1)*dx
      ENDDO
      DO j = 1, NY
         YLAT(j) = yorig+(j-1)*dy
      ENDDO
!
!--------------------------------------------------------------
!
      IMAX = NX
      JMAX = NY

       WRITE(divafile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(VARFILEBIN)
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UREADC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX*JMAX)
!
      nbval = 0
       DO i=1,NX*NY
         IF(U(i) .ne. VALEXC) THEN
          nbval = nbval+1
          VECFLD(nbval) = U(i)
        ENDIF
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
!
      nbval2 = 0
       DO i=1,NX*NY
        IF(U(i) .ne. VALEXC) THEN
           nbval2 = nbval2+1
           U(i) = VECFLD(nbval2)
        ENDIF
      ENDDO
!
      KMAX = 1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX)
!
!==============================================================
!
      imax = NX
      jmax = NY
!n      divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILENCD)
       WRITE(divafile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(VARFILENCD)


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
        close(12)
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

       x=MAXVAL(VECFLD(1:nbval2))
       y=MINVAL(VECFLD(1:nbval2))
       call jmbround(x,y,field_max,field_min)

!x      divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
      WRITE(*,*) 'found ',TRIM(divafile)
      INQUIRE(FILE=TRIM(divafile),EXIST=exist)
      IF(exist) THEN
       CALL NC_RWD2DCL(imax,jmax,var,field_max,field_min,TRIM(divafile))
      ELSE
        WRITE(*,*) 'did not find ',TRIM(divafile)
        STOP
      ENDIF
!
        if(isanalysis == 2) then

        WRITE(datafile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(VARFILEASC)
!
       OPEN(10, FILE=TRIM(datafile), STATUS='OLD')
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       CLOSE(10)
!
      IF(itrans .eq. 13 .or. itrans .eq. 23) then
        OPEN(2,FILE='nblinv',STATUS='OLD')
        READ(2,*) nblninv
        CLOSE(2)
        WRITE(VARFILE,
     &'(a,"_inverf")')TRIM(var_name)
        WRITE(INVERFILE,
     &'("../input/divadata/",a)')TRIM(VARFILE)
        close(12)
        open(unit=12,file=INVERFILE)
        CALL ANAMINV(nblninv,nbval,VECFLD)
      ELSE
        CALL TRANSINV(itrans,nbval,VECFLD)
      ENDIF
!
       OPEN(11, FILE=TRIM(datafile))
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       CLOSE(11)

       endif
!
      STOP
  999 continue
      write(*,*)'anatrans: total number of data does not match for '
      write(*,*)TRIM(datafile)
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
