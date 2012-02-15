!
      PROGRAM dv2Dasc2nc
!
      implicit none
!
!---------------------------------------------------------------------!
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT
      integer, DIMENSION(:,:),  ALLOCATABLE :: mask
!
      REAL*4 , DIMENSION(:,:),  ALLOCATABLE :: var, verr
!
      integer                   :: i,j,ii,jj
      integer                   :: NX, NY, NK, istep
      integer*4                 :: IMAX, JMAX
      real*4                    :: VALEXC,val,field_max,field_min,x,y
      real*4                    :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=255)        :: file_name,divafile,comments
      CHARACTER (len=99)         :: EXECIN
      CHARACTER (len=99)         :: VARFILEIN,VARFILEOU,VARFILE
      CHARACTER (len=99)         :: var_name
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./ncname'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)VARFILEIN
      READ(2,*)VARFILEOU
      READ(2,*)var_name
      READ(2,*)istep
      CLOSE(2)
!--------------------------------------------------------------
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
      var(1:NX,1:NY) = VALEXC
      i=1
      j=1
      EXECIN=TRIM(VARFILEOU)
      OPEN(2,FILE=EXECIN,STATUS='OLD')
 2    read(2,*,end=999) ii,jj,val
      i=ii
      j=jj
      var(i,j) = val
      goto 2
 999  continue
      CLOSE(2)
!
      imax = NX
      jmax = NY

      mask = 0
      DO j=1,NY
        DO i=1,NX
         IF(var(i,j) .ne. VALEXC) THEN
           mask(i,j) = 1
          ENDIF
        ENDDO
      ENDDO

       x=MAXVAL(var(1:imax,1:jmax),dim=1,MASK= (mask(:,:) .eq. 1))
       y=MINVAL(var(1:imax,1:jmax),dim=1,MASK= (mask(:,:) .eq. 1))
       call jmbround(x,y,field_max,field_min)


      divafile = '../output/3Danalysis/Fields/'//TRIM(VARFILEIN)
      INQUIRE(FILE=TRIM(divafile),EXIST=exist)
      IF(exist) THEN
!
!x      WRITE(*,*)' nc file ',TRIM(VARFILEIN),imax,jmax
!x      WRITE(*,*)' file name',TRIM(divafile)

      CALL NC_RWD2DCL(imax,jmax,var,field_max,field_min,TRIM(divafile))
!
      ENDIF

      STOP
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
