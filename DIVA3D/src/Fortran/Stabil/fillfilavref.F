!
      PROGRAM dv2Dnc2asc
!
      implicit none
!
!---------------------------------------------------------------------!
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: LONVAL,LATVAL
      real*4                    :: VALEXC,val
      real*4                    :: xorig, yorig, dx, dy, xend, yend
!
      REAL*4 , DIMENSION(:),  ALLOCATABLE :: dtpvar!, ascvar, binvar
!
      integer                   :: i,j,ii,jj,ic,n
      integer                   :: NX, NY, NK, istep
      integer*4                 :: dtplines
      integer*4                 :: KMAX, ipr, nw, IMAX, JMAX, flag
!
      CHARACTER (len=255)        :: file_name,divafile,comments
      CHARACTER (len=99)         :: EXECIN
      CHARACTER (len=99)         :: BINFILE, ASCFILE, DTPFILE
      CHARACTER (len=99)         :: var_name
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./refinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)BINFILE
      READ(2,*)DTPFILE
      READ(2,*)dtplines
      READ(2,*)val
      CLOSE(2)

        WRITE(divafile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(BINFILE)
!--------------------------------------------------------------
      file_name = '../output/3Danalysis/Fields/GridInfo.dat'
      open(unit=90,file=TRIM(file_name))
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
      NX = INT(xend)
      NY = INT(yend)
      ALLOCATE(dtpvar(dtplines))
      ALLOCATE(LONVAL(dtplines))
      ALLOCATE(LATVAL(dtplines))
!
      ALLOCATE(U(NX*NY))
!
      IMAX = NX
      JMAX = NY
        close(84)
        open (unit=84,file=TRIM(divafile),form='unformatted')
        CALL UREADC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX*JMAX)
!
        ic = 0
        DO j = 1,jmax
          DO i = 1,imax
             ic = ic + 1
             if(U(ic) /= VALEXC) U(ic)=val
            U(ic)=val
          ENDDO
        ENDDO
!
      KMAX = 1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX)
!
        WRITE(divafile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(DTPFILE)
       OPEN(10, FILE=TRIM(divafile), STATUS='OLD')
       DO n = 1,dtplines
         read(10,*,end=999) LONVAL(n),LATVAL(n),dtpvar(n)
         if(dtpvar(n) /= VALEXC) dtpvar(n)=val
       ENDDO    
       CLOSE(10)
       OPEN(10, FILE=TRIM(divafile), STATUS='OLD')
       DO n = 1,dtplines
         write(10,*) LONVAL(n),LATVAL(n),dtpvar(n)
       ENDDO    

      STOP
 999  write(*,*) 'fillfilavref: problem with datapoint file:'
      write(*,*) TRIM(DTPFILE)
      STOP
      END
