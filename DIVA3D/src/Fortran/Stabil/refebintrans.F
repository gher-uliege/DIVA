!
      PROGRAM RefeBinTrans
!
      implicit none
!
!---------------------------------------------------------------------!
!
      real*8                           :: W8
      REAL*4, DIMENSION(:) ,    ALLOCATABLE :: U, VECFLD
      real*4                    :: VALEXC
      real*4                    :: xorig, yorig, dx, dy, xend, yend
!
      integer                   :: i,j,ii,jj,ic,n
      integer                   :: NX, NY, NK, itrans,nbval,nbval2
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
      EXECIN='./fort.44'
      OPEN(44,FILE=EXECIN,STATUS='OLD')
      READ(44,'(a)')BINFILE
      READ(44,*)itrans
      CLOSE(44)

        WRITE(divafile,
     &'("../input/divarefe/",a)')TRIM(BINFILE)
!--------------------------------------------------------------
      file_name = '../input/divarefe/GridInfo.dat'
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
!
      ALLOCATE(U(NX*NY))
      ALLOCATE(VECFLD(NX*NY))
!
      IMAX = NX
      JMAX = NY
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UREADC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX*JMAX)
!
!trtrtrtrtrtrtrtrtrtrtrtrtrtrtttrttttrtrtrtrt
!            
      nbval = 0
       DO i=1,NX*NY
         IF(U(i) .ne. VALEXC) THEN
          nbval = nbval+1
          VECFLD(nbval) = U(i)
        ENDIF
      ENDDO
!
         CALL TRANSFUN(itrans,nbval,VECFLD)
!
      nbval2 = 0
       DO i=1,NX*NY
        IF(U(i) .ne. VALEXC) THEN
           nbval2 = nbval2+1
           U(i) = VECFLD(nbval2)
        ENDIF
      ENDDO
!
!trtrtrtrtrtrtrtrtrtrtrtrtrtrtttrttttrtrtrtrt
!
      KMAX = 1
      ipr=4
      close(84)
      open (unit=84,file=TRIM(divafile),form='unformatted')
      CALL UWRITC(84,W8,U,VALEXC,IPR,IMAX,JMAX,KMAX,IMAX)
!
      STOP
      END
