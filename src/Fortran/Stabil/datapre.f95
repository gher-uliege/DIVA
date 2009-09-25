!************************************************************************
      PROGRAM DATAPRE
!************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: i,ivar, istep,LEVELS,nbccol
      INTEGER :: fnum,lev,first_level,last_level,ndata
!
!      INTEGER, DIMENSION(:),    ALLOCATABLE  :: FNUMBERS
      REAL*4 :: lon,lat,val, wght
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
      CHARACTER (len=20)   :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO,VARFILEOU
      CHARACTER (len=255)               :: file_name
!x!x      CHARACTER (len=99)                :: DATAFILE
      CHARACTER (len=99), DIMENSION(2)  :: var_name


!
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) first_level
      READ(44,*) last_level
      READ(44,*) nbccol
      CLOSE(44)

      EXECIN='../input/STBinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) comments
      READ(2,*) var_name(1)
      READ(2,*) comments
      READ(2,*) var_name(2)
      READ(2,*) comments
      CLOSE(2)
!
      DO ivar = 1,2

         DO istep = first_level,last_level  !1,LEVELS
!
           WRITE(VARFILEIN,'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
           WRITE(datafile,'("../input/divadata/",a)')TRIM(VARFILEIN)
!
           WRITE(VARFILEOU,'(a,".1",i4.4)')TRIM(var_name(ivar)),istep
           WRITE(divafile,'("./data/",a)')TRIM(VARFILEOU)
!
           WRITE(VARFINFO,'(a,".1",i4.4,".info")')TRIM(var_name(ivar)),istep
           WRITE(infodata,'("./data/",a)')TRIM(VARFINFO)
!
            OPEN(10, FILE=datafile, STATUS='OLD')
            OPEN(11, FILE=divafile)
            OPEN(12, FILE=infodata)
             ndata = 0
             wght = 1.
             if(nbccol == 3) then
 1              read(10,*,end=999) lon,lat,val
                ndata = ndata + 1
                write(11,*) lon,lat,val,wght
                goto 1
             endif
             if(nbccol >= 4) then
 2              read(10,*,end=999) lon,lat,val,wght
                ndata = ndata + 1
                write(11,*) lon,lat,val,wght
                goto 2
             endif
  999        continue
             write(12,*)'total number of data: '
             write(12,*)ndata
             CLOSE(10)
             CLOSE(11)
             CLOSE(12)
         ENDDO
      ENDDO
      STOP
!
!-----Done--------------------------------------------------------------
!
      END




