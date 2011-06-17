************************************************************************
      PROGRAM GEBCOPREP
************************************************************************
!
      IMPLICIT NONE
!
      REAL*4 :: lon,lat,val
      REAL*4 :: lowlon, uplon, lowlat, uplat
!
      CHARACTER (len=99)                :: FILEOUT,datafile
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: topofile
      CHARACTER (len=99), DIMENSION(2)  :: var_name
      LOGICAL                          :: exist
!
!-----------------------------------------------------------------------
!
      WRITE(datafile,
     &'("../input/",a)')'topo.gebco'

      WRITE(FILEOUT,
     &'("../input/",a)')'modif_topo.gebco'
!
      INQUIRE(FILE='fort.44',EXIST=exist)
      if(exist) then

      OPEN(44, FILE='fort.44')
      READ(44,*) lowlon, uplon, lowlat, uplat
!
      OPEN(10, FILE=TRIM(datafile), STATUS='OLD')
      OPEN(12, FILE=TRIM(FILEOUT))
      OPEN(120, FILE='../output/tmpprt')
 2    read(10,*,end=999) lon,lat,val
      IF(lon .gt. 180.) THEN
!!         write(*,*) 'changing lon value',lon,'==> ',lon-360.
         lon = lon - 360.
      ENDIF
      IF(lon .le. uplon .AND. lon .ge. lowlon) then
         IF(lat .le. uplat .AND. lat .ge. lowlat) then
            if(val .gt. 0.) val = -1.*val
         ENDIF
      ENDIF
      write(12,*) lon,lat,val
      goto 2

      else

      OPEN(10, FILE=TRIM(datafile), STATUS='OLD')
      OPEN(12, FILE=TRIM(FILEOUT))
      OPEN(120, FILE='../output/tmpprt')
 1    read(10,*,end=999) lon,lat,val
      IF(lon .gt. 180.) THEN
         lon = lon - 360.
      ENDIF
      write(12,*) lon,lat,val
      goto 1

      endif

 999  continue
      CLOSE(10)
      CLOSE(12)
      STOP
!
!-----Done--------------------------------------------------------------
!
      END




