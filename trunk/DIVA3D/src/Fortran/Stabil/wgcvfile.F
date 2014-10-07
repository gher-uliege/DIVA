!
      PROGRAM WCGCVSAMP
!
      implicit none
!
!-------------------------------------------------------------
!
      integer, parameter    :: NVT = 7
      INTEGER :: i
!
      REAL    :: rmin,rmax
      REAL, DIMENSION(0:NVT)  :: LSN,SN
!
!--------------------------------------------------------------
!
!
      OPEN(2,FILE='fort.11',STATUS='OLD')
      READ(2,*) rmin,rmax
      CLOSE(2)
!
      LSN(0) = LOG(rmin)
      LSN(NVT) = LOG(rmax)
      DO I = 1,NVT-1
       LSN(I) = LSN(0) + FLOAT(I)*(LSN(NVT)-LSN(0))/NVT
      ENDDO
      SN(:) = EXP(LSN(:))
      open(2,FILE='../input/gcvsampling.dat')
      DO I = 0,NVT
       write(2,*) SN(I)
      ENDDO
      close(2)
!
      stop
      end

