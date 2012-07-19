************************************************************************
      PROGRAM DATAMIX
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,ic1,ic2,nbccol,nblines
     &,nbcols,nbcolm,nbcolp,nblinep,nblinem
     &,nbccols,nbccolm,nbccolp
      INTEGER :: fnum,lev,first_level,last_level,itrans,iplus,iminus
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL,ANAVAL
     &                                     , MISVAL,DIFVAL
      REAL*4 :: lon,lat,val, wght
!
      CHARACTER (len=256) :: divafile,infodata,comments
      CHARACTER (len=256) :: datafile,analfile,misffile
      CHARACTER (len=20)   :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFILEP,VARFILEM
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name
!
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) nblines
      READ(44,*) istep
      CLOSE(44)
      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(ANAVAL(nblines))
      ALLOCATE(MISVAL(nblines))
      ALLOCATE(DIFVAL(nblines))
!
      WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep
      WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)

      WRITE(VARFILEP,
     &'(a,".1",i4.4,".fieldatdatapoint.anl")')TRIM(var_name),istep
      WRITE(analfile,
     &'("../output/3Danalysis/Fields/",a)')TRIM(VARFILEP)
!
      OPEN(10, FILE=datafile, STATUS='OLD')
!
      DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
      ENDDO
      CLOSE(10)
!
      OPEN(10, FILE=analfile, STATUS='OLD')
!
      DO n = 1,nblines
           read(10,*,end=998) LON,LAT,ANAVAL(n)
      ENDDO
      CLOSE(10)
!
      DO n = 1,nblines
         MISVAL(n)=abs(ANAVAL(n) - VARVAL(n))
         if(MISVAL(n) /= 0.) then
         DIFVAL(n)=(ANAVAL(n) - VARVAL(n))/MISVAL(n)
         else
         DIFVAL(n) = 0
         endif
      ENDDO        
!
       WRITE(VARFILEM,
     &'(a,".1",i4.4,".misfitatdatapoint.dat")')TRIM(var_name),istep
       WRITE(misffile,
     &'("../output/3Danalysis/Divaqcfiles/",a)')TRIM(VARFILEM)
!
      CLOSE(11)
      OPEN(11, FILE=misffile, STATUS='NEW')
!
      DO n = 1,nblines
          write(11,*)  LONVAL(n),LATVAL(n),MISVAL(n),DIFVAL(n)
      ENDDO
      CLOSE(11)
!
      STOP
!
 998  WRITE(*,*)' dvqcfiles: problem when reading analysis file!'
      WRITE(*,*)'          ',TRIM(analfile)
      STOP
!
 999  WRITE(*,*)' dvqcfiles: problem when reading data file!'
      WRITE(*,*)'          ',TRIM(datafile)
      STOP
!
!-----Done--------------------------------------------------------------
!
      END




