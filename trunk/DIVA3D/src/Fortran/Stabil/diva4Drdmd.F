!
      PROGRAM DV4DRDMD
!
      implicit none
!
!--------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,irewrt
!
      DOUBLE PRECISION :: lon,lat,val
!
!x      integer , DIMENSION(:) ,  ALLOCATABLE :: flags
!x      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: dep
!x      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT, clo, cla
!x     &                                       , CORLEN, SN, VARBAK
!
!x      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1,clbnds
!
      integer                   :: i,j,k,klev,ic,lev,chlen
      integer                   :: NX,NY,NK,NM

      integer*4                 :: KMAX,ipr,nw,IMAX,JMAX

      integer                   :: 
     &  L_INSTITUT,L_PRODUCTION,L_Email
!
      real*4                    :: VALEXC, zz,time_val,var_min,var_max
!
      real*4                    :: xorig, yorig, dx, dy, xend, yend
     &                            , WestBL,SouthBL,EastBL,NorthBL
!
      character (len=8)          :: yy1yy2
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=20)         :: EXECIN
      CHARACTER (len=22)         :: DEPTHS
      CHARACTER (len=99)         :: VARFILEIN
      CHARACTER (len=255)        :: file_name, file_name2, ncliste
      character (len=255)        :: title_string,cellmeth
     &                            , INSTITUT, PRODUCTION, Email
     &                            , InsEmail,INSTITUT0
      character (len=30)         :: Ref_time
      CHARACTER (len=99)         :: var_shname, var_cfname, var_name
     &                            , dat_name, var1_name, prmliste
      character (len=99)         :: var_lgname
      character (len=5)          :: chname
      character (len=20)         :: var_units,vrb_units
      character (len=20)                          :: s_units
      character (len=20) , DIMENSION(2)           :: l_units
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./md.info'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) var_shname
      READ(2,*) yy1yy2
      CLOSE(2)
!
      EXECIN='./glatt.info'
      INQUIRE(FILE=TRIM(EXECIN),EXIST=exist)
      if(exist) then
       irewrt = 1
       OPEN(2,FILE=EXECIN,STATUS='OLD')
       READ(2,*) INSTITUT0
       READ(2,*) InsEmail
       READ(2,*) PRODUCTION
       READ(2,*) Email
       CLOSE(2)
       WRITE(INSTITUT,'(
     &a,". E-mail: ",a)')TRIM(INSTITUT0),TRIM(InsEmail)
      else
       irewrt = 0
       INSTITUT = 'bidon'
       PRODUCTION = 'bidon'
       Email = 'bidon'
      endif
!

      write(*,*)' iwewrt   = ',irewrt
      write(*,*)' INSTITUT   = ',TRIM(INSTITUT)
      write(*,*)' PRODUCTION   = ',TRIM(PRODUCTION)
      write(*,*)' Email   = ',TRIM(Email)

      L_INSTITUT = LEN_TRIM(INSTITUT)
      L_PRODUCTION = LEN_TRIM(PRODUCTION)
      L_Email = LEN_TRIM(Email)
!
      IF(TRIM(yy1yy2) == '-9999') THEN
      WRITE(file_name,'(
     &"../output/",a,".4Danl.nc")')TRIM(var_shname)
      ELSE
      WRITE(file_name,'(
     &"../output/",a,".",a,".4Danl.nc")')TRIM(var_shname),TRIM(yy1yy2)
      ENDIF


      write(*,*)' file_name = ',TRIM(file_name)



      CALL NC_RD4DMD(NX,NY,NK,NM,irewrt,
     &  WestBL,SouthBL,EastBL,NorthBL,dx,dy,
     &  var_min,var_max,TRIM(var_units),TRIM(s_units),
     &  L_INSTITUT,TRIM(INSTITUT),L_PRODUCTION,TRIM(PRODUCTION),
     &  L_Email,TRIM(Email),
     &  TRIM(var_shname),TRIM(file_name))
!
!--------------------------------------------------------------
! Writing an info text file for matadata files:
      WRITE(file_name2,'("../output/",
     &a,".Metainfo.txt")')TRIM(var_shname)
      close(12)
      OPEN(UNIT=12,FILE=file_name2)
!
      write(12,1201)
      write(12,1202)NorthBL
      write(12,1204)WestBL
      write(12,1203)EastBL
      write(12,1205)SouthBL
      write(12,1206)dx,dy,s_units
!
 1201 FORMAT(//,10X,'Geographical coordinates:',/,
     &          10X,'*************************')
 1202 FORMAT(4X,'Northen boundary ..........:',F10.5)
 1203 FORMAT(4X,'Eastern boundary ..........:',F10.5)
 1204 FORMAT(4X,'Western boundary ..........:',F10.5)
 1205 FORMAT(4X,'Southern boundary .........:',F10.5)
 1206 FORMAT(4X,'Grid resolution  ..........:',F6.4,' by ',F6.4,1X,a)

      stop
      end
