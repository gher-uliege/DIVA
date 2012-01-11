!
      PROGRAM DV4DREAD
!
      implicit none
!
!--------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,ipar
!
      DOUBLE PRECISION :: lon,lat,val
!
      integer , DIMENSION(:) ,  ALLOCATABLE :: flags
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT
     &                                       , CORLEN, SN, VARBAK
!
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1,clbnds
     &                                        , varbot
     &                                        , varb1, varb2
!
      REAL*4 , DIMENSION(:,:,:),  ALLOCATABLE :: var, var1, var2
     &                                        , verr,reler,dbins
     &                                        , obins,rlfield
!
      integer                   :: i,j,k,klev,ic,top_lev,kuw,time_len
     &                           , list_len
      integer                   :: NX,NY,NK,ndata,nl,ivars,chlen,lev
      integer*4                 :: KMAX,ipr,nw,IMAX,JMAX
      integer                   :: ICOOC,IREG,ISPEC, ilev, nbdat
!
      real*4                     :: VALEXC, zz,time_val,
     &              var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &              vbt_min,vbt_max,var1_min,var1_max,
     &              vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &              var2_min,var2_max,verel_min,verel_max,
     &             obin_min,obin_max,rl_min,rl_max,x,y,xr,yr
      real*4                     ::
     &              var_min0,var_max0,ver_min0,ver_max0,dbin_min0,
     &              vbt_min0,vbt_max0,var1_min0,var1_max0,dbin_max0,
     &              vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &              var2_min0,var2_max0,verel_min0,verel_max0,
     &              obin_min0,obin_max0,rl_min0,rl_max0
!
      real*4                     :: xorig, yorig, dx, dy, xend, yend
     &                            , WestBL,SouthBL,EastBL,NorthBL
!
      character (len=8)         :: Real_clock
      integer                    :: xy1,xy2
!
      character (len=4)          :: yy1,yy2
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=20)         :: EXECIN
      CHARACTER (len=22)         :: DEPTHS
      CHARACTER (len=99)         :: VARFILEIN
      CHARACTER (len=255)        :: file_name, file_name2, ncliste
      character (len=255)        :: title_string,cellmeth
     &                            , INSTITUT, PRODUCTION
     &                            , SOURCE, COMMENT
      character (len=30)         :: Ref_time
      CHARACTER (len=99)         :: var_shname, var_cfname, var_name
     &                            , dat_name, var1_name, prmliste
      character (len=99)         :: var_lgname
      character (len=5)          :: chname
      character (len=20)         :: var_units,vrb_units
      character (len=20)                          :: s_units
      character (len=20) , DIMENSION(2)           :: l_units
      character (len=255), DIMENSION(:),ALLOCATABLE :: all_vars
     &                                               , all_files
     &                                               , les_files
     &                                               , var_shname_l
     &                                               , cellmeth_l
     &                                               , Ref_time_l
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
      EXECIN='./fort.44'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*) time_len
      CLOSE(2)
      list_len = time_len
!
      ALLOCATE(flags(time_len))
      ALLOCATE(all_vars(time_len))
      ALLOCATE(all_files(time_len))
      ALLOCATE(les_files(time_len))
      ALLOCATE(clbnds(2,time_len))
!
      EXECIN='../3DWORK/3Dncliste'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      DO I = 1,time_len
      READ(2,*) les_files(I),flags(I)
      ENDDO
!
      J = 0
      DO I = 1, time_len
        IF(flags(I) == 1) then
           J = J + 1
           all_files(J) = les_files(I)
        ENDIF
      ENDDO
      time_len = J
      ALLOCATE(var_shname_l(time_len))
      ALLOCATE(cellmeth_l(time_len))
      ALLOCATE(Ref_time_l(time_len))
!
      WRITE(file_name,'(a)')TRIM(all_files(1))
      chlen = 1
      DO while(file_name(chlen:chlen) .ne. ' ') 
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      chname = file_name(chlen-17:chlen-13)
      read(chname,'(I5)') MINLEV
      chname = file_name(chlen-11:chlen-7)
      read(chname,'(I5)')MAXLEV
      MINLEV = MINLEV - 10000
      MAXLEV = MAXLEV - 10000
      var_shname = file_name(1:chlen-24)
      DO I = 1,time_len
      WRITE(file_name,'(a)')TRIM(all_files(I))
      all_vars(I) = file_name(1:chlen-19)
      ENDDO
!
      top_lev = MAXLEV - MINLEV + 1
      ALLOCATE(dep(top_lev))
      ALLOCATE(CORLEN(top_lev))
      ALLOCATE(SN(top_lev))
      ALLOCATE(VARBAK(top_lev))
!
!     Read the grid data from GridInfo.dat
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
      NX = INT(xend)
      NY = INT(yend)
      NK = top_lev
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(var(NX,NY,NK))
      ALLOCATE(var1(NX,NY,NK))
      ALLOCATE(var2(NX,NY,NK))
      ALLOCATE(verr(NX,NY,NK))
      ALLOCATE(reler(NX,NY,NK))
      ALLOCATE(dbins(NX,NY,NK))
      ALLOCATE(obins(NX,NY,NK))
      ALLOCATE(rlfield(NX,NY,NK))
!
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
      ALLOCATE(varbot(1:NX,1:NY))
      ALLOCATE(varb1(1:NX,1:NY))
      ALLOCATE(varb2(1:NX,1:NY))
!
!xxx      EXECIN='../3DWORK/3DNCinfo'
!xxx      INQUIRE(FILE=EXECIN,EXIST=exist)
!xxx      if(exist) then
!xxx          close(12)
!xxx          OPEN(UNIT=12,FILE=EXECIN)
!xxx          read(12,*) INSTITUT
!xxx          read(12,*) PRODUCTION
!xxx          read(12,*) SOURCE
!xxx          read(12,*) COMMENT
!xxx          read(12,*) var_lgname
!xxx          read(12,*) var_units
!xxx          read(12,*) vrb_units
!xxx          read(12,*) title_string
!xxx          read(12,*) l_units(1)
!xxx          read(12,*) l_units(2)
!xxx          J = 0
!xxx          DO  I = 1, list_len
!xxx            IF(flags(I) == 1) then
!xxx              J = J+1
!xxx              read(12,*) var_shname_l(J)
!xxx              read(12,*) cellmeth_l(J)
!xxx              read(12,*) Ref_time_l(J)
!xxx            ELSE
!xxx              read(12,*) comments
!xxx              read(12,*) comments
!xxx              read(12,*) comments
!xxx            ENDIF
!xxx          ENDDO
!xxx      endif
!
! Writing an info text file for matadata files:
!xxx      WRITE(file_name2,'("../output/3Danalysis/",
!xxx     &a,".4DNCMetainfo")')TRIM(var_shname)
!xxx      close(12)
!xxx      OPEN(UNIT=12,FILE=file_name2)
!
      chlen = 1
      DO while(l_units(1)(chlen:chlen) .ne. ' ')
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      if (chlen == 2) then
         s_units = l_units(1)
      else
         s_units = l_units(1)(1:chlen-5)
      endif
!
      write(12,*) s_units
!
      WestBL = xorig
      SouthBL = yorig
      EastBL = xorig + dx*xend
      NorthBL =yorig + dy*yend
!
!xxx      write(12,1111)SouthBL
!xxx      write(12,1111)NorthBL
!xxx      write(12,1111)WestBL
!xxx      write(12,1111)EastBL
!xxx      write(12,1112)dx
!xxx      write(12,1112)dy
      CALL DATE_AND_TIME(Real_clock) 
      write(12,*)TRIM(Real_clock)

        chlen = 1
        DO while(var_shname(chlen:chlen) .ne. ' ') 
           chlen = chlen + 1
        ENDDO
        chlen = chlen - 1
        yy1 = var_shname(chlen-7:chlen-4)
        yy2 = var_shname(chlen-3:chlen)
        read (yy1,'(I4.4)') xy1
        read (yy2,'(I4.4)') xy2
!xxx      write(12,1113)xy1
!xxx      write(12,1113)xy2
!
!xxx 1111 FORMAT(1X,F7.3)
!xxx 1112 FORMAT(1X,F5.3)
!xxx 1113 FORMAT(1X,I4.4)
!--------------------------------------------------------------
!
!
! Writing an info text file for matadata files:
      WRITE(file_name2,'("../output/3Danalysis/",
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
      WRITE(file_name2,'("../output/3Danalysis/",
     &a,".4Danl.nc")')TRIM(var_shname)
!
      clbnds = 0.
      ipar = 1
!
      WRITE(title_string,'("DIVA 4D analysis of ",a)')TRIM(var_shname)
!
      var_name = var_shname
      chlen = 1
      DO while(var_shname(chlen:chlen) .ne. ' ')
!         IF(var_shname(chlen:chlen) .eq. '.') 
!     &             WRITE(var_name(chlen:chlen),'("_")')
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      var_name = var_shname(1:chlen-9)
!
      DO ivars = 1,time_len

      WRITE(file_name,'("../output/3Danalysis/",
     &a)')TRIM(all_files(ivars))

!!
      imax = NX
      jmax = NY
!
!!        var_shname = var_shname_l(ivars)
!
      var1_name = all_files(ivars)
      chlen = 1
      DO while(var1_name(chlen:chlen) .ne. ' ')
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      dat_name = var1_name(1:chlen-18)

      WRITE(var1_name,'("../output/3Danalysis/",
     &a,"nbrdata")')TRIM(dat_name)
      close(2)
      OPEN(UNIT=2,FILE=TRIM(var1_name))
      read(2,*) comments

      WRITE(prmliste,'("../output/3Danalysis/",
     &a,"3Dparam")')TRIM(dat_name)
        close(3)
        OPEN(UNIT=3,FILE=prmliste)
!
!
      CALL NC_NC_RD4DCL(imax,jmax,nk,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min0,var_max0,vbt_min0,vbt_max0,ver_min0,ver_max0,
     &  dbin_min0,dbin_max0,var1_min0,var1_max0,var2_min0,var2_max0,
     &  verel_min0,verel_max0,vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &  obin_min0,obin_max0,rl_min,rl_max,
     &  VALEXC,l_units,
     &  LEN_TRIM(file_name2),TRIM(file_name2),
     &  LEN_TRIM(var_shname_l(ivars)),TRIM(var_shname_l(ivars)),
     &  LEN_TRIM(var_lgname),TRIM(var_lgname),
     &  LEN_TRIM(var_units),TRIM(var_units),
     &  LEN_TRIM(vrb_units),TRIM(vrb_units),
     &  LEN_TRIM(title_string),TRIM(title_string),
     &  LEN_TRIM(cellmeth),TRIM(cellmeth),
     &  LEN_TRIM(Ref_time),TRIM(Ref_time),
     &  LEN_TRIM(INSTITUT),TRIM(INSTITUT),
     &  LEN_TRIM(PRODUCTION),TRIM(PRODUCTION),
     &  LEN_TRIM(SOURCE),TRIM(SOURCE),
     &  LEN_TRIM(COMMENT),TRIM(COMMENT))
!
      ENDDO  !ivars
!
 1201 FORMAT(//,10X,'Geographical coordinates:',/,
     &          10X,'*************************')
 1202 FORMAT(4X,'Northen boundary ..........:',F10.5)
 1203 FORMAT(4X,'Eastern boundary ..........:',F10.5)
 1204 FORMAT(4X,'Western boundary ..........:',F10.5)
 1205 FORMAT(4X,'Southern boundary .........:',F10.5)
 1206 FORMAT(4X,'Grid resolution  ..........:',F6.4,' by ',F6.4,1X,a)

 2001 FORMAT(//,4X,'VARIABLE: ',a,' at month ',F10.5,/,4X,'********')
 2002 FORMAT(4X,'DEPTH',4X,'LAYER DATA NUMBER',2X,'CORELATION LENGTH'
     &,4X,'S/N RATIO',4X,'VARBAK')
 2003 FORMAT(4X,F8.2,6X,I6.4,14X,F6.4,10X,F6.3,4X,F6.3)
 2015 FORMAT(/,4X,'%%NetCDF FILE FIELDS%%',/,
     &         4X,'*************************',12X,'MIN VALUE',
     &4X,'MAX VALUE')
                   
 2004 FORMAT(4X,'VARIABLE                  ',6X,F10.5,4X,F10.5)
 2005 FORMAT(4X,'BOTTOM VARIABLE Val       ',6X,F10.5,4X,F10.5)
 2006 FORMAT(4X,'VARIABLE ERROR            ',6X,F10.5,4X,F10.5)
 2007 FORMAT(4X,'DATA/BINS                 ',6X,F10.5,4X,F10.5)
 2008 FORMAT(4X,'MASKED VARIABLE treshold 1',6X,F10.5,4X,F10.5)
 2009 FORMAT(4X,'MASKED VARIABLE treshold 2',6X,F10.5,4X,F10.5)
 2010 FORMAT(4X,'VARIABLE RELATIVE ERROR   ',6X,F10.5,4X,F10.5)
 2011 FORMAT(4X,'MASKED BOTTOM treshold 1  ',6X,F10.5,4X,F10.5)
 2012 FORMAT(4X,'MASKED BOTTOM treshold 2  ',6X,F10.5,4X,F10.5)
 2013 FORMAT(4X,'OUTLIERS BINS             ',6X,F10.5,4X,F10.5)
 2014 FORMAT(4X,'RELATIVE CORELATION LENGTH',6X,F10.5,4X,F10.5)

      stop
      end
