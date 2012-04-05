!
      PROGRAM DV4D_QC_NCWT
!
      implicit none
!
!--------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,ipar
!
!!      real*4 :: lon,lat,val,x,y
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
     &                                 , Nabla_var,Delta_var,Norm_var
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE ::
     &              var_rms,ver_rms,var1_rms,var2_rms,verel_rms
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
     &      , Nabla_min,Nabla_max,Delta_min,Delta_max,Norm_min,Norm_max
      real*4                     ::
     &              var_min0,var_max0,ver_min0,ver_max0,dbin_min0,
     &              vbt_min0,vbt_max0,var1_min0,var1_max0,dbin_max0,
     &              vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &              var2_min0,var2_max0,verel_min0,verel_max0,
     &              obin_min0,obin_max0,rl_min0,rl_max0
     &      , Nabla_min0,Nabla_max0,Delta_min0,Delta_max0,Norm_min0
     &                              ,Norm_max0
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
      CHARACTER (len=255)        :: file_name, file_name2, file_4Dnc
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
      write(*,*)' WRITE 4D NetCdf file gathering:'      
      J = 0
      DO I = 1, time_len
        IF(flags(I) == 1) then
           J = J + 1
           all_files(J) = les_files(I)
           write(*,*)TRIM(all_files(J))
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
!      divafile = '../output/3Danalysis/Fields/GridInfo.dat'
      divafile = '../output/3Danalysis/GridInfo.dat'
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
      ALLOCATE(Nabla_var(NX,NY,NK))
      ALLOCATE(Delta_var(NX,NY,NK))
      ALLOCATE(Norm_var(NX,NY,NK))
!
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
      ALLOCATE(varbot(1:NX,1:NY))
      ALLOCATE(varb1(1:NX,1:NY))
      ALLOCATE(varb2(1:NX,1:NY))
!
      ALLOCATE(var_rms(NK))
      ALLOCATE(ver_rms(NK))
      ALLOCATE(var1_rms(NK))
      ALLOCATE(var2_rms(NK))
      ALLOCATE(verel_rms(NK))
!
      EXECIN='../3DWORK/3DNCinfo'
      INQUIRE(FILE=EXECIN,EXIST=exist)
      if(exist) then
          close(12)
          OPEN(UNIT=12,FILE=EXECIN)
          read(12,*) INSTITUT
          read(12,*) PRODUCTION
          read(12,*) SOURCE
          read(12,*) COMMENT
          read(12,*) var_lgname
          read(12,*) var_units
          read(12,*) vrb_units
          read(12,*) title_string
          read(12,*) l_units(1)
          read(12,*) l_units(2)
          J = 0
          DO  I = 1, list_len
            IF(flags(I) == 1) then
              J = J+1
              read(12,*) var_shname_l(J)
              read(12,*) cellmeth_l(J)
              read(12,*) Ref_time_l(J)
            ELSE
              read(12,*) comments
              read(12,*) comments
              read(12,*) comments
            ENDIF
          ENDDO
      endif
!
! Writing an info text file for matadata files:
      WRITE(file_name2,'("../output/3Danalysis/",
     &a,".4DNCMetainfo")')TRIM(var_shname_l(1))
      close(12)
      OPEN(UNIT=12,FILE=file_name2)
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
      write(12,1111)SouthBL
      write(12,1111)NorthBL
      write(12,1111)WestBL
      write(12,1111)EastBL
      write(12,1112)dx
      write(12,1112)dy
!
 1111 FORMAT(1X,F7.3)
 1112 FORMAT(1X,F5.3)
!--------------------------------------------------------------
!
!
      WRITE(title_string,'("DIVA 4D analysis of ",a)')TRIM(var_shname)
!
!!!!      var_name = var_shname
      chlen = 1
      DO while(var_shname(chlen:chlen) .ne. ' ')
!         IF(var_shname(chlen:chlen) .eq. '.') 
!     &             WRITE(var_name(chlen:chlen),'("_")')
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      var_name = var_shname(1:chlen-9)
!
! Writing an info text file for matadata files:

      WRITE(file_name2,'("../output/3Danalysis/",
     &a,".Metainfo.txt")')TRIM(var_name)
      INQUIRE(FILE=file_name2,EXIST=exist)
      if(exist) then
      write(*,*)TRIM(file_name2),'present'
      else
      close(12)
      OPEN(UNIT=12,FILE=file_name2)
!
      write(12,1201)
      write(12,1202)NorthBL
      write(12,1204)WestBL
      write(12,1203)EastBL
      write(12,1205)SouthBL
      write(12,1206)dx,dy,s_units
      endif
!
      WRITE(file_4Dnc,'("../output/3Danalysis/",
     &a,".4Danl.nc")')TRIM(var_shname)
!
      clbnds = 0.
      ipar = 1
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
      CALL NC_RD3DCL(imax,jmax,nk,ipar,time_val,clbnds(1:2,ivars),
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &  obin_min,obin_max,rl_min,rl_max,VALEXC,
     &  file_name,var_shname_l(ivars))
!     &  var_lgname,cellmeth,var_units,vrb_units,l_units,Ref_time,
!     &  title_string,INSTITUT,PRODUCTION,SOURCE,COMMENT)
!
        CALL CALRMS(imax,jmax,nk,valexc,var,var1,var2,verr,reler
     &             ,var_rms,ver_rms,var1_rms,var2_rms,verel_rms)
!
      CALL CALDIVANORM(IMAX,JMAX,nk,dx,dy,VALEXC,var
     &                ,Nabla_var,Delta_var,Norm_var)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(Nabla_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Nabla_var(:,:,k) /= valexc))
       resmin1(1:jmax,k)=MINVAL(Nabla_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Nabla_var(:,:,k) /= valexc))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      Nabla_max = yr
      Nabla_min = xr

!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(Norm_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Norm_var(:,:,k) /= valexc))
       resmin1(1:jmax,k)=MINVAL(Norm_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Norm_var(:,:,k) /= valexc))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      Norm_max = yr
      Norm_min = xr

!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(Delta_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Delta_var(:,:,k) /= valexc))
       resmin1(1:jmax,k)=MINVAL(Delta_var(1:imax,1:jmax,k),dim=1,
     &                        MASK= (Delta_var(:,:,k) /= valexc))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      Delta_max = yr
      Delta_min = xr
!
      close(12)
      OPEN(UNIT=12,FILE=file_name2,STATUS='OLD',POSITION='APPEND')
      write(12,2001)TRIM(var_shname_l(ivars)),time_val
      write(12,2002)
      do lev=1,NK
      read(2,*) ilev, nbdat
      read(3,*)CORLEN(lev),SN(lev),VARBAK(lev)
      write(12,2003)dep(lev),nbdat,CORLEN(lev),SN(lev),VARBAK(lev)
      enddo
      write(12,2015)
      write(12,2004)var_min,var_max
      write(12,2008)var1_min,var1_max
      write(12,2009)var2_min,var2_max
      write(12,2005)vbt_min,vbt_max
      write(12,2011)vbt1_min,vbt1_max
      write(12,2012)vbt2_min,vbt2_max
      write(12,2006)ver_min,ver_max
      write(12,2010)verel_min,verel_max
      write(12,2007)dbin_min,dbin_max
      write(12,2013)obin_min,obin_max
      write(12,2014)rl_min,rl_max
      close(12)

      IF(ivars == 1) THEN
        var_min0 = var_min
        var_max0 = var_max
        ver_min0 = ver_min
        ver_max0 = ver_max
        dbin_min0 = dbin_min
        var1_min0 = var1_min
        var1_max0 = var1_max
        dbin_max0 = dbin_max
        var2_min0 = var2_min
        var2_max0 = var2_max
        verel_min0 = verel_min
        verel_max0 = verel_max
        obin_min0 = obin_min
        obin_max0 = obin_max
        rl_min0 = rl_min
        rl_max0 = rl_max
        vbt_min0 = vbt_min
        vbt_max0 = vbt_max
        vbt1_min0 = vbt1_min
        vbt1_max0 = vbt1_max
        vbt2_min0 = vbt2_min
        vbt2_max0 = vbt2_max
        Nabla_max0 = Nabla_max
        Nabla_min0 = Nabla_min
        Delta_max0 = Delta_max
        Delta_min0 = Delta_min
        Norm_max0 = Norm_max
        Norm_min0 = Norm_min
      ENDIF
!
        IF(var_min0 >= var_min) var_min0 = var_min
        IF(var_max0 <= var_max) var_max0 = var_max
        IF(ver_min0 >= ver_min) ver_min0 = ver_min
        IF(ver_max0 <= ver_max) ver_max0 = ver_max
        IF(dbin_min0 >= dbin_min) dbin_min0 = dbin_min
        IF(dbin_max0 <= dbin_max) dbin_max0 = dbin_max
        IF(var1_min0 >= var1_min) var1_min0 = var1_min
        IF(var1_max0 <= var1_max) var1_max0 = var1_max
        IF(var2_min0 >= var2_min) var2_min0 = var2_min
        IF(var2_max0 <= var2_max) var2_max0 = var2_max
        IF(verel_min0 >= verel_min) verel_min0 = verel_min
        IF(verel_max0 <= verel_max) verel_max0 = verel_max
        IF(obin_min0 >= obin_min) obin_min0 = obin_min
        IF(obin_max0 <= obin_max) obin_max0 = obin_max
        IF(rl_min0 >= rl_min) rl_min0 = rl_min
        IF(rl_max0 <= rl_max) rl_max0 = rl_max
        IF(vbt_min0 >= vbt_min) vbt_min0 = vbt_min
        IF(vbt_max0 <= vbt_max) vbt_max0 = vbt_max
        IF(vbt1_min0 >= vbt1_min) vbt1_min0 = vbt1_min
        IF(vbt1_max0 <= vbt1_max) vbt1_max0 = vbt1_max
        IF(vbt2_min0 >= vbt2_min) vbt2_min0 = vbt2_min
        IF(vbt2_max0 <= vbt2_max) vbt2_max0 = vbt2_max
        IF(Nabla_max0 <= Nabla_max) Nabla_max0 = Nabla_max
        IF(Nabla_min0 >= Nabla_min) Nabla_min0 = Nabla_min
        IF(Delta_max0 <= Delta_max) Delta_max0 = Delta_max
        IF(Delta_min0 >= Delta_min) Delta_min0 = Delta_min
        IF(Norm_max0 <= Norm_max) Norm_max0 = Norm_max
        IF(Norm_min0 >= Norm_min) Norm_min0 = Norm_min
!
        cellmeth =  cellmeth_l(ivars)
        Ref_time = Ref_time_l(ivars)
!
!------
!
      CALL NCDF_CLIM(imax,jmax,nk,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  Nabla_var,Delta_var,Norm_var,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,
     &  var_rms,ver_rms,var1_rms,var2_rms,verel_rms,
     &  var_min0,var_max0,vbt_min0,vbt_max0,ver_min0,ver_max0,
     &  dbin_min0,dbin_max0,var1_min0,var1_max0,var2_min0,var2_max0,
     &  verel_min0,verel_max0,vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &  Nabla_min0,Nabla_max0,Delta_min0,Delta_max0,Norm_min0,Norm_max0,
     &  obin_min0,obin_max0,rl_min,rl_max,
     &  VALEXC,l_units,
     &  LEN_TRIM(file_4Dnc),TRIM(file_4Dnc),
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
      WRITE(file_name2,'("../output/3Danalysis/",
     &a,".4DNCMetainfo")')TRIM(var_shname_l(1))
      close(12)
      OPEN(UNIT=12,FILE=file_name2,STATUS='OLD',POSITION='APPEND')
      write(12,*)(dep(lev),lev=1,NK)
      close(12)
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
 2015 FORMAT(/,4X,'%%NetCDF FILE FIELDS%%',12X,'MIN VALUE',
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
