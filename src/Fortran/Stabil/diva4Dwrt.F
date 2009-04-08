!
      PROGRAM DV4DNCWT
!
      implicit none
!
!--------------------------------------------------------------------!
!
      INTEGER :: MINLEV,MAXLEV
      INTEGER :: ipar
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
      integer                   :: i,j,top_lev,time_len
     &                           , list_len
      integer                   :: NX, NY, NK, ivars,chlen
      integer*4                 :: IMAX, JMAX
!
      real*4                     :: VALEXC, time_val,
     &              var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,
     &              vbt_min,vbt_max,var1_min,var1_max,
     &              vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &              var2_min,var2_max,verel_min,verel_max,
     &             obin_min,obin_max,rl_min,rl_max
      real*4                     ::
     &              var_min0,var_max0,ver_min0,ver_max0,dbin_min0,
     &              vbt_min0,vbt_max0,var1_min0,var1_max0,dbin_max0,
     &              vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &              var2_min0,var2_max0,verel_min0,verel_max0,
     &              obin_min0,obin_max0,rl_min0,rl_max0
!
      real*4                     :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=20)         :: EXECIN
      CHARACTER (len=255)        :: file_name, file_name2
      character (len=255)        :: title_string,cellmeth
     &                            , INSTITUT, PRODUCTION
     &                            , SOURCE, COMMENT
      character (len=30)         :: Ref_time
      CHARACTER (len=99)         :: var_shname, var_name
      character (len=99)         :: var_lgname
      character (len=5)          :: chname
      character (len=20)         :: var_units,vrb_units
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
!
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
!--------------------------------------------------------------
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
      CALL NC_RD3DCL(imax,jmax,nk,ipar,time_val,clbnds(1:2,ivars),
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,
     &  var_min,var_max,vbt_min,vbt_max,ver_min,ver_max,dbin_min,
     &  dbin_max,var1_min,var1_max,var2_min,var2_max,verel_min,
     &  verel_max,vbt1_min,vbt1_max,vbt2_min,vbt2_max,
     &  obin_min,obin_max,rl_min,rl_max,VALEXC,
     &  file_name,var_shname_l(ivars))
!     &  var_lgname,cellmeth,var_units,vrb_units,l_units,Ref_time,
!     &  title_string,INSTITUT,PRODUCTION,SOURCE,COMMENT)
!

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
!
        cellmeth =  cellmeth_l(ivars)
        Ref_time = Ref_time_l(ivars)
!
      CALL NC_4DCLIM(imax,jmax,nk,ipar,time_len,time_val,clbnds,
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,
     &  var_min0,var_max0,vbt_min0,vbt_max0,ver_min0,ver_max0,
     &  dbin_min0,dbin_max0,var1_min0,var1_max0,var2_min0,var2_max0,
     &  verel_min0,verel_max0,vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,
     &  obin_min0,obin_max0,rl_min,rl_max,
     &  VALEXC,l_units,
     &  LEN_TRIM(file_name2),TRIM(file_name2),
     &  LEN_TRIM(var_name),TRIM(var_name),
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
      stop
      end
