!
      PROGRAM DVTSDSWRT
!
      implicit none
!
!---------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,ichge
!
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: Z, dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT,rmsp,rmsn
!!     &                                     , VALEXU,CORLEN,SN,VARBAK
!
!
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE :: temp,psal,N2
     &                                           , RHO,ert,ers
      REAL*4 , DIMENSION(:,:,:),    ALLOCATABLE   :: alpha,beta
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1
     &                                        , resmax2,resmin2
!
      real*4              :: temp_min,temp_max,psal_min,psal_max
     &                     , N2_min,N2_max
     &                     , RHO_min,RHO_max,ert_min,ers_min
     &                     , ert_max,ers_max
     &                     , x,y,xr,yr
      REAL*4 ,DIMENSION(2,1)                 :: clbnds
      REAL*4 ,DIMENSION(2)                   :: time_val
!
      integer,DIMENSION(:,:,:),  ALLOCATABLE :: mask,mask1,mask2,masker
!
      integer            :: i,j,k,klev,ic,top_lev,kuw,kwu,ipar,iparpar
      integer                   :: NX, NY, NK, ndata, nl,chlen,iun
      integer*4                 :: KMAX, ipr, nw, IMAX, JMAX, flag
      integer                   :: ICOOC, IREG, ISPEC
      integer                   :: nm,xm1,xm2,xy1,xy2
!
      real*4                     :: zz, VALEXC
!
      real*4                     :: xorig, yorig, dx, dy, xend, yend
!
      CHARACTER (len=255)        :: divafile,comments
      CHARACTER (len=255)         :: EXECIN
      CHARACTER (len=22)         :: DEPTHS
      CHARACTER (len=99)         :: VARFILEIN
      CHARACTER (len=255)        :: file_name, ncliste,prmliste
      character (len=255)        :: INSTITUT,PRODUCTION,SOURCE,COMMENT
      character (len=255), DIMENSION(2) ::
     &                                   title_string,Ref_time, cellmeth
      CHARACTER (len=99), DIMENSION(2) :: var_shname,var_name,var_cfname
      character (len=99), DIMENSION(2)         :: var_lgname
      character (len=20), DIMENSION(2)         :: var_units,vrb_units
      character (len=20), DIMENSION(2)         :: l_units
      character (len=99)         :: varname
      character (len=4)          :: yy1,yy2
      character (len=2)          :: mm1,mm2
      CHARACTER (len=28)                :: REPORT
      CHARACTER (len=26)                :: INFOLY1,INFOLY2
!
      LOGICAL                          :: exist
!--------------------------------------------------------------
!
!
! reading procedure information parameters
      EXECIN='../input/STBinfo'
      OPEN(2,FILE=TRIM(EXECIN),STATUS='OLD')
      READ(2,*)! comments
      READ(2,*) var_name(1)
      READ(2,*)! comments
      READ(2,*) var_name(2)
      READ(2,*)! comments
      READ(2,*) MINLEV
      READ(2,*)! comments
      READ(2,*) MAXLEV
      READ(2,*)! comments
      READ(2,*)! surface_lev
      CLOSE(2)
      varname = var_name(1)
      chlen = 1
      DO while(varname(chlen:chlen) .ne. ' ') 
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      var_name(1) = varname(1:chlen-14)
      varname = var_name(2)
      chlen = 1
      DO while(varname(chlen:chlen) .ne. ' ') 
         chlen = chlen + 1
      ENDDO
      chlen = chlen - 1
      var_name(2) = varname(1:chlen-14)
!
      EXECIN='./fort.44'
      OPEN(2,FILE=TRIM(EXECIN),STATUS='OLD')
      READ(2,*) iparpar
      CLOSE(2)
!
      DO ivar = 1,2
!
      IF(ivar == 1) THEN
      EXECIN='../input/3Dinfosalinity'
      ELSE
      EXECIN='../input/3Dinfotemper'
      ENDIF
      OPEN(2,FILE=TRIM(EXECIN),STATUS='OLD')
      READ(2,*)! comments
      READ(2,*) var_shname(ivar)
      READ(2,*)! comments
      READ(2,*)! MINLEV
      READ(2,*)! comments
      READ(2,*)! MAXLEV
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) ! comments
      READ(2,*) title_string(ivar)
      READ(2,*) !comments
      READ(2,*) var_lgname(ivar)
      READ(2,*) !comments
      READ(2,*) var_units(ivar)
      IF(iparpar .lt. 36) then
        time_val(ivar) = 0.
        xm1 = 0
        xm2 = 0
        xy1 = 0
        xy2 = 0
        WRITE(Ref_time(ivar),'("Months since xxxx-01-01 ")')
        WRITE(cellmeth(ivar),'(
     &"time: mean within month time: mean over years")')
        close(2)
        INSTITUT = 'Not specified'
        PRODUCTION = 'Not specified'
        SOURCE = 'Not specified'
        COMMENT = 'Not specified'
      ELSE
        READ(2,*)! comments
        READ(2,*) ipar
          READ(2,*)! comments
          READ(2,*) Ref_time(ivar)
          READ(2,*)! comments
          READ(2,*) time_val(ivar)
          READ(2,*)! comments
          READ(2,*) cellmeth(ivar)
          READ(2,*)! comments
          READ(2,*) INSTITUT
          READ(2,*)! comments
          READ(2,*) PRODUCTION
          READ(2,*)! comments
          READ(2,*) SOURCE
          READ(2,*)! comments
          READ(2,*) COMMENT
          clbnds = 0.
          CLOSE(2)
      ENDIF
      ENDDO
!
      varname=var_shname(1)

      IF(ipar == 1) then
        chlen = 1
        DO while(varname(chlen:chlen) .ne. ' ') 
           chlen = chlen + 1
        ENDDO
        chlen = chlen - 1
        mm2 = varname(chlen-1:chlen)
        mm1 = varname(chlen-3:chlen-2)
        yy2 = varname(chlen-8:chlen-5)
        yy1 = varname(chlen-12:chlen-9)
        if(yy1(1:4) .EQ. 'xxxx') yy1='1850'
        if(yy2(1:4) .EQ. 'xxxx') yy2='2050'
        WRITE(Ref_time,
     &'("Months since ",a,"-01-01")'),yy1(1:4)
        read (mm1,'(I2.2)') xm1
        read (mm2,'(I2.2)') xm2
        read (yy1,'(I4.4)') xy1
        read (yy2,'(I4.4)') xy2
        if(xm1 > xm2) then
           nm = xm2 + (12 - xm1) + 1
        else
           nm = xm2 - xm1 + 1
        endif
        time_val = 12*(nint(float(xy2)-float(xy1))/2)
     &                        +float(xm1-1)+float(nm)/2
        clbnds(1,1)= 1.*xm1 !float(xm1)
        clbnds(2,1)= 1.*xm2 !float(xm2)
        WRITE(cellmeth,'(
     &"time: mean within month time: mean over years")')
      ENDIF
!
      top_lev = MAXLEV - MINLEV + 1
      ALLOCATE(dep(top_lev))

      ALLOCATE(Z(top_lev))
      REPORT    = '../output/3Danalysis/SIGMA'
      INFOLY1   = '../output/3Danalysis/BN2'
      INFOLY2   = '../output/3Danalysis/TST'
      OPEN(UNIT=12,FILE=REPORT)
      OPEN(UNIT=121,FILE=INFOLY2)
      OPEN(UNIT=120,FILE=INFOLY1)
!
!     Read the grid data from GridInfo.dat
!--------------------------------------------------------------
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
!arrays memory allocation
      NX = INT(xend)
      NY = INT(yend)
      NK = top_lev
!
      ALLOCATE(temp(NX,NY,NK))
      ALLOCATE(psal(NX,NY,NK))
      ALLOCATE(N2(NX,NY,NK-1))
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(mask(1:NX,1:NY,1:NK))
      ALLOCATE(RHO(NX,NY,NK))
      ALLOCATE(ert(NX,NY,NK))
      ALLOCATE(ers(NX,NY,NK))
      ALLOCATE(alpha(NX,NY,NK))
      ALLOCATE(beta(NX,NY,NK))
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
      ALLOCATE(resmax2(1:NY,NK-1))
      ALLOCATE(resmin2(1:NY,NK-1))
      ALLOCATE(rmsp(NK))
      ALLOCATE(rmsn(NK))
      imax = NX
      jmax = NY
!
      WRITE(file_name,'("../output/3Danalysis/",a,".1",i4.4,
     &".1",i4.4,".anl.nc")')TRIM(var_shname(1)),MINLEV,MAXLEV

      CALL NC_RDVARER(imax,jmax,nk,psal,ers,xlon,ylat,dep,
     &  psal_min,psal_max,ers_min,ers_max,VALEXC,l_units,
     &  TRIM(file_name),TRIM(var_name(1)))
!
      WRITE(file_name,'("../output/3Danalysis/",a,".1",i4.4,
     &".1",i4.4,".anl.nc")')TRIM(var_shname(2)),MINLEV,MAXLEV

      CALL NC_RDVARER(imax,jmax,nk,temp,ert,xlon,ylat,dep,
     &  temp_min,temp_max,ert_min,ert_max,VALEXC,l_units,
     &  TRIM(file_name),TRIM(var_name(2)))
!
      k=0
      DO i = MINLEV,MAXLEV
         k = k+1
         Z(k)  = -1.*dep(k) ! negatif values for depts
      ENDDO
!
! retreiving masks from data and levels dept
      mask = 1
      DO k = 1,NK
         DO j = 1,jmax
           DO i = 1,imax
             if(psal(i,j,k) == valexc) mask(i,j,k) = 0
           ENDDO
         ENDDO
      ENDDO
!
      DO k = NK-1,1,-1
      DO j = 1,jmax
      DO i = 1,imax
      if ((mask(i,j,k+1) - mask(i,j,k)) .lt. 0 )
     & mask(i,j,k) = mask(i,j,k+1)
      ENDDO
      ENDDO
      ENDDO
!
      CALL alpbetsig
     &  (valexc,nx,ny,nk,mask,dep,psal,temp,alpha,beta,RHO)
!
      DO k = 1,NK
       resmax1(1:jmax,k)=MAXVAL(RHO(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin1(1:jmax,k)=MINVAL(RHO(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax1)
      x = MINVAL(resmin1)
      call jmbround(x,y,xr,yr)
      RHO_max = yr
      RHO_min = xr
!
      CALL brunt
     &  (valexc,nx,ny,nk,Z,mask,alpha,beta,temp,psal,N2,rmsp,rmsn)
!
      DO k = 1,NK-1
       resmax2(1:jmax,k)=MAXVAL(N2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
       resmin2(1:jmax,k)=MINVAL(N2(1:imax,1:jmax,k),dim=1,
     &                        MASK= (mask(:,:,k) .eq. 1))
      ENDDO
      y = MAXVAL(resmax2)
      x = MINVAL(resmin2)
      call jmbround(x,y,xr,yr)
      N2_min = xr
      N2_max = yr
!
      WRITE(file_name,'("../output/3Danalysis/",
     &"TEMP_SAL_",a,a,"_",a,a,"_1",i4.4,"_1",i4.4,".nc")')
     &mm1,mm2,yy1,yy2,MINLEV,MAXLEV
!
      CALL NC_TSDN2(IMAX, JMAX,nk,time_val,
     &  psal,temp,ers,ert,RHO,N2,xlon,ylat,dep,
     &  psal_min,psal_max,temp_min,temp_max,ers_min,ers_max,ert_min,
     &  ert_max,RHO_min,RHO_max,N2_min,N2_max,
     &  VALEXC,l_units,
     &  LEN_TRIM(file_name),TRIM(file_name),
     &  LEN_TRIM(var_name(1)),TRIM(var_name(1)),
     &  LEN_TRIM(var_lgname(1)),TRIM(var_lgname(1)),
     &  LEN_TRIM(var_units(1)),TRIM(var_units(1)),
     &  LEN_TRIM(var_name(2)),TRIM(var_name(2)),
     &  LEN_TRIM(var_lgname(2)),TRIM(var_lgname(2)),
     &  LEN_TRIM(var_units(2)),TRIM(var_units(2)),
     &  LEN_TRIM(title_string(1)),TRIM(title_string(1)),
     &  LEN_TRIM(cellmeth(1)),TRIM(cellmeth(1)),
     &  LEN_TRIM(Ref_time(1)),TRIM(Ref_time(1)),
     &  LEN_TRIM(INSTITUT),TRIM(INSTITUT),
     &  LEN_TRIM(PRODUCTION),TRIM(PRODUCTION),
     &  LEN_TRIM(SOURCE),TRIM(SOURCE),
     &  LEN_TRIM(COMMENT),TRIM(COMMENT))
!
      stop
      end
!
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
