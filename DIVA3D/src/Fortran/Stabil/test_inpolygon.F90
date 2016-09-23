Program test_inpolygon

implicit none

!------------------------------------------------
! adapted by Sylvain watelet - 13/07/2015 
! swatelet@ulg.ac.be
!------------------------------------------------

integer::i,j,npol,n
integer::imin,jmin
integer::nshape,npoints
integer::obs,idlen
real,allocatable::px(:),py(:)
integer,allocatable::ivect(:),jvect(:)
real::x,y
logical::in,on
 character(len=250)::ncfile,contfile,outfile,dim1,dim2,dim3,dim4,dim5,dim6

!--------------------------------------------------------------------!
!
      INTEGER :: ivar, istep,MINLEV,MAXLEV
      INTEGER :: step,ipar
!
      DOUBLE PRECISION :: lon,lat,val
!
      integer , DIMENSION(:) ,  ALLOCATABLE :: flags
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: dep
      REAL*4 , DIMENSION(:)  ,  ALLOCATABLE :: XLON,YLAT, time_val
      REAL*4 , DIMENSION(:,:), ALLOCATABLE :: CORLEN, SN, VARBAK
!
      REAL*4 ,DIMENSION(:,:),    ALLOCATABLE :: resmax1,resmin1,clbnds
!
      REAL*4 ,DIMENSION(:,:,:),    ALLOCATABLE :: varbot		&
     &                                        , varb1, varb2
!
      REAL*4 , DIMENSION(:,:,:,:),  ALLOCATABLE :: var, var1, var2	&
     &                                        , verr,reler,dbins	&
     &                                        , obins,rlfield

	real*4,dimension(:),allocatable	:: obslon,obslat,obsdepth,obstime
!
      integer                   :: k,klev,ic,top_lev,kuw,time_len	&
     &                           , list_len
      integer                   :: NX,NY,NK,ndata,nl,ivars,chlen,lev
      integer*4                 :: KMAX,ipr,nw,IMAX,JMAX
      integer                   :: ICOOC,IREG,ISPEC, ilev, nbdat
!
      real*4                     :: VALEXC, zz,				&
     &              var_min,var_max,ver_min,ver_max,dbin_min,dbin_max,	&
     &              vbt_min,vbt_max,var1_min,var1_max,			&
     &              vbt1_min,vbt1_max,vbt2_min,vbt2_max,		&
     &              var2_min,var2_max,verel_min,verel_max,		&
     &             obin_min,obin_max,rl_min,rl_max,xr,yr	
      real*4                     ::					&
     &              var_min0,var_max0,ver_min0,ver_max0,dbin_min0,	&
     &              vbt_min0,vbt_max0,var1_min0,var1_max0,dbin_max0,	&
     &              vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,		&
     &              var2_min0,var2_max0,verel_min0,verel_max0,		&
     &              obin_min0,obin_max0,rl_min0,rl_max0		
!
      real*4                     :: xorig, yorig, dx, dy, xend, yend	&
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
      CHARACTER (len=255)        :: file_name, file_name2, ncliste, file_4Dnc
      character (len=255)        :: title_string,cellmeth		&
     &                            , INSTITUT, PRODUCTION		&
     &                            , SOURCE, COMMENT, Email
      character (len=30)         :: Ref_time
	character(len=30),dimension(:),allocatable:: obsid,obsid1,obsid2
      CHARACTER (len=99)         :: var_shname, var_cfname, var_name	&
     &                            , dat_name, var1_name, prmliste
      character (len=99)         :: var_lgname
      character (len=5)          :: chname
      character (len=20)         :: var_units,vrb_units
      character (len=20)                          :: s_units
      character (len=20) , DIMENSION(2)           :: l_units

	character(len=256)	:: obsid_lgname,obsid_coord
	character(len=256)	:: obslon_units,obslat_units
	character(len=256)	:: obsdepth_units,obsdepth_positive,obstime_units

      character (len=255), DIMENSION(:),ALLOCATABLE :: all_vars		&
     &                                               , all_files	&
     &                                               , les_files	&
     &                                               , var_shname_l	&
     &                                               , cellmeth_l	&
     &                                               , Ref_time_l
!--------------------------------------------------------------------!

call get_command_argument(1,ncfile)
call get_command_argument(2,contfile)
call get_command_argument(3,outfile)
call get_command_argument(4,dim1)
call get_command_argument(5,dim2)
call get_command_argument(6,dim3)
call get_command_argument(7,dim4)
call get_command_argument(8,dim5)
call get_command_argument(9,dim6)

open(10,file=contfile,status="old")

read(10,*) nshape
read(10,*) npoints

allocate(px(npoints),py(npoints))

Do i=1,npoints
	read(10,*) px(i),py(i)
Enddo
 close(10)

npol = size(px)

!------------------------------------------------
! Read 4D NCDF
!------------------------------------------------

read(dim1,'(i10)') NX
read(dim2,'(i10)') NY
read(dim3,'(i10)') NK
read(dim4,'(i10)') time_len
write(*,*) NX,NY,NK,time_len
read(dim5,'(i10)') obs
read(dim6,'(i10)') idlen
!write(*,*) obs,idlen

imax=NX
jmax=NY

allocate(ivect(NX*NY))
ivect=0
allocate(jvect(NX*NY))
jvect=0

file_name2=ncfile
var_shname=ncfile(21:len_trim(ncfile)-9)


      ALLOCATE(clbnds(2,time_len))
      ALLOCATE(XLON(NX))
      ALLOCATE(YLAT(NY))
      ALLOCATE(var(NX,NY,NK,time_len))
      ALLOCATE(var1(NX,NY,NK,time_len))
      ALLOCATE(var2(NX,NY,NK,time_len))
      ALLOCATE(verr(NX,NY,NK,time_len))
      ALLOCATE(reler(NX,NY,NK,time_len))
      ALLOCATE(dbins(NX,NY,NK,time_len))
      ALLOCATE(obins(NX,NY,NK,time_len))
      ALLOCATE(rlfield(NX,NY,NK,time_len))
!
      ALLOCATE(resmax1(1:NY,NK))
      ALLOCATE(resmin1(1:NY,NK))
      ALLOCATE(varbot(1:NX,1:NY,time_len))
      ALLOCATE(varb1(1:NX,1:NY,time_len))
      ALLOCATE(varb2(1:NX,1:NY,time_len))
!
      ALLOCATE(dep(NK))
      ALLOCATE(CORLEN(NK,time_len))
      ALLOCATE(SN(NK,time_len))
      ALLOCATE(VARBAK(NK,time_len))
!
	allocate(time_val(time_len))
	allocate(obsid(obs))
        allocate(obsid1(obs))
        allocate(obsid2(obs))
	allocate(obslon(obs))
	allocate(obslat(obs))
	allocate(obsdepth(obs))
	allocate(obstime(obs))

	title_string=""
	Ref_time=""
	var_lgname=""
	var_units=""
	vrb_units=""
	INSTITUT=""
	PRODUCTION=""
	SOURCE=""
	Email=""
	COMMENT=""
	cellmeth=""
	l_units=""
	
	obsid_lgname=""
	obsid_coord=""
	obslon_units=""
	obslat_units=""
	obsdepth_units=""
	obsdepth_positive=""
	obstime_units=""

      CALL NC_RD4Dfinal(imax,jmax,nk,ipar,time_len,time_val,clbnds,		&
     &  var,var1,var2,verr,reler,dbins,obins,rlfield,varbot,varb1,varb2,	&
     &  xlon,ylat,dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,			&
     &  var_min0,var_max0,vbt_min0,vbt_max0,ver_min0,ver_max0,			&
     &  dbin_min0,dbin_max0,var1_min0,var1_max0,var2_min0,var2_max0,		&
     &  verel_min0,verel_max0,vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,		&
     &  obin_min0,obin_max0,rl_min,rl_max,					&
     &  VALEXC,l_units,								&
     &  LEN_TRIM(file_name2),TRIM(file_name2),					&
     &  LEN_TRIM(var_shname),TRIM(var_shname),					&
     &  LEN_TRIM(var_lgname),var_lgname,					&
     &  LEN_TRIM(var_units),var_units,		  				&
     &  LEN_TRIM(vrb_units),vrb_units,						&
     &  LEN_TRIM(title_string),title_string,					&
     &  LEN_TRIM(cellmeth),cellmeth,						&
     &  LEN_TRIM(Ref_time),Ref_time,						&
     &  LEN_TRIM(INSTITUT),INSTITUT,						&
     &  LEN_TRIM(PRODUCTION),PRODUCTION,					&
     &  LEN_TRIM(SOURCE),SOURCE,						&
     &  LEN_TRIM(Email),Email,							&
     &  LEN_TRIM(COMMENT),COMMENT,						&
     &  obs,idlen,obsid,obsid_lgname,obsid_coord,obslon,obslon_units,		&
     &  obslat,obslat_units,obsdepth,obsdepth_units,obsdepth_positive,          &
     &  obstime,obstime_units,obsid1,obsid2)

!Do i=1,obs
!write(*,*) obsid(i)
!Enddo

!write(*,*) obslon_units


!------------------------------------------------
! Test inpolygon
!------------------------------------------------

k=0
Do j=1,NY
	Do i=1,NX
     		x = xlon(i)
     		y = ylat(j)

     		in = inpolygon (x, y, px, py,on)

     		!write(6,*) 'in ',in
     		if (in) then
      		 write (6,'(A)',advance='no') '.'
		 k=k+1
		 ivect(k)=i
		 jvect(k)=j
     		else
       		 write (6,'(A)',advance='no') 'X'
		 var(i,j,:,:)=VALEXC
		 var1(i,j,:,:)=VALEXC
		 var2(i,j,:,:)=VALEXC
		 reler(i,j,:,:)=VALEXC
		 verr(i,j,:,:)=VALEXC
		 dbins(i,j,:,:)=VALEXC
		 obins(i,j,:,:)=VALEXC
		 rlfield(i,j,:,:)=VALEXC
		 varbot(i,j,:)=VALEXC
		 varb1(i,j,:)=VALEXC
		 varb2(i,j,:)=VALEXC
     		end if
	Enddo
   
   	write (6,'(A)') 
Enddo

deallocate(px,py)

!------------------------------------------------
! Compute new bounding box
!------------------------------------------------

imin=max(minval(ivect(1:k))-1,1)
jmin=max(minval(jvect(1:k))-1,1)
imax=min(maxval(ivect(1:k))+1,NX)
jmax=min(maxval(jvect(1:k))+1,NY)

write(*,*) imin,jmin
write(*,*) imax,jmax

!------------------------------------------------
! Write cutted 4D NCDF
!------------------------------------------------

file_4Dnc=outfile

      CALL NC_WR4Dfinal(							&
     &  (imax-imin)+1,(jmax-jmin)+1,nk,ipar,time_len,time_val,clbnds,		&
     &  var(imin:imax,jmin:jmax,:,:),var1(imin:imax,jmin:jmax,:,:),		&
     &	var2(imin:imax,jmin:jmax,:,:),verr(imin:imax,jmin:jmax,:,:),		&
     &	reler(imin:imax,jmin:jmax,:,:),dbins(imin:imax,jmin:jmax,:,:),		&
     &	obins(imin:imax,jmin:jmax,:,:),rlfield(imin:imax,jmin:jmax,:,:),	&
     &  varbot(imin:imax,jmin:jmax,:),varb1(imin:imax,jmin:jmax,:),		&
     &	varb2(imin:imax,jmin:jmax,:),						&
     &  xlon(imin:imax),ylat(jmin:jmax),dep,CORLEN,SN,VARBAK,IREG,ISPEC,ICOOC,	&
     &  var_min0,var_max0,vbt_min0,vbt_max0,ver_min0,ver_max0,			&
     &  dbin_min0,dbin_max0,var1_min0,var1_max0,var2_min0,var2_max0,		&
     &  verel_min0,verel_max0,vbt1_min0,vbt1_max0,vbt2_min0,vbt2_max0,		&
     &  obin_min0,obin_max0,rl_min,rl_max,					&
     &  VALEXC,l_units,								&
     &  LEN_TRIM(file_4Dnc),TRIM(file_4Dnc),					&	
     &  LEN_TRIM(var_shname),TRIM(var_shname),					&
     &  LEN_TRIM(var_lgname),TRIM(var_lgname),					&
     &  LEN_TRIM(var_units),TRIM(var_units),					&
     &  LEN_TRIM(vrb_units),TRIM(vrb_units),					&
     &  LEN_TRIM(title_string),TRIM(title_string),				&
     &  LEN_TRIM(cellmeth),TRIM(cellmeth),					&
     &  LEN_TRIM(Ref_time),TRIM(Ref_time),					&
     &  LEN_TRIM(INSTITUT),TRIM(INSTITUT),					&
     &  LEN_TRIM(PRODUCTION),TRIM(PRODUCTION),					&
     &  LEN_TRIM(SOURCE),TRIM(SOURCE),						&
     &  LEN_TRIM(Email),TRIM(Email),						&
     &  LEN_TRIM(COMMENT),TRIM(COMMENT),					&
     &  obs,idlen,obsid,obsid_lgname,obsid_coord,obslon,obslon_units,		&
     &  obslat,obslat_units,obsdepth,obsdepth_units,obsdepth_positive,		&
     &  obstime,obstime_units,obsid1,obsid2,					&
     &  LEN_TRIM(file_name2),TRIM(file_name2))

!------------------------------------------------
 
contains


! Copyright (C) 2006-2015 Frederick (Rick) A Niles, and Søren Hauberg
! from GNU octave
! GPL 3 or later
! http://hg.savannah.gnu.org/hgweb/octave/file/aa36fb998a4d/scripts/geometry/inpolygon.m

 function InPolygon(x,y,px,py,on) result(in)
  implicit none
  real, intent(in) :: px(:),py(:), x,y
  logical, intent(out), optional :: on
  logical :: in

  integer :: i,j, npol
  real :: dx, dy, distance

  npol = size(px)
  j = npol
  in = .false.

  do i = 1, npol
    dx = px(j) - px(i)
    dy = py(j) - py(i)
    ! distance = [distance from (x,y) to edge] * length(edge)
    distance = dx * (y - py(i)) - (x - px(i)) * dy
    !
    ! is y between the y-values of edge i,j
    !        AND (x,y) on the left of the edge ?
    
    if (((py(i) <= y .and. y < py(j)) .or. (py(j) <= y .and. y < py(i))).and. 0 < distance *dy) then
      in = .not. in
    end if

   ! Check if (x,y) are actually on the boundary of the polygon.

    if (present(on)) then
      if (((py(i) <= y .and. y <= py(j)) .or. (py(j) <= y .and. y <= py(i)))  &
      .and. ((px(i) <= x .and. x <= px(j)) .or. (px(j) <= x .and. x <= px(i))) &
      .and. (0 == distance .or. dx == 0)) then
      on = .true.
    end if
  end if
 j = i
end do
end function InPolygon
end program test_inpolygon
