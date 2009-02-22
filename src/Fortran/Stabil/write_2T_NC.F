
      SUBROUTINE NC_OUTLEV1(klev,iter,rec,TP,SA,N2,SIG
     &                       ,clo,cla,im4,jm4,valexu)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
       CHARACTER (len=16)                     :: ANADAT

        integer*4                               :: im4,jm4
        integer                               :: im,jm
        real*4  ,dimension(im4,jm4)             :: dmh,dmmh
     &                                         , TP,SA,SIG,N2
        real*4  ,dimension(im4)                :: clo
        real*4  ,dimension(jm4)                :: cla
        real*4                                :: fmis, valexu, hrss
!
        CHARACTER (len=11) , dimension(4)     :: var1, vard1, u1
!
	integer  ,dimension(3)                :: dim
	integer  ,dimension(4)                :: id1
	integer  ,dimension(3)                :: start, count
!
	integer                         :: lonid,latid,levid,timeid
	integer                         :: idlon,idlat,idlev,idtime
        integer                         :: ncid,status
        integer                         :: icdf, klev, num
        integer                         :: iter, rec
!
	save                            :: icdf
	save                            :: ncid
	save                            :: id1
	save                            :: lonid,latid,levid,timeid
	save                            :: idlon,idlat,idlev,idtime
      LOGICAL                           :: exist
!
!----------------------------------------------------------
!
	data fmis /-1.e22/

	data var1 /'TP','SA','N2','NVF'/

	data vard1 /'Temperature','Salinity','B.V.-freq','LOCATIONS'/

	data u1 /'c','parts/thnd',' ',' '/
!
!---------------------------------------------------------------
!
      IM = IM4
      JM = JM4
!
      ANADAT='stblev1.1xxxx.nc'            !  file name
!
      WRITE(ANADAT(10:13),'(I4.4)') klev
      WRITE(ANADAT(4:5),'(I2.2)') klev
      WRITE(ANADAT(6:7),'(I2.2)') klev+1
      WRITE(ANADAT(8:9),'(I2.2)') rec

      write(*,*)'===wrt_2T====== ',iter,rec,ANADAT
!
        INQUIRE(FILE=ANADAT,EXIST=exist)
      if(.NOT. EXIST) then
!!      if (iter == 1 .and. rec == 1) then
          icdf = 0
	!-----------------------
	! create the data file
	!-----------------------

	status = nf_create(ANADAT, nf_share,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Put in define mode
	!----------------------
 	
	status = nf_redef(ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Define dimensions
	!----------------------

	status=nf_def_dim(ncid,'lon',IM,lonid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'lat',JM,latid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'time',nf_unlimited,timeid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------------
        ! Define coordinate variables
	!----------------------------
 
	status=nf_def_var(ncid,'lon',nf_float,1, lonid,idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'lat',nf_float,1,latid ,idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'time',nf_float,1,timeid ,idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------------------------     
        ! Give attributes to coordinate variables 
	!-----------------------------------------

	status=nf_put_att_text(ncid,idlon,'units',12,'degrees_east')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idlat,'units',13,'degrees_north')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idtime,'units',5,'hours')
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Define data variables
	!-----------------------

	dim(1)=lonid
	dim(2)=latid
	dim(3)=timeid

	do num=1,4

	status=nf_def_var(ncid,var1(num),nf_float,3,dim,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value'
     &  ,nf_float,1,fmis)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo
 
	!----------------------
	!Put global attributes
	!----------------------

	status=nf_put_att_text(ncid,NF_GLOBAL,'title',35
     &,		' DIVA: Hydrostatic constrain study ')

	!--------------------
	!End define mode
	!--------------------
      
	status = nf_enddef(ncid)
       if (status .ne. nf_noerr) call handle_err(status)


       status = nf_put_var_real(ncid,idlon,clo)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlat,cla)
       if (status .ne. nf_noerr) call handle_err(status)

      else

!x       if(iter == 1 .and. rec == 2) then
	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)


	status=nf_inq_dimid(ncid,'lon',idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lat',idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'time',idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	do num=1,4

          status=nf_inq_varid(ncid,var1(num),id1(num))
          if (status .ne. nf_noerr) call handle_err(status)

	enddo

	status=nf_inq_dimlen(ncid,idtime,icdf)
	if (status .ne. nf_noerr) call handle_err(status)

!x      endif
!
      endif


        status = nf_sync(ncid)

	icdf=icdf+1

	hrss = (icdf-1)*1. 
	status = nf_put_var1_real(ncid, idtime, icdf, hrss)


	start(1)=1
	start(2)=1
	start(3)=icdf
	count(1)=IM
	count(2)=JM
	count(3)=1

        call trfh(dmh,TP,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(1), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

        call trfh(dmh,SA,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(2), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

        call trfh(dmh,N2,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(3), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        call trfh(dmh,SIG,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(4), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
	if (status .ne. nf_noerr) call handle_err(status)
 
    	RETURN
        END
!
      SUBROUTINE NC_OUTLEV2(klev,iter,rec,TP,SA,N2,SIG
     &                       ,clo,cla,im4,jm4,valexu)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
       CHARACTER (len=16)                     :: ANADAT

        integer*4                               :: im4,jm4
        integer                               :: im,jm
        real*4  ,dimension(im4,jm4)             :: dmh,dmmh
     &                                         , TP,SA,SIG,N2
        real*4  ,dimension(im4)                :: clo
        real*4  ,dimension(jm4)                :: cla
        real*4                                :: fmis, valexu, hrss
!
        CHARACTER (len=11) , dimension(4)     :: var1, vard1, u1
!
	integer  ,dimension(3)                :: dim
	integer  ,dimension(4)                :: id1
	integer  ,dimension(3)                :: start, count
!
	integer                         :: lonid,latid,levid,timeid
	integer                         :: idlon,idlat,idlev,idtime
        integer                         :: ncid,status
        integer                         :: icdf, klev, num
        integer                         :: iter, rec
!
	save                            :: icdf
	save                            :: ncid
	save                            :: id1
	save                            :: lonid,latid,levid,timeid
	save                            :: idlon,idlat,idlev,idtime
      LOGICAL                           :: exist
!
!----------------------------------------------------------
!
	data fmis /-1.e22/

	data var1 /'TP','SA','N2','NVF'/

	data vard1 /'Temperature','Salinity','B.V.-freq','LOCATIONS'/

	data u1 /'c','parts/thnd',' ',' '/
!
!---------------------------------------------------------------
!
      IM = IM4
      JM = JM4
!
      ANADAT='stblev2.1xxxx.nc'            !  file name
!
      WRITE(ANADAT(10:13),'(I4.4)') klev
      WRITE(ANADAT(4:5),'(I2.2)') klev-1
      WRITE(ANADAT(6:7),'(I2.2)') klev
      WRITE(ANADAT(8:9),'(I2.2)') rec
!
      write(*,*)'===wrt_2T====== ',iter,rec,ANADAT

        INQUIRE(FILE=ANADAT,EXIST=exist)
      if(.NOT. EXIST) then
!!      if (iter == 1 .and. rec == 1) then
          icdf = 0
	!-----------------------
	! create the data file
	!-----------------------

	status = nf_create(ANADAT, nf_share,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Put in define mode
	!----------------------
 	
	status = nf_redef(ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------
        ! Define dimensions
	!----------------------

	status=nf_def_dim(ncid,'lon',IM,lonid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'lat',JM,latid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_dim(ncid,'time',nf_unlimited,timeid)
	if (status .ne. nf_noerr) call handle_err(status)

	!----------------------------
        ! Define coordinate variables
	!----------------------------
 
	status=nf_def_var(ncid,'lon',nf_float,1, lonid,idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'lat',nf_float,1,latid ,idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_def_var(ncid,'time',nf_float,1,timeid ,idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------------------------     
        ! Give attributes to coordinate variables 
	!-----------------------------------------

	status=nf_put_att_text(ncid,idlon,'units',12,'degrees_east')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idlat,'units',13,'degrees_north')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idtime,'units',5,'hours')
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Define data variables
	!-----------------------

	dim(1)=lonid
	dim(2)=latid
	dim(3)=timeid

	do num=1,4

	status=nf_def_var(ncid,var1(num),nf_float,3,dim,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value'
     &  ,nf_float,1,fmis)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo
 
	!----------------------
	!Put global attributes
	!----------------------

	status=nf_put_att_text(ncid,NF_GLOBAL,'title',35
     &,		' DIVA: Hydrostatic constrain study ')

	!--------------------
	!End define mode
	!--------------------
      
	status = nf_enddef(ncid)
       if (status .ne. nf_noerr) call handle_err(status)


       status = nf_put_var_real(ncid,idlon,clo)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlat,cla)
       if (status .ne. nf_noerr) call handle_err(status)

      else

!x       if(iter == 1 .and. rec == 2) then
	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)


	status=nf_inq_dimid(ncid,'lon',idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lat',idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'time',idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	do num=1,4

          status=nf_inq_varid(ncid,var1(num),id1(num))
          if (status .ne. nf_noerr) call handle_err(status)

	enddo

!x      endif

	status=nf_inq_dimlen(ncid,idtime,icdf)
	if (status .ne. nf_noerr) call handle_err(status)
!
      endif


        status = nf_sync(ncid)

	icdf=icdf+1

	hrss = (icdf-1)*1. 
	status = nf_put_var1_real(ncid, idtime, icdf, hrss)


	start(1)=1
	start(2)=1
	start(3)=icdf
	count(1)=IM
	count(2)=JM
	count(3)=1

        call trfh(dmh,TP,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(1), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

        call trfh(dmh,SA,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(2), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

        call trfh(dmh,N2,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(3), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        call trfh(dmh,SIG,im,jm,valexu,fmis)
	status=nf_put_vara_real(ncid,id1(4), start, count,dmh)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
	if (status .ne. nf_noerr) call handle_err(status)
 
    	RETURN
        END
	
!-----------------------------------------------------------------------
! Define subroutine
!-----------------------------------------------------------------------
	
	SUBROUTINE handle_err(status)
	include "netcdf.inc" 
	INTEGER STATUS
	IF (status .NE.nf_noerr) THEN
	   print *,nf_strerror(status)
	   STOP 'Stopped'
	ENDIF
	END
	
	!-----------------------------------------

        subroutine trfh(dmh,fld,im,jm,valexu,fmis)

        real*4 ,dimension(im,jm)         :: dmh,fld
        real*4 fmis
!
        dmh = fmis
        do j=1,jm
          do i=1,im
             if(fld(i,j) /= valexu) dmh(i,j) = fld(i,j)
          enddo
        enddo

         return
	end

        subroutine trnfh(dmmh,im,jm,km,fld,valexu,fmis)

        real*4, dimension(im,jm,km)     :: dmmh,fld
        real*4                          :: fmis, valexu
!
        dmmh = fmis

        do j=1,jm
          do i=1,im
            do k=1,KM
               if(fld(i,j,k) /= valexu) dmmh(i,j,k) = fld(i,j,k)
            enddo
          enddo
        enddo

        return
	end

	!------------------------------------------------
!---------------------------------------------------------------------
