!
      SUBROUTINE NC_OUTDAT2D(klev,iter,rec,TP,SA,N2,SIG,clo,cla,dep,im4,jm4,valexu)
!
       IMPLICIT NONE
!
!-------------------------------------------------------------------
!
	include "netcdf.inc" 
!
       CHARACTER (len=19)                     :: ANADAT

        integer*4                             :: im4,jm4
        integer                               :: im,jm
        real*4  ,dimension(im4,jm4,2)           :: TP,SA
        real*4  ,dimension(im4,jm4)             :: SIG,N2
        real*4  ,dimension(im4)                :: clo
        real*4  ,dimension(jm4)                :: cla
        real*4  ,dimension(2)                 :: dep
        real*4                                :: fmis, valexu, hrss
!
        CHARACTER (len=11) , dimension(4)     :: var1, vard1, u1
!
	integer  ,dimension(3)                :: dim1
	integer  ,dimension(4)                :: id1,dim
	integer  ,dimension(3)                :: start1, count1
	integer  ,dimension(4)                :: start, count
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
      ANADAT='nsquare_T1.1xxxx.nc'            !  file name
!
      WRITE(ANADAT(13:16),'(I4.4)') klev
!
      if (iter == 1 .and. rec == 1) then
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

	status=nf_def_dim(ncid,'lev',2,levid)
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

	status=nf_def_var(ncid,'lev',nf_float,1,levid ,idlev)
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

	status=nf_put_att_text(ncid,idlev,'units',13,'level')
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_att_text(ncid,idtime,'units',5,'hours')
	if (status .ne. nf_noerr) call handle_err(status)

	!-----------------------
        ! Define data variables
	!-----------------------

	dim(1)=lonid
	dim(2)=latid
	dim(3)=levid
	dim(4)=timeid

	do num=1,2

	status=nf_def_var(ncid,var1(num),nf_float,4,dim,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value',nf_float,1,valexu)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo


	dim1(1)=lonid
	dim1(2)=latid
	dim1(3)=timeid

	do num=3,4

	status=nf_def_var(ncid,var1(num),nf_float,3,dim1,id1(num))
	if (status .ne. nf_noerr) call handle_err(status)

!       print*,'create the file 2',num


	status=nf_put_att_text(ncid,id1(num),'long_name',11,vard1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 3',num

	status=nf_put_att_text(ncid,id1(num),'units',11,u1(num))
	if (status .ne. nf_noerr) call handle_err(status)


!       print*,'create the file 4',num

	status=nf_put_att_real(ncid,id1(num),'missing_value',nf_float,1,valexu)
	if (status .ne. nf_noerr) call handle_err(status)

	enddo
 
	!----------------------
	!Put global attributes
	!----------------------

	status=nf_put_att_text(ncid,NF_GLOBAL,'title',35,		' DIVA: Hydrostatic stabilisation ')

	!--------------------
	!End define mode
	!--------------------
      
	status = nf_enddef(ncid)
       if (status .ne. nf_noerr) call handle_err(status)


       status = nf_put_var_real(ncid,idlon,clo)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlat,cla)
       if (status .ne. nf_noerr) call handle_err(status)

       status = nf_put_var_real(ncid,idlev,dep)
       if (status .ne. nf_noerr) call handle_err(status)

      else

       if(rec == 1) then
	!-----------------------
        ! Open the data file 	
	!-----------------------

	status = nf_open(ANADAT, nf_write,ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lon',idlon)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lat',idlat)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'lev',idlev)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_inq_dimid(ncid,'time',idtime)
	if (status .ne. nf_noerr) call handle_err(status)

	do num=1,4

          status=nf_inq_varid(ncid,var1(num),id1(num))
          if (status .ne. nf_noerr) call handle_err(status)

	enddo

      endif

	status=nf_inq_dimlen(ncid,idtime,icdf)
	if (status .ne. nf_noerr) call handle_err(status)
!
      endif


        status = nf_sync(ncid)

	icdf=icdf+1

	hrss = (icdf-1)*1. 
	status = nf_put_var1_real(ncid, idtime, icdf, hrss)

	start1(1)=1
	start1(2)=1
	start1(3)=icdf
	count1(1)=IM
	count1(2)=JM
	count1(3)=1

	status=nf_put_vara_real(ncid,id1(3), start1, count1,N2)
	if (status .ne. nf_noerr) call handle_err(status)

	status=nf_put_vara_real(ncid,id1(4), start1, count1,SIG)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
	if (status .ne. nf_noerr) call handle_err(status)

	start(1)=1
	start(2)=1
	start(3)=1
	start(4)=icdf
	count(1)=IM
	count(2)=JM
	count(3)=2
	count(4)=1

	status=nf_put_vara_real(ncid,id1(1), start, count,TP)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)

	status=nf_put_vara_real(ncid,id1(2), start, count,SA)
	if (status .ne. nf_noerr) call handle_err(status)

        status = nf_sync(ncid)
 
    	RETURN
        END
	
!----------------------------------------------------------------------
! Define subroutine
!----------------------------------------------------------------------
!x	SUBROUTINE handle_err(status)
!x	include "netcdf.inc" 
!x	INTEGER STATUS
!x	IF (status .NE.nf_noerr) THEN
!x	   print *,nf_strerror(status)
!x	   STOP 'Stopped'
!x	ENDIF
!x	END
!---------------------------------------------------------------------
