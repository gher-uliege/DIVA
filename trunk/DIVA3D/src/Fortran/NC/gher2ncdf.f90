Program gher2ncdf
implicit none

!------------------------------------------------
! Sylvain Watelet - 02/12/2014
! swatelelet@ulg.ac.be
!------------------------------------------------

      integer nmax,iw
      parameter(nmax=10000,iw=100000000)
      REAL*4 U(iw)
      REAL*4 X(nmax)
      REAL*4 Y(nmax)

      integer NX, NY, KMAX, ipr, nw, IMAX, JMAX
      real VALEXU
      real*8 W8(1)
      real xorig, yorig, dx, dy, xend, yend, valex
      
      character(len=256)::gherfile,myfield,infofile

call get_command_argument(1,gherfile)
write(*,*) "The gher file to transform is ", gherfile

call get_command_argument(2,infofile)
!write(*,*) "The info file to transform is ", infofile

call get_command_argument(3,myfield)
!write(*,*) "The field to transform is ", myfield

open (unit=10,file=trim(gherfile),form='unformatted')
CALL UREADC(10,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)

      if (KMAX.ne.1) stop "2D Field only, please."

      ipr=4
      nw=imax*jmax
      if(nw.gt.iw) then
      write(6,*) 'Severe error, increase iw netcdfoutput.f'
      stop 'Severe error, increase iw netcdfoutput.f'
      endif
      if(imax.gt.nmax.or.jmax.gt.nmax) then
      write(6,*) 'Severe error, increase iw netcdfoutput.f'
      stop 'Severe error, increase nmax netcdfoutput.f'
      endif


!    Reads the grid data from info file
!----------------------------------------------
      open(unit=90,file=trim(infofile))
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend

      NX=int((xend-xorig)/dx)
      NY=int((yend-yorig)/dy)

      close(90)
!      write(*,*) 'valex = ,', valex 

      call netcdfout(U,IMAX,JMAX,VALEXU,xorig,yorig,dx,dy,X,Y,valex,myfield)

      close(10)

      stop
End program


!    Subroutine to write the NetCDF file
!---------------------------------------
! ----------------------------------------------------------------------
      subroutine netcdfout(U,IMAX,JMAX,VALEXU,xorig,yorig,dx,dy,X,Y,valex,myfield)
    
      include 'netcdf.inc'


      character(256)::myfield
      character(len=len(trim(myfield))+3) FILE_NAME ! name of the output file
!      parameter (FILE_NAME=trim(myfield)//'.nc')
!-------------------------
      real*4 U(IMAX,JMAX)
      real*4 X(IMAX)          ! X-ccordinate
      real*4 Y(JMAX)          ! Y-coordinate
      integer NDIMS          ! variable dimensions
      parameter (NDIMS=2)
      
      integer ncid, varid    ! file and variable IDs
      integer dimids(NDIMS)  ! dimension IDs
      integer lon_dimid, lat_dimid, lat_varid, lon_varid
      integer retval          ! error handling
      
      integer i, j, i_1, j_1        ! loop indice
      integer NX, NY
      
      integer START(NDIMS), COUNT(NDIMS)
!     START = vector of integers specifying the index in the variable where the first of the
!     data values will be written.
!     COUNT = A vector of integers specifying the edge lengths along each dimension of the
!     block of data values to written.

      data START /1, 1/    ! start at first value

      character*(*) LAT_NAME, LON_NAME
      parameter (LAT_NAME='lat', LON_NAME='lon')
      character*(*) UNITS
      parameter (UNITS = 'units')
      character*(*) LAT_UNITS, LON_UNITS
      parameter (LAT_UNITS = 'Degrees_north')
      parameter (LON_UNITS = 'Degrees_east')
      
      character*(*) VALID_MIN
      parameter (VALID_MIN = 'valid_min')
      character*(*) VALID_MAX
      parameter (VALID_MAX = 'valid_max')
      character*(*) MISSING_VALUE
      parameter (MISSING_VALUE = 'missing_value')
      real field_min, field_max, valex



      NX = IMAX
      NY = JMAX
      
      COUNT(1) = NX      ! number of columns
      COUNT(2) = NY      ! number of lines
      
!      print *, ' NX=' ,COUNT(1)
!      print *, ' NY=' ,COUNT(2)
!      print *, ' start(1)=' ,START(1)
!      print *, ' start(2)=' ,START(2)
      
!     Create the coordinates values
!----------------------------------

      do i = 1, IMAX
         X(i) = xorig + (i - 1) * dx
      end do
      do j = 1, JMAX
         Y(j) = yorig + (j - 1) * dy
      end do

!     Look for 1st value not equal to VALEX
!------------------------------------------
      i_1=1
      j_1=1

      do i = 1, IMAX
      do j = 1, JMAX
      if(U(i,j) .ne. VALEXU) then
      i_1 = i
      j_1 = j
      goto 9988
      endif
      enddo
      enddo

 9988 continue

!     Looking for min and max field values
!-----------------------------------------
      field_min = U(i_1,j_1)
      field_max = U(i_1,j_1)
      do i = 1, IMAX
      do j = 1, JMAX
      if(U(i,j) .ne. VALEXU) then
      if(U(i,j) .le. field_min) field_min = U(i,j)
      if(U(i,j) .ge. field_max) field_max = U(i,j)
      endif
      end do
      end do
!      print *, ' field_min= ',field_min
!      print *, ' field_max= ',field_max

!-----------------------------------------------------------------------
!     Always check the return code of every netCDF function call. In
!     this example program, any retval which is not equal to nf_noerr
!     (0) will call handle_err, which prints a netCDF error message, and
!     then exits with a non-zero return code.
!-----------------------------------------------------------------------

!     Create the netCDF file.
!----------------------------

      FILE_NAME=trim(myfield)//'.nc'

!     The nf_clobber parameter tells netCDF to
!     overwrite this file, if it already exists.

      retval = nf_create(FILE_NAME, NF_CLOBBER, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!     Define the dimensions.
!---------------------------

!     NetCDF will hand back an ID for each.

      retval = nf_def_dim(ncid, LON_NAME, NX, lon_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, LAT_NAME, NY, lat_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!     Define the coordinate variables.
!-------------------------------------

!     They will hold the coordinate  iformation, that is,
!     the latitudes and longitudes. A varid is returned for each.

      retval = nf_def_var(ncid, LAT_NAME, NF_FLOAT, 1, lat_dimid,lat_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_var(ncid, LON_NAME, NF_FLOAT, 1, lon_dimid,lon_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!     The dimids array is used to pass the IDs of the dimensions of
!     the variables. Note that in fortran arrays are stored in
!     column-major format.

      dimids(1) = lon_dimid
      dimids(2) = lat_dimid

!      print *,'latdimid', lat_dimid
!      print *,'Londimid', lon_dimid

!    Define the variable.
!-------------------------

!    The type of the variable; in this case = float

      retval = nf_def_var(ncid, trim(myfield), NF_FLOAT, NDIMS, dimids, varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      
!     Assign units attributes to coordinate var data.
!----------------------------------------------------

!     This attaches a text attribute to each of the coordinate
!     variables, containing the units.

      retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS),LAT_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS),LON_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)

!     This attaches a minimum and maximum values attribute to the
!     variable

      retval = nf_put_att_real(ncid, varid, VALID_MIN, NF_REAL, 1,field_min)
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_put_att_real(ncid, varid, VALID_MAX, NF_REAL, 1,field_max)
      if (retval .ne. nf_noerr) call handle_err(retval)

!     Define the missing_value read in (GridInfo.dat => not here) gher file
      retval = nf_put_att_real(ncid, varid, MISSING_VALUE, NF_REAL, 1,valexu)
      if (retval .ne. nf_noerr) call handle_err(retval)      

!     End define mode.
!---------------------

!    This tells netCDF we are done defining metadata.

      retval = nf_enddef(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!    Write the netCDF file
!--------------------------

!    Write the coordinate variable data. This will put the latitudes
!    and longitudes of our data grid into the netCDF file.

      retval = nf_put_var_real(ncid, lat_varid, Y)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_var_real(ncid, lon_varid, X)
      if (retval .ne. nf_noerr) call handle_err(retval)

!    Writes the data into the netCDF file

      retval = nf_put_vara_real(ncid, varid, START, COUNT, U)
      if (retval .ne. nf_noerr) call handle_err(retval)

!    Close the file.
!--------------------

!    This frees up any internal netCDF resources
!    associated with the file, and flushes any buffers.

      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

      print *,'*** SUCCESS writing NetCDF file  ', './input/', trim(myfield),'.nc'
      end

!-----------------------------------------------------------------------
      subroutine handle_err(errcode)
!    This subroutine handles errors by printing an error message and
!    exiting with a non-zero status.

      implicit none
      include 'netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end

!-----------------------------------------------------------------------

      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
!                ======
!-----------------------------------------------------------------------
! Reads the field C(I,J,K) from fortran unit iu
! returns the field in the array c4 if the returned iprecr=4
! returns the field in the array c8 if the returned iprecr=8
! returns the values if imaxr,jmaxr,kmaxr found in the file
!
! JMB 6/3/91
!-----------------------------------------------------------------------
!
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      integer*4 imaxc,jmaxc,kmaxc,iprec,nbmots
      
! in the calling routin you can specify the following equivalence to
! save memory space:
!      equivalence(c,c4)

!      equivalence(c,c8)
!
! skip KBLANC lines
      do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1    continue
!
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!
! pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc

!      print *, 'iprecr=', iprec
!      print *, 'imaxr=', imaxc
!      print *, 'jmaxr=', jmaxc
!      print *, 'kmaxr=', kmaxc
!      print *, 'nbmotr=', nbmots
!      print *, 'valexr=', valexc


!
! compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!

! if pathological case, read only four values C0 and DCI,DCJ,DCK
! and return
! them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
        nl=0
        ir=4
        endif
!
!
! single precision
        if(iprec.eq.4) then
        do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                      else
!
! double precision
        if(iprec.eq.8) then
        do 20 kl=1,nl
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,ir)
                      else
        goto 99
        endif
        endif
!
        return
 99      continue
        write(*,*) 'Data error in UREADC, not a conform file'
        return
100      continue
        write(*,*) 'Data error in UREADC, EOF reached'
        write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

        return
        end

