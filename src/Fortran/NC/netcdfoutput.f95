!C---------------------
!C     PROGRAM NCOUTPUT
!C---------------------
!C     December 2006: C. Troupin
!C     Update: M.Ouberdous, October 2007
!
!C     provides an output in NetCDF format for the analysed and error
!C     fields
!
!C     input =
!C     -----
!
!C     fort84: analysis output file in GHER format
!C     fort87: error output file in GHER format
!C     GridInfo.dat =  x/y orig, dx/y, x/y end
!
!C     output =
!C     ------
!
!C     result.nc: analysis + error file in NetCDF format
!C
!
!C ----------------------------------------------------------------------

      program ncoutput
      implicit none
!C      include 'netcdf.inc'
      integer nmax,iw
      parameter(nmax=10000,iw=100000000)
      REAL*4 U(iw)
      REAL*4 V(iw)
      REAL*4 X(nmax)
      REAL*4 Y(nmax)

      integer NX, NY, KMAX, ipr, nw, IMAX, JMAX
      real VALEXU
      real*8 W8
      real xorig, yorig, dx, dy, xend, yend

!C     Reads the grid data from GridInfo.dat
!C----------------------------------------------
      open(unit=90,file='GridInfo.dat')
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend

      NX=int((xend-xorig)/dx)
      NY=int((yend-yorig)/dy)

      close(90)

!C     Subroutine to write the NetCDF files
!C-----------------------------------------

      open (unit=84,form='unformatted')
      CALL UREADC(84,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
      
      open (unit=87,form='unformatted')
      CALL UREADC(87,W8,V,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
      
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
      call netcdfout(U,V,IMAX,JMAX,VALEXU,xorig,yorig,dx,dy,X,Y)


      stop
      end


!C ----------------------------------------------------------------------
      subroutine netcdfout(U1,U2,IMAX,JMAX,VALEXU,xorig,yorig,dx,dy,X,Y)

      include 'netcdf.inc'

      character*(*) FILE_NAME ! name of the output file
      parameter (FILE_NAME='results.nc')
!C-------------------------
      real*4 U1(IMAX,JMAX)    ! analysed field
      real*4 U2(IMAX,JMAX)    ! error field
      real*4 X(IMAX)          ! X-ccordinate
      real*4 Y(JMAX)          ! Y-coordinate

      integer NDIMS           ! variable dimensions
      parameter (NDIMS=2)

      integer ncid, varid1, varid2   ! file and variable IDs
      integer dimids(NDIMS)   ! dimension IDs
      integer lon_dimid, lat_dimid, lat_varid, lon_varid
      integer retval          ! error handling

      integer i, j, i_1, j_1, i_2, j_2          ! loop indice
      integer NX, NY

      integer START(NDIMS), COUNT(NDIMS)
!C     START = vector of integers specifying the index in the variable where the first of the
!C     data values will be written.
!C     COUNT = A vector of integers specifying the edge lengths along each dimension of the
!C     block of data values to written.

      data START /1, 1/     ! start at first value

      character*(*) LAT_NAME, LON_NAME
      parameter (LAT_NAME='y', LON_NAME='x')
      character*(*) UNITS
      parameter (UNITS = 'units')
      character*(*) LAT_UNITS, LON_UNITS
      parameter (LAT_UNITS = 'Degrees_north')
      parameter (LON_UNITS = 'Degrees_east')

      character*(*) VALID_MIN
      parameter (VALID_MIN = 'valid_min')
      character*(*) VALID_MAX
      parameter (VALID_MAX = 'valid_max')
      real field_min, field_max
!c      parameter(field_min=-10.0, field_max=50.0)
      real error_min, error_max
!c      parameter(error_min=0.0, error_max=1.0)

      NX = IMAX
      NY = JMAX

      COUNT(1) = NX       ! number of columns
      COUNT(2) = NY       ! number of lines

!C      print *, ' NX=' ,COUNT(1)
!C      print *, ' NY=' ,COUNT(2)
!C      print *, ' start(1)=' ,START(1)
!C      print *, ' start(2)=' ,START(2)

!C     Create the coordinates values
!C----------------------------------

      do i = 1, IMAX
         X(i) = xorig + (i - 1) * dx
      end do
      do j = 1, JMAX
         Y(j) = yorig + (j - 1) * dy
      end do

!C     Look for 1st value not equal to VALEX
!C------------------------------------------
      i_1=1
      j_1=1
      
      do i = 1, IMAX
      do j = 1, JMAX
      if(U1(i,j) .ne. VALEXU) then
      i_1 = i
      j_1 = j
      goto 9988
      endif
      enddo
      enddo
      
      i_2=1
      j_2=1
      do i = 1, IMAX
      do j = 1, JMAX
      if(U2(i,j) .ne. VALEXU) then
      i_2 = i
      j_2 = j
      goto 9988
      endif
      enddo
      enddo
      
 9988 continue

!C     Looking for min and max field values
!C-----------------------------------------
      field_min = U1(i_1,j_1)
      field_max = U1(i_1,j_1)
      do i = 1, IMAX
      do j = 1, JMAX
      if(U1(i,j) .ne. VALEXU) then
      if(U1(i,j) .le. field_min) field_min = U1(i,j)
      if(U1(i,j) .ge. field_max) field_max = U1(i,j)
      endif
      end do
      end do
!C      print *, ' field_min= ',field_min
!C      print *, ' field_max= ',field_max

!C     Looking for min and max error values
!C-----------------------------------------
      error_min = U2(i_1,j_1)
      error_max = U2(i_1,j_1)
      do i = 1, IMAX
      do j = 1, JMAX
      if(U2(i,j) .ne. VALEXU) then
      if(U2(i,j) .le. error_min) error_min = U2(i,j)
      if(U2(i,j) .ge. error_max) error_max = U2(i,j)
      endif
      end do
      end do
      print *, ' error_min= ',error_min
      print *, ' error_max= ',error_max
!c
!C-----------------------------------------------------------------------
!C     Always check the return code of every netCDF function call. In
!C     this example program, any retval which is not equal to nf_noerr
!C     (0) will call handle_err, which prints a netCDF error message, and
!C     then exits with a non-zero return code.
!C-----------------------------------------------------------------------

!C     Create the netCDF file.
!C----------------------------

!C     The nf_clobber parameter tells netCDF to
!C     overwrite this file, if it already exists.

      retval = nf_create(FILE_NAME, NF_CLOBBER, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     Define the dimensions.
!C---------------------------

!C     NetCDF will hand back an ID for each.

      retval = nf_def_dim(ncid, LON_NAME, NX, lon_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, LAT_NAME, NY, lat_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)


!C     Define the coordinate variables.
!C-------------------------------------

!C     They will hold the coordinate  iformation, that is,
!C     the latitudes and longitudes. A varid is returned for each.

      retval = nf_def_var(ncid, LAT_NAME, NF_FLOAT, 1, lat_dimid,lat_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_var(ncid, LON_NAME, NF_FLOAT, 1, lon_dimid,lon_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     The dimids array is used to pass the IDs of the dimensions of
!C     the variables. Note that in fortran arrays are stored in
!C     column-major format.

      dimids(1) = lon_dimid
      dimids(2) = lat_dimid

!C      print *,'latdimid', lat_dimid
!C      print *,'Londimid', lon_dimid

!C     Define the variable.
!C-------------------------

!C     The type of the variable; in this case = float

      retval = nf_def_var(ncid, "analyzed_field",NF_FLOAT, NDIMS, dimids, varid1)
      if (retval .ne. nf_noerr) call handle_err(retval)
      
      retval = nf_def_var(ncid, "error_field",NF_FLOAT, NDIMS, dimids, varid2)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     Assign units attributes to coordinate var data.
!C----------------------------------------------------

!C     This attaches a text attribute to each of the coordinate
!C     variables, containing the units.

      retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS),LAT_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS),LON_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     This attaches a minimum and maximum values attribute to the
!C     variable and the error

      retval = nf_put_att_real(ncid, varid1, VALID_MIN, NF_REAL, 1,field_min)
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_put_att_real(ncid, varid1, VALID_MAX, NF_REAL, 1,field_max)
      if (retval .ne. nf_noerr) call handle_err(retval)
      
      
      retval = nf_put_att_real(ncid, varid2, VALID_MIN, NF_REAL, 1,error_min)
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_put_att_real(ncid, varid2, VALID_MAX, NF_REAL, 1,error_max)
      if (retval .ne. nf_noerr) call handle_err(retval)


!C     End define mode.
!C---------------------

!C     This tells netCDF we are done defining metadata.

      retval = nf_enddef(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     Write the netCDF file
!C--------------------------

!C     Write the coordinate variable data. This will put the latitudes
!C     and longitudes of our data grid into the netCDF file.

      retval = nf_put_var_real(ncid, lat_varid, Y)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_var_real(ncid, lon_varid, X)
      if (retval .ne. nf_noerr) call handle_err(retval)

!C     Writes the data into the netCDF file

      retval = nf_put_vara_real(ncid, varid1, START, COUNT, U1)
      if (retval .ne. nf_noerr) call handle_err(retval)
      
!C     Writes the data into the netCDF file

      retval = nf_put_vara_real(ncid, varid2, START, COUNT, U2)
      if (retval .ne. nf_noerr) call handle_err(retval)


!C     Close the file.
!C--------------------

!C     This frees up any internal netCDF resources
!C     associated with the file, and flushes any buffers.

      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

      print *,'*** SUCCESS writing NetCDF file  ', FILE_NAME
      end

!C-----------------------------------------------------------------------
      subroutine handle_err(errcode)
!C     This subroutine handles errors by printing an error message and
!C     exiting with a non-zero status.

      implicit none
      include 'netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end

!C-----------------------------------------------------------------------

      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
!C                ======
!C-----------------------------------------------------------------------
!C Reads the field C(I,J,K) from fortran unit iu
!C returns the field in the array c4 if the returned iprecr=4
!C returns the field in the array c8 if the returned iprecr=8
!C returns the values if imaxr,jmaxr,kmaxr found in the file
!C
!C JMB 6/3/91
!C-----------------------------------------------------------------------
!C
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      integer*4 imaxc,jmaxc,kmaxc,iprec,nbmots

!C in the calling routin you can specify the following equivalence to
!C save memory space:
!C      equivalence(c,c4)

!C      equivalence(c,c8)
!C
!C skip KBLANC lines
       do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1     continue
!C
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!C
!C pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc

!C      print *, 'iprecr=', iprec
!C      print *, 'imaxr=', imaxc
!C      print *, 'jmaxr=', jmaxc
!C      print *, 'kmaxr=', kmaxc
!C      print *, 'nbmotr=', nbmots
!C      print *, 'valexr=', valexc


!C
!C compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!C

!C if pathological case, read only four values C0 and DCI,DCJ,DCK
!C and return
!C them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!C
!C
!C single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
!C
!C double precision
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
!C
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

         return
         end
