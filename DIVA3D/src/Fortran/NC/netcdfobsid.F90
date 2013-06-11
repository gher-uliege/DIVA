! netcdfobsid adds coordinates of observations (longitude, latitude, depth and time) 
! and observation identifier to a NetCDF file
!
! Call as
! netcdfobsid <obsid.txt> <file.nc> <time unit>
!
! obsid.txt: text file with 5 columns (longitude, latitude, depth and time and id separated by space)
! file.nc: netcdf file
! time unit: unit of the time column in obsid.txt
!
! For example:
! 
!
!
! Compile with something like:
!
! gfortran $(nc-config --fflags) -o netcdfobsid netcdfobsid.f90  $(nc-config --flibs)
!
! Execute:
!
! ./netcdfobsid
!

#define ERROR_STOP stop

module utils
 
 integer, parameter :: maxlen = 256
 integer :: timeOrigin(6) = (/1900,1,1,0,0,0/)

 contains



! parses data of the form yyyy-mm-hhTHH:MM:SS.SSS
! for example
!   1988-02-15T00:00:00.000
  


  subroutine parseISOData(str,year,month,day,hour,minute,seconds)
   implicit none
   character(len=*), intent(in) :: str
   integer, intent(out) :: year,month,day,hour,minute
   real, intent(out) :: seconds
   
   integer :: i,j

   ! year
   i = index(str,'-')
   read(str(1:i-1),*) year
   
   ! month
   j = indexof(str,'-',i+1)
   read(str(i+1:j-1),*) month

   ! day
   i = indexof(str,'T',j+1)
   read(str(j+1:i-1),*) day

   ! hour
   j = indexof(str,':',i+1)
   read(str(i+1:j-1),*) hour

   ! minute
   i = indexof(str,':',j+1)
   read(str(j+1:i-1),*) minute

   ! second
   read(str(i+1:),*) seconds

   write(6,*) 'date ',year,month,day,hour,minute,seconds
   contains


  end subroutine parseISOData

  function indexof(str,substr,start) result(ind)
   character(len=*), intent(in) :: str, substr
   integer, intent(in) :: start
   integer :: ind
   
   ind = index(str(start:),substr)
   if (ind /= -1) ind = ind+start-1
  end function indexof


  function mjd(y,m,d,h,min,s)
   implicit none
   integer, intent(in) :: d,m,y,h,min
   real, intent(in)  :: s
   real(8) :: mjd

! Mathematicians and programmers have naturally 
! interested themselves in mathematical and computational 
! algorithms to convert between Julian day numbers and 
! Gregorian dates. The following conversion algorithm is due 
! to Henry F. Fliegel and Thomas C. Van Flandern: 
! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

! ModifiedJulianDay = 0 for 1858-11-17 CE.

   mjd = (( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +        &
        ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -   &
        ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +  &
        d - 32075 - 2400001)*1d0 + h/24d0 + min/(24*60d0)+ s/(24*60*60d0)               
  end function mjd


  function numberOfLines(file,unit) result(count)
   implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: unit

    integer :: count, iostat

    count = 0
    open(unit,file=file)

    do 
      read(unit,*,iostat=iostat)
      if (iostat /= 0) exit
      count = count+1
    end do
    close(unit)

   end function numberOfLines

   subroutine loadObsFile(file,unit,coord,ids)
    implicit none
    character(len=*), intent(in) :: file
    integer, intent(in) :: unit
    real(8), pointer :: coord(:,:)
    character(len=*), pointer :: ids(:)

    integer :: count,i,j,ncoord, tmp(4), iostat
    integer :: year,month,day,hour,minute
    real :: seconds
    character(len=maxlen) :: date
    real(8) :: t0

    t0 = mjd(timeOrigin(1),timeOrigin(2),timeOrigin(3), &
         timeOrigin(4),timeOrigin(5),real(timeOrigin(6)))

!    call parseISOData('1988-02-15T10:22:01.023',year,month,day,hour,minute,seconds)
!    call parseISOData('1988-2-15T1:2:01.023',year,month,day,hour,minute,seconds)

    count = numberOfLines(file,unit)
    allocate(ids(count))
    
    open(unit,file=file) 

! !   try reading first line for 4, 3 or 2 coordinates    
!     do ncoord = 4,2,-1
!       rewind(unit)
!       read(unit,*,iostat=iostat) (tmp(i), i=1,ncoord), ids(1)
!       if (iostat == 0) exit
!     end do

!     write(6,*) 'ncoord ',ncoord, iostat,count

!     if (iostat /= 0) then
!       write(0,"(A,A,':',I2,A,A)") 'Error: ',trim(__FILE__),__LINE__,' unable to read coordinates from file ',trim(file)
!       close(unit)
!       ERROR_STOP
!     end if

    ncoord = 4
    allocate(coord(ncoord,count))
    rewind(unit)

    do j=1,count
      read(unit,*) (coord(i,j), i=1,3), date, ids(j)
      call parseISOData(date,year,month,day,hour,minute,seconds)
      coord(4,j) = mjd(year,month,day,hour,minute,seconds) - t0
    end do
    close(unit)

   end subroutine loadObsFile


   subroutine saveNCObsFile(ncfile,coord,ids)
    use netcdf
    implicit none
    
    character(len=*), intent(in) :: ncfile,ids(:)
    real(8) :: coord(:,:)
    integer :: iostat, strlen

    character(len=maxlen) :: timeunit
    integer :: ncid, status, dimids, varidcoord(4), varid, dimidstr
    integer :: i,j


    write(timeunit,'("days since ",I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') timeOrigin(1),timeOrigin(2),timeOrigin(3), &
         timeOrigin(4),timeOrigin(5),timeOrigin(6)

    write(6,*) 'timeunits ',timeunit
    
  ! longest id
  strlen = 0
  do j=1,size(ids)
    strlen = max(strlen,len_trim(ids(j)))
  end do

  write(6,*) 'strlen ',strlen, ids

! call check_error(nf90_open('example.nc',nf90_write,ncid))
! call check_error(nf90_redef(ncid))

 call check_error(nf90_create('example.nc',nf90_clobber,ncid))

 write(6,*) __LINE__

 ! define the dimension longitude and latitude of size
 ! approxiate size

 call check_error(nf90_def_dim(ncid, 'observations', size(ids), dimids))

 call check_error(nf90_def_dim(ncid, 'idlen', strlen, dimidstr))

 write(6,*) __LINE__

 call check_error(nf90_def_var(ncid, 'obsid', nf90_char, (/dimidstr,dimids /), varid))
 write(6,*) __LINE__
 call check_error(nf90_put_att(ncid, varid, 'long_name', 'observation identifier'))

 call check_error(nf90_def_var(ncid, 'obslon', nf90_float, dimids, varidcoord(1)))
 call check_error(nf90_put_att(ncid, varidcoord(1), 'units', 'degrees_east'))

 call check_error(nf90_def_var(ncid, 'obslat', nf90_float, dimids, varidcoord(2)))
 call check_error(nf90_put_att(ncid, varidcoord(2), 'units', 'degrees_north'))

 if (size(coord,1) > 2) then
   call check_error(nf90_def_var(ncid, 'obsdepth', nf90_float, dimids, varidcoord(3)))
   call check_error(nf90_put_att(ncid, varidcoord(3), 'units', 'meters'))
   call check_error(nf90_put_att(ncid, varidcoord(3), 'positive', 'down'))

   if (size(coord,1) > 3) then

     call check_error(nf90_def_var(ncid, 'obstime', nf90_float, dimids, varidcoord(4)))
     call check_error(nf90_put_att(ncid, varidcoord(4), 'units', timeunit))
   end if
 end if

 write(6,*) __LINE__

 ! define a string as attribute of the variable





 ! end definitions: leave define mode

 status = nf90_enddef(ncid)
 call check_error(status)

 ! store the variable temp in the netcdf file

 do i = 1,size(ids)
   call check_error(nf90_put_var(ncid,varid,trim(ids(i)),(/1,i/)))
 end do

 do i = 1,size(coord,1)
   call check_error(nf90_put_var(ncid,varidcoord(i),coord(i,:)))
 end do

 call check_error(nf90_close(ncid))
   end subroutine saveNCObsFile

 subroutine check_error(status)
  use netcdf
  integer, intent ( in) :: status

  if(status /= nf90_noerr) then
    write(6,*) 'NetCDF error: ',trim(nf90_strerror(status))
    ERROR_STOP
    stop "Stopped"
  end if
 end subroutine check_error

end module utils

program netcdfobsid
 use netcdf
 use utils
 implicit none

 character(len=maxlen) :: file,ncfile,timeunit
 integer :: ncid, status, dimids, varidcoord(4), varid, dimidstr
 integer :: i,j
 real :: temp(6,4)
 real(8), pointer :: coord(:,:)
 character(len=maxlen), pointer :: ids(:)
 integer :: iargc,iostat,unit = 10, strlen

  if (iargc().ne.2) then
     write(6,*) 'Usage: netcdfobsid <obsid.txt> <file.nc>'
     ERROR_STOP 
  end if

  call getarg(1,file)
  call getarg(2,ncfile)

  call loadObsFile(file,unit,coord,ids)
  call saveNCObsFile(ncfile,coord,ids)


  deallocate(coord,ids)

end program netcdfobsid
