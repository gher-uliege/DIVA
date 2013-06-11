! netcdfobsid adds coordinates of observations (longitude, latitude, depth and time) 
! and observation identifier to a NetCDF file
!
! Call as
! netcdfobsid <obsid.txt> <file.nc>
!
! obsid.txt: text file with 5 columns: longitude (degrees north), latitude (degrees east), 
! depth (meters, positif in water) and time (yyyy-mm-ddTHH:MM:SS and id separated by space)
!   
! file.nc: netcdf file where the information is appended (file must exist)
!
! Compile with something like:
!
! gfortran $(nc-config --fflags) -o netcdfobsid netcdfobsid.f90  $(nc-config --flibs)
!

#define ERROR_STOP stop
#define check(status) call check_error(status,__FILE__,__LINE__)

module divaio

 integer, parameter :: maxlen = 256
 ! time origin (/year,month,day,hour,minute,seconds/)
 integer :: timeOrigin(6) = (/1900,1,1,0,0,0/)

contains

 ! parses data of the form yyyy-mm-hhTHH:MM:SS.SSS
 ! for example
 !   1988-02-15T00:00:00.000
 !   1988-02-15T00:00:10
 function parseISODate(str,year,month,day,hour,minute,seconds) result(status)
  implicit none
  character(len=*), intent(in) :: str
  integer, intent(out) :: year,month,day,hour,minute
  real, intent(out) :: seconds

  integer :: i,j,iostat, status

  status = -1
  ! year
  i = index(str,'-')
  read(str(1:i-1),*,iostat=iostat) year
  if (iostat /= 0) return

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

  !write(6,*) 'date ',year,month,day,hour,minute,seconds

  status = 0
 end function parseISODate


 ! index of sub-string in string str starting at index start
 ! retuns -1 is sub-string is not found
 function indexof(str,substr,start) result(ind)
  character(len=*), intent(in) :: str, substr
  integer, intent(in) :: start
  integer :: ind

  ind = index(str(start:),substr)
  if (ind /= -1) ind = ind+start-1
 end function indexof

 ! modified julian day number

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


 ! returns the number of lines in a file
 ! the file is open using the provided unit

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

 ! load a observation file in ASCII format
 subroutine loadObsFile(file,unit,coord,ids)
  implicit none
  character(len=*), intent(in) :: file
  integer, intent(in) :: unit
  real(8), pointer :: coord(:,:)
  character(len=*), pointer :: ids(:)

  integer :: count,i,j,ncoord, iostat
  integer :: year,month,day,hour,minute
  real :: seconds
  character(len=maxlen) :: date
  real(8) :: t0

  t0 = mjd(timeOrigin(1),timeOrigin(2),timeOrigin(3), &
       timeOrigin(4),timeOrigin(5),real(timeOrigin(6)))

  !    call parseISODate('1988-2-15T1:2:01.023',year,month,day,hour,minute,seconds)

  count = numberOfLines(file,unit)
  allocate(ids(count))

  open(unit,file=file) 

  ncoord = 4
  allocate(coord(ncoord,count))
  rewind(unit)

  do j=1,count
    read(unit,*,iostat=iostat) (coord(i,j), i=1,3), date, ids(j)
    if (iostat /= 0) then
      write(0,"(A,A,':',I3,A,I10,A)") 'Error: ',trim(__FILE__),__LINE__, &
           ' unable to read line ',j,' from file ',trim(file)
      close(unit)
      ERROR_STOP      
    end if
    
    iostat = parseISODate(date,year,month,day,hour,minute,seconds)
    if (iostat /= 0) then
      write(0,"(A,A,':',I3,A,A,A,I10,A,A)") 'Error: ',trim(__FILE__),__LINE__, &
           ' unable to parse date ',trim(date),' at line ',j,' from file ',trim(file)
      close(unit)
      ERROR_STOP      
    end if

    coord(4,j) = mjd(year,month,day,hour,minute,seconds) - t0
  end do
  close(unit)

 end subroutine loadObsFile

 ! save a observation file in NetCDF format
 subroutine saveNCObsFile(ncfile,coord,ids)
  use netcdf
  implicit none

  character(len=*), intent(in) :: ncfile,ids(:)
  real(8) :: coord(:,:)
  integer :: strlen

  character(len=maxlen) :: timeunit
  integer :: ncid, dimids, varidcoord(4), varid, dimidstr
  integer :: i,j


  write(timeunit,'("days since ",I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
       timeOrigin(1),timeOrigin(2),timeOrigin(3), &
       timeOrigin(4),timeOrigin(5),timeOrigin(6)

  ! longest id
  strlen = 0
  do j=1,size(ids)
    strlen = max(strlen,len_trim(ids(j)))
  end do

  
  check(nf90_open(ncfile,nf90_write,ncid))
  check(nf90_redef(ncid))
   
!  check(nf90_create(ncfile,nf90_clobber,ncid))

  check(nf90_def_dim(ncid, 'observations', size(ids), dimids))
  check(nf90_def_dim(ncid, 'idlen', strlen, dimidstr))

  check(nf90_def_var(ncid, 'obsid', nf90_char, (/dimidstr,dimids /), varid))
  check(nf90_put_att(ncid, varid, 'long_name', 'observation identifier'))

  check(nf90_def_var(ncid, 'obslon', nf90_double, dimids, varidcoord(1)))
  check(nf90_put_att(ncid, varidcoord(1), 'units', 'degrees_east'))

  check(nf90_def_var(ncid, 'obslat', nf90_double, dimids, varidcoord(2)))
  check(nf90_put_att(ncid, varidcoord(2), 'units', 'degrees_north'))

  if (size(coord,1) > 2) then
    check(nf90_def_var(ncid, 'obsdepth', nf90_double, dimids, varidcoord(3)))
    check(nf90_put_att(ncid, varidcoord(3), 'units', 'meters'))
    check(nf90_put_att(ncid, varidcoord(3), 'positive', 'down'))

    if (size(coord,1) > 3) then

      check(nf90_def_var(ncid, 'obstime', nf90_double, dimids, varidcoord(4)))
      check(nf90_put_att(ncid, varidcoord(4), 'units', timeunit))
    end if
  end if

  check(nf90_enddef(ncid))

  ! store the variable temp in the netcdf file

  do i = 1,size(ids)
    check(nf90_put_var(ncid,varid,trim(ids(i)),(/1,i/)))
  end do

  do i = 1,size(coord,1)
    check(nf90_put_var(ncid,varidcoord(i),coord(i,:)))
  end do

  check(nf90_close(ncid))
 end subroutine saveNCObsFile

 subroutine check_error(status,file,line)
  use netcdf
  implicit none
  integer, intent ( in) :: status
  character(len=*), intent ( in), optional :: file
  integer, intent ( in), optional :: line

  if(status /= nf90_noerr) then
    if (present(file) .and. present(line)) then
      write(6,*) 'NetCDF error: ',file,line,trim(nf90_strerror(status))
    else
      write(0,*) 'NetCDF error: ',trim(nf90_strerror(status))
    end if

    ERROR_STOP
  end if
 end subroutine check_error

end module divaio

program netcdfobsid
 use divaio
 implicit none

 character(len=maxlen) :: file,ncfile
 real(8), pointer :: coord(:,:)
 character(len=maxlen), pointer :: ids(:)
 integer :: iargc, unit = 10

 if (iargc().ne.2) then
   write(0,*) 'Usage: netcdfobsid <obsid.txt> <file.nc>'
   ERROR_STOP 
 end if

 call getarg(1,file)
 call getarg(2,ncfile)

 call loadObsFile(file,unit,coord,ids)
 call saveNCObsFile(ncfile,coord,ids)

 deallocate(coord,ids)

end program netcdfobsid
