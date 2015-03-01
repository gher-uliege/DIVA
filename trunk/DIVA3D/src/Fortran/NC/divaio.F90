! input-output module for DIVA

! Alexander Barth
#define ERROR_STOP call exit(1)
#define check(status) call check_error(status,__FILE__,__LINE__)

module divaio

 integer, parameter :: maxlen = 512
 ! time origin (/year,month,day,hour,minute,seconds/)
 integer :: timeOrigin(6) = (/1900,1,1,0,0,0/)

 interface num2str
   module procedure  &
        num2str_integer, &
        num2str_real
 end interface num2str

contains

 !_______________________________________________________
 !
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

  hour = 0
  minute = 0
  seconds = 0

  status = -1
  ! year
  i = index(str,'-')
  if (i == 0) return

  read(str(1:i-1),*,iostat=iostat) year
  if (iostat /= 0) return

  ! month
  j = indexof(str,'-',i+1)
  if (j == 0) return
  read(str(i+1:j-1),*,iostat=iostat) month
  if (iostat /= 0) return

  ! day
  i = indexof(str,'T',j+1)
  if (i == 0) then
    ! there are no hours, the remaining of the string are days
    read(str(j+1:),*,iostat=iostat) day
    if (iostat == 0) status = 0
    return
  end  if

  read(str(j+1:i-1),*,iostat=iostat) day
  if (iostat /= 0) return

  ! hour
  j = indexof(str,':',i+1)
  if (j == 0) then
    ! there are no minutes, the remaining of the string are hours
    read(str(i+1:),*,iostat=iostat) hour
    if (iostat == 0) status = 0
    return
  end  if

  read(str(i+1:j-1),*,iostat=iostat) hour
  if (iostat /= 0) return

  ! minute
  i = indexof(str,':',j+1)
  if (i == 0) then
    ! there are no seconds, the remaining of the string are minutes
    read(str(j+1:),*,iostat=iostat) minute
    if (iostat == 0) status = 0
    return
  end  if

  read(str(j+1:i-1),*,iostat=iostat) minute
  if (iostat /= 0) return

  ! second
  read(str(i+1:),*,iostat=iostat) seconds
  if (iostat /= 0) return

  status = 0
 end function parseISODate

 !_______________________________________________________
 !
 ! index of sub-string in string str starting at index start
 ! returns -1 is sub-string is not found

 function indexof(str,substr,start) result(ind)
  character(len=*), intent(in) :: str, substr
  integer, intent(in) :: start
  integer :: ind

  ind = index(str(start:),substr)
  if (ind /= 0) ind = ind+start-1
 end function indexof

 !_______________________________________________________
 !
 ! modified Julian day number

 function mjd(y,m,d,h,min,s)
  implicit none
  integer, intent(in) :: d,m,y,h,min
  real, intent(in)  :: s
  real(8) :: mjd

  ! The following conversion algorithm is due 
  ! to Henry F. Fliegel and Thomas C. Van Flandern: 
  ! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y)
  ! as follows:

  ! ModifiedJulianDay = 0 for 1858-11-17 CE.

  mjd = (( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +        &
       ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -   &
       ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +  &
       d - 32075 - 2400001)*1d0 + h/24d0 + min/(24*60d0)+ s/(24*60*60d0)
 end function mjd

 !_______________________________________________________
 !
 ! retuns the current time in mjd (and UTC)
 !

 real(8) function mjdnow() 

  integer (8)   :: current_time(8), tzoffset
  integer :: y,m,d,h,min
  real :: s

  call date_and_time(values=current_time)
  y = current_time(1)
  m = current_time(2)
  d = current_time(3)
  h = current_time(5)
  min = current_time(6)
  s = current_time(7)

  ! time zone offset in minutes
  tzoffset = current_time(4)

  mjdnow = mjd(y,m,d,h,min,s)-tzoffset/(24.*60.)
 end function mjdnow

 !_______________________________________________________
 !
 ! compute gregorian date from mjd

 subroutine gregd(mjd,y,m,d,h,min,s)
  implicit none
  real(8), intent(in)  :: mjd
  integer, intent(out) :: d,m,y,h,min
  real, intent(out)  :: s

  real(8) :: fraction
  integer              :: l,n,i,j

  ! Converting from the modified Julian day number to the Gregorian 
  ! date is performed thus:


  l = int(mjd) + 68569 + 2400001
  n = ( 4 * l ) / 146097
  n = ( 4 * l ) / 146097
  l = l - ( 146097 * n + 3 ) / 4
  i = ( 4000 * ( l + 1 ) ) / 1461001
  l = l - ( 1461 * i ) / 4 + 31
  j = ( 80 * l ) / 2447
  d = l - ( 2447 * j ) / 80
  l = j / 11
  m = j + 2 - ( 12 * l )
  y = 100 * ( n - 49 ) + i + l

  fraction = mjd-int(mjd)

  h = floor(24*fraction);   fraction = 24*fraction - h
  min = floor(60*fraction); fraction = 60*fraction - min
  s = floor(60*fraction);   fraction = 60*fraction - s

 end subroutine gregd

 !_______________________________________________________
 !
 ! replace a string

 function strrep(source,olds,news) result(s)
  implicit none
  character(*), intent(in) :: source,olds,news
  character(255) :: s

  integer :: i

  s  = source

  i = index(s,olds) 
  do while (i.ne.0)
    s = s(1:i-1)//trim(news)//s(i+len_trim(olds):len_trim(s))
    i = index(s,olds) 
  end do

 end function strrep

 !_______________________________________________________
 !
 ! convert integer to string

 function num2str_integer(x,sformat) result(s)
  implicit none
  integer, intent(in) :: x
  character(*), intent(in) :: sformat
  character(255) :: s

  integer :: stat

  write(s,sformat,iostat=stat) x

  if (stat.ne.0) then
    write(0,*) 'Error: num2str format error ',trim(sformat),stat
    ERROR_STOP
  end if

 end function num2str_integer

 !_______________________________________________________
 !
 ! convert real to string

 function num2str_real(x,sformat) result(s)
  implicit none
  real, intent(in) :: x
  character(*), intent(in) :: sformat
  character(255) :: s

  integer :: stat

  write(s,sformat,iostat=stat) x

  if (stat.ne.0) then
    write(0,*) 'Error: num2str format error ',trim(sformat)
    ERROR_STOP
  end if

 end function num2str_real

 !_______________________________________________________
 !
 ! convert a time in mjd to string using the dateformat

 function mjdstr(mjd,dateformat) result(str)
  implicit none
  real(8) :: mjd
  character(*), intent(in) :: dateformat

  character(len=len(dateformat)) :: str

  integer :: d,m,y
  integer :: hour,minute
  real    :: fraction, second


  str = dateformat

  call gregd(mjd,y,m,d,hour,minute,second)

  str = strrep(str,'YYYY',num2str(y,'(I4.4)'))
  str = strrep(str,'YY',num2str(mod(y,100),'(I2.2)'))
  str = strrep(str,'MM',num2str(m,'(I2.2)'))
  str = strrep(str,'DD',num2str(d,'(I2.2)'))

  str = strrep(str,'mm',num2str(minute,'(I2.2)'))
  str = strrep(str,'hh',num2str(hour,'(I2.2)'))

  fraction = second - int(second)
  str = strrep(str,'ss.',trim(num2str(int(second),'(I2.2)'))//'.')

  str = strrep(str,'sssss',num2str(floor(100000*fraction),'(I5.5)'))
  str = strrep(str,'ssss',num2str(floor(10000*fraction),'(I4.4)'))
  str = strrep(str,'sss',num2str(floor(1000*fraction),'(I3.3)'))
  str = strrep(str,'ss',num2str(floor(100*fraction),'(I2.2)'))
  str = strrep(str,'s',num2str(floor(10*fraction),'(I1.1)'))

 end function mjdstr

 !_______________________________________________________
 !
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

 !_______________________________________________________
 !
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
  character(len=maxlen) :: date, line
  real(8) :: t0

  t0 = mjd(timeOrigin(1),timeOrigin(2),timeOrigin(3), &
       timeOrigin(4),timeOrigin(5),real(timeOrigin(6)))

  !  i = parseISODate('1986-01-25T13:45',year,month,day,hour,minute,seconds)
  !  stop

  ! call parseISODate('1988-2-15T1:2:01.023',year,month,day,hour,minute,seconds)

  count = numberOfLines(file,unit)
  allocate(ids(count))

  open(unit,file=file) 

  read(unit,'(A)',iostat=iostat) line

  ! count number of columns
  line = ' '//line
  ncoord = 0
  do i = 2,len_trim(line)
    ! start of a column
    if (line(i-1:i-1) == ' ' .and. line(i:i) /= ' ') ncoord = ncoord+1
  end do

  if (ncoord < 3 .or. ncoord > 5) then
    write(0,"(A,A,':',I3,A,A)") 'Error: ',trim(__FILE__),__LINE__, &
         ' expect 3, 4 or 5 columns in file ',trim(file)
    close(unit)
    ERROR_STOP
  end if

  ! the last column is not a coordinate
  ncoord = ncoord - 1
  !  write(6,*) 'ncoord',ncoord

  allocate(coord(ncoord,count))
  rewind(unit)

  do j = 1,count
    if (ncoord == 4) then
      read(unit,*,iostat=iostat) (coord(i,j), i=1,ncoord-1), date, ids(j)
    else
      read(unit,*,iostat=iostat) (coord(i,j), i=1,ncoord), ids(j)
    end if

    if (iostat /= 0) then
      write(0,"(A,A,':',I3,A,I10,A)") 'Error: ',trim(__FILE__),__LINE__, &
           ' unable to read line ',j,' from file ',trim(file)
      close(unit)
      ERROR_STOP      
    end if


    if (ncoord == 4) then
      iostat = parseISODate(date,year,month,day,hour,minute,seconds)
      if (iostat /= 0) then
        write(0,"(A,A,':',I3,A,A,A,I10,A,A)") 'Error: ',trim(__FILE__),   &
             __LINE__, ' unable to parse date ',trim(date),' at line ',j, &
             ' from file ',trim(file)
        close(unit)
        ERROR_STOP      
      end if

      coord(4,j) = mjd(year,month,day,hour,minute,seconds) - t0
    end if
  end do

  close(unit)

 end subroutine loadObsFile


 !_______________________________________________________
 !
 ! save a observation file in NetCDF format

 subroutine saveNCObsFile(ncfile,coord,ids)
  use netcdf
  implicit none

  character(len=*), intent(in) :: ncfile,ids(:)
  real(8) :: coord(:,:)
  integer :: strlen

  character(len=maxlen) :: timeunit, coordinates
  integer :: ncid, dimids, varidcoord(4), varid, dimidstr
  integer :: i,j


  write(timeunit, &
       '("days since ",I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
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

  coordinates =  'obslat obslon'

  if (size(coord,1) > 2) then
    check(nf90_def_var(ncid, 'obsdepth', nf90_double, dimids, varidcoord(3)))
    check(nf90_put_att(ncid, varidcoord(3), 'units', 'meters'))
    check(nf90_put_att(ncid, varidcoord(3), 'positive', 'down'))
    coordinates = 'obsdepth ' // coordinates

    if (size(coord,1) > 3) then

      check(nf90_def_var(ncid, 'obstime', nf90_double, dimids, varidcoord(4)))
      check(nf90_put_att(ncid, varidcoord(4), 'units', timeunit))
      coordinates = 'obstime ' // coordinates
    end if
  end if

  check(nf90_put_att(ncid, varid, 'coordinates', coordinates))

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

 !_______________________________________________________
 !
 ! check for netcdf error
 ! use with macro check to specify automatically file and line

 subroutine check_error(status,file,line)
  use netcdf
  implicit none
  integer, intent ( in) :: status
  character(len=*), intent ( in), optional :: file
  integer, intent ( in), optional :: line

  if(status /= nf90_noerr) then
    if (present(file) .and. present(line)) then
      write(0,*) 'NetCDF error: ',file,'line ',line, &
           trim(nf90_strerror(status))
    else
      write(0,*) 'NetCDF error: ',trim(nf90_strerror(status))
    end if

    ERROR_STOP
  end if
 end subroutine check_error

end module divaio
