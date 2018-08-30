! netcdfobsid adds coordinates of observations (longitude, latitude, depth and
! time) and observation identifier to an existing netCDF file
!
! Call as
! netcdfobsid <obsid.txt> <file.nc>
!
! obsid.txt: text file with 5 columns: longitude (degrees north), latitude
! (degrees east), depth (meters, positive in water) and time
! (yyyy-mm-ddTHH:MM:SS, seconds, minutes and hours might be omitted) and id
! separated by space. The id cannot contain any space. The time or the time and
! depth column can be omitted.
!
! file.nc: netCDF file where the information is appended (file must exist)
!
! Compile with something like:
!
! gfortran $(nf-config --fflags) -o netcdfobsid divaio.F90 netcdfobsid.F90 \
!   $(nf-config --flibs)
!

#define ERROR_STOP call exit(1)
#define check(status) call check_error(status,__FILE__,__LINE__)


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

! LocalWords:  netcdfobsid obsid txt nc netcdf gfortran config fflags
! LocalWords:  flibs str Fliegel Flandern jd ModifiedJulianDay
