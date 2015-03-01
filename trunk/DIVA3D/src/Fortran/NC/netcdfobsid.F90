! netcdfobsid adds coordinates of observations (longitude, latitude, depth and 
! time) and observation identifier to a NetCDF file
!
! Call as
! netcdfobsid <obsid.txt> <file.nc>
!
! obsid.txt: text file with 5 columns: longitude (degrees north), latitude 
! (degrees east), depth (meters, positive in water) and time 
! (yyyy-mm-ddTHH:MM:SS, seconds, minutes and days might be omitted) and id 
! separated by space. The id cannot contain a space. The time or the time and 
! depth column can be omitted.
!   
! file.nc: netcdf file where the information is appended (file must exist)
!
! Compile with something like:
!
! gfortran $(nc-config --fflags) -o netcdfobsid netcdfobsid.F90 \
!   $(nc-config --flibs)
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
