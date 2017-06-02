The Fortran sources are organised in different sub-directories (Calc, Mesh, ...). 

To compile the code you have two main possibilities:

1. Use the *divacompileall* script:
    1. edit *divacompile_options* according to your installation (compiler, libraries etc),
    2. run script `divacompileall`.
 
A file *compilation.log* summarises the result of the compilation (number of executables etc).
 
2. Use the *Makefile*
    1. edit the file 'Makefile'
    2. run `make` or `make all`

In both cases you need to have the netCDF library installed.
Following the procedures from
* http://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html (netCDF C library) and
* http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html (netCDF Fortran).

Once these are installed, you should get the possibility to use the tools *nc-config* and *nf-config*, which allow you to determine 
* the locations of the netCDF include directory:
`nf-config --flibs`
* the library directories:
`nf-config --fflags`
* the Fortran compiler:
`nf-config --fc`

