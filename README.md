[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![Build Status](https://travis-ci.org/gher-uliege/DIVA.svg?branch=master)](https://travis-ci.org/gher-uliege/DIVA)
[![DOI](https://zenodo.org/badge/80114691.svg)](https://zenodo.org/badge/latestdoi/80114691)

![made-with-bash](https://img.shields.io/badge/Made%20with-Bash-1f425f.svg) 


⚠️⚠️ this original `DIVA` tool will remain available, but will not be further developped. For new features, users are invited to switch to the generalization in N-dimensions [`DIVAnd`](https://github.com/gher-uliege/DIVAnd.jl) using the [`Julia`](julialang.org/) programming language.

# DIVA (Data-Interpolating Variational Analysis)

DIVA allows the spatial interpolation of data (*analysis*) in an optimal way, comparable to *optimal interpolation* (OI). In comparison to OI, it takes into account coastlines, sub-basins and advection. Calculations are highly optimized and rely on a [*finite element*](https://en.wikipedia.org/wiki/Finite_element_method) resolution. 

Tools to generate the finite element mesh are provided as well as tools to optimize the parameters of the analysis. Quality control of data can be performed and error fields can be calculated. In addition, *detrending* of data is possible. Finally 3D and 4D extensions are included with emphasis on direct computations of climatologies from [Ocean Data View](https://odv.awi.de/) (ODV) spreadsheet files.

![Diva logo](https://cloud.githubusercontent.com/assets/11868914/24106959/c6d8fb44-0d89-11e7-921b-a36fcccf5a21.png)

## Getting started

### Prerequisites

✅ A fortran compiler: [gfortran](https://gcc.gnu.org/wiki/GFortran), [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html), pgf, ...          
✅ The [NetCDF](https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf-f90.html) library for Fortran, needed for the writing of output files.           
✅ [gnuplot](http://www.gnuplot.info/)[optional] for the creation of graphics.

For Debian architectures, you can run:
```bash
sudo apt-get install -y software-properties-common	# needed to use add-apt-repository
sudo add-apt-repository universe  	                # needed to get netCDF
sudo apt-get install -y git make                   # needed for the compilation
sudo apt-get install -y gfortran netcdf-bin libnetcdf-dev libnetcdff-dev
```

### Installing

1. Download the latest stable [release](https://github.com/gher-ulg/DIVA/releases) and extract the archive:
```bash
tar xvf DIVA-4.7.2.tar.gz
```
or clone the project and checkout the last version:
```bash
git clone git@github.com:gher-uliege/DIVA.git
git checkout v4.7.2
```

2. Go in the source directory
```bash
cd DIVA3D/src/Fortran/
```
3. Run the compilation script:
```bash
make
```
#### Notes
- the compiler (by default `gfortran`) and its flags can be modified by editing `Makefile`
- the netCDF _library_ and _include_ flags are deduced from `nf-config` command, which provides the options with which netCDF was build.     
The values can be specified differently (if for example you use a non-standard path) by editing the lines
```bash
export nclib=$(shell nf-config --flibs)
export ncinc=$(shell nf-config --fflags)
```

### Testing

Go in the main execution directory (*divastripped*) and run the tests:
```bash
cd ../../divastripped/
divatest
divatest0
...
```
## How does it work?

DIVA is a software tool developed for gridding in situ data.
It uses a _finite-element_ method to solve a variational principle which takes into account:
 1. the distance between analysis and data (observation constraint),
 2. the regularity of the analysis (*smoothness* constraint),
 3. physical laws (behaviour constraint). 
 
 ![800px-diva_gridding_canary](https://cloud.githubusercontent.com/assets/11868914/24946939/09c918fc-1f65-11e7-9974-06264c70ec1e.png)

The advantage of the method over classic interpolation methods is multiple:
* the coastline are taken into account during the analysis, since the variational principle is solved only in the region covered by the sea. This prevents the information from traveling across boundaries (e.g., peninsula, islands, etc) and then produce artificial mixing between water masses.
* the numerical cost is not dependent on the number of data, but on the number of degrees of freedom, itself related to the size of the finite-element mesh. 

##  How to try DIVA without installing?

If you are familiar with Ocean Data View [ODV](http://odv.awi.de/) software tool, you can perfom `DIVA` gridding when plotting vertical or horizontal sections, as the other 25000 scientists using ODV.

You can also use basic DIVA features in a web application [Diva on Web](https://ec.oceanbrowser.net/emodnet/diva.html) if you have your data ready in a simple three-column ascii file or ODV ascii spreadsheet format. 

![divaonweb](https://cloud.githubusercontent.com/assets/11868914/24947093/a980dd26-1f65-11e7-8715-f1e50bd69a83.png)

## Related tools 

* [`DIVAnd.jl`](https://github.com/gher-uliege/DIVAnd.jl): n-dimensional variational analysis of arbitrarily located observations (written in Julia).
* [`DIVAndNN.jl`](https://github.com/gher-uliege/DIVAndNN.jl): multivariate DIVAnd using a neural network.
* [`DIVAnd.py`](https://github.com/gher-uliege/DIVAnd.py) (not developed anymore): the Python interface to `DIVAnd.jl`.
* [`DivaPythonTools`](https://github.com/gher-uliege/DivaPythonTools): a set of utilies to read, write and plot the content of input or output files used in DIVA.

## Publications & documents 

Check the [GHER publications](https://orbi.uliege.be/reports?action=1&conjunction_1=AND&field_1=affil&value_1_authority=&value_1=GHER&type=DSO%2FA01&type=DSO%2FA02&type=DSO%2FA03&type=DSO%2FA04&type=DSO%2FA05&type=DSO%2FA99&type=DSO%2FB01&type=DSO%2FB02&year_start=&year_end=&references=&model=none&format=apa&abstract_length=&sort_1=issued_dt%3Adesc&sort_1=author_sort%3Aasc&sort_1=title_sort%3Aasc&output=html&lang_report=en&title=GHER+Publications+) for the most recent updates.

### Articles

#### Theory

Barth, A., Beckers, J.-M., Troupin, C., Alvera-Azcárate, A., and Vandenbulcke, L.: divand-1.0: n-dimensional variational data analysis for ocean observations, Geosci. Model Dev., 7, 225-241, [doi:10.5194/gmd-7-225-2014](https://doi.org/10.5194/gmd-7-225-2014), 2014.

Troupin, C.; Sirjacobs, D.; Rixen, M.; Brasseur, P.; Brankart, J.-M.; Barth, A.; Alvera-Azcárate, A.; Capet, A.; Ouberdous, M.; Lenartz, F.; Toussaint, M.-E. & Beckers, J.-M. (2012) Generation of analysis and consistent error fields using the Data Interpolating Variational Analysis (Diva). *Ocean Modelling*, **52-53**: 90-101. doi:[10.1016/j.ocemod.2012.05.002](https://doi.org/10.1016/j.ocemod.2012.05.002)

Beckers, J.-M.; Barth, A.; Troupin, C. & Alvera-Azcárate, A. Some approximate and efficient methods to assess error fields in spatial gridding with DIVA (Data Interpolating Variational Analysis) (2014). *Journal of Atmospheric and Oceanic Technology*,  **31**: 515-530. doi:[10.1175/JTECH-D-13-00130.1](https://doi.org/10.1175/JTECH-D-13-00130.1)

#### Applications 

Capet, A.; Troupin, C.; Carstensen, J.; Grégoire, M. & Beckers, J.-M. Untangling spatial and temporal trends in the variability of the Black Sea Cold Intermediate Layer and mixed Layer Depth using the DIVA detrending procedure (2014). *Ocean Dynamics*, **64**: 315-324. doi:[10.1007/s10236-013-0683-4](https://doi.org/10.1007/s10236-013-0683-4)

Troupin, C.; Machín, F.; Ouberdous, M.; Sirjacobs, D.; Barth, A. & Beckers, J.-M. High-resolution Climatology of the North-East Atlantic using Data-Interpolating Variational Analysis (Diva) (2010). *Journal of Geophysical Research*, **115**: C08005. doi:[10.1029/2009JC005512](https://doi.org/10.1029/2009JC005512)

### User guide

The most recent version is available in [PDF](https://github.com/gher-ulg/Diva-User-Guide/raw/master/DivaUserGuide.pdf).

### Posters and presentations

Check the complete list of documents hosted through the [ULiege Orbi catalogue](http://orbi.ulg.ac.be/orbi-report?query=%28%28affil%3A%22GeoHydrodynamics+and+Environment+Research%22%29+OR+%28affil%3A%22Oc%C3%A9anographie+physique%22%29%29&model=a&format=apa&sort_by0=1&order0=DESC&sort_by1=3&order1=ASC&sort_by2=2&order2=ASC&output=html&language=en&title=GHER+publications).

## Acknowledgments

The DIVA development has received funding from:
- the European Union Sixth Framework Programme (FP6/2002-2006) under grant agreement n° 026212, [SeaDataNet](http://www.seadatanet.org/), 
- the Seventh Framework Programme (FP7/2007-2013) under grant agreement n° 283607, SeaDataNet II, 
- SeaDataCloud and 
- [EMODNet](http://www.emodnet.eu/) (MARE/2008/03 - Lot 3 Chemistry - SI2.531432) from the [Directorate-General for Maritime Affairs and Fisheries](http://ec.europa.eu/dgs/maritimeaffairs_fisheries/index_en.htm).
