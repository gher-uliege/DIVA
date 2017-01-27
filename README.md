# DIVA (Data-Interpolating Variational Analysis)

DIVA allows the spatial interpolation of data (*analysis*) in an optimal way, comparable to optimal interpolation (OI). In comparison to OI, it takes into account coastlines, sub-basins and advection. Calculations are highly optimized and rely on a [finite element](https://en.wikipedia.org/wiki/Finite_element_method) resolution. 

Tools to generate the finite element mesh are provided as well as tools to optimize the parameters of the analysis. Quality control of data can be performed and error fields can be calculated. In addition, *detrending* of data is possible. Finally 3D and 4D extensions are included with emphasis on direct computations of climatologies from Ocean Data View (ODV) spreadsheet files.

The DIVA development has received funding from the European Union Sixth Framework Programme (FP6/2002-2006) under grant agreement n° 026212, [SeaDataNet](http://www.seadatanet.org/), Seventh Framework Programme (FP7/2007-2013) under grant agreement n° 283607, SeaDataNet II, SeaDataCloud and [EMODNet](http://www.emodnet.eu/) (MARE/2008/03 - Lot 3 Chemistry - SI2.531432) from the Directorate-General for Maritime Affairs and Fisheries.

## How does it work?

Diva is a software developed for gridding in situ data.
It uses a finite-element method to solve a variational principle which takes into account:
 1. the distance between analysis and data (observation constraint),
 2. the regularity of the analysis (smoothness constraint),
 3. physical laws (behaviour constraint). 

The advantage of the method over classic interpolation methods is multiple:
* the coastline are taken into account during the analysis, since the variational principle is solved only in the region covered by the sea. This prevents the information from traveling across boundaries (e.g., peninsula, islands, etc) and then produce artificial mixing between water masses.
* the numerical cost is not dependent on the number of data, but on the number of degrees of freedom, itself related to the size of the finite-element mesh. 

##  How to try DIVA without installing ?

If you are familiar with Ocean Data View [ODV](http://odv.awi.de/) you can access DIVA gridding when plotting vertical or horizontal sections as the other 25000 scientists using ODV.

You can also use basic DIVA features in a web application [Diva on Web](http://gher-diva.phys.ulg.ac.be/web-vis/diva.html) if you have your data ready in a simple three-column ascii file or ODV ascii spreadsheet format. 

##  Publications & documents 

### Articles

Beckers, J.-M.; Barth, A.; Troupin, C. & Alvera-Azcárate, A. Some approximate and efficient methods to assess error fields in spatial gridding with DIVA (Data Interpolating Variational Analysis) Journal of Atmospheric and Oceanic Technology, 2014, 31, 515-530. doi:[10.1175/JTECH-D-13-00130.1](http://dx.doi.org/10.1175/JTECH-D-13-00130.1)

Capet, A.; Troupin, C.; Carstensen, J.; Grégoire, M. & Beckers, J.-M. Untangling spatial and temporal trends in the variability of the Black Sea Cold Intermediate Layer and mixed Layer Depth using the DIVA detrending procedure Ocean Dynamics, 2014, 64, 315-324. doi:[10.1007/s10236-013-0683-4](http://dx.doi.org/10.1007/s10236-013-0683-4)

Troupin, C.; Machín, F.; Ouberdous, M.; Sirjacobs, D.; Barth, A. & Beckers, J.-M. High-resolution Climatology of the North-East Atlantic using Data-Interpolating Variational Analysis (Diva) Journal of Geophysical Research, 2010, 115, C08005. doi:[10.1029/2009JC005512](http://dx.doi.org/10.1029/2009JC005512)

Troupin, C.; Sirjacobs, D.; Rixen, M.; Brasseur, P.; Brankart, J.-M.; Barth, A.; Alvera-Azcárate, A.; Capet, A.; Ouberdous, M.; Lenartz, F.; Toussaint, M.-E. & Beckers, J.-M. Generation of analysis and consistent error fields using the Data Interpolating Variational Analysis (Diva) Ocean Modelling, 2012, 52-53, 90-101. doi:[10.1016/j.ocemod.2012.05.002](http://dx.doi.org/10.1016/j.ocemod.2012.05.002)

### User guide

Last version: [DivaUserGuide_June2016](http://modb.oce.ulg.ac.be/mediawiki/upload/DIVA/notes/DivaUserGuide_June2016.pdf)

### 
