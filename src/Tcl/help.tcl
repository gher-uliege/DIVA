#######################################################
proc p_help_about {} {

  global Dict

  dialog .messagebox $Dict(About) $Dict(Version) "" 0 $Dict(Ok)
}
#######################################################

proc p_help {topic {subtopic 0} {w .help}} {

  global Dict
  toplevel $w
  dpos $w 200 200
  wm title $w $Dict(Help)
  wm iconname $w $Dict(DivaHelp)
  button $w.ok -text OK -command "destroy $w"
  text $w.t -relief raised -bd 2 -yscrollcommand "$w.s set" -setgrid true
  scrollbar $w.s -relief flat -command "$w.t yview"
  pack $w.ok -side bottom -fill x
  pack $w.s -side right -fill y
  pack $w.t -expand yes -fill both

  switch -exact $topic {
    general "set text [HelpGeneral]"
    data "set text [HelpData $subtopic]" 
    fem "set text [HelpFem $subtopic]"
    diva "set text [HelpDiva $subtopic]"
    view "set text [HelpGraph $subtopic]"
    option "set text [HelpOption $subtopic]"
  }
 
  $w.t insert 0.0 $text
  $w.t mark set insert 0.0

}

#######################################################
proc HelpGeneral { } {

set GENERAL {

 General informations
----------------------

Following are given some informations to work well with the interface. 

Diva essentially consist of tree main panel to work with files:

	- one to extract data from the database
	- one to create meshes 
	- one to analyse the data with the VARIATIONAL INVERSE MODEL 

Note that all these files have defaults extensions:
(See other topics for more details)

	*.des 		for database description files
	*.data<depth>	for data extracted from the database
			(output from the first panel)
	*.cont		for contour files
	*.mesh		for mesh files
			(output from the second panel)
	*.depth		for depth files
	*.bat		for bathymetry files
	*.anl		for analysis files 
			(output from the third panel)

It's highly recommended	to use these file extensions.

In addition, each of these panels has it's own related view panel to 
visualize the computed results.

Last but not least, there is a simple directory tool to organize your 
work as well, also a language tool, easily extendable to other languages. 
By now, only english and french are available.

Note that the directory tool doesn't create the directory. You have to
creae them with the shell mkdir command.

For most of these panels, you can save and reload different configurations.
Saving default configurations increases your productivity.



 <The way to work with DIVA>
-----------------------------

The easiest way to know how to work with diva is to follow 
the flow diagram given bellow. 


	<Extraction of data>	-->
	(+visualization)	  | 
				  | --> <Analyse> 
				  |     (+visualization) 
	<Mesh creation>		-->
	(+visualization)



 <The way to extract data>
---------------------------

The easiest way to know how to extract data from the database
is to follow the flow diagram given bellow.

	DB Description file name		|
	Generic data output file name		|
	Years, Period + Depths list		|
	Contour description file OR Position	|--> OK	
	One data type at least			|
	One data nature				|
	Quality criterion			|


 <The way to create a mesh>
----------------------------

The easiest way to know how to create a mesh is to follow the 
flow diagram given bellow.

*	2D case:

        Contour file name			|
        Mesh output file name			|
        Uniform OR non uniform mesh		|
        Contour description file OR Position	|--> OK
        Reference lenght			|
        Surface coef				|
        Smooth number				|

*	3D case:

        Contour file name			|
        Mesh output file name			|
	Bathymetry file name			|
	Depth file name				|
        Uniform OR non uniform mesh		|--> OK
        Contour description file OR Position	|
        Reference lenght			|
        Surface coef				|
        Smooth number				|


 <The way to analyse the data>
-------------------------------

The easiest way to know how to analyse data is to follow the 
flow diagram given bellow.

*        2D case:

        Data file name				|	
        Mesh file name				|
        Analysis output file name		|
        <Reference field>			|--> OK 
        Output grid				|
        Transient results			|


*        3D case:

        Depth file name				|
        Mesh file name				|
        Analysis output file name		|
        <Reference field>			|--> OK
        Output grid				|
        Transient results			|

	<Reference field>:

	None	--> alpha, mu only

	Mean value		|
				|--> (car length, S/N ratio)
	Linear regression 	|     OR (alpha,mu)


 <The way to view extracted data>
----------------------------------

The easiest way to know how to view extrated data is to follow 
the flow diagram given bellow.

*       2D case:

        Data file name				|
        Contour (optional)			|
        Selected depth (optional)		|--> OK
	Comments on graphics			|


*       3D case:

        Depth file name                         |
        Contour (optional)                      |
        Selected depth (optional)               |--> OK
        Comments on graphics                    |



 <The way to view created meshes>
----------------------------------

The easiest way to know how to view created meshes is to follow
the flow diagram given bellow.

*       2D case:

        Mesh file name                          |
        Selected depth (optional)               |--> OK
        Comments on graphics                    |


*       3D case:

        Mesh file name                          |
        Depth file name                         |
        Selected depth (optional)               |--> OK
        Comments on graphics                    |


 <The way to view analysis results>
----------------------------------

The easiest way to know how to view analysis results is to follow
the flow diagram given bellow.


        Analysis file name			|
        Selected depth (optional)		|
	Display depth (optional)		|
        Comments on graphics			|--> OK
	Color levels				|
	Compute color scale for each depth(opt.)|



For more informations, see other topics.
}


}

#######################################################


#######################################################
proc HelpData { sub } {
#######################################################


upvar #2 $sub var
global Dict

set DBDESCR { 

DB Description file name
-------------------------

Enter here the full name of the file describing which data file names are used
to search for the extraction of data.

Ex. : /home/Bank/MODB/NETCDF/LISTE

Contents of file LISTE :

/home/Bank/MODB/NetCDF/modb_0015.nc
/home/Bank/MODB/NetCDF/modb_0030.nc
/home/Bank/MODB/NetCDF/modb_0060.nc
/home/Bank/MODB/NetCDF/modb_0100.nc
/home/Bank/MODB/NetCDF/modb_0500.nc

...

The data files must have one of these three extension :

  "nc" for files written with the NetCDF MODB format
  "med" for files written with the MED GHER format
  "pdf" for files written with the MODB/MEDATLAS format
}

set OUTGENNAME {

Generic data output file name
------------------------------

Enter here the generic name of the extracted data files.

Ex. : /home/dupond/Diva/DATA/adriatic.dat

A set of files will be generated, each one for the selected depth.

      /home/dupont/Diva/DATA/adriatic.dat.01
      /home/dupont/Diva/DATA/adriatic.dat.02
      /home/dupont/Diva/DATA/adriatic.dat.03

      ...

Be carefull : all previous files with the same generic name will be deleted !

A little file /home/dupond/Diva/DATA/adriatic.dat.depth will be also generete
and contains in the first columns the depth related to the data file name given
in the second column.

Contents of /home/dupond/Diva/DATA/adriatic.dat.depth :

       10        /home/dupond/Modb/Travail/DATA/data2.dat.01
       20        /home/dupond/Modb/Travail/DATA/data2.dat.02
       50        /home/dupond/Modb/Travail/DATA/data2.dat.03
       
       ... 
}
 
set PERIOD {

Years - Period
--------------

Ex: I would like to get all the data from 1960 to 1980 for the first three 
    months of each year

    Enter years from 1960 to 1980
    Enter period from 0101 to 0331 (mmdd)
}

set POSITION {

Contour description file - Position
-----------------------------------

If you already have a contour description file used to create a mesh, you can 
use the same to bound the region where you want the data. Select "Contour
description file" and load a file.

If you prefer define a rectangular region, select "Position" and fill the 
entries with values.

The minimum and maximum latitude and longitude are given in degres.

In BOTH cases you must enter at least one depth over wich the data will be 
extracted.
}

set DEPTH {

Depth
-----

Enter the depths you want the data to be extracted for.

Add or delete items in list with buttons "Add" and "Del".

If you want the data located at the surface, enter "1" (near the surface)

There will be one data file for each depth (see "Output file generic name"
subtopic). 
}

set FLAG {

Quality flag criterion
----------------------

Two types of flags are introduced: one for the whole profile (profile quality 
flag) and one for each particular observation (observation quality flag).

Flag                  Profile                  Observation

0                     Correct                  Correct
1                     Information is misssing  Information is misssing
                      to complete the QC       to complete the QC
2-6                   Global statistical       Global statistical
                      check                    check
7                     Accurate range checking  Accurate range checking
8                     Physical check & rough   Physical check & rough
                      range checking           range checking
9                     Conformity check         Conformity check

The individual observation flag value corresponds to the first stage (from 9
to 1) of the quality control for wich the measue has failed.
For the general profile flag, a flag value of 9 is affected if the header 
information do not satisfy the conformity check; a flag value of 8 is affected
as soon as more than 50% of the measures of the profile have a flag greater (or
equal) than 8; a flag value of 7 is affected as soon as more than 50% of the 
measures have a flag greater (orequal) than 7. The flag values 6 to 2 are 
reserved for the global statistical check that will be fulfilled on the final
data bank. The flag value 1 is required for data or profiles that have 
successfully followed the whole quality control process but if one or more 
stages could not be performed due to the lack of the required information (such
as the depth checking without depth, or the salinity check without both 
temperature and salinity). The flag value 0 qualifies data for wich no obvious
error has been detected yet.

}

set CONF {

Load - Save configuration
-------------------------

Use this if you want to save the configuration of the window in a file.
All choices you have selected in the window will be saved.
}

return $var

}

#######################################################
proc HelpFem { sub } {
#######################################################

upvar #2 $sub var

set 2D3D {

2D or 3D mesh
-------------

Select 2D if you want to reconstruct the data for the surface layer only.
A mesh file will be generated and it does not take into account the depth
to detect if a mesh is in land or in sea.

In the 3D case you need a bathymetry file (regular grid in the GHER format)
and also a file containing the depth where you want the recontruction (the
file created by the DATA extraction procedure with the "depth" extension).
The rigigity of some meshes (those in land) will be therefore set to a high
value. 
}

set CONTOUR {

Contour name
------------

Enter here the name of the file describing the contour of the domain
to mesh.

You can select a name with the "Load" button. A "File Select" window appears
and you can choose a existing file name with the ".cont" extension or you can 
select a directory from this window and then complete the name by hand in the 
entry.

This ASCII file must contain

		N   
		M1 S1 
		X1 Y1 
		.....
		Mn Sn 
		Xn Yn 

where N is the number of contours
      Mn is the number of points describing the contour #n
      Sn is the type of the contour #n (0=sea inside , 1=land inside, sea on
                                        the left)
      Xn and Yn are the coordinates of points defining contour #n
      
Be sure to ordinate x y to have area to mesh on the left.
Don't close the contour (don't put the first point at the end).

A good way is to space out the points defining the contour with a length that
is approximatively the same (or higher, but not lower) that the size of a 
triangular mesh.

File "name.cont" define the region to mesh.

File "name.cont.dens" is optional and is used to defined region where the 
density must be higher or lower than the default value defined in the 
"Reference Length" entry (see "Uniform, non-uniform" topic)

These two files must have the same name (ex : name) and the extensions '.cont'
and '.cont.dens'

In both case (uniform or non-uniform) you need a contour file with the .cont
extension.

The software creates three files:

- .../name.mesh containing the topology of the mesh
- .../name.mh4 containing the number of vertex and interface nodes and the 
               number of meshes.
- .../name.mh5 that takes into account the depth in the 3D case.
}

set OUTMESHNAME {

Output Mesh
-----------

Fill this entry with a generic name for the results file.

If you choose /home/dupont/Diva/Mesh/name.mesh, then 2 (2D case) or 3 (3D case) 
files are generated when you click the OK button:

- /home/dupont/Diva/Mesh/name.mesh containing the topology of the mesh
- /home/dupont/Diva/Mesh/name.mh4 containing the number of vertex and 
  interface nodes and the number of meshes.
- /home/dupont/Diva/Mesh/name.mh5 describing meshes that are inside or outside
  the land for a given depth (3D case)

You can select a name with the "Select" button. A "File Select" window appears
and you can choose a existing file name with the ".mesh" extension (wich will be
erased if you don't change its name) or you can select a directory from this 
window and then complete the name by hand in the entry.
}


set UNIFMESH {

Uniform, Non-Uniform mesh
-------------------------

If you select "Uniform Mesh", you only have to provide a contour file (with
the ".cont" extension).
This file is describes in the "Contour name" subtopic.
 
If you select "Non-Uniform mesh", you have to provide two files :

- one with the ".cont" extension (contour file)
- one with the ".cont.dens" extension describing the regions of the domain with
  a different density mesh than referencied in the "Reference Length" entry.

This file (".cont.dens", when used) must contain

                M  
                RL1 M1 
                X1 Y1 
                .....
                RLm Mm 
                Xm Ym 

where M is the number of contours where the density is different of the default
      RLm is the reference length in the contour #m
      Mm is the number of points describing the contour #m 
      Xm and Ym are the coordinates of points defining contour #m
}

set REFLENGTH {

Reference length
----------------

This is the value that fixes the length of the side of the triangular elements
touching an outline.  This value can change when non-uniform mesh is choosen 
(see '.cont.dens' file).
         
Don't fix it too small !!!
}

set SURFREF {

Surface Reference Coefficient
-----------------------------
         
This value controls the shape of the triangular element.
        
The value 1.5 will provide triangles close to equilateral triangles.
}

set NBSMOOTH {

Number of Smoothings
--------------------
        
A smoothing leads to a more regular shape of the elements.
         
3 is a good choice.
}

set CONF {

Load - Save configuration
-------------------------

Use this if you want to save the configuration of the window in a file.
All choices you have selected in the window will be saved.
}

return $var

}


#######################################################
proc HelpDiva { sub } {
#######################################################

upvar #2 $sub var

set 2D3D {

2D or 3D
--------

In the 2D case, you will reconstruct data over the surface layer only (or
another layer that have been correctly meshed).
You have to provide a single file containing the data and a mesh (2 or 3D mesh).
So you don't need a bathymetry file at all. 
The result file will be a single array written in the GHER format and there
will be no information concerning the depth where the reconstruction has been
made.
In the 3D case, you have to provide a LIST of data file, a 3D mesh and also 
a bathymetry file (the region covered by the bathymetry file must cover the
region where we want the reconstruction.
}

set DATAFNAME {

Data file name
--------------

Enter here the name of the file containing the data to be reconstruct.

This ASCII file must contain :

	x1 y1 val1
	...
	xn yn valn

where val is the value of the specific data at the x y coordinate. 
}

set MESHFNAME {

Mesh name
---------

Enter the name of the mesh (with .mh extension) used to reconstruct the 
gridded data.
See the Mesh section for how to generate this file.

This file must have the .mesh extension and must go with another file with the
.mh4 extension and the same name.

ex: test.mesh and test.mh4 (select test.mesh)
}

set OUTNAME {

Analysis output file name
-------------------------

Enter the name for the results of the calculation.
You can select an existing one (or a directory) with the Load button and change
its name by hand in the entry. 
The result will be a multiple layer (if multiple depth) grid where the 
information concerning the depth (and the domain) is stored in a file with the 
same name but with the ".info" extension.

}

set ERROR {

Statistical error computation
-----------------------------

In parallel to the analysis, one might compute the statistical error
map of a field. Select the check button and specify the reference field
variance. The field is stored in the output file suffixed by 'error'.
The info file is created appropriately. 

}

set DATATR {

Reference field
---------------

These options specifie the treatment to be applied on the data located in the 
mesh.
         
There are 3 options defined :
         
1. None: no treatment is performed on the original data
2. Mean value: the mean value of the data in the mesh is substracted
3. Linear regression: the linear regression of the data in the mesh 
   is substracted
4. A seminormed analysis (alpha0=0), with specified characteristic length and 
data weight.
         
Note :  When strong extrapolation is required, do not use option 3 !
}

set CLSN {

Characteristic length - Signal/Noise ratio
------------------------------------------

The characteristic length is interpreted as the 'radius of influence' of a data
at a given point.

The Signal/Noise ratio gives the (variance of signal)/(variance of noise) ratio.

Both numbers are used to compute 3 different parameters :
        
alpha0 :  1 / L**4
alpha1 :  2 / L**2
mu     :  (S/N ratio) * 4 * PI / L**2

Increasing mu tends to interpolate data (small spatial structures)
Decreasing mu leads to lossy approximation.

You may choose to set either the 

              Char. Length and S/N ratio
                
                          OR

                Alpha0, Alpha1 and Mu 
}

set ALPHA {

Alpha0, alpha1, mu
------------------

See Characteristic length - Signal/Noise ratio topic.

}

set OUTGRID {

Output grid
-----------

xorigin and yorigin set the origin of the output grid, the grid where the 
resulting reconstruction is written.
The values are related to the mesh origin (as are the coordinates in the data
file).

dx and dy give the spacing in the regular ouput grid.

xend and yend give the dimension of the 2D output matrix.

The size of the ouput grid matrix is thus :

     (xend-xorigin)/dx+1
     (yend-yorigin)/dy+1

The exclusion value is used to fill the ouput matrix when a cell (a point)
correspond with a point on land.

Use "Selected Depth" if you want to recontruct (in the 3D case) the data for
a given depth located in the list of data file name. The result will be a 
one layer grid where the information concerning the depth (and the domain) 
is stored in a file with the same name but with the ".info" extension.
}

set OUTVOL {

Transient results
-----------------

This parameter determines the amount of information appearing to the screen
while computing the data reconstruction.

         0  ->  minimal information
         1  
         2  ->  medium  information
         3  
         4  ->  maximum information
}

set CONF {

Load - Save configuration
-------------------------

Use this if you want to save the configuration of the window in a file.
All choices you have selected in the window will be saved.
}

return $var 
} 

#######################################################
proc HelpGraph { sub } {
#######################################################


upvar #2 $sub var

set DATA {

Data view
---------

The data view menu entry provides a simple way to visualize data extracted 
from the data base.

In the 2D case, specify one data file. In the 3D case specify a depth file.
You are also able to display a contour by setting on the "display contour" 
checkbox and choosing some contour file (.cont extension).

To select a particular depth, make the "Selected depth" check box active.
You will be prompted to select a depth at viewing.
 
You can also give to your graphic a title and text for the horizontal and
vertical axis.

}

set MESH {

Mesh view
---------

The mesh view menu entry provides a simple way to visualize mesh created 
from a contour file with the file-mesh entry.

In the 2D case, specify one mesh file. In the 3D case specify a depth file.

To select a particular depth, make the "Selected depth" check box active.
You will be prompted to select a depth at viewing.

You can also give to your graphic a title and text for the horizontal and
vertical axis.

}

set DIVA {

Analysis view
-------------

The analysis view menu entry provides a simple way to visualize analysis 
computed from a data and a mesh file with the file-analysis entry.

In the 2D case, specify one mesh file. In the 3D case specify a depth file.

To select a particular depth, make the "Selected depth" check box active.
You will be prompted to select a depth at viewing. To add the depth text
to the graphic make the "display depth or level active.

You can also give to your graphic a title and text for the horizontal and
vertical axis.

Color levels can also be specified to give the graphic more precision.
If you want diva to compute the color scale for each depth, make the
"compute color scale for each depth" check box active.

}
return $var
}

#######################################################
proc HelpOption { sub } {
#######################################################


upvar #2 $sub var

set COORDINATES {

Coordinates
-----------

The software provides two possibilities:
- either an internal coordinate change from degrees to kilometers. This 
is particularly indicated in high latitudes,
where one should avoid a distortion of the space.
All dimensions are then in degrees in the interface.
Degrees in the interface corresponds to 111.700 km 
(i.e. latitudinal degrees). 
Internal conversion is assumed for all parameters, meshes, 
longitude, latitude, output boxes,... 
- or no coordinate change: the user choses his own coordinate system
(degrees or kilometers), but all specifications have to be consistent,
i.e. in the same dimensions. 

}

set DIRECTORIES {

Directories
-----------

To allow you to organize your work as well, specify the different
working directories for 
	the database, 
	the extracted data, 
	the mesh files
	the analysis files,
	the description files,
	the bathymetry files,
	the contour files,
	the depth files and the temp
and the temporary result directory

All the default search paths for the file selection boxes will be created 
using these prefixes.
}

set PREFERENCES {

Default language
-----------------

Select your own language. 
At now, only french and english version are available.

}

return $var
}
