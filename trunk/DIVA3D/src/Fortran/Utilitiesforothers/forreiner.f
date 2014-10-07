         function DIVAWRITEDATA(xdata,ydata,valdata,wdata,ndata,
     &                                   xwhere,ywhere,nwhere)
C                 +++++++++++++
        implicit none
        integer i
C
C  To write out some data file in binary format
C   for use with DIVA compiled for ODV set by iodv=1 in iodv.h
C   and compiler option -cpp -DDIVABINARYFILES
C  Inputs
C   Number of data points
       integer ndata
C   Coordinates, values and weights of data points
       double precision xdata(ndata)
       double precision ydata(ndata)
       double precision valdata(ndata)
       double precision wdata(ndata)
C   Number of points where solution is asked
C       (in addition to data locations and regular grid)
       integer nwhere
C   Coordinates and values of data points
       double precision xwhere(nwhere)
       double precision ywhere(nwhere)
C   Output
C     two binary files ready for diva (fort.20 and fort.79)
       integer DIVAWRITEDATA
C     status DIVAWRITEDATA on return
C         0: OK
C         +1: problem creating fort.20 (data)
C         +2: problem creating fort.79 (additional desired analysis locations)
C         so for example 3: problem creating both fort.*
       DIVAWRITEDATA=0
       do i=1,ndata
       write(20,ERR=10) xdata(i),ydata(i),valdata(i),wdata(i)
       enddo
       goto 20
 10    continue
       DIVAWRITEDATA=DIVAWRITEDATA+1
 20    continue
       do i=1,nwhere
       write(79,ERR=30) xwhere(i),ywhere(i)
       enddo
       goto 40
 30    continue
       DIVAWRITEDATA=DIVAWRITEDATA+2
 40    continue
       return
       end
CCCCCCCCCCCC
       function DIVAREADRESULT(resudata,errdata,ndata,
     &                      resuwhere,errwhere,nwhere,
     &                      resugrid,errgrid,NX,NY)
C               ++++++++++++++
        implicit none
        integer i
        double precision x,y
C
C  To read some results file in binary format
C   for use with DIVA compiled for ODV set by iodv=1 in iodv.h
C   and compiler option -cpp -DDIVABINARYFILES
CCCCCCCCCCCCC
C  Inputs
CCCCCCCCCCCCC
C   Number of data points
       integer ndata

C
C   Number of points where solution is asked
C       (in addition to data locations and regular grid)
       integer nwhere

C   Grid points asked NX NY
       integer NX,NY
C
CCCCCCCCCCCCC
C   Outputs
CCCCCCCCCCCCC
C   Analysis result and errors at data points
       double precision resudata(ndata)
       double precision errdata(ndata)
C
C   Analysis result and error at specifically desired points
C (for isopycnal change for example on non-cartesian grid)
       double precision resuwhere(nwhere)
       double precision errwhere(nwhere)
C
C   Analysis result and error on regular grid stored in a 1D array 
C      in Fortran style (first index varies first)
       double precision resugrid(NX*NY)
       double precision errgrid(NX*NY)


C    Output value
      integer DIVAREADRESULT
C     status DIVAREADRESULT on return
C         0: OK
C         +1: problem reading fort.71 (analysis at data locations)
C         +2: problem reading fort.82 (analysis at desired locations)
C         +4: problem reading fort.72 (error at data locations)
C         +8: problem reading fort.73 (error at desired locations)
C        +16: problem reading fort.83 (analysis on grid)
C        +32: problem reading fort.86 (error on grid)
       DIVAREADRESULT=0
       do i=1,ndata
       read(71,ERR=10) x,y,resudata(i)
       enddo
       goto 20
 10    continue
       DIVAREADRESULT=DIVAREADRESULT+1
 20    continue
       do i=1,nwhere
       read(82,ERR=30) resuwhere(i)
       enddo
       goto 40
 30    continue
       DIVAREADRESULT=DIVAREADRESULT+2
 40    continue
       do i=1,ndata
       read(72,ERR=50) errdata(i)
       enddo
       goto 60
 50    continue
       DIVAREADRESULT=DIVAREADRESULT+4
 60    continue
       do i=1,nwhere
       read(73,ERR=70) errwhere(i)
       enddo
       goto 80
 70    continue
       DIVAREADRESULT=DIVAREADRESULT+8
 80    continue              
       do i=1,NX*NY
       read(83,ERR=90) resugrid(i)
       enddo
       goto 100
 90    continue
       DIVAREADRESULT=DIVAREADRESULT+16
 100   continue
       do i=1,NX*NY
       read(86,ERR=110) errgrid(i)
       enddo
       goto 120
 110    continue
       DIVAREADRESULT=DIVAREADRESULT+32
 120   continue



       return
       end
CCCCCCCCCCCC
 

