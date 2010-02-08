!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C           Data Interpolating Variational Analysis  (4.1)             C
!C           ========================================                   C
!C                                                                      C
!C                     GHER - UNIVERSITY OF LIEGE                       C
!C                                                                      C
!C                        ver 4.1     March 2007                        C
!C                        ver 4.1.2   November 2007                     C
!C                        ver 4.2     Januaray 2008                     C
!C     Jean-Marie Beckers, Damien Sirjacobs, Mohamed Ouberdous          C
!C     Charles Troupin                                                  C
!C                                                                      C
!C                                                                      C
!C   This code is designed to solve 2-D differential or variational     C
!C   problems of elliptic type, using the finite element method.        C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!C**********************************************************************
!C
!C                   List of data files and modules:
!C                   -------------------------------
!C
!C   - General file:                          *FORT.10: module driver
!C   --------------------------------------------------------------------
!C   - MATHPR module
!C   (description of the mathematical problem)
!C                                            *FORT.12: parameters
!C   --------------------------------------------------------------------
!C   - TOPOLO module
!C   (topological data input)
!C                                            *FORT.11: F.E. topology
!C   --------------------------------------------------------------------
!C   - MESHGN module
!C   (generation of a regular f.e. mesh)
!C                                            *FORT.14: topology matrix
!C   --------------------------------------------------------------------
!C   - DATAPR module
!C   (data to be fitted by r.m.s. approximation)
!C                                            *FORT.20: data to be fitted
!C                                            *FORT.22: regression param.
!C   --------------------------------------------------------------------
!C   - BCONDI module
!C   (boundary conditions)
!C                                            *FORT.30: bound. conditions
!C   --------------------------------------------------------------------
!C   - VARSSK
!C   (sources and sinks variable in space)
!C                                            *FORT.40: sources distrib.
!C   --------------------------------------------------------------------
!C   - CONSTR
!C   (additional constraint)
!C                                            *FORT.50: constraint descr.
!C                                                 (nodal properties)
!C   --------------------------------------------------------------------
!C   - SOLVER
!C   (resolution of linear system)            *FORT.60: elementary stifness
!C                                               (pseudo-topology)
!C   --------------------------------------------------------------------
!C   - STORES
!C   (results storage)           *FORT.13: xori,yori,dx,dy
!C                               *FORT.22: regression parameters
!C                               *FORT.79: x,y (sparse points for sol.)
!C                               *FORT.80: x,y,iel,isub (gridded)
!C                               *FORT.81: nodes, ddl, sol
!C                               *FORT.82: field estimate at x,y (sparse)
!C                               *FORT.83: gridded field estimate (ascii)
!C                               *FORT.84: gridded field estimate (standard bimg)
!C
!C   --------------------------------------------------------------------
!C   - ESTERR
!C   (error estimation)
!C                               *FORT.15: variance of background field
!C                               *FORT.85: error estimate at x,y (sparse)
!C                               *FORT.86: gridded error estimate (ascii)
!C                               *FORT.87: gridded error estimate (standard bimg)
!C   --------------------------------------------------------------------
!C   - COORD
!C   (coord change)            works on data read in
!C                               *FORT.20: input data
!C                               *FORT.11: topology
!C
!C   --------------------------------------------------------------------
!C   - GCVFAC
!C   (Generalized Cross Validation
!C                             Estimates the analysis error
!C                             (same grid as the analysis)
!C
!C   --------------------------------------------------------------------
!C   - DATAQC
!C   (data quality check)
!C                             Estimates of expected
!C                             data-analysis differences
!C   - COVAR
!C   (Diva covariance calculation)
!C                             Calculation of Diva kernel
!C                             for subsequent error fields
!C
!C**********************************************************************
!C
!C                       List of temporary files
!C                       ------------------------
!C
!C   - when ITYP=2 :                          *FORT.32: condensation vct
!C
!C**********************************************************************
!C                      Variable names nomenclature
!C                      ---------------------------
!C
!C     - T....   : table of real variables
!C     - K....   : table of integer variables
!C     - N....   : integer, number of smth.
!C     - I....   : integer, index in a sequence
!C
!C     Variables type : implicit real*4 or integer*4
!C                      (unless otherwise stated)
!C
!C***********************************************************************
!
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  (MAIN PROGRAM)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      include'../Module/Calc/divapre.h'
      include'../Module/Calc/divainc.h'
      
      character*6 modul

      RPI=2*asin(1.D0)
!c      write(6,*) 'Pi',RPI
      IJMB=0
!C  DIMENSION OF STORAGE VECTORS (see include file)
      iprc=4
      iprc=8

!C  By DEFAULT : NO COORDINATE CHANGE REQUESTED
      icoordchange=0
      itcs=0

!C  INPUT/OUTPUT FILES : OPEN statement

      open(unit=10,file='fort.10')
      open(unit=11,file='fort.11')
      open(unit=12,file='fort.12')
      open(unit=13,file='fort.13')
      open(unit=14,file='fort.14')
      open(unit=15,file='fort.15')
      open(unit=20,file='fort.20')
      open(unit=22,file='fort.22')
      open(unit=30,file='fort.30')
      open(unit=50,file='fort.50')
      open(unit=60,file='fort.60')
      open(unit=79,file='fort.79')
      open(unit=80,file='fort.80')
      open(unit=81,file='fort.81')
      open(unit=82,file='fort.82')
      open(unit=83,file='fort.83')
      open(unit=84,file='fort.84',form='unformatted')
      open(unit=85,file='fort.85')
      open(unit=86,file='fort.86')
      open(unit=87,file='fort.87',form='unformatted')

      write(6,920)
 920  format(72('&'),/,20x,'D.I.V.A. - 4.2.2 - Execution track ...',/,72('&'),/)
!C
!C  INPUT OF MODULES (ipr is an indicator of the amount of data
!C                    to be printed)

 810  format(a6,i2)
 820  format(/,10x,31('$'),/,10x,'CALL TO MATHPR MODULE: IPR = ',I2,/,10x,31('$'),/)
 830  format(/,10x,31('$'),/,10x,'CALL TO TOPOLO MODULE: IPR = ',I2,/,10x,31('$'),/)
 835  format(/,10x,31('$'),/,10x,'CALL TO MESHGN MODULE: IPR = ',I2,/,10x,31('$'),/)
 840  format(/,10x,31('$'),/,10x,'CALL TO DATAPR MODULE: IPR = ',I2,/,10x,31('$'),/)
 850  format(/,10x,31('$'),/,10x,'CALL TO BCONDI MODULE: IPR = ',I2,/,10x,31('$'),/)
 860  format(/,10x,31('$'),/,10x,'CALL TO CONSTR MODULE: IPR = ',I2,/,10x,31('$'),/)
 880  format(/,10x,31('$'),/,10x,'CALL TO SOLVER MODULE: IPR = ',I2,/,10x,31('$'),/)
 890  format(/,10x,31('$'),/,10x,'CALL TO STORES MODULE: IPR = ',I2,/,10x,31('$'),/)
 900  format(/,10x,31('$'),/,10x,'CALL TO ESTERR MODULE: IPR = ',I2,/,10x,31('$'),/)
 902  format(/,10x,31('$'),/,10x,'CALL TO COVAR  MODULE: IPR = ',I2,/,10x,31('$'),/)
 905  format(/,10x,31('$'),/,10x,'CALL TO COORD  MODULE: IPR = ',I2,/,10x,31('$'),/)
 906  format(/,10x,31('$'),/,10x,'CALL TO GCVFAC MODULE: IPR = ',I2,/,10x,31('$'),/)
 907  format(/,10x,31('$'),/,10x,'CALL TO DATAQC MODULE: IPR = ',I2,/,10x,31('$'),/)
      
      NINTDIVA=nent
      NREADIVA=nrea
      read(5,*,END=1010,ERR=1010) II,JJ
      NINTDIVA=II
      NREADIVA=JJ
 1010 continue
!C#ifdef DIVADYNAMIC
!C      write(6,*) 'Dynamic memory allocation'
!C      write(6,*) 'Integer',NINTDIVA
!C      write(6,*) 'Real',NREADIVA
!C     allocate S(NREADIVA)
!C     allocate L(NINTDIVA)
!C#endif
 10   read(10,810) modul,ipr
      if(modul.eq.'mathpr') then
         write(6,820) ipr
         call mathpr(ipr)
         goto 10
      endif
      if(modul.eq.'topolo') then
         write(6,830) ipr
         call topolo(ipr)
         goto 10
      endif
      if(modul.eq.'meshgn') then
         write(6,835) ipr
         call meshgn(ipr)
         goto 10
      endif
      if(modul.eq.'datapr') then
         write(6,840) ipr
         call datapr(ipr)
         goto 10
      endif
      if(modul.eq.'constr') then
         write(6,860) ipr
         call constr(ipr)
         goto 10
      endif
      if(modul.eq.'bcondi') then
         write(6,850) ipr
         call bcondi(ipr)
         goto 10
      endif
      if(modul.eq.'solver') then
         write(6,880) ipr
         call solver(ipr)
         goto 10
      endif
      if(modul.eq.'stores') then
         write(6,890) ipr
         call stores(ipr)
         goto 10
      endif
      if(modul.eq.'esterr') then
         write(6,900) ipr
         call esterr(ipr)
         goto 10
      endif
      if(modul.eq.'covar') then
         write(6,902) ipr
         call covar(ipr)
         goto 10
      endif
      if(modul.eq.'coord') then
         write(6,905) ipr
         call coord(ipr)
         goto 10
      endif
      if(modul.eq.'gcvfac') then
         write(6,906) ipr
         call gcvfac(ipr)
         goto 10
      endif
      if(modul.eq.'dataqc') then
         write(6,907) ipr
         call dataqc(ipr)
         goto 10
      endif
      
      if(modul.eq.'stopex') goto 999
      write(6,*) '!!!! Undefined module: execution stopped !!!!'
      stop
 999  write(6,*)' MAXIMUM NUMBER OF INTEGER USED: ',IENMAX
      write(6,*)' MAXIMUM NUMBER OF REAL    USED: ',IREMAX
      write(6,*)' PRIOR ESTIMATE OF INTEGER USED: ',NINTDIVA
      write(6,*)' PRIOR ESTIMATE OF REAL    USED: ',NREADIVA
      write(6,910)
 910  format(/,72('&'),/,20x,'D.I.V.A. - 4.2.2 - Execution Completed ! ',/,72('&'))

      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      close(20)
      close(30)
      close(50)
      close(60)
      close(79)
      close(80)
      close(81)
      close(82)
      close(83)
      close(84)
      close(85)
      close(86)
      close(87)
      stop
      end
