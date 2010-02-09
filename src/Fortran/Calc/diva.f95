C!RefCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C!RefC                                                                      C
C!RefC           Data Interpolating Variational Analysis  (4.1)             C
C!RefC           ========================================                   C
C!RefC                                                                      C
C!RefC                     GHER - UNIVERSITY OF LIEGE                       C
C!RefC                                                                      C
C!RefC                        ver 4.1     March 2007                        C
C!RefC                        ver 4.1.2   November 2007                     C
C!RefC                        ver 4.2     Januaray 2008                     C
C!RefC     Jean-Marie Beckers, Damien Sirjacobs, Mohamed Ouberdous          C
C!RefC     Charles Troupin                                                  C
C!RefC                                                                      C
C!RefC                                                                      C
C!RefC   This code is designed to solve 2-D differential or variational     C
C!RefC   problems of elliptic type, using the finite element method.        C
C!RefC                                                                      C
C!RefCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C!Ref
C!RefC**********************************************************************
C!RefC
C!RefC                   List of data files and modules:
C!RefC                   -------------------------------
C!RefC
C!RefC   - General file:                          *FORT.10: module driver
C!RefC   --------------------------------------------------------------------
C!RefC   - MATHPR module
C!RefC   (description of the mathematical problem)
C!RefC                                            *FORT.12: parameters
C!RefC   --------------------------------------------------------------------
C!RefC   - TOPOLO module
C!RefC   (topological data input)
C!RefC                                            *FORT.11: F.E. topology
C!RefC   --------------------------------------------------------------------
C!RefC   - MESHGN module
C!RefC   (generation of a regular f.e. mesh)
C!RefC                                            *FORT.14: topology matrix
C!RefC   --------------------------------------------------------------------
C!RefC   - DATAPR module
C!RefC   (data to be fitted by r.m.s. approximation)
C!RefC                                            *FORT.20: data to be fitted
C!RefC                                            *FORT.22: regression param.
C!RefC   --------------------------------------------------------------------
C!RefC   - BCONDI module
C!RefC   (boundary conditions)
C!RefC                                            *FORT.30: bound. conditions
C!RefC   --------------------------------------------------------------------
C!RefC   - VARSSK
C!RefC   (sources and sinks variable in space)
C!RefC                                            *FORT.40: sources distrib.
C!RefC   --------------------------------------------------------------------
C!RefC   - CONSTR
C!RefC   (additional constraint)
C!RefC                                            *FORT.50: constraint descr.
C!RefC                                                 (nodal properties)
C!RefC   --------------------------------------------------------------------
C!RefC   - SOLVER
C!RefC   (resolution of linear system)            *FORT.60: elementary stifness
C!RefC                                               (pseudo-topology)
C!RefC   --------------------------------------------------------------------
C!RefC   - STORES
C!RefC   (results storage)           *FORT.13: xori,yori,dx,dy
C!RefC                               *FORT.22: regression parameters
C!RefC                               *FORT.79: x,y (sparse points for sol.)
C!RefC                               *FORT.80: x,y,iel,isub (gridded)
C!RefC                               *FORT.81: nodes, ddl, sol
C!RefC                               *FORT.82: field estimate at x,y (sparse)
C!RefC                               *FORT.83: gridded field estimate (ascii)
C!RefC                               *FORT.84: gridded field estimate (standard bimg)
C!RefC
C!RefC   --------------------------------------------------------------------
C!RefC   - ESTERR
C!RefC   (error estimation)
C!RefC                               *FORT.15: variance of background field
C!RefC                               *FORT.85: error estimate at x,y (sparse)
C!RefC                               *FORT.86: gridded error estimate (ascii)
C!RefC                               *FORT.87: gridded error estimate (standard bimg)
C!RefC   --------------------------------------------------------------------
C!RefC   - COORD
C!RefC   (coord change)            works on data read in
C!RefC                               *FORT.20: input data
C!RefC                               *FORT.11: topology
C!RefC
C!RefC   --------------------------------------------------------------------
C!RefC   - GCVFAC
C!RefC   (Generalized Cross Validation
C!RefC                             Estimates the analysis error
C!RefC                             (same grid as the analysis)
C!RefC
C!RefC   --------------------------------------------------------------------
C!RefC   - DATAQC
C!RefC   (data quality check)
C!RefC                             Estimates of expected
C!RefC                             data-analysis differences
C!RefC   - COVAR
C!RefC   (Diva covariance calculation)
C!RefC                             Calculation of Diva kernel
C!RefC                             for subsequent error fields
C!RefC
C!RefC**********************************************************************
C!RefC
C!RefC                       List of temporary files
C!RefC                       ------------------------
C!RefC
C!RefC   - when ITYP=2 :                          *FORT.32: condensation vct
C!RefC
C!RefC**********************************************************************
C!RefC                      Variable names nomenclature
C!RefC                      ---------------------------
C!RefC
C!RefC     - T....   : table of real variables
C!RefC     - K....   : table of integer variables
C!RefC     - N....   : integer, number of smth.
C!RefC     - I....   : integer, index in a sequence
C!RefC
C!RefC     Variables type : implicit real*4 or integer*4
C!RefC                      (unless otherwise stated)
C!RefC
C!RefC***********************************************************************
C!Ref
C!RefC%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C!RefC
C!RefC   SUBROUTINE LIST:
C!RefC     -  (MAIN PROGRAM)
C!RefC
C!RefC%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      include'../Module/Calc/divapre.h'
      include'../Module/Calc/divainc.h'
      
      character*6 modul

      RPI=2*asin(1.D0)
C!Refc      write(6,*) 'Pi',RPI
      IJMB=0
C!RefC  DIMENSION OF STORAGE VECTORS (see include file)
      iprc=4
      iprc=8

C!RefC  By DEFAULT : NO COORDINATE CHANGE REQUESTED
      icoordchange=0
      itcs=0

C!RefC  INPUT/OUTPUT FILES : OPEN statement

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
C!RefC
C!RefC  INPUT OF MODULES (ipr is an indicator of the amount of data
C!RefC                    to be printed)

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
C!RefC#ifdef DIVADYNAMIC
C!RefC      write(6,*) 'Dynamic memory allocation'
C!RefC      write(6,*) 'Integer',NINTDIVA
C!RefC      write(6,*) 'Real',NREADIVA
C!RefC     allocate S(NREADIVA)
C!RefC     allocate L(NINTDIVA)
C!RefC#endif
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
      write(6,*) 'C!RefC!RefC!RefC!Ref Undefined module: execution stopped C!RefC!RefC!RefC!Ref'
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
