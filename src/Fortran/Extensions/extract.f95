!C --------------------------------------------------------------------
!C ---  Simple Interface to Select or Extract Data
!C --------------------------------------------------------------------
!C ---
!C ---  Select a subset of the NetCDF data base
!C ---  MODB output format
!C ---
!C ---       Roland Schoenauen (01 FEB 95)
!C ---
!C ---
!C ---  With extended possible criterion,
!C ---  MED output format added
!C ---  Possibility to extract data (3 posssible output formats)
!C ---
!C ---       Jean-Michel Brankart (10 APR 95)
!C ---
!C ---
!C ---  Extended to MODB and MED formatted Data Bases
!C ---  NetCDF output format added
!C ---
!C ---       Jean-Michel Brankart (22 JUN 95)
!C ---
!C ---
!C --------------------------------------------------------------------

      PROGRAM SELECT

      IMPLICIT NONE

      INTEGER*4 MAX
      PARAMETER ( MAX = 50 )

      INTEGER*4 INPFRM,OUTSPC
      CHARACTER CSFNAM*100, PDFLST*100, NCLST*100, MEDPTH*100
      CHARACTER SELOUT*100, EXTOUT*100

      INTEGER*4 PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,I,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX,NATURE,QCMIN,QCMAX,NDEPTH
      CHARACTER TYPE*3, CNTNAM*100
      REAL*4 DEPSTA,DEPEND,DEPTH(MAX)
      REAL*4 LATMIN,LATMAX,LONMIN,LONMAX

    
      WRITE (6,*) 
      WRITE (6,*) 
      WRITE (6,*) '-----------------------'
      WRITE (6,*) '--- DATA EXTRACTION ---' 
      WRITE (6,*) '-----------------------'
      WRITE (6,*) 

!C --- INPUT SPECIFICATIONS ---

      WRITE (6,*) 'INPUT DATA BASE FORMAT ?'
      WRITE (6,*) ' -1- MODB/MEDATLAS FORMAT'
      WRITE (6,*) ' -2- NetCDF MODB FORMAT'
      WRITE (6,*) ' -3- MED GHER FORMAT'
      READ (5,*) INPFRM
      WRITE (6,*) 
      WRITE (6,*) 'INPUT DATA BASE DESCRIPTION ?'
      IF (INPFRM.EQ.1) THEN
        WRITE (6,*) ' PATH TO A LIST OF THE .PDF FILES TO OPEN ?'
        READ (5,'(A)') PDFLST
        write(6,*) PDFLST
        call chblnk(PDFLST)
        write(6,*) 'after',PDFLST
        write(6,*) LEN(PDFLST)
      ELSEIF (INPFRM.EQ.2) THEN
        WRITE (6,*) ' PATH TO A LIST OF THE .NC FILES TO OPEN ?'
        READ (5,'(A)') NCLST
      ELSEIF (INPFRM.EQ.3) THEN
        WRITE (6,*) ' PATH WHERE THE MED BANK CAN BE FOUND ?'
        READ (5,'(A)') MEDPTH
      ELSE
        STOP 'WRONG INPUT!'
      ENDIF
      WRITE (6,'(2A)') '?????JMN', PDFLST
      WRITE (6,'(2A)') '?????JMN', NCLST
      WRITE (6,'(2A)') '?????JMN', MEDPTH

!C --- OUTPUT SPECIFICATIONS ---

      WRITE (6,*) 'OUTPUT SPECIFICATION ?'
      WRITE (6,*) ' SELECT A SUBSET OF THE ORIGINAL BASE:'
      WRITE (6,*) ' -1- MODB/MEDATLAS FORMAT'
      WRITE (6,*) ' -2- NetCDF MODB FORMAT'
      WRITE (6,*) ' -3- MED GHER FORMAT'
      WRITE (6,*) ' EXTRACT DATA AT A SPECFIED DEPTH:'
      WRITE (6,*) ' -4- TEM/SAL OR SAL/TEM FORMAT: LAT,LON,VAL1,VAL2'
      WRITE (6,*) ' -5- DIVA INTERFACE INPUT FORMAT: LAT,LON,VAL'
      WRITE (6,*) ' -6- QC INPUT FORMAT: LAT,LON,VAL,PR'
      READ (5,*) OUTSPC
      WRITE (6,*) 
      WRITE (6,*) 'OUTPUT DATA BASE DESCRIPTION ?'
      IF (OUTSPC.EQ.1) THEN
        IF (INPFRM.EQ.3) THEN
          STOP 'CANNOT CREATE SUBSET OF A MED FORMATTED DATA BASE!'
        ENDIF
        WRITE (6,*) ' NAME OF THE .PDF OUTPUT FILE ?'
        READ(5,'(A)') SELOUT
        write(6,*) ' .PDF OUTPUT FILE:',SELOUT
      ELSEIF (OUTSPC.EQ.2) THEN
        IF (INPFRM.EQ.3) THEN
          STOP 'CANNOT CREATE SUBSET OF A MED FORMATTED DATA BASE!'
        ENDIF
        WRITE (6,*) ' PATH OF THE EMPTY TARGET DIRECTORY ?'
        READ(5,'(A)') SELOUT
      ELSEIF (OUTSPC.EQ.3) THEN
        IF (INPFRM.EQ.3) THEN
          STOP 'CANNOT CREATE SUBSET OF A MED FORMATTED DATA BASE!'
        ENDIF
        WRITE (6,*) ' PATH OF THE EMPTY TARGET DIRECTORY ?'
        READ(5,'(A)') SELOUT
      ELSEIF (OUTSPC.EQ.4) THEN
        WRITE (6,*) ' NAME OF THE OUTPUT DATA FILE ?'
        READ (5,'(A)') EXTOUT
      ELSEIF (OUTSPC.EQ.5) THEN
        WRITE (6,*) ' NAME OF THE OUTPUT DATA FILE ?'
        READ (5,'(A)') EXTOUT
      ELSEIF (OUTSPC.EQ.6) THEN
        WRITE (6,*) ' NAME OF THE OUTPUT DATA FILE ?'
        READ (5,'(A)') EXTOUT
      ELSE
        STOP 'WRONG OUTPUT!'
      ENDIF
      WRITE(6,*)

!C --- EXTARCTION CRITERION SPECIFICATIONS ---

      WRITE(6,*) 'HEADER EXTRACTION CRITERIA DESCRIPTION ?'
      WRITE(6,*) ' DATE CRITERION:'
      WRITE(6,*) '  -ENTER FIRST_YEAR,LAST_YEAR (YYYY)'
      WRITE(6,*) '         0,0 IF NO RESTRICTION'
      READ (5,*) PERSTA, PEREND
      WRITE(6,*) '  -ENTER STARTING_DATE < ENDING_DATE (MMDD)'
      WRITE(6,*) '         0,0 IF NO RESTRICTION'
      READ (5,*) SPRSTA, SPREND
      WRITE(6,*) '  -NUMBER OF MONTH TO CONSIDER (0 IF NO RESTRICTION)'
      READ (5,*) NBMNTH
      IF (NBMNTH.NE.0) THEN
        WRITE(6,*) '   WHICH ONES (1,2,..)?'
        READ(5,*) (MONTHS(I),I=1,NBMNTH)
      ENDIF
      WRITE(6,*) ' POSITION CRITERION:'
      WRITE(6,*) '  -ENTER NAME OF THE CONTOUR DESCRIPTION FILE'
      WRITE(6,*) '      (* IF NO RESTRICTION)'
      READ (5,'(A)') CNTNAM
      WRITE(6,*) '  -ENTER MINIMAL AND MAXIMAL LATITUDE'
      WRITE(6,*) '      (LATMIN=LATMAX IF NO RESTRICTION)'
      READ (5,*) LATMIN, LATMAX
      WRITE(6,*) '  -ENTER MINIMAL AND MAXIMAL LONGITUDE'
      WRITE(6,*) '      (LONMIN=LONMAX IF NO RESTRICTION)'
      READ (5,*) LONMIN, LONMAX
      WRITE(6,*) ' TYPES OF PROFILES TO EXTRACT (CTD,BOTTLES,BT)'
      WRITE(6,*) '     ENTER YYN,YNY,NYY,YNN,NYN OR NNY' 
      WRITE(6,*) '        OR YYY IF NO RESTRICTION'
      READ (5,'(A)') TYPE
      WRITE(6,*) ' QUALITY FLAGS CRITERION:'
      WRITE(6,*) '  -ENTER RANGE OF ACCEPTABLE GLOBAL FLAG (1,6)'
      WRITE(6,*) '      (GQCMIN=10 IF NO RESTRICTION)'
      READ (5,*) GQCMIN,GQCMAX
      WRITE(6,*)

      WRITE(6,*) 'DATA EXTRACTION CRITERIA DESCRIPTION ?'
      IF (OUTSPC.GT.3) THEN
        WRITE(6,*) ' NATURE OF THE (FIRST [IF OUT=4]) DATA TO EXTRACT:'
        WRITE(6,*) ' -1- TEMPERATURE'
        WRITE(6,*) ' -2- SALINITY'
        READ (5,*) NATURE
        WRITE(6,*) ' QUALITY FLAGS CRITERION:'
        WRITE(6,*) '  -ENTER RANGE OF ACCEPTABLE FLAG (1,6)'
        READ (5,*) QCMIN,QCMAX
        WRITE(6,*) ' NUMBER OF DEPTH OF EXTRACTION ?'
        READ(5,*) NDEPTH
        WRITE(6,*) NDEPTH
        IF (NDEPTH .GT. MAX) STOP 'TOO MANY DEPTHS; RESIZE MAX'
        WRITE(6,*) '   WHICH ONES (10.,20.,..)?'
!C        READ(5,*) (DEPTH(I),I=1,NDEPTH)
        do  I=1,NDEPTH
        read(5,*) DEPTH(I)
        enddo
        WRITE(6,*) (DEPTH(I),I=1,NDEPTH)

      ELSE
        WRITE(6,*) ' ENTER STARTING_DEPTH,ENDING_DEPTH (>=0)'
        WRITE(6,*) '   (DEPSTA=DEPEND IF NO RESTRICTION)'
        READ (5,*) DEPSTA, DEPEND
        WRITE (6,*) DEPSTA, DEPEND
      ENDIF
      WRITE (6,*) 

!C --- CALL THE SELECTION OR EXTRACTION ROUTINE ---

      WRITE (6,*)  INPFRM , OUTSPC
      WRITE (6,*) 'Searching ...'
      IF (OUTSPC.LE.3) THEN
        IF (INPFRM.EQ.1) THEN
          CALL USELMODB(PDFLST,   OUTSPC,SELOUT,                      &
                      PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,      &
                      CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,     &
                      GQCMIN,GQCMAX,   DEPSTA,DEPEND)
        ELSEIF (INPFRM.EQ.2) THEN
          CALL USELCDF(NCLST,   OUTSPC,SELOUT,                         &
                      PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,       &
                      CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,      &
                      GQCMIN,GQCMAX,   DEPSTA,DEPEND)
        ELSEIF (INPFRM.EQ.3) THEN
          STOP 'CANNOT CREATE SUBSET OF A MED FORMATTED DATA BASE!'
        ENDIF
      ELSE
        IF (INPFRM.EQ.1) THEN
          CALL UEXTMODB(PDFLST, OUTSPC,EXTOUT,                          &
                      PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,        &
                      CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,       &
                      GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,              &
                      NDEPTH,DEPTH)
        ELSEIF (INPFRM.EQ.2) THEN
          CALL UEXTCDF(NCLST,   OUTSPC,EXTOUT,                           &
                      PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,         &
                      CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,        &
                      GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,               &
                      NDEPTH,DEPTH)
        ELSEIF (INPFRM.EQ.3) THEN
          CALL UEXTMED(MEDPTH, OUTSPC,EXTOUT,                             &
                      PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,          &
                      CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,         &
                      GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,                &
                      NDEPTH,DEPTH)
        ENDIF
      ENDIF

      END
!C ---------------------------------------------------------------------
!C --- This subroutine extracts data from the main MODB Data Base
!C ---
!C ---      INPUT FORMAT = NetCDF DATA FORMAT
!C ---      ACTION = create a subset of the original data base
!C ---               (with possible format translation)
!C ---
!C --- Several criterions can be used :
!C ---
!C ---    1) Date
!C ---    2) Position
!C ---    3) Type of data
!C ---    4) Depth
!C ---    5) Quality Flags
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---    NCLST  :  File containing the list of the Netcdf files
!C ---                                                   (CHAR*100)
!C ---    OUTSPC :  1 = MODB/MEDATLAS output
!C ---              2 = NetCDF output
!C ---              3 = MED output                       (INT*4)
!C ---    SELOUT :  Description of the output file(s)    (CHAR*100)
!C ---    PERSTA :  period start (year in YYYY)          (INT*4)
!C ---    PEREND :  period end   (year in YYYY)          (INT*4)
!C ---    SPRSTA :  sub-period start (MMDD)              (INT*4)
!C ---    SPREND :  sub-period end   (MMDD)              (INT*4)
!C ---    NBMNTH :  number of months to extract          (INT*4)
!C ---    MONTHS :  list of months to extract            (INT*4)
!C ---    CNTNAM :  file containing the contour          (CHAR*100)
!C ---    LATMIN :  min latitude of domain (in deg.min)  (REAL*4)
!C ---    LATMAX :  max latitude of domain (in deg.min)  (REAL*4)
!C ---    LONMIN :  min longitude of domain (in deg.min) (REAL*4)
!C ---    LONMAX :  max longitude of domain (in deg.min) (REAL*4)
!C ---    TYPE   :  data type (YYY for H09, H10 and H13) (CHAR*3)
!C ---    GQCMIN :  minimal flag                         (INT*4)
!C ---    GQCMAX :  maximal flag                         (INT*4)
!C ---    DEPSTA :  depth start  (in positive meters)    (REAL*4)
!C ---    DEPEND :  depth end    (in positive meters)    (REAL*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen  (20 Oct 94)    (reading NetCDF files)
!C ---  Jean-Michel Brankart (10 Apr 95)
!C ---  Jean-Michel Brankart (20 Jun 95)  (NetCDF output format added)
!C ---------------------------------------------------------------------

      SUBROUTINE USELCDF(NCLST,   OUTSPC,SELOUT,                           &
                        PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,         &
                        CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,        &
                        GQCMIN,GQCMAX,   DEPSTA,DEPEND)

      IMPLICIT NONE

      INCLUDE 'netcdf.inc'

      INTEGER*4 MAX
      PARAMETER (MAX=5000)

!C --- Variables for the extraction procedure ---

      INTEGER*4 OUTSPC,PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX
      REAL*4 DEPSTA,DEPEND,LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER NCLST*100, SELOUT*100, CNTNAM*100, TYPE*3

      INTEGER*4 J,I,N,DDMM,MMDD,YYYY,K,NEWREC,II,MM,LL,STRLG
      REAL*4 XLON,XLAT,LONLAT(2)
      CHARACTER LINE(50)*80, CR*13, PDFNAM*17,PATH*100
      CHARACTER PDF*200 , FILENM*200 , CHMNTH*2
      LOGICAL MTEST,CTEST,INAREA

!C --- Variables for reading NetCDF Files ---

      INTEGER RECID,LEVID,LATID,LONID,CHAID,TEXID,DIMID
      INTEGER IDLAT,IDLON,IDDAT,IDLEH,IDLED,IDHEA,IDATA,IDTYP

      INTEGER RCODE,NCID,DIMREC, NTEXT , NCID2
      INTEGER START(4),COUNT(4)

      CHARACTER TEXT(20)*80,NAME(10)*35, NAME2*50

!C --- Other Variables ---

      REAL*4 XQC (9000)

!C --- Variables for reading MODB Files ---

      INTEGER*4 IU,DIAG
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,IQCPRO
      INTEGER*4 QCP(MAX),QCT(MAX),QCS(MAX),DATE,TIME,DEPTH
      INTEGER*4 LAT2,LON2

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1,CHAR1

      REAL*4  LAT3,LON3

      REAL*4 PRES(MAX),TEM(MAX),SAL(MAX)

!C-----End variable declaration---------------------------------------------


!C --- Open input data base and prepare output files ---
      write(6,*) 'Retrieving list of files',NCLST
      OPEN (UNIT=10,FILE=NCLST)
      DO I=1,10
        READ(10,'(A)') NAME(I)
        write(6,*) 'filename',NAME(I)
      ENDDO
      CLOSE(10)

      IF (OUTSPC.EQ.1) THEN
        OPEN (UNIT=12,FILE=SELOUT)
      ELSEIF (OUTSPC.EQ.2) THEN
        LL=STRLG(SELOUT)
        CALL FGENNC(15,SELOUT(1:LL)//'/modb_0015.nc')
        CALL FGENNC(30,SELOUT(1:LL)//'/modb_0030.nc')
        CALL FGENNC(60,SELOUT(1:LL)//'/modb_0060.nc')
        CALL FGENNC(100,SELOUT(1:LL)//'/modb_0100.nc')
        CALL FGENNC(500,SELOUT(1:LL)//'/modb_0500.nc')
        CALL FGENNC(1000,SELOUT(1:LL)//'/modb_1000.nc')
        CALL FGENNC(1500,SELOUT(1:LL)//'/modb_1500.nc')
        CALL FGENNC(2000,SELOUT(1:LL)//'/modb_2000.nc')
        CALL FGENNC(2500,SELOUT(1:LL)//'/modb_2500.nc')
        CALL FGENNC(3000,SELOUT(1:LL)//'/modb_3000.nc')
      ELSEIF (OUTSPC.EQ.3) THEN
        LL=STRLG(SELOUT)
        IF (NBMNTH.EQ.0) THEN
          II=12
          DO I = 1,12
            MONTHS(I) = I
          ENDDO         
        ELSE
          II=NBMNTH 
        ENDIF
        DO I = 1 , II
          WRITE(CHMNTH,'(I2.2)') MONTHS(I)
          FILENM=SELOUT(1:LL)//'/m'//CHMNTH//'.med'
          OPEN (UNIT=70+MONTHS(I),FILE=FILENM)
        ENDDO
      ENDIF
         

!C --- Loop over the 10 NetCDF input files ---

      DO I = 1,10

         NCID = NCOPN (NAME(I),NCNOWRIT,RCODE)

!C --- Get Information on NetCDF File ---

         RECID = NCDID (NCID,'record',RCODE)

         CALL NCDINQ (NCID,RECID,NAME2,DIMREC,RCODE)

         IDLAT = NCVID (NCID,'lat',RCODE)
         IDLON = NCVID (NCID,'lon',RCODE)
         IDDAT = NCVID (NCID,'date',RCODE)
         IDTYP = NCVID (NCID,'type',RCODE)
         IDLEH = NCVID (NCID,'lenhead',RCODE)
         IDLED = NCVID (NCID,'lendata',RCODE)
         IDHEA = NCVID (NCID,'header',RCODE)
         IDATA = NCVID (NCID,'data',RCODE)

!C --- Close File if DIMREC = 0 ---
         IF (DIMREC .EQ. 0) GOTO 20

!C --- Make Loop on DIMREC (profiles) ---

         DO J = 1,DIMREC

!C --- Read LAT, LON, DATE AND TYPE ---

            CALL NCVGT1 (NCID,IDLAT,J,XLAT,RCODE)
            CALL NCVGT1 (NCID,IDLON,J,XLON,RCODE)
            CALL NCVGT1 (NCID,IDDAT,J,DATE,RCODE)

            START(1) = 1
            START(2) = J

            COUNT (1) = 3
            COUNT (2) = 1
       
            CALL NCVGTC (NCID,IDTYP,START,COUNT,DTYPE,3,RCODE)


!C --- Select on Type, Position and Date  ---

            DDMM = INT (DATE / 10000)
            MMDD = DDMM / 100 + (DDMM - (DDMM / 100)*100) * 100
            MM   = MMDD / 100
            YYYY = DATE - DDMM * 10000

            IF ( ( (DTYPE.EQ.'H10') .AND. (TYPE(1:1).EQ.'Y') ) .OR.          &
                ( (DTYPE.EQ.'H09') .AND. (TYPE(2:2).EQ.'Y') ) .OR.          &
                ( (DTYPE.EQ.'H13') .AND. (TYPE(3:3).EQ.'Y') ) ) THEN
            IF ( (LATMIN .LE. XLAT  .AND. LATMAX .GE. XLAT) .OR.             &
                (LATMIN .EQ. LATMAX) )  THEN
            IF ( (LONMIN .LE. XLON  .AND. LONMAX .GE. XLON) .OR.             &
                (LONMIN .EQ. LONMAX) )  THEN
            IF ( (PERSTA .LE. YYYY  .AND. PEREND .GE. YYYY) .OR.             &
                (PERSTA .EQ. 0     .AND. PEREND .EQ. 0   ) ) THEN
            IF ( (SPRSTA .LE. MMDD  .AND. SPREND .GE. MMDD) .OR.             &
                (SPRSTA .EQ. 0     .AND. SPREND .EQ. 0   ) ) THEN

            MTEST = NBMNTH .EQ. 0
            IF (.NOT.(MTEST)) THEN
              DO K = 1,NBMNTH
                MTEST = MTEST .OR. ( MM .EQ. MONTHS(K) )
              ENDDO
            ENDIF
            IF (MTEST) THEN

            LONLAT(1) = XLON
            LONLAT(2) = XLAT
            IF ( .NOT. ( CNTNAM(1:1) .EQ. '*' ) ) THEN
              CTEST = INAREA ( LONLAT , CNTNAM ) 
            ELSE
              CTEST = .TRUE.
            ENDIF
            IF (CTEST) THEN

!C --- Read Entire HEADER  ---

               CALL NCVGT1 (NCID,IDLEH,J,NTEXT,RCODE)
               CALL NCVGT1 (NCID,IDLED,J,NBREC,RCODE)
               
               START(1) = 1
               START(2) = 1
               START(3) = J

               COUNT (1) = 80
               COUNT (2) = NTEXT
               COUNT (3) = 1

               CALL NCVGTC (NCID,IDHEA,START,COUNT,TEXT,NTEXT*80,RCODE)

!C --- Select on the value of QCPRO

               DO II = 1,NTEXT
                  IF    (TEXT(II)(1:7).EQ.'*GLOBAL' .OR.         &
                        TEXT(II)(1:7).EQ.'*Global' .OR.          &
                        TEXT(II)(1:7).EQ.'*global') THEN
                         READ (TEXT(II),307) QCPRO
                         IQCPRO = 1
                  ENDIF
               ENDDO

               IF (IQCPRO .EQ. 0) THEN
!c                  WRITE (6,*) 'No GLOBAL PROFILE  PROFILE QC found !'
                   QCPRO=0
               ENDIF

               IF ( (GQCMIN .LE. QCPRO .AND. GQCMAX .GE. QCPRO) .OR. (GQCMIN .EQ. 10) ) THEN

!C --- Read Profile Data

               START(1) = 1
               START(2) = 1
               START(3) = J

               COUNT (1) = NBREC
               COUNT (2) = 1
               COUNT (3) = 1

               CALL NCVGT (NCID,IDATA,START,COUNT,PRES,RCODE)

               START(2) = 2
               CALL NCVGT (NCID,IDATA,START,COUNT,TEM,RCODE)
      
               START(2) = 3
               CALL NCVGT (NCID,IDATA,START,COUNT,SAL,RCODE)

               START(2) = 4
               CALL NCVGT (NCID,IDATA,START,COUNT,XQC,RCODE)

               DO K = 1,NBREC
                  QCP(K) = INT (XQC(K) / 100.)
                  QCT(K) = INT ((XQC(K) - QCP(K)*100.) / 10.)
                  QCS(K) = INT (XQC(K) - QCP(K)*100. - QCT(K) * 10.)
               ENDDO

!C --- Select according to depth

            NEWREC = 0
            DO K=1,NBREC
              IF ( (PRES(K) .GE. DEPSTA .AND. PRES(K) .LE. DEPEND) .OR. (DEPSTA .EQ. DEPEND) )        THEN
                NEWREC = NEWREC + 1
                PRES(NEWREC) = PRES(K)
                SAL(NEWREC) = SAL(K)
                TEM(NEWREC) = TEM(K)
              ENDIF
            ENDDO

!C --- Writing output (MODB / NetCDF / MED  formats)

!C           WRITE(6,*) 'Extracting ',TEXT(1)(2:19),' ...'
            IF (OUTSPC.EQ.1) THEN

              CALL WPDF3 (12,1,1,QC1,QC2,QC3,QC4,NBPAR,NEWREC,QCPRO,    &
                     QCP,QCT,QCS,PR,DTYPE,DATE,TIME,DEPTH,LAT1,LON1,    &
                     LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,TEXT,NTEXT)

              WRITE (12,*)

            ELSEIF (OUTSPC.EQ.2) THEN


!C-------------------------------------------NetCDF-output-code-(start)---
         NBREC=NEWREC
         LL=STRLG(SELOUT)

!C --- Open the right NetCDF file


         IF (NBREC .LE. 15) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0015.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 30) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0030.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 60) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0060.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 100) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0100.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 1000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_1000.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 1500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_1500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 2000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_2000.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 2500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_2500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 3000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_3000.nc',NCWRITE,RCODE)

         ELSE
            WRITE (6,*) 'There is a problem, profile is too long ...'
            WRITE (6,*) 'MAX = 3000'
            WRITE (6,*) 'NBREC = ',NBREC
            STOP

         ENDIF

         RECID = NCDID (NCID2,'record',RCODE)
         LEVID = NCDID (NCID2,'level',RCODE)
         CHAID = NCDID (NCID2,'charlen',RCODE)
         TEXID = NCDID (NCID2,'textlen',RCODE)
         DIMID = NCDID (NCID2,'dim4',RCODE)

         CALL NCDINQ (NCID2,RECID,NAME2,DIMREC,RCODE)

         IDLAT = NCVID (NCID2,'lat',RCODE)
         IDLON = NCVID (NCID2,'lon',RCODE)
         IDDAT = NCVID (NCID2,'date',RCODE)
         IDTYP = NCVID (NCID2,'type',RCODE)
         IDLEH = NCVID (NCID2,'lenhead',RCODE)
         IDLED = NCVID (NCID2,'lendata',RCODE)
         IDHEA = NCVID (NCID2,'header',RCODE)
         IDATA = NCVID (NCID2,'data',RCODE)

         N = DIMREC + 1

         CALL NCVPT1 (NCID2,IDLAT,N,XLAT,RCODE)
         CALL NCVPT1 (NCID2,IDLON,N,XLON,RCODE)
         CALL NCVPT1 (NCID2,IDDAT,N,DATE,RCODE)
         CALL NCVPT1 (NCID2,IDLEH,N,NTEXT,RCODE)
         CALL NCVPT1 (NCID2,IDLED,N,NBREC,RCODE)

         START(1) = 1
         START(2) = N

         COUNT (1) = 3
         COUNT (2) = 1

         CALL NCVPTC (NCID2,IDTYP,START,COUNT,DTYPE,3,RCODE)

         START(1) = 1
         START(2) = 1
         START(3) = N

         COUNT (1) = 80
         COUNT (2) = NTEXT
         COUNT (3) = 1

         CALL NCVPTC (NCID2,IDHEA,START,COUNT,TEXT,NTEXT*80,RCODE)

         START(1) = 1
         START(2) = 1
         START(3) = N

         COUNT (1) = NBREC
         COUNT (2) = 1
         COUNT (3) = 1

         CALL NCVPT (NCID2,IDATA,START,COUNT,PRES,RCODE)

         START(2) = 2
         CALL NCVPT (NCID2,IDATA,START,COUNT,TEM,RCODE)

         START(2) = 3
         CALL NCVPT (NCID2,IDATA,START,COUNT,SAL,RCODE)

         START(2) = 4
         CALL NCVPT (NCID2,IDATA,START,COUNT,XQC,RCODE)

         CALL NCCLOS (NCID2,RCODE)
!C--------------------------------------------NetCDF-output-code-(end)------
              

            ELSEIF (OUTSPC.EQ.3) THEN

              WRITE (70+MM,FMT=100) TEXT(1)(2:19),DATE,XLAT,XLON,QC1,QC2,QC3,QCPRO,NEWREC,DTYPE
              DO K = 1,NEWREC
                 WRITE (70+MM,FMT=101) PRES(K),TEM(K),SAL(K),QCP(K),QCT(K),QCS(K)
              ENDDO

            ENDIF


            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF

         ENDDO

20       CALL NCCLOS (NCID,RCODE)

      ENDDO

!C --- Close ouput files ---

      IF (OUTSPC.EQ.1) THEN
        CLOSE (12)
      ELSEIF (OUTSPC.EQ.3) THEN
        IF (NBMNTH.EQ.0) THEN
          II = 12
        ELSE
          II = NBMNTH
        ENDIF
        DO I = 1 , II 
          CLOSE (70+MONTHS(I))
        ENDDO
      ENDIF

!C --- Format Declaration ---

100   FORMAT (A18,1x,I8.8,1x,F8.3,1x,F8.3,1x,3(I1),1x,I1,1x,I4.4,1x,A3)
101   FORMAT (f6.1,1x,2(F6.3,1x),3(I1))
307   FORMAT (29x,i1)

      END
!C ---------------------------------------------------------------------
!C --- This subroutine extracts data from the main MODB Data Base
!C ---
!C ---      INPUT FORMAT = MODB/MEDATLAS DATA FORMAT
!C ---      ACTION = create a subset of the original data base
!C ---               (with possible format translation)
!C ---
!C --- Several criterions can be used :
!C ---
!C ---    1) Date
!C ---    2) Position
!C ---    3) Type of data
!C ---    4) Depth
!C ---    5) Quality Flags
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---    NCLST  :  File containing the list of the Netcdf files
!C ---                                                   (CHAR*100)
!C ---    OUTSPC :  1 = MODB/MEDATLAS output
!C ---              2 = NetCDF output
!C ---              3 = MED output                       (INT*4)
!C ---    SELOUT :  Description of the output file(s)    (CHAR*100)
!C ---    PERSTA :  period start (year in YYYY)          (INT*4)
!C ---    PEREND :  period end   (year in YYYY)          (INT*4)
!C ---    SPRSTA :  sub-period start (MMDD)              (INT*4)
!C ---    SPREND :  sub-period end   (MMDD)              (INT*4)
!C ---    NBMNTH :  number of months to extract          (INT*4)
!C ---    MONTHS :  list of months to extract            (INT*4)
!C ---    CNTNAM :  file containing the contour          (CHAR*100)
!C ---    LATMIN :  min latitude of domain (in deg.min)  (REAL*4)
!C ---    LATMAX :  max latitude of domain (in deg.min)  (REAL*4)
!C ---    LONMIN :  min longitude of domain (in deg.min) (REAL*4)
!C ---    LONMAX :  max longitude of domain (in deg.min) (REAL*4)
!C ---    TYPE   :  data type (YYY for H09, H10 and H13) (CHAR*3)
!C ---    GQCMIN :  minimal flag                         (INT*4)
!C ---    GQCMAX :  maximal flag                         (INT*4)
!C ---    DEPSTA :  depth start  (in positive meters)    (REAL*4)
!C ---    DEPEND :  depth end    (in positive meters)    (REAL*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen  (20 Oct 94)    (reading NetCDF files)
!C ---  Jean-Michel Brankart (10 Apr 95)
!C ---  Jean-Michel Brankart (20 Jun 95)  (NetCDF output format added)
!C ---------------------------------------------------------------------

      SUBROUTINE USELMODB(PDFLST,   OUTSPC,SELOUT,                    &
                        PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,    &
                        CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,   &
                        GQCMIN,GQCMAX,   DEPSTA,DEPEND)

      IMPLICIT NONE

      INCLUDE 'netcdf.inc'

      INTEGER*4 MAX
      PARAMETER (MAX=5000)

!C --- Variables for the extraction procedure ---

      INTEGER*4 OUTSPC,PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX
      REAL*4 DEPSTA,DEPEND,LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER PDFLST*100, SELOUT*100, CNTNAM*100, TYPE*3

      INTEGER*4 J,I,N,DDMM,MMDD,YYYY,K,NEWREC,II,MM,LL,STRLG
      REAL*4 XLON,XLAT,LONLAT(2)
      CHARACTER LINE(50)*80, CR*13, PDFNAM*17,PATH*100
      CHARACTER PDF*200 , FILENM*200 , CHMNTH*2
      CHARACTER PDFNAME*200
      LOGICAL MTEST,CTEST,INAREA

!C --- Variables for reading NetCDF Files ---

      INTEGER RECID,LEVID,LATID,LONID,CHAID,TEXID,DIMID
      INTEGER IDLAT,IDLON,IDDAT,IDLEH,IDLED,IDHEA,IDATA,IDTYP

      INTEGER RCODE,DIMREC, NTEXT , NCID2
      INTEGER START(4),COUNT(4)

      CHARACTER TEXT(20)*80, NAME(10)*35, NAME2*50

!C --- Other Variables ---

      REAL*4 XQC (9000)

!C --- Variables for reading MODB Files ---

      INTEGER*4 IU,DIAG
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,IQCPRO
      INTEGER*4 QCP(MAX),QCT(MAX),QCS(MAX),DATE,TIME,DEPTH
      INTEGER*4 LAT2,LON2,IDEPTH

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1,CHAR1

      REAL*4  LAT3,LON3

      REAL*4 PRES(MAX),TEM(MAX),SAL(MAX)

!C-----End variable declaration---------------------------------------------


!C --- Open input data base and prepare output files ---
      write(6,*) 'PDFLIST', PDFLST
      OPEN (UNIT=10,FILE=PDFLST)
      
      IF (OUTSPC.EQ.1) THEN
        OPEN (UNIT=12,FILE=SELOUT)
      ELSEIF (OUTSPC.EQ.2) THEN
        LL=STRLG(SELOUT)
        CALL FGENNC(15,SELOUT(1:LL)//'/modb_0015.nc')
        CALL FGENNC(30,SELOUT(1:LL)//'/modb_0030.nc')
        CALL FGENNC(60,SELOUT(1:LL)//'/modb_0060.nc')
        CALL FGENNC(100,SELOUT(1:LL)//'/modb_0100.nc')
        CALL FGENNC(500,SELOUT(1:LL)//'/modb_0500.nc')
        CALL FGENNC(1000,SELOUT(1:LL)//'/modb_1000.nc')
        CALL FGENNC(1500,SELOUT(1:LL)//'/modb_1500.nc')
        CALL FGENNC(2000,SELOUT(1:LL)//'/modb_2000.nc')
        CALL FGENNC(2500,SELOUT(1:LL)//'/modb_2500.nc')
        CALL FGENNC(3000,SELOUT(1:LL)//'/modb_3000.nc')
      ELSEIF (OUTSPC.EQ.3) THEN
        LL=STRLG(SELOUT)
        IF (NBMNTH.EQ.0) THEN
          II=12
          DO I = 1,12
            MONTHS(I) = I
          ENDDO         
        ELSE
          II=NBMNTH 
        ENDIF
        DO I = 1 , II
          WRITE(CHMNTH,'(I2.2)') MONTHS(I)
          FILENM=SELOUT(1:LL)//'/m'//CHMNTH//'.med'
          OPEN (UNIT=70+MONTHS(I),FILE=FILENM)
        ENDDO
      ENDIF

!C --- Loop over the PDF input files ---

 100  CONTINUE
         READ(10,'(A)',END=200) PDFNAME
        WRITE(6,'(2A)') 'Opening ',PDFNAME

!C --- Loop over profiles in the PDF file

         OPEN(UNIT=20,FILE=PDFNAME)
 300     CONTINUE


            CALL RPDF2 (20,IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,         &
                      QCP,QCT,QCS,PR,DTYPE,DATE,TIME,IDEPTH,LAT1,LON1,      &
                      LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,DIAG,TEXT,NTEXT)

            IF (DIAG .EQ. 1) GOTO 400

!C           WRITE(6,'(2A)')'Searching in ',PR

            CALL TOPOS ( XLAT , XLON , LAT1 , LAT2 , LAT3 , LON1 , LON2 , LON3 )

!C --- Select on Type, Position and Date  ---

            DDMM = INT (DATE / 10000)
            MMDD = DDMM / 100 + (DDMM - (DDMM / 100)*100) * 100
            MM   = MMDD / 100
            YYYY = DATE - DDMM * 10000

            IF ( ( (DTYPE.EQ.'H10') .AND. (TYPE(1:1).EQ.'Y') ) .OR.   &
                ( (DTYPE.EQ.'H09') .AND. (TYPE(2:2).EQ.'Y') ) .OR.    &
                ( (DTYPE.EQ.'H13') .AND. (TYPE(3:3).EQ.'Y') ) ) THEN
            IF ( (LATMIN .LE. XLAT  .AND. LATMAX .GE. XLAT) .OR.      &
                (LATMIN .EQ. LATMAX) )  THEN
            IF ( (LONMIN .LE. XLON  .AND. LONMAX .GE. XLON) .OR.      &
                (LONMIN .EQ. LONMAX) )  THEN
            IF ( (PERSTA .LE. YYYY  .AND. PEREND .GE. YYYY) .OR.      &
                (PERSTA .EQ. 0     .AND. PEREND .EQ. 0   ) ) THEN
            IF ( (SPRSTA .LE. MMDD  .AND. SPREND .GE. MMDD) .OR.      &
                (SPRSTA .EQ. 0     .AND. SPREND .EQ. 0   ) ) THEN

            MTEST = NBMNTH .EQ. 0
            IF (.NOT.(MTEST)) THEN
              DO K = 1,NBMNTH
                MTEST = MTEST .OR. ( MM .EQ. MONTHS(K) )
              ENDDO
            ENDIF
            IF (MTEST) THEN

            LONLAT(1) = XLON
            LONLAT(2) = XLAT
            IF ( .NOT. ( CNTNAM(1:1) .EQ. '*' ) ) THEN
              CTEST = INAREA ( LONLAT , CNTNAM ) 
            ELSE
              CTEST = .TRUE.
            ENDIF
            IF (CTEST) THEN

!C --- Select on the value of QCPRO

               IF ( (GQCMIN .LE. QCPRO .AND. GQCMAX .GE. QCPRO) .OR. (GQCMIN .EQ. 10) ) THEN

!C --- Select according to depth

            NEWREC = 0
            DO K=1,NBREC
              IF ( (PRES(K) .GE. DEPSTA .AND. PRES(K) .LE. DEPEND) .OR. (DEPSTA .EQ. DEPEND) )        THEN
                NEWREC = NEWREC + 1
                PRES(NEWREC) = PRES(K)
                SAL(NEWREC) = SAL(K)
                TEM(NEWREC) = TEM(K)
              ENDIF
            ENDDO

!C --- Writing output (MODB / NetCDF / MED  formats)

!C           WRITE(6,*) 'Extracting ',TEXT(1)(2:19),' ...'
            IF (OUTSPC.EQ.1) THEN

              CALL WPDF2 (12,1,1,QC1,QC2,QC3,QC4,NBPAR,NEWREC,QCPRO,   &
                     QCP,QCT,QCS,PR,DTYPE,DATE,TIME,IDEPTH,LAT1,LON1,  &
                     LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,TEXT,NTEXT)

              WRITE (12,*)

            ELSEIF (OUTSPC.EQ.2) THEN


!C-------------------------------------------NetCDF-output-code-(start)---
         NBREC=NEWREC
         LL=STRLG(SELOUT)

!C --- Open the right NetCDF file


         IF (NBREC .LE. 15) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0015.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 30) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0030.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 60) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0060.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 100) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0100.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_0500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 1000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_1000.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 1500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_1500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 2000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_2000.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 2500) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_2500.nc',NCWRITE,RCODE)

         ELSEIF (NBREC .LE. 3000) THEN
            NCID2 = NCOPN (SELOUT(1:LL)//'/modb_3000.nc',NCWRITE,RCODE)

         ELSE
            WRITE (6,*) 'There is a problem, profile is too long ...'
            WRITE (6,*) 'MAX = 3000'
            WRITE (6,*) 'NBREC = ',NBREC
            STOP

         ENDIF

         RECID = NCDID (NCID2,'record',RCODE)
         LEVID = NCDID (NCID2,'level',RCODE)
         CHAID = NCDID (NCID2,'charlen',RCODE)
         TEXID = NCDID (NCID2,'textlen',RCODE)
         DIMID = NCDID (NCID2,'dim4',RCODE)

         CALL NCDINQ (NCID2,RECID,NAME2,DIMREC,RCODE)

         IDLAT = NCVID (NCID2,'lat',RCODE)
         IDLON = NCVID (NCID2,'lon',RCODE)
         IDDAT = NCVID (NCID2,'date',RCODE)
         IDTYP = NCVID (NCID2,'type',RCODE)
         IDLEH = NCVID (NCID2,'lenhead',RCODE)
         IDLED = NCVID (NCID2,'lendata',RCODE)
         IDHEA = NCVID (NCID2,'header',RCODE)
         IDATA = NCVID (NCID2,'data',RCODE)

         N = DIMREC + 1

         CALL NCVPT1 (NCID2,IDLAT,N,XLAT,RCODE)
         CALL NCVPT1 (NCID2,IDLON,N,XLON,RCODE)
         CALL NCVPT1 (NCID2,IDDAT,N,DATE,RCODE)
         CALL NCVPT1 (NCID2,IDLEH,N,NTEXT,RCODE)
         CALL NCVPT1 (NCID2,IDLED,N,NBREC,RCODE)

         START(1) = 1
         START(2) = N

         COUNT (1) = 3
         COUNT (2) = 1

         CALL NCVPTC (NCID2,IDTYP,START,COUNT,DTYPE,3,RCODE)

         START(1) = 1
         START(2) = 1
         START(3) = N

         COUNT (1) = 80
         COUNT (2) = NTEXT
         COUNT (3) = 1

         CALL NCVPTC (NCID2,IDHEA,START,COUNT,TEXT,NTEXT*80,RCODE)

         START(1) = 1
         START(2) = 1
         START(3) = N

         COUNT (1) = NBREC
         COUNT (2) = 1
         COUNT (3) = 1

         CALL NCVPT (NCID2,IDATA,START,COUNT,PRES,RCODE)

         START(2) = 2
         CALL NCVPT (NCID2,IDATA,START,COUNT,TEM,RCODE)

         START(2) = 3
         CALL NCVPT (NCID2,IDATA,START,COUNT,SAL,RCODE)

         START(2) = 4
         CALL NCVPT (NCID2,IDATA,START,COUNT,XQC,RCODE)

         CALL NCCLOS (NCID2,RCODE)
!C--------------------------------------------NetCDF-output-code-(end)------
              

            ELSEIF (OUTSPC.EQ.3) THEN

              WRITE (70+MM,FMT=102) TEXT(1)(2:19),DATE,XLAT,XLON,QC1,QC2,QC3,QCPRO,NEWREC,DTYPE
              DO K = 1,NEWREC
                 WRITE (70+MM,FMT=101) PRES(K),TEM(K),SAL(K),QCP(K),QCT(K),QCS(K)
              ENDDO

            ENDIF


            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF

            GOTO 300
 400     CONTINUE
         CLOSE(20)
         GOTO 100
 200  CONTINUE

      CLOSE(10)

!C --- Close ouput files ---

      IF (OUTSPC.EQ.1) THEN
        CLOSE (12)
      ELSEIF (OUTSPC.EQ.3) THEN
        IF (NBMNTH.EQ.0) THEN
          II = 12
        ELSE
          II = NBMNTH
        ENDIF
        DO I = 1 , II 
          CLOSE (70+MONTHS(I))
        ENDDO
      ENDIF

!C --- Format Declaration ---

102   FORMAT (A18,1x,I8.8,1x,F8.3,1x,F8.3,1x,3(I1),1x,I1,1x,I4.4,1x,A3)
101   FORMAT (f6.1,1x,2(F6.3,1x),3(I1))
307   FORMAT (29x,i1)

      END
!C ---------------------------------------------------------------------
!C --- This subroutine extracts data from the main MODB Data Base
!C ---
!C ---      INPUT FORMAT = NetCDF DATA FORMAT
!C ---      ACTION = extract lists of data from the original data base
!C ---               (in the perspective of analysis or control)
!C ---
!C --- Several criterions can be used :
!C ---
!C ---    1) Date
!C ---    2) Position
!C ---    3) Type of data
!C ---    4) Depth
!C ---    5) Quality Flags
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---    NCLST  :  File containing the list of the Netcdf files
!C ---                                                   (CHAR*100)
!C ---    OUTSPC :  4 = TEM/SAL or SAL/TEM
!C ---              5 = Diva Interface Input Format
!C ---              6 = QC Input Format                  (INT*4)
!C ---    EXTOUT :  Description of the output file(s)    (CHAR*100)
!C ---    PERSTA :  period start (year in YYYY)          (INT*4)
!C ---    PEREND :  period end   (year in YYYY)          (INT*4)
!C ---    SPRSTA :  sub-period start (MMDD)              (INT*4)
!C ---    SPREND :  sub-period end   (MMDD)              (INT*4)
!C ---    NBMNTH :  number of months to extract          (INT*4)
!C ---    MONTHS :  list of months to extract            (INT*4)
!C ---    CNTNAM :  file containing the contour          (CHAR*100)
!C ---    LATMIN :  min latitude of domain (in deg.min)  (REAL*4)
!C ---    LATMAX :  max latitude of domain (in deg.min)  (REAL*4)
!C ---    LONMIN :  min longitude of domain (in deg.min) (REAL*4)
!C ---    LONMAX :  max longitude of domain (in deg.min) (REAL*4)
!C ---    TYPE   :  data type (YYY for H09, H10 and H13) (CHAR*3)
!C ---    GQCMIN :  minimal flag                         (INT*4)
!C ---    GQCMAX :  maximal flag                         (INT*4)
!C ---    NATURE :  nature of the data to extract        (INT*4)
!C ---    QCMIN  :  minimal flag (data)                  (INT*4)
!C ---    QCMAX  :  maximal flag (data)                  (INT*4)
!C ---    DEPTH  :  depth  (in positive meters)          (REAL*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen  (20 Oct 94)     (reading NetCDF files)
!C ---  Jean-Michel Brankart (10 Apr 95)
!C ---------------------------------------------------------------------

      SUBROUTINE UEXTCDF(NCLST,   OUTSPC,EXTOUT,                       &
                        PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,     &
                        CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,    &
                        GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,           &
                        NDEPTH,DEP)

      IMPLICIT NONE

      INCLUDE 'netcdf.inc'

      INTEGER*4 MAX
      PARAMETER (MAX=5000)

!C --- Variables for the extraction procedure ---

      INTEGER*4 OUTSPC,PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX,QCMIN,QCMAX,NATURE,INTERP
      REAL*4 DEPSTA,DEPEND,LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER NCLST*100, SELOUT*100, CNTNAM*100, TYPE*3

      INTEGER*4 P,J,I,N,DDMM,MMDD,YYYY,K,NEWREC,II,MM,LL,STRLG,CPT
      INTEGER*4 QCOBS1,QCOBS2,NDEPTH,KK
      REAL*4 XLON,XLAT,LONLAT(2),DEP(NDEPTH),VALEX,VALEXP
      CHARACTER LINE(50)*80, CR*13, PDFNAM*17,PATH*100
      CHARACTER PDF*200 , FILENM*200 , CHMNTH*2 , EXTOUT*100
      LOGICAL MTEST,CTEST,QTEST,INAREA
      CHARACTER*2 CHARI

!C --- Variables for reading NetCDF Files ---

      INTEGER RECID,LEVID,LATID,LONID,CHAID,TEXID,DIMID
      INTEGER IDLAT,IDLON,IDDAT,IDLEH,IDLED,IDHEA,IDATA,IDTYP

      INTEGER RCODE,NCID,DIMREC, NTEXT
      INTEGER START(4),COUNT(4)

      CHARACTER TEXT(20)*80,NAME(30)*35, NAME2*50

!C --- Other Variables

      REAL*4 XQC (9000)

!C --- Variables for reading MODB Files ---

      INTEGER*4 IU,DIAG,INPUT_FILES
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,IQCPRO
      INTEGER*4 QCP(MAX),QCT(MAX),QCS(MAX),DATE,TIME
      INTEGER*4 LAT2,LON2

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1,CHAR1

      REAL*4  LAT3,LON3,VAL,VAL2,DELTA,DEPTH

      REAL*4 PRES(MAX),TEM(MAX),SAL(MAX)

!C-----End variable declaration------------------------------------------


!C --- Open input data base and prepare output files ---
!c     mr
      INPUT_FILES=0
      write(6,*) 'NCLST',NCLST
      OPEN (UNIT=10,FILE=NCLST)
      DO 51 I=1,1000
        READ(10,'(A)', END=52) NAME(I)
        WRITE(6,'(A)') NAME(I)
        INPUT_FILES=INPUT_FILES+1
 51   ENDDO
 52   CONTINUE
      CLOSE(10)

      LL=STRLG(EXTOUT)
      DO I=1,NDEPTH
        WRITE(CHARI,'(I2.2)') I
        OPEN (UNIT=10+I,FILE=EXTOUT(1:LL)//'.'//CHARI)
      ENDDO

      CPT = 0

!C --- Loop over the 11 NetCDF input files ---
!C     mr
      DO I = 1,INPUT_FILES

         NCID = NCOPN (NAME(I),NCNOWRIT,RCODE)

!C --- Get Information on NetCDF File ---

         RECID = NCDID (NCID,'record',RCODE)

         CALL NCDINQ (NCID,RECID,NAME2,DIMREC,RCODE)

         IDLAT = NCVID (NCID,'lat',RCODE)
         IDLON = NCVID (NCID,'lon',RCODE)
         IDDAT = NCVID (NCID,'date',RCODE)
         IDTYP = NCVID (NCID,'type',RCODE)
         IDLEH = NCVID (NCID,'lenhead',RCODE)
         IDLED = NCVID (NCID,'lendata',RCODE)
         IDHEA = NCVID (NCID,'header',RCODE)
         IDATA = NCVID (NCID,'data',RCODE)

!C --- Close File if DIMREC = 0 ---
         IF (DIMREC .EQ. 0) GOTO 20

!C --- Make Loop on DIMREC (profiles) ---

         DO J = 1,DIMREC

!C --- Read LAT, LON and DATE ---

            CALL NCVGT1 (NCID,IDLAT,J,XLAT,RCODE)
            CALL NCVGT1 (NCID,IDLON,J,XLON,RCODE)
            CALL NCVGT1 (NCID,IDDAT,J,DATE,RCODE)

            START(1) = 1
            START(2) = J

            COUNT (1) = 3
            COUNT (2) = 1
       
            CALL NCVGTC (NCID,IDTYP,START,COUNT,DTYPE,3,RCODE)


!C --- Select on Position, Type and Date  ---

            DDMM = INT (DATE / 10000)
            MMDD = DDMM / 100 + (DDMM - (DDMM / 100)*100) * 100
            MM   = MMDD / 100
            YYYY = DATE - DDMM * 10000

            IF ( ( (DTYPE.EQ.'H10') .AND. (TYPE(1:1).EQ.'Y') ) .OR.     &
                ( (DTYPE.EQ.'H09') .AND. (TYPE(2:2).EQ.'Y') ) .OR.      &
                ( (DTYPE.EQ.'H13') .AND. (TYPE(3:3).EQ.'Y') ) ) THEN
            IF ( (LATMIN .LE. XLAT  .AND. LATMAX .GE. XLAT) .OR.        &
                (LATMIN .EQ. LATMAX) )  THEN
            IF ( (LONMIN .LE. XLON  .AND. LONMAX .GE. XLON) .OR.        &
                (LONMIN .EQ. LONMAX) )  THEN
            IF ( (PERSTA .LE. YYYY  .AND. PEREND .GE. YYYY) .OR.        &
                (PERSTA .EQ. 0     .AND. PEREND .EQ. 0   ) ) THEN
            IF ( (SPRSTA .LE. MMDD  .AND. SPREND .GE. MMDD) .OR.        &
                (SPRSTA .EQ. 0     .AND. SPREND .EQ. 0   ) ) THEN

            MTEST = NBMNTH .EQ. 0
            IF (.NOT.(MTEST)) THEN
              DO K = 1,NBMNTH
                MTEST = MTEST .OR. ( MM .EQ. MONTHS(K) )
              ENDDO
            ENDIF
            IF (MTEST) THEN

            LONLAT(1) = XLON
            LONLAT(2) = XLAT
            IF ( .NOT. ( CNTNAM(1:1) .EQ. '*' ) ) THEN
              CTEST = INAREA ( LONLAT , CNTNAM ) 
            ELSE
              CTEST = .TRUE.
            ENDIF
            IF (CTEST) THEN

!C --- Read Entire HEADER  ---

            CALL NCVGT1 (NCID,IDLEH,J,NTEXT,RCODE)
            CALL NCVGT1 (NCID,IDLED,J,NBREC,RCODE)
            
            START(1) = 1
            START(2) = 1
            START(3) = J

            QTEST=.TRUE.
            IF ( (GQCMIN.NE.10).OR.(OUTSPC.EQ.6) ) THEN

               QTEST=.FALSE.

               COUNT (1) = 80
               COUNT (2) = NTEXT
               COUNT (3) = 1

               CALL NCVGTC (NCID,IDHEA,START,COUNT,TEXT,NTEXT*80,RCODE)

!C --- Select on the value of QCPRO

               DO II = 1,NTEXT
                  IF    (TEXT(II)(1:7).EQ.'*GLOBAL' .OR.  &
                        TEXT(II)(1:7).EQ.'*Global' .OR.   &
                        TEXT(II)(1:7).EQ.'*global') THEN
                         READ (TEXT(II),307) QCPRO
                         IQCPRO = 1
                  ENDIF
               ENDDO

               IF (IQCPRO .EQ. 0) THEN
!c                  WRITE (6,*) 'No GLOBAL PROFILE  PROFILE QC found !'
                   QCPRO=0
               ENDIF

               IF ( (GQCMIN .LE. QCPRO .AND. GQCMAX .GE. QCPRO) .OR. (GQCMIN .EQ. 10)  )   THEN
                  QTEST=.TRUE.
               ENDIF
           ENDIF

           IF (QTEST) THEN

!C --- Read Profile Data

               START(1) = 1
               START(2) = 1
               START(3) = J

               COUNT (1) = NBREC
               COUNT (2) = 1
               COUNT (3) = 1

               CALL NCVGT (NCID,IDATA,START,COUNT,PRES,RCODE)

               START(2) = 2
               CALL NCVGT (NCID,IDATA,START,COUNT,TEM,RCODE)
      
               START(2) = 3
               CALL NCVGT (NCID,IDATA,START,COUNT,SAL,RCODE)

               START(2) = 4
               CALL NCVGT (NCID,IDATA,START,COUNT,XQC,RCODE)

               DO K = 1,NBREC
                  QCP(K) = INT (XQC(K) / 100.)
                  QCT(K) = INT ((XQC(K) - QCP(K)*100.) / 10.)
                  QCS(K) = INT (XQC(K) - QCP(K)*100. - QCT(K) * 10.)
               ENDDO

!C --- Loop on the different depth

            DO P=1,NDEPTH
              DEPTH=DEP(P)
           
!C --- Test over profile data and selection of the convenient depth

            VALEX =  99.999
            VALEXP = -999.9

            K = 0
 10         CONTINUE
              K = K + 1
              IF ( K .GT. NBREC ) GOTO 11
              IF ( PRES(K) .LT. DEPTH ) THEN
                GOTO 10 
              ELSEIF ( PRES(K) .EQ. DEPTH ) THEN
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ELSE
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ENDIF

              IF ( INTERP .EQ. 1 ) THEN

              KK = K
 15             CONTINUE
                KK = KK - 1
                IF ( KK .LT. 1 ) THEN
                  GOTO 11
                ELSE
                  IF ( PRES(KK) .EQ. VALEXP ) THEN
                    GOTO 15
                  ELSE
                    IF (NATURE.EQ.1) THEN
                      IF ( TEM(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ELSE
                      IF ( SAL(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF

!C --- Test on the obs. flag values

            IF (QCMIN.NE.10) THEN
            IF (OUTSPC.NE.4) THEN
 
              IF (NATURE.EQ.1) THEN
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCT(K)
                  QCOBS2 = QCT(K)
                ELSE
                  QCOBS1 = QCT(KK)
                  QCOBS2 = QCT(K)
                ENDIF
              ELSE
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCS(K)
                  QCOBS2 = QCS(K)
                ELSE
                  QCOBS1 = QCS(KK)
                  QCOBS2 = QCS(K)
                ENDIF
              ENDIF

              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.  &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.    &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ELSE

              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCT(K)
                QCOBS2 = QCT(K)
              ELSE
                QCOBS1 = QCT(KK)
                QCOBS2 = QCT(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.  &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.   &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF
   
              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCS(K)
                QCOBS2 = QCS(K)
              ELSE
                QCOBS1 = QCS(KK)
                QCOBS2 = QCS(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.    &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.     &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ENDIF
            ENDIF
   

           CPT = CPT + 1
!C --- A convenient value exist in the profile : VAL

           IF (OUTSPC.EQ.4) THEN
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                  VAL2= SAL (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                  VAL2= TEM (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                  VAL2= SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                  VAL2= TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ELSE
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ENDIF

!C --- Writing output

              IF (OUTSPC.EQ.4) THEN

                 WRITE(10+P,*) XLON , XLAT , VAL , VAL2

              ELSEIF (OUTSPC.EQ.5) THEN

                 WRITE(10+P,*) XLON , XLAT , VAL

              ELSEIF (OUTSPC.EQ.6) THEN

                 WRITE(10+P,308) XLON , XLAT , VAL , TEXT(1)(2:19)

              ENDIF

 11         CONTINUE

            ENDDO

            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF

         ENDDO

20       CALL NCCLOS (NCID,RCODE)

      ENDDO

      DO I=1,NDEPTH
        CLOSE(10+I)
      ENDDO

      WRITE(6,'(A)') 'Number of observations:'
      WRITE(6,*) CPT
      WRITE(6,'(A)') 'Stored in the files:'
      LL=STRLG(EXTOUT)
      WRITE(6,'(A)') EXTOUT(1:LL)//'.*'

307   FORMAT (29x,i1)
308   FORMAT (3F15.8,a20)

      END
!C ---------------------------------------------------------------------
!C --- This subroutine extracts data from the main MODB Data Base
!C ---
!C ---      INPUT FORMAT = MODB/MEDATLAS DATA FORMAT
!C ---      ACTION = extract lists of data from the original data base
!C ---               (in the perspective of analysis or control)
!C ---
!C --- Several criterions can be used :
!C ---
!C ---    1) Date
!C ---    2) Position
!C ---    3) Type of data
!C ---    4) Depth
!C ---    5) Quality Flags
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---    PDFLST :  File containing the list of the Netcdf files
!C ---                                                   (CHAR*100)
!C ---    OUTSPC :  4 = TEM/SAL or SAL/TEM
!C ---              5 = Diva Interface Input Format
!C ---              6 = QC Input Format                  (INT*4)
!C ---    EXTOUT :  Description of the output file(s)    (CHAR*100)
!C ---    PERSTA :  period start (year in YYYY)          (INT*4)
!C ---    PEREND :  period end   (year in YYYY)          (INT*4)
!C ---    SPRSTA :  sub-period start (MMDD)              (INT*4)
!C ---    SPREND :  sub-period end   (MMDD)              (INT*4)
!C ---    NBMNTH :  number of months to extract          (INT*4)
!C ---    MONTHS :  list of months to extract            (INT*4)
!C ---    CNTNAM :  file containing the contour          (CHAR*100)
!C ---    LATMIN :  min latitude of domain (in deg.min)  (REAL*4)
!C ---    LATMAX :  max latitude of domain (in deg.min)  (REAL*4)
!C ---    LONMIN :  min longitude of domain (in deg.min) (REAL*4)
!C ---    LONMAX :  max longitude of domain (in deg.min) (REAL*4)
!C ---    TYPE   :  data type (YYY for H09, H10 and H13) (CHAR*3)
!C ---    GQCMIN :  minimal flag                         (INT*4)
!C ---    GQCMAX :  maximal flag                         (INT*4)
!C ---    NATURE :  nature of the data to extract        (INT*4)
!C ---    QCMIN  :  minimal flag (data)                  (INT*4)
!C ---    QCMAX  :  maximal flag (data)                  (INT*4)
!C ---    DEPTH  :  depth  (in positive meters)          (REAL*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Jean-Michel Brankart (19 Jun 95)
!C ---------------------------------------------------------------------

      SUBROUTINE UEXTMODB(PDFLST,   OUTSPC,EXTOUT,                       &
                        PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,       &
                        CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,      &
                        GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,             &
                        NDEPTH,DEP)

      IMPLICIT NONE

      INTEGER*4 MAX
      PARAMETER (MAX=5000)

!C --- Variables for the data extraction procedure

      INTEGER*4 OUTSPC,PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX,QCMIN,QCMAX,NATURE,INTERP
      REAL*4 DEPSTA,DEPEND,LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER PDFLST*100, SELOUT*100, CNTNAM*100, TYPE*3

      INTEGER*4 P,J,I,N,DD,MMDD,YYYY,K,NEWREC,II,MM,LL,STRLG,CPT
      INTEGER*4 QCOBS1,QCOBS2,NDEPTH,KK
!c mr
      REAL*4 XLON,XLAT,LONLAT(2),DEP(50),VALEX,VALEXP
!c      REAL*4 XLON,XLAT,LONLAT(2),DEP(NDEPTH),VALEX,VALEXP

      CHARACTER LINE(50)*80, CR*13, PDFNAM*17,PATH*100
      CHARACTER PDF*200 , FILENM*200 , CHMNTH*2 , EXTOUT*100
      CHARACTER NAME*200 
      LOGICAL MTEST,CTEST,QTEST,INAREA
      CHARACTER*2 CHARI
!c mr
      CHARACTER nomdedieu*200
      INTEGER IOERR

!C --- Other Variables

      REAL*4 XQC (9000)

!C --- Variables for reading MODB Files ---

      INTEGER*4 IU,DIAG
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO
      INTEGER*4 QCP(MAX),QCT(MAX),QCS(MAX),DATE,TIME,IDEPTH
      INTEGER*4 LAT2,LON2,NTEXT

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1,CHAR1
      CHARACTER*80 TEXT(200)

      REAL*4  LAT3,LON3,VAL,VAL2,DELTA,DEPTH

      REAL*4 PRES(MAX),TEM(MAX),SAL(MAX)

!C --- End variable declaration ---

      WRITE(6,*)
      
      write(6,*) 'PDFLST???',PDFLST
      OPEN (UNIT=10,FILE=PDFLST)

      LL=STRLG(EXTOUT)
      DO I=1,NDEPTH
        WRITE(CHARI,'(I2.2)') I
!c mr         OPEN (UNIT=10+I,FILE=EXTOUT(1:LL)//'.'//CHARI, IOSTAT=IOERR)
        OPEN (UNIT=30+I,FILE=EXTOUT(1:LL)//'.'//CHARI, IOSTAT=IOERR)
        WRITE(6,*) I, IOERR
      ENDDO

      CPT = 0

!C --- Loop over the PDF Files ---

 100  CONTINUE
         READ(10,'(A)',END=200) NAME
        WRITE(6,'(2A)')'Opening ??',NAME
        
!C --- Loop over profiles in the PDF file

         OPEN(UNIT=2,FILE=NAME)
 300     CONTINUE

            CALL RPDF2 (2,IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,     &
                      QCP,QCT,QCS,PR,DTYPE,DATE,TIME,IDEPTH,LAT1,LON1, &
                      LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,DIAG,TEXT,NTEXT)
            IF (DIAG .EQ. 1) GOTO 400

!C           WRITE(6,'(2A)')'Searching in ',PR

            CALL TOPOS ( XLAT , XLON , LAT1 , LAT2 , LAT3 , LON1 , LON2 , LON3 )



       
!C --- Select on Position, Type and Date  ---

            DD = DATE / 1000000
            MM = ( DATE - DD * 1000000 ) / 10000
            YYYY = DATE - DD * 1000000 - MM * 10000
            MMDD = MM * 100 + DD

            IF ( ( (DTYPE.EQ.'H10') .AND. (TYPE(1:1).EQ.'Y') ) .OR.    &
                ( (DTYPE.EQ.'H09') .AND. (TYPE(2:2).EQ.'Y') ) .OR.     &
                ( (DTYPE.EQ.'H13') .AND. (TYPE(3:3).EQ.'Y') ) ) THEN
            IF ( (LATMIN .LE. XLAT  .AND. LATMAX .GE. XLAT) .OR.       &
                (LATMIN .EQ. LATMAX) )  THEN
            IF ( (LONMIN .LE. XLON  .AND. LONMAX .GE. XLON) .OR.       &
                (LONMIN .EQ. LONMAX) )  THEN
            IF ( (PERSTA .LE. YYYY  .AND. PEREND .GE. YYYY) .OR.       &
                (PERSTA .EQ. 0     .AND. PEREND .EQ. 0   ) ) THEN
            IF ( (SPRSTA .LE. MMDD  .AND. SPREND .GE. MMDD) .OR.       &
                (SPRSTA .EQ. 0     .AND. SPREND .EQ. 0   ) ) THEN

            MTEST = NBMNTH .EQ. 0
            IF (.NOT.(MTEST)) THEN
              DO K = 1,NBMNTH
                MTEST = MTEST .OR. ( MM .EQ. MONTHS(K) )
              ENDDO
            ENDIF
            IF (MTEST) THEN

            LONLAT(1) = XLON
            LONLAT(2) = XLAT
            IF ( .NOT. ( CNTNAM(1:1) .EQ. '*' ) ) THEN
              CTEST = INAREA ( LONLAT , CNTNAM ) 
            ELSE
              CTEST = .TRUE.
            ENDIF
            IF (CTEST) THEN


!C --- Select on the value of QCPRO

            IF ( (GQCMIN .LE. QCPRO .AND. GQCMAX .GE. QCPRO) .OR. (GQCMIN .EQ. 10)  )   THEN

!C --- Loop on the different depth

            DO P=1,NDEPTH
              DEPTH=DEP(P)

!C --- Test over profile data and selection of the convenient depth

            VALEX =  99.999
            VALEXP = -999.9

            K = 0
 10         CONTINUE
              K = K + 1
              IF ( K .GT. NBREC ) GOTO 11
              IF ( PRES(K) .LT. DEPTH ) THEN
                GOTO 10
              ELSEIF ( PRES(K) .EQ. DEPTH ) THEN
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ELSE
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ENDIF

              IF ( INTERP .EQ. 1 ) THEN

              KK = K
 15             CONTINUE
                KK = KK - 1
                IF ( KK .LT. 1 ) THEN
                  GOTO 11
                ELSE
                  IF ( PRES(KK) .EQ. VALEXP ) THEN
                    GOTO 15
                  ELSE
                    IF (NATURE.EQ.1) THEN
                      IF ( TEM(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ELSE
                      IF ( SAL(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF


!C --- Test on the obs. flag values

            IF (QCMIN.NE.10) THEN
            IF (OUTSPC.NE.4) THEN
 
              IF (NATURE.EQ.1) THEN
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCT(K)
                  QCOBS2 = QCT(K)
                ELSE
                  QCOBS1 = QCT(KK)
                  QCOBS2 = QCT(K)
                ENDIF
              ELSE
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCS(K)
                  QCOBS2 = QCS(K)
                ELSE
                  QCOBS1 = QCS(KK)
                  QCOBS2 = QCS(K)
                ENDIF
              ENDIF

              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.  &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.   &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ELSE

              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCT(K)
                QCOBS2 = QCT(K)
              ELSE
                QCOBS1 = QCT(KK)
                QCOBS2 = QCT(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.  &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.   &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF
   
              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCS(K)
                QCOBS2 = QCS(K)
              ELSE
                QCOBS1 = QCS(KK)
                QCOBS2 = QCS(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.   &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.    &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ENDIF
            ENDIF
   

           CPT = CPT + 1
!C --- A convenient value exist in the profile : VAL

           IF (OUTSPC.EQ.4) THEN
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                  VAL2= SAL (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                  VAL2= TEM (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                  VAL2= SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                  VAL2= TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ELSE
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ENDIF

!C --- Writing output

              IF (OUTSPC.EQ.4) THEN

!c mr                  WRITE(10+P,*) XLON , XLAT , VAL , VAL2
		WRITE(30+P,*) XLON , XLAT , VAL , VAL2

              ELSEIF (OUTSPC.EQ.5) THEN
!c  mr                  WRITE(10+P,*, IOSTAT=IOERR) XLON , XLAT , VAL
   		  WRITE(30+P,*, IOSTAT=IOERR) XLON , XLAT , VAL

              ELSEIF (OUTSPC.EQ.6) THEN

!c mr                  WRITE(10+P,308) XLON , XLAT , VAL , PR
		 WRITE(30+P,308) XLON , XLAT , VAL , PR

              ENDIF

 11         CONTINUE

            ENDDO

            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF

            GOTO 300
400      CONTINUE
         CLOSE(2)

         GOTO 100
200   CONTINUE

      CLOSE (10)
!c mr
      WRITE(30,*) I, IOERR

      DO I=1,NDEPTH
!c mr         CLOSE(10+I,IOSTAT=IOERR)
	CLOSE(30+I,IOSTAT=IOERR)
      ENDDO

      WRITE(6,'(A)') 'Number of observations:'
      WRITE(6,*) CPT
      WRITE(6,'(A)') 'Stored in the files:'
      LL=STRLG(EXTOUT)
      WRITE(6,'(A)') EXTOUT(1:LL)//'.*'

307   FORMAT (29x,i1)
308   FORMAT (3F15.8,a20)

      END
!C ---------------------------------------------------------------------
!C --- This subroutine extracts data from the main MODB Data Base
!C ---
!C ---      INPUT FORMAT = MED(GHER) DATA FORMAT
!C ---      ACTION = extract lists of data from the original data base
!C ---               (in the perspective of analysis or control)
!C ---
!C --- Several criterions can be used :
!C ---
!C ---    1) Date
!C ---    2) Position
!C ---    3) Type of data
!C ---    4) Depth
!C ---    5) Quality Flags
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---    PDFLST :  File containing the list of the Netcdf files
!C ---                                                   (CHAR*100)
!C ---    OUTSPC :  4 = TEM/SAL or SAL/TEM
!C ---              5 = Diva Interface Input Format
!C ---              6 = QC Input Format                  (INT*4)
!C ---    EXTOUT :  Description of the output file(s)    (CHAR*100)
!C ---    PERSTA :  period start (year in YYYY)          (INT*4)
!C ---    PEREND :  period end   (year in YYYY)          (INT*4)
!C ---    SPRSTA :  sub-period start (MMDD)              (INT*4)
!C ---    SPREND :  sub-period end   (MMDD)              (INT*4)
!C ---    NBMNTH :  number of months to extract          (INT*4)
!C ---    MONTHS :  list of months to extract            (INT*4)
!C ---    CNTNAM :  file containing the contour          (CHAR*100)
!C ---    LATMIN :  min latitude of domain (in deg.min)  (REAL*4)
!C ---    LATMAX :  max latitude of domain (in deg.min)  (REAL*4)
!C ---    LONMIN :  min longitude of domain (in deg.min) (REAL*4)
!C ---    LONMAX :  max longitude of domain (in deg.min) (REAL*4)
!C ---    TYPE   :  data type (YYY for H09, H10 and H13) (CHAR*3)
!C ---    GQCMIN :  minimal flag                         (INT*4)
!C ---    GQCMAX :  maximal flag                         (INT*4)
!C ---    NATURE :  nature of the data to extract        (INT*4)
!C ---    QCMIN  :  minimal flag (data)                  (INT*4)
!C ---    QCMAX  :  maximal flag (data)                  (INT*4)
!C ---    DEPTH  :  depth  (in positive meters)          (REAL*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Jean-Michel Brankart (10 Jun 95)
!C ---------------------------------------------------------------------

      SUBROUTINE UEXTMED(MEDPTH,   OUTSPC,EXTOUT,                        &
                        PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS,       &
                        CNTNAM,LATMIN,LATMAX,LONMIN,LONMAX,   TYPE,      &
                        GQCMIN,GQCMAX,   NATURE,QCMIN,QCMAX,             &
                        NDEPTH,DEP)

      IMPLICIT NONE

      INTEGER*4 MAX
      PARAMETER (MAX=5000)

!C --- Variables for the extraction procedure ---

      INTEGER*4 OUTSPC,PERSTA,PEREND,SPRSTA,SPREND,NBMNTH,MONTHS(12)
      INTEGER*4 GQCMIN,GQCMAX,QCMIN,QCMAX,NATURE,INTERP,MTH(12)
      REAL*4 DEPSTA,DEPEND,LATMIN,LATMAX,LONMIN,LONMAX
      CHARACTER MEDPTH*100, SELOUT*100, CNTNAM*100, TYPE*3

      INTEGER*4 P,J,I,N,DD,MMDD,YYYY,K,NEWREC,II,MM,LL,STRLG,CPT
      INTEGER*4 QCOBS1,QCOBS2,MEND,MSTA,NDEPTH,KK
      REAL*4 XLON,XLAT,LONLAT(2),DEP(NDEPTH),VALEX,VALEXP
      CHARACTER LINE(50)*80, CR*13, PDFNAM*17,PATH*100
      CHARACTER PDF*200 , FILENM*200 , CHMNTH*2 , EXTOUT*100
      CHARACTER NAME*200 
      LOGICAL MTEST,CTEST,QTEST,INAREA,MTHBOO
      CHARACTER*2 CHARI

!C --- Other Variables

      REAL*4 XQC (9000)

!C --- Variables for reading MED Files ---

      INTEGER*4 QC1,QC2,QC3,NBREC,QCPRO
      INTEGER*4 QCP(MAX),QCT(MAX),QCS(MAX),DATE

      CHARACTER*18 PR
      CHARACTER*3  DTYPE

      REAL*4  VAL,VAL2,DELTA,DEPTH

      REAL*4 PRES(MAX),TEM(MAX),SAL(MAX)

!C --- End variable declaration ---

      WRITE(6,*)

      LL=STRLG(EXTOUT)
      DO I=1,NDEPTH
        WRITE(CHARI,'(I2.2)') I
        OPEN (UNIT=10+I,FILE=EXTOUT(1:LL)//'.'//CHARI)
      ENDDO

      CPT = 0

!C --- Determine which files are to be opened (month)

      IF (SPRSTA .EQ. 0 .AND. SPREND .EQ. 0) THEN
        IF (NBMNTH.EQ.0) THEN
          II=12
          DO I = 1,12
            MONTHS(I) = I
          ENDDO
        ELSE
          II=NBMNTH
        ENDIF
      ELSE
        MSTA=SPRSTA/100
        MEND=SPREND/100
        IF (NBMNTH.EQ.0) THEN
          II=MEND-MSTA+1
          DO I = MSTA,MEND
            MONTHS(I) = I
          ENDDO
        ELSE
          DO I = 1,NBMNTH
            MTH(I) = MONTHS(I)
          ENDDO
          K = 0
          DO I = 1,NBMNTH
            MTHBOO = .FALSE.
            DO J = MSTA,MEND
              MTHBOO=MTHBOO .OR. ( MTH(I) .EQ. J )
            ENDDO
            IF ( MTHBOO ) THEN
              K = K + 1
              MONTHS(K) = MTH(I)
            ENDIF
          ENDDO
          II = K
        ENDIF
      ENDIF

!C --- Loop over the MED files to open ---

      LL=STRLG(MEDPTH)
      DO I = 1 , II
          WRITE(CHMNTH,'(I2.2)') MONTHS(I)
          FILENM=MEDPTH(1:LL)//'/m'//CHMNTH//'.med'
          WRITE(6,'(2A)')'Opening ',FILENM
          OPEN (UNIT=70,FILE=FILENM)

!C --- Loop over profiles in the MED file ---

 300      CONTINUE

            READ(70,FMT=1000,END=400) PR,DATE,XLAT,XLON, QC1,QC2,QC3,QCPRO,NBREC,DTYPE
            DO K = 1,NBREC
               READ(70,FMT=2000,END=400) PRES(K),TEM(K),SAL(K),QCP(K),QCT(K),QCS(K)
            ENDDO

!C --- Select on Position, Type and Date  ---

            DD = DATE / 1000000
            MM = ( DATE - DD * 1000000 ) / 10000
            YYYY = DATE - DD * 1000000 - MM * 10000
            MMDD = MM * 100 + DD

            IF ( ( (DTYPE.EQ.'H10') .AND. (TYPE(1:1).EQ.'Y') ) .OR.    &
                ( (DTYPE.EQ.'H09') .AND. (TYPE(2:2).EQ.'Y') ) .OR.     &
                ( (DTYPE.EQ.'H13') .AND. (TYPE(3:3).EQ.'Y') ) ) THEN
            IF ( (LATMIN .LE. XLAT  .AND. LATMAX .GE. XLAT) .OR.       &
                (LATMIN .EQ. LATMAX) )  THEN
            IF ( (LONMIN .LE. XLON  .AND. LONMAX .GE. XLON) .OR.      &
                (LONMIN .EQ. LONMAX) )  THEN
            IF ( (PERSTA .LE. YYYY  .AND. PEREND .GE. YYYY) .OR.      &
                (PERSTA .EQ. 0     .AND. PEREND .EQ. 0   ) ) THEN
            IF ( (SPRSTA .LE. MMDD  .AND. SPREND .GE. MMDD) .OR.      &
                (SPRSTA .EQ. 0     .AND. SPREND .EQ. 0   ) ) THEN

            LONLAT(1) = XLON
            LONLAT(2) = XLAT
            IF ( .NOT. ( CNTNAM(1:1) .EQ. '*' ) ) THEN
              CTEST = INAREA ( LONLAT , CNTNAM ) 
            ELSE
              CTEST = .TRUE.
            ENDIF
            IF (CTEST) THEN


!C --- Select on the value of QCPRO

            IF ( (GQCMIN .LE. QCPRO .AND. GQCMAX .GE. QCPRO) .OR. (GQCMIN .EQ. 10)  )   THEN

!C --- Loop on the different depth

            DO P=1,NDEPTH
              DEPTH=DEP(P)

!C --- Test over profile data and selection of the convenient depth

            VALEX =  99.999
            VALEXP = -999.9

            K = 0
 10         CONTINUE
              K = K + 1
              IF ( K .GT. NBREC ) GOTO 11
              IF ( PRES(K) .LT. DEPTH ) THEN
                GOTO 10
              ELSEIF ( PRES(K) .EQ. DEPTH ) THEN
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 0
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ELSE
                IF (NATURE.EQ.1) THEN
                  IF ( TEM(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ELSE
                  IF ( SAL(K) .NE. VALEX ) THEN
                    INTERP = 1
                  ELSE
                    GOTO 10
                  ENDIF
                ENDIF
              ENDIF

              IF ( INTERP .EQ. 1 ) THEN

              KK = K
 15             CONTINUE
                KK = KK - 1
                IF ( KK .LT. 1 ) THEN
                  GOTO 11
                ELSE
                  IF ( PRES(KK) .EQ. VALEXP ) THEN
                    GOTO 15
                  ELSE
                    IF (NATURE.EQ.1) THEN
                      IF ( TEM(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ELSE
                      IF ( SAL(KK) .EQ. VALEX ) THEN
                        GOTO 15
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF

!C --- Test on the obs. flag values

            IF (QCMIN.NE.10) THEN
            IF (OUTSPC.NE.4) THEN
 
              IF (NATURE.EQ.1) THEN
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCT(K)
                  QCOBS2 = QCT(K)
                ELSE
                  QCOBS1 = QCT(KK)
                  QCOBS2 = QCT(K)
                ENDIF
              ELSE
                IF (INTERP.EQ.0) THEN
                  QCOBS1 = QCS(K)
                  QCOBS2 = QCS(K)
                ELSE
                  QCOBS1 = QCS(KK)
                  QCOBS2 = QCS(K)
                ENDIF
              ENDIF

              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND. &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.    &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ELSE

              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCT(K)
                QCOBS2 = QCT(K)
              ELSE
                QCOBS1 = QCT(KK)
                QCOBS2 = QCT(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND. &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.  &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF
   
              IF (INTERP.EQ.0) THEN
                QCOBS1 = QCS(K)
                QCOBS2 = QCS(K)
              ELSE
                QCOBS1 = QCS(KK)
                QCOBS2 = QCS(K)
              ENDIF
              IF ( ( (QCMIN .LE. QCOBS1 .AND. QCMAX .GE. QCOBS1)   .AND.    &
                    (QCMIN .LE. QCOBS2 .AND. QCMAX .GE. QCOBS2) ) .OR.   &
                    (QCMIN .EQ. 10)  )   THEN
                CONTINUE
              ELSE
                GOTO 11
              ENDIF

            ENDIF
            ENDIF
   

           CPT = CPT + 1
!C --- A convenient value exist in the profile : VAL

           IF (OUTSPC.EQ.4) THEN
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                  VAL2= SAL (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                  VAL2= TEM (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                  VAL2= SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                  VAL2= TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ELSE
              IF ( INTERP .EQ. 0 ) THEN
                IF (NATURE.EQ.1) THEN
                  VAL = TEM (K)
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL (K)
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ELSE
                DELTA = (DEPTH-PRES(KK)) / (PRES(K)-PRES(KK))
                IF (NATURE.EQ.1) THEN
                  VAL = TEM(KK) + DELTA * ( TEM(K)-TEM(KK) )
                ELSEIF (NATURE.EQ.2) THEN
                  VAL = SAL(KK) + DELTA * ( SAL(K)-SAL(KK) )
                ELSE
                  STOP 'BAD NATURE'
                ENDIF
              ENDIF
           ENDIF

!C --- Writing output

              IF (OUTSPC.EQ.4) THEN

                 WRITE(10+P,*) XLON , XLAT , VAL , VAL2

              ELSEIF (OUTSPC.EQ.5) THEN

                 WRITE(10+P,*) XLON , XLAT , VAL

              ELSEIF (OUTSPC.EQ.6) THEN

                 WRITE(10+P,308) XLON , XLAT , VAL , PR

              ENDIF

 11         CONTINUE

            ENDDO

            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF

            GOTO 300
400      CONTINUE
         CLOSE(70)

      ENDDO

      DO I=1,NDEPTH
        CLOSE(10+I)
      ENDDO

      WRITE(6,'(A)') 'Number of observations:'
      WRITE(6,*) CPT
      WRITE(6,'(A)') 'Stored in the files:'
      LL=STRLG(EXTOUT)
      WRITE(6,'(A)') EXTOUT(1:LL)//'.*'

307   FORMAT (29x,i1)
308   FORMAT (3F15.8,a20)
1000  FORMAT (A18,1x,I8.8,1x,F8.3,1x,F8.3,1x,3(I1),1x,I1,1x,I4.4,1x,A3)
2000  FORMAT (f6.1,1x,2(F6.3,1x),3(I1))


      END
!C ---------------------------------------------------------------------
!C ---
!C ---  Subroutine for Reading Profile Data File :  rpdf2.f
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---  Inputs :
!C ---
!C ---     IU = Logical Unit
!C ---
!C ---  Outputs :
!C ---
!C ---     IT = 0 if no temp ; 1 if temp  (INTEGER*4)
!C ---     IS = 0 if no sal  ; 1 if sal   (INTEGER*4)
!C ---
!C ---     PR = Profile Reference (CHAR*18)
!C ---     DTYPE = Data Type   (CHAR*3)
!C ---     DATE  = Date        (INTEGER*4)
!C ---     TIME  = Time        (INTEGER*4)
!C ---     LAT1  = 'N' or 'S'  (CHAR*1)
!C ---     LAT2  = Lat Degrees (INTEGER*4)
!C ---     LAT3  = Lat Minutes (REAL*4)
!C ---     LON1  = 'E' or 'W'  (CHAR*1)
!C ---     LON2  = Lon Degrees (INTEGER*4)
!C ---     LON3  = Lon Minutes (REAL*4)
!C ---     DEPTH = Bottom Depth (INTEGER*4)
!C ---     QC1   = Quality Flag on Date and Time (INTEGER*4)
!C ---     QC2   = Quality Flag on latitude      (INTEGER*4)
!C ---     QC3   = Quality Flag on longitude     (INTEGER*4)
!C ---     QC4   = Quality Flag on bottom depth  (INTEGER*4)
!C ---     NBPAR = Number of measured parameters (INTEGER*4)
!C ---     NBREC = Number of record lines        (INTEGER*4)
!C ---     QCPRO = Global Profile QC
!C ---
!C ---     PRES (MAX) = Pressure Measurements           (REAL*4)
!C ---     TEM  (MAX) = Temperature Measurements        (REAL*4)
!C ---     SAL  (MAX) = Salinity Measurements           (REAL*4)
!C ---     QCP  (MAX) = Quality Check for Pressure      (INTEGER*4)
!C ---     QCT  (MAX) = Quality Check for Temperature   (INTEGER*4)
!C ---     QCS  (MAX) = Quality Check for Salinity      (INTEGER*4)
!C ---
!C ---     DIAG = Reading Diagnostic   (INTEGER*4)
!C ---             : = 0 : no  problem
!C ---             : = 1 : end of file
!C ---             : = 2 : incomplete header
!C ---             : = 3 : no existing observation
!C ---             : > 3 : incomplete profile
!C ---                     -> real number of record = DIAG - 3
!C ---
!C ---      TEXT (NTEXT) =  Line 4 up to the end (*) of the header
!C ---                      (CHAR*80)
!C ---      NTEXT        =  Number of text lines   (INTEGER*4)
!C ---
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen - Jean-Michel Brankart  (Sept.94)
!C ---  Roland Schoenauen - Jean-Michel Brankart  (Dec.94)
!C ---         Extension to rpdf.f
!C ---------------------------------------------------------------------

      SUBROUTINE RPDF2 (IU,IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,   &
                      QCP,QCT,QCS,PR,DTYPE,DATE,TIME,DEPTH,LAT1,LON1,  &
                      LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,DIAG,TEXT,NTEXT)

!C --- Variable Declaration --

      IMPLICIT NONE

      INTEGER*4 IU,DIAG,I,NTEXT
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO
!C     INTEGER*4 QCP,QCT,QCS,DATE,TIME,DEPTH
      INTEGER*4 QCP(NBREC),QCT(NBREC),QCS(NBREC),DATE,TIME,DEPTH
      INTEGER*4 LAT2,LON2

      CHARACTER*80 LINE
      CHARACTER*80 TEXT (200)
      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1,CHAR1

      REAL*4  LAT3,LON3

      REAL*4 PRES(NBREC),TEM(NBREC),SAL(NBREC)

!C --- Format Declaration ---

300   FORMAT (a80)
301   FORMAT (1x,a18,11x,a3)
302   FORMAT (6x,i8.8,6x,i4.4,5x,a1,i2.2,1x,f5.2,5x,a1,i3.3,1x,f5.2,7x,i6,4x,4i1)
303   FORMAT (15x,i2,14x,i5)
307   FORMAT (29x,i1)
317   FORMAT (f6.1,2x,f6.3,2x,f6.3,2x,3i1)


!C --- READING FOLLOWING PROFILE IN FILE
!C --- ------- --------- ------- -- ----

!C --- LOOKING FOR THE FIRST RECORD OF THE FOLLOWING PROFILE

 400  CONTINUE
        READ (IU,300,END=500) LINE
      IF (LINE(1:1) .NE. '*') GOTO 400

!C --- Read 1st line
      READ (LINE,301) PR,DTYPE

!C --- READING THE NEXT INFORMATION OF THE HEADER

!C --- Read 2nd line
      READ (IU,300,END=600) LINE
      READ (LINE,302) DATE,TIME,LAT1,LAT2,LAT3,LON1,LON2,LON3,DEPTH, QC1,QC2,QC3,QC4

!C --- Read 3rd line
      READ (IU,300,END=600) LINE
      READ (LINE,303) NBPAR,NBREC 

!C --- Read TEXT lines up to the end of the header

      NTEXT = 0
10    READ (IU,300,END=700) LINE
      NTEXT = NTEXT + 1
      TEXT (NTEXT) = LINE
      IF (TEXT(NTEXT)(1:1) .EQ. '*') GOTO 10
      NTEXT = NTEXT - 1

      IF (NTEXT .GE. 4) READ (TEXT(4),307) QCPRO

!C --- READING DATA PROFILES

!C     READ (LINE,317) PRES(1),TEM(1),SAL(1),QCP,QCT,QCS
      READ (LINE,317) PRES(1),TEM(1),SAL(1),QCP(1),QCT(1),QCS(1)
      DO I = 2,NBREC
         READ (IU,300,END=800) LINE
!C        READ (LINE,317) PRES(I),TEM(I),SAL(I),QCP,QCT,QCS
         READ (LINE,317) PRES(I),TEM(I),SAL(I),QCP(I),QCT(I),QCS(I)
      ENDDO

!C --- READING DIAGNOSTICS

!C --- the whole profile has been correctly read

      DIAG = 0
      GOTO 1000

!C --- no more profile in this file

 500  DIAG = 1
      GOTO 1000

!C --- incomplete header

 600  DIAG = 2
      GOTO 1000

!C --- no existing data

 700  DIAG = 3
      GOTO 1000

!C --- incomplete profile

 800  DIAG = 3 + I - 1
      GOTO 1000

 1000 RETURN

      END
!C ---------------------------------------------------------------------
!C ---
!C ---  Subroutine for Writing Profile Data File
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---  Inputs :
!C ---
!C ---     IU = Logical Unit
!C ---
!C ---     IT = 0 if no temp ; 1 if temp  (INTEGER*4)
!C ---     IS = 0 if no sal  ; 1 if sal   (INTEGER*4)
!C ---
!C ---     PR = Profile Reference (CHAR*18)
!C ---     DTYPE = Data Type   (CHAR*3)
!C ---     DATE  = Date        (INTEGER*4)
!C ---     TIME  = Time        (INTEGER*4)
!C ---     LAT1  = 'N' or 'S'  (CHAR*1)
!C ---     LAT2  = Lat Degrees (INTEGER*4)
!C ---     LAT3  = Lat Minutes (REAL*4)
!C ---     LON1  = 'E' or 'W'  (CHAR*1)
!C ---     LON2  = Lon Degrees (INTEGER*4)
!C ---     LON3  = Lon Minutes (REAL*4)
!C ---     DEPTH = Bottom Depth (INTEGER*4)
!C ---     QC1   = Quality Flag on Date and Time (INTEGER*4)
!C ---     QC2   = Quality Flag on latitude      (INTEGER*4)
!C ---     QC3   = Quality Flag on longitude     (INTEGER*4)
!C ---     QC4   = Quality Flag on bottom depth  (INTEGER*4)
!C ---     NBPAR = Number of measured parameters (INTEGER*4)
!C ---     NBREC = Number of record lines        (INTEGER*4)
!C ---     QCPRO = Global Profile QC
!C ---
!C ---     PRES (MAX) = Pressure Measurements           (REAL*4)
!C ---     TEM  (MAX) = Temperature Measurements        (REAL*4)
!C ---     SAL  (MAX) = Salinity Measurements           (REAL*4)
!C ---     QCP  (MAX) = Quality Check for Pressure      (INTEGER*4)
!C ---     QCT  (MAX) = Quality Check for Temperature   (INTEGER*4)
!C ---     QCS  (MAX) = Quality Check for Salinity      (INTEGER*4)
!C ---
!C ---     TEXT(NTEXT) =  Text of header to be added after 3rd header
!C ---                    line (usually 13)                 (CHAR*80)
!C ---     NTEXT  =  Number of header lines to be added after the
!C ---               3rd one  (usually 13)              (INTEGER*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen (Sept. 94)
!C ---  Roland Schoenauen - Jean-Michel Brankart (Dec. 94)
!C ---     Difference :  puts header lines of 'customized' type
!C ---------------------------------------------------------------------

      SUBROUTINE WPDF2 (IU,IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,       &
                      QCP,QCT,QCS,PR,DTYPE,DATE,TIME,DEPTH,LAT1,LON1,     &
                      LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,TEXT,NTEXT)

!C --- Variable Declaration --

      IMPLICIT NONE

      INTEGER*4 IU,I,NTEXT
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO
!C     INTEGER*4 QCP,QCT,QCS,DATE,TIME,DEPTH
      INTEGER*4 QCP(NBREC),QCT(NBREC),QCS(NBREC),DATE,TIME,DEPTH
      INTEGER*4 LAT2,LON2

      INTEGER*4 STRLG
      CHARACTER FORM*5

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1
      CHARACTER*80 TEXT(200)

      REAL*4  LAT3,LON3

      REAL*4 PRES(NBREC),TEM(NBREC),SAL(NBREC)

!C --- Format Declaration ---

301   FORMAT ('*',a18,1x,'Data Type=',a3)
302   FORMAT ('*DATE=',i8.8,1x,'TIME=',i4.4,1x,'LAT=',a1,i2.2,1x,f5.2,1x,'LON=',a1,i3.3,1x,f5.2,1x,'DEPTH=',i6,1x,'QC=',4i1)
303   FORMAT ('*NB PARAMETERS=',i2,1x,'RECORD LINES=',i5)
304   FORMAT ('*','PRES',1x,'SEA PRESSURE                  ','DECIBARS                      ',1x,'def.=','-999.9')
305   FORMAT ('*','TEMP',1x,'SEA TEMPERATURE               ','CELSIUS DEGREE                ',1x,'def.=','99.999')
306   FORMAT ('*','PSAL',1x,'PRACTICAL SALINITY            ','P.S.U.                        ',1x,'def.=','99.999')
307   FORMAT ('*GLOBAL PROFILE QUALITY FLAG=',i1)
308   FORMAT ('*DC HISTORY=')
309   FORMAT ('*')
310   FORMAT ('*DM HISTORY=')
311   FORMAT ('*')
312   FORMAT ('*COMMENT=')
313   FORMAT ('*')
314   FORMAT ('*SURFACE SAMPLES=')
315   FORMAT ('*')
316   FORMAT ('*','PRES   ','TEMP    ','PSAL   ')
317   FORMAT (f6.1,2x,f6.3,2x,f6.3,2x,3i1)


!C --- WRITING ON FILE

      WRITE (IU,301) PR,DTYPE
      WRITE (IU,302) DATE,TIME,LAT1,LAT2,LAT3,LON1,LON2,LON3,DEPTH,QC1,QC2,QC3,QC4
      WRITE (IU,303) NBPAR,NBREC 

      DO I=1,NTEXT
         IF ( I .EQ. 4 ) THEN
           WRITE(IU,307) QCPRO
         ELSE
           WRITE (FORM,'(a2,i2.2,a1)') '(a' , STRLG(TEXT(I)),')'
           WRITE (IU,FMT=FORM) TEXT(I)
         ENDIF
      ENDDO


      DO I = 1,NBREC
!C        WRITE (IU,317) PRES(I),TEM(I),SAL(I),QCP,QCT,QCS
         WRITE (IU,317) PRES(I),TEM(I),SAL(I),QCP(I),QCT(I),QCS(I)
      ENDDO

      END


!C -----------------------------------------------
!C --- COUNT LENGTH OF CHAR
!C -----------------------------------------------

      INTEGER*4 FUNCTION STRLG (NOM)

      CHARACTER*80 NOM

      STRLG = 1

      DO I = LEN(NOM), 1, -1
         IF (NOM(I:I) .NE. ' ') THEN
            STRLG = I
            GOTO 10
         ENDIF
      ENDDO

10    RETURN

      END
!C ---------------------------------------------------------------------
!C ---
!C ---  Subroutine for Writing Profile Data File
!C ---
!C ---------------------------------------------------------------------
!C ---
!C ---  Inputs :
!C ---
!C ---     IU = Logical Unit
!C ---
!C ---     IT = 0 if no temp ; 1 if temp  (INTEGER*4)
!C ---     IS = 0 if no sal  ; 1 if sal   (INTEGER*4)
!C ---
!C ---     PR = Profile Reference (CHAR*18)
!C ---     DTYPE = Data Type   (CHAR*3)
!C ---     DATE  = Date        (INTEGER*4)
!C ---     TIME  = Time        (INTEGER*4)
!C ---     LAT1  = 'N' or 'S'  (CHAR*1)
!C ---     LAT2  = Lat Degrees (INTEGER*4)
!C ---     LAT3  = Lat Minutes (REAL*4)
!C ---     LON1  = 'E' or 'W'  (CHAR*1)
!C ---     LON2  = Lon Degrees (INTEGER*4)
!C ---     LON3  = Lon Minutes (REAL*4)
!C ---     DEPTH = Bottom Depth (INTEGER*4)
!C ---     QC1   = Quality Flag on Date and Time (INTEGER*4)
!C ---     QC2   = Quality Flag on latitude      (INTEGER*4)
!C ---     QC3   = Quality Flag on longitude     (INTEGER*4)
!C ---     QC4   = Quality Flag on bottom depth  (INTEGER*4)
!C ---     NBPAR = Number of measured parameters (INTEGER*4)
!C ---     NBREC = Number of record lines        (INTEGER*4)
!C ---     QCPRO = Global Profile QC
!C ---
!C ---     PRES (MAX) = Pressure Measurements           (REAL*4)
!C ---     TEM  (MAX) = Temperature Measurements        (REAL*4)
!C ---     SAL  (MAX) = Salinity Measurements           (REAL*4)
!C ---     QCP  (MAX) = Quality Check for Pressure      (INTEGER*4)
!C ---     QCT  (MAX) = Quality Check for Temperature   (INTEGER*4)
!C ---     QCS  (MAX) = Quality Check for Salinity      (INTEGER*4)
!C ---
!C ---     TEXT(NTEXT) =  Text of header to be added after 3rd header
!C ---                    line (usually 13)                 (CHAR*80)
!C ---     NTEXT  =  Number of header lines to be added after the
!C ---               3rd one  (usually 13)              (INTEGER*4)
!C ---
!C ---------------------------------------------------------------------
!C ---  Roland Schoenauen (Sept. 94)
!C ---  Roland Schoenauen - Jean-Michel Brankart (Dec. 94)
!C ---     Difference :  puts header lines of 'customized' type
!C ---------------------------------------------------------------------

      SUBROUTINE WPDF3 (IU,IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO,     &
                      QCP,QCT,QCS,PR,DTYPE,DATE,TIME,DEPTH,LAT1,LON1,   &
                      LAT2,LON2,LAT3,LON3,PRES,TEM,SAL,TEXT,NTEXT)

!C --- Variable Declaration --

      IMPLICIT NONE

      INTEGER*4 IU,I,NTEXT
      INTEGER*4 IT,IS,QC1,QC2,QC3,QC4,NBPAR,NBREC,QCPRO
!C     INTEGER*4 QCP,QCT,QCS,DATE,TIME,DEPTH
      INTEGER*4 QCP(NBREC),QCT(NBREC),QCS(NBREC),DATE,TIME,DEPTH
      INTEGER*4 LAT2,LON2

      INTEGER*4 STRLG
      CHARACTER FORM*5

      CHARACTER*18 PR
      CHARACTER*3  DTYPE
      CHARACTER*1  LAT1,LON1
      CHARACTER*80 TEXT(200)

      REAL*4  LAT3,LON3

      REAL*4 PRES(NBREC),TEM(NBREC),SAL(NBREC)

!C --- Format Declaration ---

301   FORMAT ('*',a18,1x,'Data Type=',a3)
302   FORMAT ('*DATE=',i8.8,1x,'TIME=',i4.4,1x,'LAT=',a1,i2.2,1x,f5.2,1x,'LON=',a1,i3.3,1x,f5.2,1x,'DEPTH=',i6,1x,'QC=',4i1)
303   FORMAT ('*NB PARAMETERS=',i2,1x,'RECORD LINES=',i5)
304   FORMAT ('*','PRES',1x,'SEA PRESSURE                  ','DECIBARS                      ',1x,'def.=','-999.9')
305   FORMAT ('*','TEMP',1x,'SEA TEMPERATURE               ','CELSIUS DEGREE                ',1x,'def.=','99.999')
306   FORMAT ('*','PSAL',1x,'PRACTICAL SALINITY            ','P.S.U.                        ',1x,'def.=','99.999')
307   FORMAT ('*GLOBAL PROFILE QUALITY FLAG=',i1)
308   FORMAT ('*DC HISTORY=')
309   FORMAT ('*')
310   FORMAT ('*DM HISTORY=')
311   FORMAT ('*')
312   FORMAT ('*COMMENT=')
313   FORMAT ('*')
314   FORMAT ('*SURFACE SAMPLES=')
315   FORMAT ('*')
316   FORMAT ('*','PRES   ','TEMP    ','PSAL   ')
317   FORMAT (f6.1,2x,f6.3,2x,f6.3,2x,3i1)


!C --- WRITING ON FILE

!C     WRITE (IU,301) PR,DTYPE
!C     WRITE (IU,302) DATE,TIME,LAT1,LAT2,LAT3,LON1,LON2,LON3,DEPTH,
!C    +               QC1,QC2,QC3,QC4
!C     WRITE (IU,303) NBPAR,NBREC

!C --- Update the number of records

      WRITE (TEXT(3),303) 3,NBREC

      DO I=1,NTEXT
         WRITE (FORM,'(a2,i2.2,a1)') '(a' , STRLG(TEXT(I)),')'
         WRITE (IU,FMT=FORM) TEXT(I)
      ENDDO

      DO I = 1,NBREC
!C        WRITE (IU,317) PRES(I),TEM(I),SAL(I),QCP,QCT,QCS
         WRITE (IU,317) PRES(I),TEM(I),SAL(I),QCP(I),QCT(I),QCS(I)
      ENDDO

      END
      subroutine fgennc(size,name)
      include 'netcdf.inc'
      integer  iret,size
      integer*4 ll,strlg
      external strlg
      character*80 name
!* netCDF id
      integer  ncid
!* dimension ids
      integer  recorddim, leveldim, charlendim, textlendim, dim3dim, dim4dim
!* variable ids
      integer  latid, lonid, dateid, typeid, lenheadid, lendataid, headerid, dataid
!* variable shapes
      integer dims(3)
!* corners and edge lengths
      integer corner(3), edges(3)
!* data variables
      real lat(1)
      real lon(1)
      integer date(1)
      character*3 type
      integer lenhead(1)
      integer lendata(1)
!* enter define mode
      ll=strlg(name)
      ncid = nccre (name(1:ll), NCCLOB, iret)
!* define dimensions
      recorddim = ncddef(ncid, 'record', NCUNLIM, iret)
      leveldim = ncddef(ncid, 'level', size , iret)
      charlendim = ncddef(ncid, 'charlen', 80, iret)
      textlendim = ncddef(ncid, 'textlen', 20, iret)
      dim3dim = ncddef(ncid, 'dim3', 3, iret)
      dim4dim = ncddef(ncid, 'dim4', 4, iret)
!* define variables
      dims(1) = recorddim
      latid = ncvdef (ncid, 'lat', NCFLOAT, 1, dims, iret)
      dims(1) = recorddim
      lonid = ncvdef (ncid, 'lon', NCFLOAT, 1, dims, iret)
      dims(1) = recorddim
      dateid = ncvdef (ncid, 'date', NCLONG, 1, dims, iret)
      dims(2) = recorddim
      dims(1) = dim3dim
      typeid = ncvdef (ncid, 'type', NCCHAR, 2, dims, iret)
      dims(1) = recorddim
      lenheadid = ncvdef (ncid, 'lenhead', NCLONG, 1, dims, iret)
      dims(1) = recorddim
      lendataid = ncvdef (ncid, 'lendata', NCLONG, 1, dims, iret)
      dims(3) = recorddim
      dims(2) = textlendim
      dims(1) = charlendim
      headerid = ncvdef (ncid, 'header', NCCHAR, 3, dims, iret)
      dims(3) = recorddim
      dims(2) = dim4dim
      dims(1) = leveldim
      dataid = ncvdef (ncid, 'data', NCFLOAT, 3, dims, iret)
!* leave define mode
      call ncendf(ncid, iret)
      call ncclos (ncid, iret)
      end
      FUNCTION INAREA ( X , AREA )

      IMPLICIT NONE

      INTEGER NCMAX,NPMAX
      PARAMETER(NCMAX=100,NPMAX=10000)

      INTEGER NC,NP(NCMAX),I,J,ITEST(NCMAX),CONCAVE,SEA(NCMAX),K
      REAL*4 XC(2,NPMAX,NCMAX),X(2),A,B,Z(2)
      LOGICAL TEST,INAREA
      EXTERNAL CONCAVE
      CHARACTER*100 AREA

!C LECTURE DES CONTOURS

      OPEN(UNIT=30,FILE=AREA,STATUS='OLD')
      READ(30,*) NC
      IF (NC.GT.NCMAX) CALL ERROR('AUGMENTER NCMAX','INAREA')
      DO 10 I=1,NC
        READ(30,*) NP(I),SEA(I)
        IF ((NP(I)+1).GT.NPMAX) CALL ERROR('AUGMENTER NPMAX','INAREA')
        DO 11 J=1,NP(I)
          READ(30,*) XC(1,J,I),XC(2,J,I)
 11     CONTINUE
 10   CONTINUE
      CLOSE(30)

!C TEST D'APPARTENANCE DU POINT X AUX CONTOURS DEFINIS DANS 'AREA'

      DO 30 I=1,NC
        ITEST(I)=CONCAVE(X,XC(1,1,I),NP(I),NPMAX)
        IF (ITEST(I).EQ.3) CALL ERROR('CONTOUR INCOHERENT:','ITEST=3')
 30   CONTINUE

      TEST=.FALSE.
      
!C SEA(I) = 0  ==>  L'INTERIEUR DU CONTOUR EST MER

      DO 40 I=1,NC
        IF ( SEA(I).EQ.0 ) THEN
          TEST = TEST .OR. ( ITEST(I).EQ. 2 )  .OR. ( ITEST(I).EQ.1 )
        ENDIF
 40   CONTINUE

!C SEA(I) = 1  ==>  L'INTERIEUR DU CONTOUR EST TERRE

      DO 50 I=1,NC
        IF ( SEA(I).NE.0 ) THEN
          TEST = TEST .AND. ( ( ITEST(I).EQ.2 ) .OR. (ITEST(I).EQ.0) )
        ENDIF
 50   CONTINUE

      INAREA=TEST

      END

      FUNCTION CONCAVE(X,XC,NP,NPMAX)

!C CETTE FONCTION TESTE SI LE POINT X APPARTIENT AU DOMAINE
!C CONCAVE DEFINI PAR LE CONTOUR XC(NP)
!C      CONCAVE = 0  : LE POINT EST HORS DU DOMAINE
!C      CONCAVE = 1  : LE POINT EST DANS LE DOMAINE
!C      CONCAVE = 2  : LE POINT EST SUR LE CONTOUR
!C      CONCAVE = 3  : INCOHERENCE DANS LA CONCLUSION
!C
!C REMARQUE IMPORTANTE : IL FAUT QUE NPMAX >= NP+1

      IMPLICIT NONE

      INTEGER CONCAVE,NP,NPMAX,CSUP,CINF,I1,I2,I,K
      REAL*4 X(2),XC(2,NPMAX),X1MIN,X1MAX,X2MIN,X2MAX,S,DELTA
      LOGICAL CONT

!C CHOIX DE LA DIRECTION DE LA DROITE DE RECHERCHE

      X1MIN=XC(1,1)
      X1MAX=XC(1,1)
      X2MIN=XC(2,1)
      X2MAX=XC(2,1)
      DO 10 I=2,NP
        IF (XC(1,I).LT.X1MAX) X1MIN=XC(1,I)
        IF (XC(1,I).GT.X1MAX) X1MAX=XC(1,I)
        IF (XC(2,I).LT.X1MAX) X2MIN=XC(2,I)
        IF (XC(2,I).GT.X1MAX) X2MAX=XC(2,I)
 10   CONTINUE
      IF (ABS(X1MAX-X1MIN).LT.ABS(X2MAX-X2MIN)) THEN
        I1=2
        I2=1
      ELSE
        I1=1
        I2=2
      ENDIF

!C TEST: LE POINT 1 NE PEUT ˆTRE UNE DEGENERESCENCE

      CONT=.TRUE.
      IF (XC(I1,1).EQ.X(I1)) THEN
        IF (XC(I2,1).NE.X(I2)) THEN
          I=I2
          I2=I1
          I1=I
        ELSE
          CONT=.FALSE.
        ENDIF
      ENDIF

      IF (CONT) THEN
         
!C ITERATION SUR LES SEGMENTS DU CONTOUR POLYGONAL
!C TEST SI ILS COUPENT LA DROITE DE RECHERCHE
!C    CINF :  NOMBRE DE COUPURE EN DESSOUS OU A GAUCHE
!C    CSUP :  NOMBRE DE COUPURE AU DESSUS OU A DROITE
!C    CONT :  TEST D'EGALIT‚ (APPARTENANCE AU CONTOUR)

      CINF=0
      CSUP=0
      XC(1,NP+1)=XC(1,1)
      XC(2,NP+1)=XC(2,1) 
      I=1
 20   CONTINUE

        S=(XC(I1,I)-X(I1))*(XC(I1,I+1)-X(I1))
        IF (S.EQ.0.) THEN
 
!C UN OU PLUSIEURS POINTS CONSECUTIFS DU CONTOUR EST SUR LA DROITE
!C DE RECHERCHE (CE N'EST JAMAIS LE CAS DU PREMIER POINT DE CHAQUE
!C COUPLE TESTE)

          K=1
 25       CONTINUE
            K=K+1
            S=(XC(I1,I)-X(I1))*(XC(I1,I+K)-X(I1))
            IF (S.EQ.0.) GOTO 25
 26       CONTINUE

          IF (S.LT.0.) THEN
            S=(XC(I2,I+1)-X(I2))*(XC(I2,I+K-1)-X(I2))
            IF (S.LE.0.) THEN
              CONT=.FALSE.
            ELSE
              IF (XC(I2,I+1).GT.X(I2)) THEN
                CSUP=CSUP+1
                I=I+K
              ELSE
                CINF=CINF+1
                I=I+K
              ENDIF
            ENDIF
          ELSE
            I=I+K
          ENDIF

        ELSE
          IF (S.LT.0.) THEN

!C LES DEUX EXTREMITES DU SEGMENT SONT DE PART ET D'AUTRE
!C DE LA DROITE DE RECHERCHE

            S=(XC(I2,I)-X(I2))*(XC(I2,I+1)-X(I2))
            IF (S.GT.0.) THEN
              IF ((XC(I2,I)-X(I2)).GT.0.) THEN
                CSUP=CSUP+1
                I=I+1
              ELSE
                CINF=CINF+1
                I=I+1
              ENDIF
            ELSE
              DELTA=(XC(I2,I+1)-XC(I2,I))/(XC(I1,I+1)-XC(I1,I))
              S=XC(I2,I)+DELTA*(X(I1)-XC(I1,I))-X(I2)
              IF (S.EQ.0.) THEN
                CONT=.FALSE.
              ELSE
                IF (S.GT.0.) THEN
                  CSUP=CSUP+1
                  I=I+1
                ELSE
                  CINF=CINF+1
                  I=I+1
                ENDIF
              ENDIF
            ENDIF
          ELSE

!C LES DEUX EXTREMITES DU SEGMENT SONT DU MEME COTE DE LA DOITE
!C DE RECHERCHE

            I=I+1
          ENDIF
        ENDIF

        IF (CONT.AND.(I.LE.NP)) GOTO 20
 30   CONTINUE  

      ENDIF

!C CONCLUSIONS: LE NOMBRE D'INTERSECTION ENTRE LE CONTOUR ET LES
!C DEUX DEMI-DROITES DE RECHERCHE EST:
!C      -PAIR : LE POINT EST EXTERIEUR
!C      -IMPAIR : LE POINT EST INTERIEUR
!C      -DE PARITE DIFFERENTE : ERREUR
!C EN CAS D'APPARTENANCE AU CONTOUR: CONT=.FALSE.

      IF (CONT) THEN
        IF (MOD(CSUP,2).EQ.0) THEN
          IF (MOD(CINF,2).EQ.0) THEN
            CONCAVE=0
          ELSE
            CONCAVE=3
          ENDIF
        ELSE
          IF (MOD(CINF,2).EQ.0) THEN
            CONCAVE=3
          ELSE
            CONCAVE=1
          ENDIF
        ENDIF
      ELSE
        CONCAVE=2
      ENDIF

      RETURN
      END
!C --------------------------------------------------------------
!C --- ROUTINE TO COMPUTE longitude AND latitude FROM MODB FORMAT
!C --------------------------------------------------------------
!C
!C     Jean-Michel Brankart (Oct. 94)

      SUBROUTINE TOPOS ( LAT , LON , LAT1 , LAT2 , LAT3 , LON1 , LON2 , LON3 )

      IMPLICIT NONE

      REAL*4 LAT,LON,LAT3,LON3
      INTEGER*4 LAT2,LON2
      CHARACTER*1 LAT1,LON1

      LAT = LAT3 / 60.
      LON = LON3 / 60.

      LAT = LAT + REAL( LAT2 )
      LON = LON + REAL( LON2 )
 
      IF ( LAT1 .EQ. 'S' ) THEN
  
        LAT = -LAT
      
      ENDIF

      IF ( LON1 .EQ. 'W' ) THEN

        LON = -LON

      ENDIF

      RETURN
      END     

      subroutine error(msg,prg)

      implicit none

      character*80 msg,prg

      write(6,*) 'erreur dans le programme:',prg
      write(6,*) 'probleme:',msg

      if (msg.ne.' ')  STOP

      return
      end

!C=====================================================================
!C===                                                               ===
!C===   C H B L N K  :  Remove Blank Characters                     ===
!C===                   at  Begining of a  Word                     ===
!C===                                                               ===
!C===   RS 16/03/92                                                 ===
!C=====================================================================

      SUBROUTINE CHBLNK (NAME)

      CHARACTER*(*) NAME

      DO 10 I = 1,LEN(NAME)
      write(6,*) '?CHBL',LEN(NAME)
      write(6,*) NAME(I:I)
         IF (NAME(1:1) .EQ. ' ') THEN
            DO 20 J = 1,LEN(NAME)-i
               NAME(J:J) = NAME(J+1:J+1)
 20         CONTINUE
         ELSE
            RETURN
         ENDIF
         NAME(LEN(NAME)-i+1:LEN(NAME)-i+1) = ' '
 10   CONTINUE

      END
      
