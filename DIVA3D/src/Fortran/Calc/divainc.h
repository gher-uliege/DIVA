C
C PSEUDO-DYNAMIC ALLOCATION OF MEMORY: S and L are the two main
C storage areas
C  - NREA   : maximum amount of real variables in main vector S
C  - NENT   : maximum amount of integer variables in main vector L
C  - IRE    : maximum index of real used during execution
C  - IEN    : maximum index of integer used during execution
C  - IREMAX : total number of real required for execution
C  - IENMAX : total number of integer required for execution
C  - IPRC   : precision (real*4 or real*8)

       COMMON /ALLO/ IPRC,IRE,IEN,IREMAX,IENMAX

C#ifdef DIVADYNAMIC
C
C
C      Declare S and L allocatable and in common
C      COMMON /SDYN/ S
C      COMMON /LDYN/ L
C
C#else

#ifdef DIVAHUGEMEMORY
      PARAMETER(nrea=255000000)
      PARAMETER(nent=25000000)
#else
      PARAMETER(nrea=180000000)
      PARAMETER(nent=20000000)
#endif

      COMMON /SDYN/ S(nrea)
      COMMON /LDYN/ L(nent)
C
C#endif
C INDEXES IN  L vector
C  - LKLINK : optimal position of node (i) in the sequence of dof
C  - LKSORT : optimal sorted sequence of nodes
C  - LKSKYH : cumulated heights in stiffness matrix columns
C  - LKCONN : connectivity table between elements and nodes
C  - LKLOCE : localization of one element in the structure
C  - LKELOS : table for localization of data to be fitted in the mesh
C  - KINDT  : index of the last data belonging to (iel-1) in kdata
C  - KDATA  : data nmbr sequence, sorted element/element
C  - KLOCS  : array for localisation of the 3 sub-elements in element
C  - KCNDI  : nodes and d.o.f. for boundary conditions
C  - KTPOL  : matrix (0:nex+1)*(0:ney+1) containing topological info
C            (-1 ==> no element is created; +i = element i is created)

      COMMON /PTRL/ LKLINK,LKSORT,LKSKYH,LKCONN,LKLOCE,LKELOS,LKINDT,
     &              LKDATA,LKLOCS,LKCNDI,LKTPOL
C
C INDEXES IN S vector
C  - LTCOOG : absolute coordinates of node (i)
C  - LTDATA : data X and Y position, value and weight
C  - LTUPPE : upper vector containing the global linear f.e. system
C  - LTLOWE : lower vector containing the global linear f.e. system
C  - LTDIAG : diagonal vector containing the global linear f.e. system
C  - LTRHSG : right-hand side of the global linear f.e. system
C  - LTKELE : elementary matrix (nddle*nddle)
C  - LTRHSE : right-hand side of the elementary system (nddle)
C  - LTVELE : elementary matrix appended in a vector
C  - LTSHAG : (i,j,k): value of shape function i, rule j, gauss p k;
C  - LTGRDS : gridded solution (when ige80=1)
C  - LTGRDE : gridded error estimate
C  - LTCNDI : values of Dirichlet boundary conditions
C  - LTPROP : array of nodal properties for constraint implementation
C  - LTCELE : X and Y coordinates of the center of element iel (ITYP=3)
C  - LTSTIF : (i) -> elementary stifness (pseudo-modification of topology)
C
      COMMON /PTRS/ LTCOOG,LTDATA,LTUPPE,LTLOWE,LTDIAG,LTRHSG,LTKELE,
     &              LTRHSE,LTVELE,LTSHAG,LTGRDS,LTCNDI,LTPROP,LTCELE,
     &              LTSTIF,LTGRDE,LRKELE,lcvpoii,lcvpoir,lcvg,lcova
C
C GENERAL DATA
C  - ITYP    : type of finite element used
C              *ITYP=2 ==> triangular FDV element
C              *ITYP=3 ==> quadrangular FDV element
C  - ISYM    : 0 ==> symetric problem ; 1 ==> non symetric
C  - IPB     : type of problem to be solved;
C              * IPB=2 ==> UNCONSTRAINT SPLINE SMOOTHING (VIM)
C  - NNEL    : number of nodes per element
C  - NDDLE   : number of d.o.f. per element
C  - NDATA   : number of data to be fitted by spline smooting
C  - NDATL   : number of data located in the f.e. mesh
C  - NG      : number of gauss points for numerical integration
C  - ISPEC   : generate the list of points where solution is asked
C              * ISPEC=0 ==> list of points available at unit 79
C              * ISPEC=1 ==> points on a regular grid are generated
C              * ISPEC=2 ==> points on a regular grid are read on 80
C              * ISPEC=3 ==> option 1 and option 0
C              * ISPEC=4 ==> option 2 and option 0
C  - NCOND   : number of boundary conditions to be fixed
C  - INFO    : number of information to be provided with bound. cond.
C  - NNPR    : number of nodal properties required by constraints
C  - ITCS    : type of constraint: ITCS=1 ==> ADVECTION CONSTR. (IPB=2)
C              (Brasseur, 1993, Ph.D. dissertation)
C  - ISTF    : index for pseudo-modification of topology by stifness
C  - IREG    : type of data pre-treatment
C              0 = none
C              1 = substract mean value
C              2 = substract linear regression
C  - ICOORDCHANGE : coordinate change requested or not (degrees to km)
C              0 = no
C              1 = yes
      COMMON/GENE/ ITYP,ISYM,IPB,NNEL,NDDLE,NDATA,NDATL,NG,ISPEC,NCOND
     &         ,INFO,NNPR,ITCS,ISTF, IREG, ICOORDCHANGE,RCOORDCHANGE
     &         ,isspheric
C
C TOPOLOGICAL VARIABLES:
C  - NNT   : total number of nodes
C  - NNT1  : total number of vertex nodes
C  - NNINT : total number of interface nodes
C  - NDDLT : total number of degrees of freedom
C  - NELT  : total number of finite elements
C  - NTERM : size of the vector containing the upper (lower) stiffness
C            matrix
C  - NEX   : number of elements on a regular mesh along X
C  - NEY   : number of elements on a regular mesh along Y
C  - IMATRX: read/generate the matrix containing topological info.
C  - DELTAX: X-step of the regular f.e. mesh
C  - DELTAX: Y-step of the regular f.e. mesh
C
      COMMON /TOPO/ DELTAX,DELTAY,
     &  NNT,NNT1,NNINT,NDDLT,NELT,NTERM,NEX,NEY,IMATRX

C
C P2 PROBLEM PARAMETERS (Spline smooting without constraint: VIM)
C  - ALPHA0 : coefficient of the function amplitude in the cost
C  - ALPHA1 : coefficient of the first derivatives in the cost
C  - WC1    : constraint weight if ITCS=1
C  - THETA  : non-dim constraint weight if ITCS=1
C  - VISC   : viscosity parameter when ITCS=1
C  - RL0    : equivalent correlation length
C  - VARBAK : variance of the background field
C  - OBSER  : obsevation error
C  - UMEAN  : calculated umean from constraint data
C  - INBU   : number of non-zero data constraints
C  - DECAYRATE: sink term decay rate
C JMB
C  - HMMU   : harmonic mean of mu
C
      COMMON /PRB2/ALPHA0,ALPHA1,WC1,VISC,RL0,VARBAK,OBSER,
     &     UMEAN,THETA,DECAYRATE,INBU
      COMMON /MUJMB/HMMU
C
C OUTPUT ON A REGULAR GRID (STORES)
C  - XORI  : X-ORIGIN OF THE REGULAR GRID
C  - DX    : X-STEP OF THE REGULAR GRID
C  - YORI  : Y-ORIGIN OF THE REGULAR GRID
C  - DY    : Y-STEP OF THE REGULAR GRID
C  - VALEX : EXCLUSION VALUE FOR THE GRIDDED SOLUTIONS
C  - NX    : NX-SIZE OF THE GRIDDED MATRIX
C  - NY    : NY-SIZE OF THE GRIDDED MATRIX
C  - RLATMIN: MIN LATITUDE OF TOPOLOGY (used in coordinate change)
C  - RLATMAX: MAX LATITUDE OF TOPOLOGY (used in coordinate change)
C  - RLONMIN: MIN LONGITUDE OF TOPOLOGY (used in coordinate change)
C  - RLONMAX: MAX LONGITUDE OF TOPOLOGY (used in coordinate change)
C  - RLATMEAN: MEAN LATITUDE OF TOPOLOGY (used in coordinate change)
C  - RLONMEAN: MEAN LONGITUDE OF TOPOLOGY (used in coordinate change)
C  - DXKM  : KM/DEGRE in LONGITUDE at LATMEAN
C  - DYKM  : KM/DEGRE in LATITUDE

      COMMON /GRID/ XORI,YORI,DX,DY,VALEX,NX,NY,
     &  RLATMIN,RLATMAX,RLATMEAN,RLONMIN,RLONMAX,RLONMEAN,
     &  DXKM,DYKM,CONVX,CONVY,xorio

C
C OPTIMISATION (SvL)
C ============
C	- OPTI : =1 IF OPTIMISATION IS REQUIRED, ELSE:=0
C  (1) LOCALISATION OF DATA IN THE FINITE ELEMENTS (datapr):
C	- TMAX : MAX X-COORD OF ELEMENTS
C	- TMIX : MIN X-COORD OF ELEMENTS
C	- TMAY : MAX Y-COORD OF ELEMENTS
C	- TMIY : MIN Y-COORD OF ELEMENTS
C	- LKNTC : 3D TABLE THAT CONTAINS THE NUMBER OF THE ELEMENTS THAT COULD
C	          CONTAIN A POINT IN THE RELATIVE AREA
C	- NCAT : NUMBER OF SUBDIVISIONS OF THE SPACE
C	- NCAX : NUMBER OF SUBDIVISIONS OF THE SPACE IN THE X DIRECTION
C	         = NUMBER OF ELEMENTS OF THE LKNTC TABLE IN THE X DIRECTION
C	- NCAY : NUMBER OF SUBDIVISIONS OF THE SPACE IN THE Y DIRECTION
C	         = NUMBER OF ELEMENTS OF THE LKNTC TABLE IN THE Y DIRECTION
C	- NCAZ : NUMBER OF ELEMENTS OF THE LKNTC TABLE IN THE Z DIRECTION
C	- TLEX : LENGTH OF SPACE IN THE X DIRECTION (CF TMAX,...)
C	- TLEY : LENGTH OF SPACE IN THE Y DIRECTION
C	- TLCX : LENGTH OF ONE SUBDIVISION OF SPACE IN THE X DIRECTION
C	- TLCY : LENGTH OF ONE SUBDIVISION OF SPACE IN THE Y DIRECTION
C	- NELKNTC : TOTAL NUMBER OF ELEMENTS OF THE LKNTC TABLE
C	            = NCAX*NCAY*NCAZ
C	- NONLOC  : NUMBER OF DATAS THAT ARE NON LOCALIZED IN THE MESH AND
C	            ARE IGNORED
C  (2) SORTING THE DATA ACCORDING TO THE SEQUENCE OF ELEMENTS (datapr):
C	- LKELOS1 : ARRAY THAT IS USED AND SORTED BY THE QUICK SORT ROUTINE
C  (3) ESTIMATION OF ERROR (esterr):
C	- TBESS : TABULATION OF THE BESSEL FUNCTION
      dimension tbess(40000)
      dimension bidon2(1,1),bidon1(1),ibidon2(1,1),ibidon1(1)
      integer opti,npadneed
      COMMON /CROC/ TMAX,TMAY,TMIX,TMIY,NCAT,NCAX,NCAY,
     &  NCAZ,TLEX,TLEY,TLCX,TLCY,NELKNTC,LKNTC,OPTI,npadneed,
     &  NONLOC,LKELOS1,TBESS,RPI

C      integer*4 ikern,iskip,ilinel,itotll,JMRELERR
      COMMON/DIVAKERN/IKERN,ISKIP,ilinel,itotll,C0C0C0,JMRELERR,ipipe

      integer*4 IPWSP,NSWSP
      COMMON/DIVAWSP/IPWSP,NSWSP
C     COMMON/DIVAMEM/NREADIVA,NINTDIVA
CJMB2012 for sources
      COMMON/QSOURCES/NSOURCES,NSOURCESLOC,ltdataQ,lkelosQ,lkindtQ,
     & lkdataQ,lkelos1Q
      COMMON/FORSOLVERS/isolver,isolverw,solverfill,solvertol


