C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  SOURCEPR
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             SOURCEPR                                 C
C             Input of list of sources                                 C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine sourcepr(ipr)
      include'divapre.h'
      include'divainc.h'
      ifortQ=24
      nsources=0
C  READ THE sources SET TO COMPUTE THE NUMBER OF DATA CONSTRAINTS
      index=0
 10   read(ifortQ,*,end=100,err=100) xxx,xxxx,xxxxx
      index=index+1
      goto 10
 100  nsources=index
      rewind(ifortQ)
      write(6,*)' Total number of sources: ndata =',nsources
      
      
C ALLOCATION OF STORAGE TABLES:
C  ==> TDATAQ(I,*)  : DATA X and Y POSITION, VALUE and WEIGHT:
C                    (the weight is defined as the mu factor in
C                    the P2 Problem:  BRASSEUR, Ph. D. dissert.)
C  ==> KELOSQ(ID,*) : LOCALIZATON OF DATA ID IN THE ELEMENT MESH ;
C                    KELOS(ID,1) = IEL where data is located
C                    KELOS(ID,2) = SUB-ELEMENT IN IEL (1,2,3 or 4)
C                    IF IEL<0 ==> DATA NON LOCALIZED
C  ==> KINDTQ(IEL)  : INDEX OF THE LAST DATA BELONGING TO (IEL-1) IN
C                    ARRAY KDATAQ
C  ==> KDATAQ(I)    : DATA NUMBER SEQUENCE, SORTED ELEMENT/ELEMENT

C  ==> KELOS1Q(nadata) : USED FOR OPTIMISATION FOR THE QUICK SORT ALGORITHM
C                       (see the 'sordtopti' routine in 'optimi.f')
      if (nsources.eq.0) then
      ltdataQ=1
      lkelosQ=1
      lkindtQ=1
      lkdataQ=1
      NSOURCESLOC=0
      lkelos1Q=1
      return
      endif

      ll=4*nsources
      call allody(ll,1,'tdataQ',ltdataQ,ipr)
      ll=2*nsources
      call allody(ll,0,'kelosQ',lkelosQ,ipr)
      call allody(nelt,0,'kindtQ',lkindtQ,ipr)
      call allody(nsources,0,'kdataQ',lkdataQ,ipr)
      call rddata(s(ltdataQ),ipr,ifortQ,nsources)
      if (icoordchange.ne.0) then
          call datallxy(s(ltdataQ),ipr)
      endif
      call allody(nsources,0,'kelos1Q',lkelos1Q,ipr)

      nonlocold=nonloc
C ASSOCIATE DATA TO ELEMENTS 
      if(ityp.eq.2) then
         call findl2(l(lkelosQ),s(ltcoog),l(lkconn),s(ltdataQ),
     &               l(lkntc),ipr,NSOURCES)
      endif

      if(ityp.eq.3) then
    
         call findl3(l(lkelosQ),s(ltcoog),l(lkconn),s(ltcele),
     &               s(ltdataQ),ipr,NSOURCES)
      endif
      
C SORT THE DATA
      if (opti.eq.1) then		!SvL
         call sortdtopti(l(lkindtQ),l(lkdataQ),l(lkelosQ),l(lkelos1Q)
     &                   ,ipr,NSOURCES,nonloc)
      endif
      if (opti.eq.0) then
         call sortdt(l(lkindtQ),l(lkdataQ),l(lkelosQ),ipr)
      endif
      write(6,*) 'sources found and non located',nsources,nonloc
      NSOURCESLOC=nsources-nonloc
      nonloc=nonlocold
      
      return
      end


