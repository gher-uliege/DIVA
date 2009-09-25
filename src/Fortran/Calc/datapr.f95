C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  DATAPR (MODULE)
C     -  FINDL2 (associate one element to each data to be fitted)
C     -  FINDL3 (associate one element to each data to be fitted)
C     -  RDDATA (read the data to be fitted by spline smooting)
C     -  SORTDT (sort the data according to the sequence of elements)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             DATAPR MODULE                            C
C             Input of data to be fitted by spline smooting            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine datapr(ipr)
      include'divapre.h'
      include'divainc.h'
cJMB another bug??? why should there be the declaration???       
c      dimension kconn(nelt,nnel),tcoog(nnt1,2)
c jmb

C  READ IREG :  TYPE OF DATA TREATMENT
C  IF YOU DON'T WANT TO USE THE OPTIMISATION SYSTEM, JUST WRITE IN 
C  './drv/diva.drv' : IREG+10 INSTEAD OF IREG (SvL)
      read (10,*) ireg 
      if(ireg.lt.10) then
         opti=1
      else 
         opti=0
	 ireg=ireg-10
      endif

c     opti=0
C  READ THE DATA SET TO COMPUTE THE NUMBER OF DATA CONSTRAINTS
      index=0
 10   read(20,*,end=100,err=100) xxx
      index=index+1
      goto 10
 100  ndata=index
      rewind(20)
      write(6,*)' Total number of data constraint: ndata =',ndata
      if(ndata.eq.0) then
      write(6,*) 'Will not try to analyse zero data'
      stop
      endif

C ALLOCATION OF STORAGE TABLES:
C  ==> TDATA(I,*)  : DATA X and Y POSITION, VALUE and WEIGHT:
C                    (the weight is defined as the mu factor in
C                    the P2 Problem:  BRASSEUR, Ph. D. dissert.)
C  ==> KELOS(ID,*) : LOCALIZATON OF DATA ID IN THE ELEMENT MESH ;
C                    KELOS(ID,1) = IEL where data is located
C                    KELOS(ID,2) = SUB-ELEMENT IN IEL (1,2,3 or 4)
C                    IF IEL<0 ==> DATA NON LOCALIZED
C  ==> KINDT(IEL)  : INDEX OF THE LAST DATA BELONGING TO (IEL-1) IN
C                    ARRAY KDATA
C  ==> KDATA(I)    : DATA NUMBER SEQUENCE, SORTED ELEMENT/ELEMENT
C  ==> KNTC(nelkntc) : USED FOR OPTIMISATION  (SvL)
C  ==> KELOS1(nadata) : USED FOR OPTIMISATION FOR THE QUICK SORT ALGORITHM
C                       (see the 'sordtopti' routine in 'optimi.f')

      ll=4*ndata
      call allody(ll,1,'tdata',ltdata,ipr)
      ll=2*ndata
      call allody(ll,0,'kelos',lkelos,ipr)
      call allody(nelt,0,'kindt',lkindt,ipr)
      call allody(ndata,0,'kdata',lkdata,ipr)
      call rddata(s(ltdata),ipr)
      if (icoordchange.ne.0) then
          call datallxy(s(ltdata),ipr)
      endif

C COMPUTE THE DIVISIONS OF SPACE FOR OPTIMISATION (SvL)
      if (opti.eq.1) then
         call divesp(s(ltcoog),l(lkconn))
c JMB added 20
         ncaz=int(100*real(nelt)/real(ncat))+20
c         write(6,*) 'Ncaz',nelt,ncat,ncaz
         nelkntc=ncax*ncay*ncaz
         call allody(ndata,0,'kelos1',lkelos1,ipr)
C JMB TO DO
C Dirty hack first allocate one point (since last allocated can be extended to 
C the end; once repel2 or repel3 finished, allocate what was actually needed
C but keep old pointer and add nelkntc-1
C simply put third element * in repel2 and repel3
C         call allody(nelkntc,0,'kntc',lkntc,ipr)
          nelkntc=ncax*ncay
          call allody(nelkntc,0,'kntc',lkntc,ipr)
C	       nelkntc=ncax*ncay*ncaz
C      call allody(nelkntc-1,0,'kntc',jjjjjj,ipr)

      endif

C ASSOCIATE DATA TO ELEMENTS 
C test
      if(ityp.eq.2) then
      if (opti.eq.1) then 		! SvL
      call repeltest2(s(ltcoog),l(lkconn),l(lkntc),ncamax)
      endif
      endif
      if(ityp.eq.3) then
      if (opti.eq.1) then 		! SvL
      call repeltest3(s(ltcoog),l(lkconn),l(lkntc),ncamax)
      endif
      endif
      if(opti.eq.1) then
C      write(6,*) 'NCAMAX found',ncamax
C Si ok, allody ici de ncax*ncay*ncamax EN plus en gardant le pointeur lkntc
          nelkntc=ncax*ncay*ncamax
          ncaz=ncamax+1
C          write(6,*) 'ncaz',ncaz
          call allody(nelkntc,0,'kntc',jjjjjj,ipr)
      
      endif
      if(ityp.eq.2) then
         if (opti.eq.1) then 		! SvL
            write (6,*) ' *** Optimisation is working 2 ***'
            call repel2(s(ltcoog),l(lkconn),l(lkntc),ncamax)
c            write(6,*) 'Now found',ncamax
         else 
	    write(6,*) ' *** You asked no optimisation ***'
         endif
         call findl2(l(lkelos),s(ltcoog),l(lkconn),s(ltdata),
     &               l(lkntc),ipr)
      endif

      if(ityp.eq.3) then
         if (opti.eq.1) then		! SvL
            write (6,*) ' *** Optimisation is working 3 ***'
            call repel3(s(ltcoog),l(lkconn),l(lkntc),ncamax)
         else 
	    write(6,*) ' *** You asked no optimisation ***'
          endif
         call findl3(l(lkelos),s(ltcoog),l(lkconn),s(ltcele),
     &               s(ltdata),ipr)
      endif
c      write(6,*) 'Space for optimization?',ncamax,ncaz
      
      nelkntc=ncax*ncay*ncaz
C      call allody(nelkntc-1,0,'kntc',jjjjjj,99)
      
C --- CODE ADDED FOR 2.2 RELEASE (RS 22 MARCH 94) ---
c      if (IREG.GT.0.AND.IREG.LT.3) then 
          call LINREG (L(LKELOS),S(LTDATA))
c      endif
C --- END OF ADDED CODE ---

C SORT THE DATA
      if (opti.eq.1) then		!SvL
         call sortdtopti(l(lkindt),l(lkdata),l(lkelos),l(lkelos1)
     &                   ,ipr)
      endif
      if (opti.eq.0) then
         call sortdt(l(lkindt),l(lkdata),l(lkelos),ipr)
      endif
      write(35,*) ndata,nonloc
      return
      end



      subroutine findl2(kelos,tcoog,kconn,tdata,kntc,ipr)

C  ASSOCIATE ONE ELEMENT TO EACH DATA TO BE FITTED (IF EXISTS)
C  !!!!!!!!!!!!!!!!  WORKS ONLY FOR ELEMENT OF TYPE 2 !!!!!!!!!!!!!!!!!

      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4),kconn(nelt,nnel),tcoog(nnt1,2),
     &          kelos(ndata,2)
      dimension kntc(ncax,ncay,*)

      do 20 id=1,ndata
c mr
c        write(6,*) 'Data ', id 
         x=tdata(id,1)
         y=tdata(id,2)
         if (opti.eq.1) then 
            call locpt2opti(x,y,tcoog,kconn,iel,isub,kntc,ipr)
C JMBTEST
c            if (iel.eq.-1) then
c            write(6,*) 'sauve qui peut',x,y
c            call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
c              if(iel.ne.-1) then 
c              write(6,*) '???problem???',x,y
c              endif
c            endif
C ENDJMBTEST
         endif
         if (opti.eq.0) then 
            call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
         endif
         kelos(id,1)=iel
         kelos(id,2)=isub
 20   continue
      nonloc=0
      do 30 id=1,ndata
         if(kelos(id,1).lt.0) nonloc=nonloc+1
 30   continue
C
C  PRINT LOCALIZATION OF DATA IN THE MESH
C
      if(ipr.ge.1) then
         write(6,910) nonloc
 910     format(/,t2,60('%'),/,' There are ',i7,
     &' data NON localized in the mesh (and ignored)'
     &,/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   LOCALIZATION OF DATA IN ELEMENT AND SUB-ELT'
         do 40 id=1,ndata
            write(6,*) id,kelos(id,1),kelos(id,2)
 40      continue
      endif
      return
      end



      subroutine findl3(kelos,tcoog,kconn,tcele,tdata,ipr)
C
C  ASSOCIATE ONE ELEMENT TO EACH DATA TO BE FITTED (IF EXISTS)
C  !!!!!!!!!!!!!!!!  WORKS ONLY FOR ELEMENT OF TYPE 3 !!!!!!!!!!!!!!!!!
C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4),kconn(nelt,nnel),tcoog(nnt1,2),
     &          kelos(ndata,2),tcele(nelt,2)
      do 20 id=1,ndata
         x=tdata(id,1)
         y=tdata(id,2)
         if (opti.eq.1) then 
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,
     &                      ipr)
         endif
         if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
         endif
         kelos(id,1)=iel
         kelos(id,2)=isub
 20   continue
C
C  COUNT THE NUMBER OF NON LOCATED DATA
C
      nonloc=0
      do 30 id=1,ndata
         if(kelos(id,1).lt.0) nonloc=nonloc+1
 30   continue
C
C  PRINT LOCALIZATION OF DATA IN THE MESH
C
      if(ipr.ge.1) then
         write(6,910) nonloc
 910     format(/,t2,60('%'),/,' There are ',i7,
     &' data NON localized in the mesh (and ignored)',
     &/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   LOCALIZATION OF DATA IN ELEMENT AND SUB-ELT'
         do 40 id=1,ndata
            write(6,*) id,kelos(id,1),kelos(id,2)
 40      continue
      endif
      return
      end




      subroutine rddata(tdata,ipr)
C
C  I/O DATA SET TO BE FITTED BY SPLINE SMOOTHONG
C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
C
C  INPUT OF DATA SET DESCRIPTION
C
C JMB add calculation of harmonic mean of mu for misfit scaling in GCV
      jmbmud=0
      hmmu=0
      do 10 i=1,ndata
         read(20,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
         if (tdata(i,4).gt.0) then
         jmbmud=jmbmud+1
         hmmu=hmmu+1./tdata(i,4)
         endif
 10   continue
      hmmu=jmbmud/hmmu
C
C OUTPUT OF DATA SET DESCRIPTION
C
      if(ipr.ge.3) then
         write(6,*)' List of X and Y position, value and weight of data'
         write(6,*)' --------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end

      subroutine sortdt(kindt,kdata,kelos,ipr)
C
C  SORT THE DATA ACCORDING TO THE SEQUENCE OF ELEMENTS
C
      include'divapre.h'
      include'divainc.h'
      dimension kdata(ndata),kelos(ndata,2),kindt(nelt)
      imaxd=0
      do 10 iel=1,nelt
         kindt(iel)=0
 10   continue

C  SORTING LOOP (SO SLOW!)
      do 20 idata=1,ndata
         iel=kelos(idata,1)
         if(iel.lt.0) then
            goto 20
         endif
         if(iel.eq.nelt) then
            imaxd=imaxd+1
            kdata(imaxd)=idata
            goto 25
         endif
         imind=kindt(iel+1)+2
         do 40 id=imaxd+1,imind,-1
            kdata(id)=kdata(id-1)
 40      continue
         imaxd=imaxd+1
         kdata(kindt(iel+1)+1)=idata
 25      continue
         do 30 ie=iel+1,nelt
            kindt(ie)=kindt(ie)+1
 30      continue
 20   continue
C  END OF SORTING LOOP

C
C storage of number of data located in the mesh
C
      ndatl=imaxd
      if (imaxd.le.0) then
       write(6,*) ' Will create valex grid'
       write(43,*) imaxd
      endif
      if(ipr.ge.1) then
         write(6,910) imaxd
 910     format(/,t2,60('%'),/,' There are ',i7,
     &' data localized in the mesh (and resorted)'
     &   ,/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   ORDERED SEQUENCE OF DATA IN ELEMENTS '
         do 50 id=1,imaxd
            write(6,*) id,kdata(id)
 50      continue
         write(6,*)'   VECTOR KINDT   '
         do 60 iel=1,nelt
            write(6,*) iel,kindt(iel)
 60      continue
      endif
      return
      end

C --- Compute Linear Regression ---

      SUBROUTINE LINREG (KELOS,TDATA)

      PARAMETER (NP = 5)

      include'divapre.h'
      include'divainc.h'

      DIMENSION TDATA (NDATA,4), KELOS(NDATA,2)
      REAL*8 XMEAN,TOTDAT,SX,SY,SXY,SX2,SY2,SV,SXV,SYV
      REAL*4 A(NP,NP), B(NP)

      INTEGER*4 INDX(NP)
C JMB I put D as REAL??
      REAL*4 D
        
C Compute Mean Value

      IF (IREG.EQ.1) THEN
         XMEAN = 0.
         TOTDAT = 0.
         DO 10 I = 1,NDATA
            IF (KELOS(I,1).GE.0) THEN
               XMEAN = XMEAN + TDATA(I,3)
               TOTDAT = TOTDAT + 1.
            ENDIF
10       CONTINUE 
         IF (TOTDAT.NE.0) XMEAN = XMEAN / TOTDAT

         DO 11 I = 1,NDATA
            TDATA (I,3) = TDATA(I,3) - XMEAN
11       CONTINUE 
         WRITE (22,*) XMEAN
         WRITE (22,*) 'Total Nb  of data : '
         WRITE (22,*) NDATA
         WRITE (22,*) 'Nb of Inside data : '
         WRITE (22,*) TOTDAT
         CLOSE (22)
      ENDIF

C Compute Linear Regression

      IF (IREG.EQ.2) THEN
         TOTDAT = 0.
         SX  = 0.
         SY  = 0.
         SXY = 0.
         SX2 = 0.
         SY2 = 0.
         SV  = 0.
         SXV = 0.
         SYV = 0.
         DO 20 I = 1,NDATA
            IF (KELOS(I,1).GE.0) THEN
               TOTDAT = TOTDAT + 1.
               SX  = SX  + TDATA(I,1)
               SY  = SY  + TDATA(I,2)
               SXY = SXY + TDATA(I,1)*TDATA(I,2)
               SX2 = SX2 + TDATA(I,1)*TDATA(I,1)
               SY2 = SY2 + TDATA(I,2)*TDATA(I,2)
               SV  = SV  + TDATA(I,3)             
               SXV = SXV + TDATA(I,1)*TDATA(I,3)
               SYV = SYV + TDATA(I,2)*TDATA(I,3)
            ENDIF
 20      CONTINUE

         A(1,1) = TOTDAT
         A(1,2) = SX
         A(1,3) = SY
         A(2,2) = SX2
         A(2,3) = SXY
         A(3,3) = SY2
         A(2,1) = A(1,2)
         A(3,1) = A(1,3)
         A(3,2) = A(2,3)
         B(1) = SV
         B(2) = SXV
         B(3) = SYV

         CALL LUDCMP (A,3,NP,INDX,D)
         IF (D.NE.0) THEN
         CALL LUBKSB (A,3,NP,INDX,B)
                     ELSE
         
         B(1)=B(1)/TOTDAT
         B(2)=0
         B(3)=0
         write(6,*) 'Using average value as reference',B(1)
         ENDIF

         DO 21 I = 1,NDATA
            TDATA (I,3)=TDATA(I,3) 
     &                  -B(1) - B(2) * TDATA(I,1) - B(3) * TDATA(I,2)
21       CONTINUE
         DO 22 I=1,3
            D = D*A(I,I)
22       CONTINUE
         WRITE (22,*) B(1),B(2),B(3)
         WRITE (22,*) 'Total Nb  of data : '
         WRITE (22,*) NDATA
         WRITE (22,*) 'Nb of Inside data : '
         WRITE (22,*) TOTDAT
         WRITE (22,*) 'Determinant of the Matrix :'
         WRITE (22,*) D
         CLOSE (22)
      ENDIF
c      VARDATA=0
c      do i=1,ndata
c      VARDATA=VARDATA+TDATA (I,3)*TDATA(I,3)
c      enddo
c      write(6,*) 'Variance of anomalies',VARDATA/ndata
c      write(33,*) VARDATA/ndata,ndata
      END
              

C -------------------------------------------------
C --- LUDCMP & LUBKSB :
C ---                   LU Matrix Decomposition
C ---                   and Backward Substitution
C ---
C --- Numerical Recipies (c)
C -------------------------------------------------

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) then
        write(6,*) '==========================='
        write(6,*) '======Singular matrix======'
        write(6,*) '==========================='
        AAMAX=1
        D=0
        endif
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
C JMB???
        imax=N
        write(6,*) 'ludcmp',imax
C JMBE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      AAMAX=ABS(A(1,1))
      AAMIN=AAMAX
      DO I=1,N
      AAMAX=MAX(AAMAX,ABS(A(I,I)))
      AAMIN=MIN(AAMIN,ABS(A(I,I)))
      ENDDO
      IF(AAMIN.LE.1E-6*AAMAX) then
      write(6,*) 'Probably ill posed fit'
      D=0
      endif
      IF(A(N,N).EQ.0.) then
      A(N,N)=TINY
      D=0
      endif
      write(6,*) 'LUDCMP',AAMIN,AAMAX
      RETURN
      END

C ----------------------------------------------

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
