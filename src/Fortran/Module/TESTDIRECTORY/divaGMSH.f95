PROGRAM divaGMSH

! This program is a driver from diva data to GMSH data file in order
! to make the meshing procedure with GMSH.
!

!#define lgrSmoothingFunction

! Module to use
! =============
 USE moduleDIVA
 USE moduleChrono
 USE vectorInterface
 USE stencilInterface
 USE moduleCoordinateInformation, initialiseCoordinateInformation => initialise


!C ----------------------------------------------------
!C     GENERATEUR DE MAILLAGE 2D MULTIPLEMENT CONNNEXE
!C       PAR LA TRIANGULATION DE DELAUNAY
!C
!C  PROGRAMMATION : BERTIN, ORBAN (AOUT 93)
!C  ADAPTATION    : SCHOENAUEN    (DECEMBRE 93)
!C
!C  Regrouping of gener.a and reorg.a
!C      sorting optimization and english version:
!C       BECKERS SIRJACOBS (2006)
!C ----------------------------------------------------

      INTEGER*4 NMAX,MMAX,NCMAX,NRMAX

      PARAMETER(NMAX=400000,MMAX=400000,NCMAX=1000,NRMAX=100)
!C     PARAMETER(NMAX=10000,MMAX=10000,NCMAX=100,NRMAX=10)

!C     NMAX: NOMBRE MAXIMUM DE NOEUD
!C     MMAX: NOMBRE MAXIMUM DE MAILLE
!C     NCMAX: NOMBRE MAXIMUM DE CONTOUR
!C     NRMAX: NOMBRE MAXIMUM DE REGION A DENSITE VARIABLE

      real*8 X(NMAX),Y(NMAX),NOEUD(NMAX,2),S
      real*8 NEWPT(NMAX,3),XMAX,XMIN,YMAX,YMIN
      real*8 L,DX,DY,NX,NY,C,XP,YP

      INTEGER*4 I,J,N,M,NP,NC,NP2,A,B,NO,MA,MAILLE(MMAX,6)
      INTEGER*4 MP,PPTC(NCMAX),PTC,K,E,F,NPC
 
      INTEGER*4 NR,KNMAX(NRMAX),P,jj,kk

      real*8 KN(NMAX,2),G(NRMAX)
      INTEGER*4 NTRI(NMAX)
      REAL*8 D
      include'iodv.h'

      INTEGER :: iStart, iEnd, i1, nbOfData
      REAL(KIND=8) :: meanPosX, meanPosY, deltaXInKm, deltaYInKm, characteristicLength
      REAL(KIND=8) :: changeCoordinate
      REAL(KIND=8), PARAMETER :: one = 1., zero = 0.
      TYPE(Chronometer) :: chrono
!ici
      CALL createDIVAContext()
      CALL initialiseCoordinateInformation()
      PRINT*, 'Pi:',getPi()
!ici

      P=0
      
      if(iodv.eq.1) then
!c**rs
      open(11,file='meshgen.prm')
!c**rs      OPEN(UNIT=11,FILE='fort.11')
!c**rs
      else
      OPEN(UNIT=11,FILE='fort.11')
      endif
            READ(11,*) P

!C     11 : FICHIER QUI CONTIENT LES PARAMETRES. SI P=1 : LONG. CARACT VARIABLE
!C     12 : CONTIENT LES DONNEES SUR LES REGIONS A MAILLER DIFFEREMENT
      NR=0
      IF(P.EQ.1) THEN
      K=0
      OPEN(UNIT=12,FILE='fort.12')
      READ(12,*) NR
      IF(NR.GT.NRMAX) STOP 'TOO MANY REGIONS, REDEFINE NRMAX'
      DO 20 I=1,NR
        READ(12,*) G(I),KNMAX(I)
        DO 10 J=1,KNMAX(I)
           READ(12,*) KN(K+J,1),KN(K+J,2)
10        CONTINUE
        K=K+KNMAX(I)
20      CONTINUE
      CLOSE(12)
      ENDIF
  
      READ(11,*) S
!C       S  : LONGUEUR CARATERISTIQUE (LUE DANS 11)

      NO=0
      MA=0
      J=0

!C     DETERMINATION DE X,YMIN ET X,YMAX ET LECTURE DES
!C      PT DE CONTOURS DANS UN FICHIER 10
      if(iodv.eq.1) then
!c**rs
      open(10,file='domain.checked')
!c**rs      OPEN(UNIT=10,FILE='fort.10')
      else
      OPEN(UNIT=10,FILE='fort.10')
      endif
      OPEN(UNIT=51,FILE='data.sce')
      READ(10,*) NC
      write(51,*) nc,0
      IF(NC.GT.NCMAX) STOP 'TOO MANY CONTOURS, REDEFINE NCMAX'
      DO 30 I=1,NC
       READ(10,*) N
       write(51,*) N,0
       PPTC(I)=N
       DO 40 K=1,N
          J=J+1
          IF(J.GE.NMAX) THEN
             WRITE(*,'(A)')  'TOO MANY POINTS ON CONTOUR, REDEFINE NMAX'
                         STOP
          ENDIF 
          READ(10,*) X(J),Y(J)
          write(51,*) x(j),y(j)
          IF(J.EQ.1) THEN
             XMIN=X(1)
             XMAX=XMIN
             YMIN=Y(1)
             YMAX=YMIN
          ENDIF
          XMIN=MIN(XMIN,X(J))
          XMAX=MAX(XMAX,X(J))
          YMIN=MIN(YMIN,Y(J))
          YMAX=MAX(YMAX,Y(J))
!C JMBB: no need to keep two identical points...
          if(j.gt.1) then
          if(abs(x(j)-x(j-1)).lt.0.000001*abs(x(j)+x(j-1))) then
          if(abs(y(j)-y(j-1)).lt.0.000001*abs(y(j)+y(j-1))) then
          write(6,*) 'Found two succesive identical points ', I, J
          j=j-1
          PPTC(i)=PPTC(i)-1
          endif
          endif
          endif
!C Dernier=premier??
          if((K.EQ.N).and.(PPTC(i).GT.2)) then
          
          if(abs(x(j)-x(j-PPTC(i)+1)).lt.0.000001*abs(x(j)+x(j-PPTC(i)+1))) then
          if(abs(y(j)-y(j-PPTC(i)+1)).lt.0.000001 *abs(y(j)+y(j-PPTC(i)+1))) then
          write(6,*) 'Found  identical points at start and end ', I, J
          j=j-1
          PPTC(i)=PPTC(i)-1
          endif
          endif
          
          endif
!C JMBE

40       CONTINUE
30    CONTINUE
      CLOSE(10)
      close(51)
      N=J

!      CALL meshWithGMSH(PPTC,X,Y,S,NC)

! ==================================================================================
! ==================================================================================
! Now implement coordinate change on the N points and Density regions if ncessessary
! ==================================================================================
! ==================================================================================

   READ(5,*,ERR=9911,END=9911) changeCoordinate

   IF ( changeCoordinate > 0. ) THEN
      CALL setIChangeCoordinate(1)
   ENDIF

   IF ( changeCoordinate > 1.5 ) THEN
      PRINT*,'Pseudo-spherical'
      CALL setISpheric(1)
   ENDIF

   IF ( changeCoordinate < 0. ) THEN
      CALL setIChangeCoordinate(-1)
   ENDIF

9911 CONTINUE

    PRINT*,'Coordinates ', getIChangeCoordinate(), getISpheric(), changeCoordinate
    PRINT*,'Testing double precision'

! FIND LAT MIN, LAT MAX
! ----------------------
    IF ( getIChangeCoordinate() < 0 ) THEN
      PRINT*,'Anisotropic case'
      CALL setMinimumLongitude(Xmin)
      CALL setMaximumLongitude(Xmax)
      CALL setMinimumLatitude(Ymin)
      CALL setMaximumLatitude(Ymax)
      CALL computeMeanLongitude()
      CALL computeMeanLatitude()
      CALL setDeltaYInKm(one)
      CALL setDeltaXInKm((-1.)*changeCoordinate)
    ENDIF

    IF ( getIChangeCoordinate() == 1 ) THEN
      CALL setMinimumLongitude(Xmin)
      CALL setMaximumLongitude(Xmax)
      CALL setMinimumLatitude(Ymin)
      CALL setMaximumLatitude(Ymax)
      CALL computeMeanLongitude()
      CALL computeMeanLatitude()
      
      deltaYInKm = ( 4. * asin(1.) * 6360. ) / 360.
      deltaXInKm = asin(1.) * getMeanLatitude() / 90.
      deltaXInKm = 6360. * cos( deltaXInKm )
      deltaXInKm = ( 4. * asin(1.) * deltaXInKm ) / 360.

      CALL setDeltaXInKm(deltaXInKm)
      CALL setDeltaYInKm(deltaYInKm)

      IF ( getISpheric() /= 1 ) THEN
         S = S * deltaYInKm
         G(1:NR) = G(1:NR) * deltaYInKm
      ENDIF
    ENDIF

    IF ( getISpheric() == 1 ) THEN
      PRINT*,'Spherical case'
      CALL setMinimumLongitude(Xmin)
      CALL setMaximumLongitude(Xmax)
      CALL setMinimumLatitude(Ymin)
      CALL setMaximumLatitude(Ymax)
      CALL computeMeanLongitude()
      CALL setMeanLatitude(zero)
      CALL setDeltaXInKm(one)
      CALL setDeltaYInKm(one)
      PRINT*,'Mean longitude',getMeanLongitude()
    ENDIF


    IF ( getIChangeCoordinate() /= 0 ) THEN

        CALL changeLongLatToXY(X(1:N),Y(1:N),N)
        XMIN = minval(X(1:N))
        XMAX = maxval(X(1:N))
        YMIN = minval(Y(1:N))
        YMAX = maxval(Y(1:N))

       IF ( p == 1 ) THEN
          Kk=0
          DO  I=1,NR
            iStart = Kk + 1
            iEnd = Kk + KNMAX(I)
            CALL changeLongLatToXY(KN(iStart:iEnd,1),KN(iStart:iEnd,2),KNMAX(I))
            Kk=Kk+KNMAX(I)
          END DO
       END IF
      
    END IF

! end coordinate change

! ==================================================================================
! ==================================================================================
! Now here, subtract mean coordinate position to enhance numerical
! precusion on mesh-generation
! ==================================================================================
! ==================================================================================

      meanPosX = sum(X(1:N)) / N
      meanPosY = sum(Y(1:N)) / N

      CALL setMeanXCoordinate(meanPosX)
      CALL setMeanYCoordinate(meanPosY)

      PRINT*, 'Now centering coordinates ',getMeanXCoordinate(), getMeanYCoordinate()

      XMAX=XMAX-meanPosX
      XMIN=XMIN-meanPosX
      YMAX=YMAX-meanPosY
      YMIN=YMIN-meanPosY

      X(1:N) = X(1:N) - meanPosX
      Y(1:N) = Y(1:N) - meanPosY

      IF ( p == 1 ) THEN
        Kk=0
        DO  I=1,NR
          iStart = Kk + 1
          iEnd = Kk + KNMAX(I)
          KN(iStart:iEnd,1) = KN(iStart:iEnd,1) - meanPosX
          KN(iStart:iEnd,2) = KN(iStart:iEnd,2) - meanPosX
          Kk=Kk+KNMAX(I)
        ENDDO
      ENDIF

! ==================================================================================
! ==================================================================================
!     Adimensionalisation des données
! ==================================================================================
! ==================================================================================

      characteristicLength = 1.0D+0 * max(XMAX-XMIN, YMAX-YMIN)

      PRINT*,'characteristicLength ',characteristicLength

      X(1:N) = X(1:N) / characteristicLength
      Y(1:N) = Y(1:N) / characteristicLength

      XMAX = XMAX / characteristicLength
      XMIN = XMIN / characteristicLength
      YMAX = YMAX / characteristicLength
      YMIN = YMIN / characteristicLength

      IF ( p == 1 ) THEN
        Kk=0
        DO  I=1,NR
          iStart = Kk + 1
          iEnd = Kk + KNMAX(I)
          KN(iStart:iEnd,1:2) = KN(iStart:iEnd,1:2) / characteristicLength
          Kk=Kk+KNMAX(I)
        ENDDO
      ENDIF

    S = S / characteristicLength

    IF ( getIChangeCoordinate() == 1 ) THEN
      IF ( getISpheric() /= 1 ) THEN
         G(1:NR) = G(1:NR) / characteristicLength
      ENDIF
    ENDIF

!      CALL meshWithGMSH(PPTC,X,Y,S,NC)


!C     RAJOUT DE POINT DANS LE CONTOUR SI LA DISTANCE ENTRE
!C      DEUX POINTS DIFFERE BCP (+ DE 2*) DE LA LONGUEUR CARACT.
 
      NP=0
      NPC=0
      NC=1
      PTC=0
      K=0
      DO 50 I=1,N
       A=I
       B=I+1
       PTC=PTC+1

       IF(PTC.EQ.PPTC(NC)) THEN 
          B=I-PTC+1
       ENDIF

       L=S
!c       write(6,*) 'Length scale (after coordinate change)',L
       
       IF(P.EQ.1) THEN
       XP=(X(A)+X(B))/2
       YP=(Y(A)+Y(B))/2
       CALL RAFLOC(KN,NR,NRMAX,KNMAX,G,XP,YP,L,NMAX)
       ENDIF
       D=SQRT(1.D0*(X(A)-X(B))**2+1.D0*(Y(A)-Y(B))**2)
       M=INT(D/L)
!c       write(6,*) M,D,L,A,B,X(A),X(B)
       IF((D/L).LT.(.5)) K=K+1 
       IF(M.GT.1) THEN
          DO 60 J=1,(M-1)
             C=REAL(J)/REAL(M)
             
             NX=X(A)+C*(X(B)-X(A))
             NY=Y(A)+C*(Y(B)-Y(A))
             NP=NP+1
             IF(NP.GE.NMAX) THEN
              WRITE(*,'(A)')  'Too many contour points'
              WRITE(*,'(A)')  ' increase NMAX'
              STOP
             ENDIF
             NEWPT(NP,1)=I
             NEWPT(NP,2)=NX
             NEWPT(NP,3)=NY
60          CONTINUE
       ENDIF
       IF(PTC.EQ.PPTC(NC)) THEN
           PPTC(NC)=PPTC(NC)+NP-NPC
           PTC=0
           NPC=NP
           NC=NC+1
       ENDIF
50    CONTINUE
      NC=NC-1
      NP2=NP
      IF((NP+N).GE.NMAX) THEN
        WRITE(*,'(A)')  'Too many contour points'
        WRITE(*,'(A)')  ' please increase NMAX'
        STOP
      ENDIF
!c      write(6,*) 'Contours',NP,N
!c      do i=1,NP
!c       write(6,*) 'NEWPT',(NEWPT(i,jj),jj=1,3)
!c       enddo
     
      IF(NP.GE.1) THEN
        DO 70 I=N,1,-1
            M=0
80          continue
!C JMB?????
            if(NP.LT.1) then
!c            write(6,*) '????',NP,I,N,M
            M=0
            goto 70
            endif 
!c JMBE
            IF((NEWPT(NP,1).EQ.I)) THEN
             X(I+NP)=NEWPT(NP,2)
             Y(I+NP)=NEWPT(NP,3)
             NP=NP-1
             M=M+1
            ELSE 
             X(I+NP)=X(I)
             Y(I+NP)=Y(I)
             M=0
           ENDIF
           IF(M.GT.0) GOTO 80
70        CONTINUE
      ENDIF
      N=N+NP2
      IF(K.GT.0) THEN
        WRITE(*,'(A)') 'Warning, contour is too fine'
!c        WRITE(*,'(A)') ' DE L, ET CE N FOIS, OU N VAUT:'
        WRITE(*,*) K
      ENDIF

      L=S
!C     CREATION DE LA BOITE
      

      DX=XMAX-XMIN
      DY=YMAX-YMIN
      NOEUD(1,1)=XMIN-DX
      NOEUD(1,2)=YMIN-DY
      NOEUD(2,1)=XMIN+2*DX
      NOEUD(2,2)=YMIN-DY
      NOEUD(3,1)=XMIN+2*DX
      NOEUD(3,2)=YMIN+2*DY
      NOEUD(4,1)=XMIN-DX
      NOEUD(4,2)=YMIN+2*DY
      NO=4
      MAILLE(1,1)=1
      MAILLE(1,2)=2
      MAILLE(1,3)=3
      MAILLE(1,4)=0
      MAILLE(1,5)=0
      MAILLE(1,6)=2
      MAILLE(2,1)=1
      MAILLE(2,2)=3
      MAILLE(2,3)=4
      MAILLE(2,4)=1
      MAILLE(2,5)=0
      MAILLE(2,6)=0
      MA=2

!c      write(6,*) 'PPTC',(PPTC(K),K=1,NC)
!c      do i=1,N
!c            write(6,*) 'X,Y',X(I),Y(I)
!c      enddo
!c      do i=1,NO
!c            write(6,*) 'node',NOEUD(I,1),NOEUD(I,2)
!c            enddo
      
!C     AJOUT DES POINTS DE CONTOUR
      WRITE(*,'(A)') 'ADDING CONTOUR POINTS'
      WRITE(*,*) N

      MP=1
      DO 90 I=1,N
        XP=X(I)
        YP=Y(I)
        write(19,*) X(I)*characteristicLength,Y(I)*characteristicLength
        CALL NEWNO(MAILLE,NOEUD,NMAX,MMAX,NO,MA,XP,YP,MP)
!c       write(6,*) 'treated node',I,'out of',N, L
90    CONTINUE
!c      write(6,*) 'fini newno'
!c      CALL SORTIE(MAILLE,NOEUD,NMAX,MMAX,NO,MA)


      CALL AJOUT(MAILLE,NOEUD,NMAX,MMAX,NO,MA,N,NC,PPTC,NCMAX)


!c      write(6,*) 'PPTCa',(PPTC(K),K=1,NC)

!C     ELIMINATION DES MAILLES DE BOITE
      WRITE(*,'(A)') 'ELIMINATION OF ELEMENTS'

      I=1
100   J=1
110   IF(MAILLE(I,J).LT.5) THEN
         CALL DELMA(MAILLE,MMAX,MA,I) 
         I=I-1
         GOTO 120
      ENDIF
      J=J+1
      IF(J.GT.3) GOTO 120
      GOTO 110
120   I=I+1
      IF(I.GT.MA) GOTO 130
      GOTO 100
130   CONTINUE

! ==================================================================================
! ==================================================================================
!     ELIMINATION DES 4 NOEUDS DE LA BOITE.
! ==================================================================================
! ==================================================================================
      nbOfData = NO - 4
      NOEUD(1:nbOfData,1:2) = NOEUD(5:NbOfData+4,1:2)

      nbOfData = MA
      MAILLE(1:nbOfData,1:3) = MAILLE(1:nbOfData,1:3) - 4

      NO=NO-4

! ==================================================================================
! ==================================================================================
!     ELIMINATION DES MAILLE DE CONCAVITE
! ==================================================================================
! ==================================================================================
      PRINT*,'ELIMINATION OF CONCAVE ELEMENTS'

      I=1
170   J=1
180      A=MAILLE(I,J)
       IF(J.LT.3) THEN
           B=MAILLE(I,J+1)
       ELSE
           B=MAILLE(I,1)
       ENDIF
       F=0
       E=1
       IF((A.LE.PPTC(1)).AND.(B.LE.PPTC(1))) THEN
!CJMB: means they are on the outer boundary??
           IF(A.EQ.PPTC(1)) E=1-PPTC(1)
           IF((MAILLE(I,J+3).EQ.0).AND.((A+E).NE.B)) F=1 
       ELSE
            DO 190 K=1,NC
             IF(A.EQ.(F+1)) E=1-PPTC(K)
             F=F+PPTC(K)
190          CONTINUE
!C JMB??? E=1-PPTC means point A is the first one of a contour
!c           write(6,*) '??',I,J,F,E
!c           IF(A.EQ.(B+E)) THEN
           IF(A.EQ.(B+E)) THEN
             F=1
!c             write(6,*) 'Need to eliminate??'
!c             write(6,*) A,B,I,J,E
!c             write(6,*) (MAILLE(I,J),J=1,6)
           ENDIF
       ENDIF
       IF(F.EQ.1) THEN
!c           write(6,*) 'Elimination of mesh',MA,NC,I
            CALL DELMACONC(MAILLE,PPTC,MMAX,NCMAX,MA,NC,I)
            
           I=0
           GOTO 200
       ENDIF
       J=J+1
       IF(J.GT.3) GOTO 200
       GOTO 180
200    I=I+1
       IF(I.GT.MA) GOTO 210
       GOTO 170
210    CONTINUE
 
      IF(MA.EQ.0) THEN
       WRITE(*,'(A)') 'STRANGE'
       WRITE(*,'(A)') ' PLEASE VERIFY YOUR CONTOURS '  
       WRITE(*,'(A)') ' DOMAIN TO LEFT'

       STOP 'Severe error in contourgen'
      ENDIF

 
! ==================================================================================
! ==================================================================================
!     RAFFINAGE
! ==================================================================================
! ==================================================================================

      PRINT*,'REFINEMENT'
      CALL RAFFIN1(MAILLE,NOEUD,NMAX,MMAX,NO,MA,S,G,KN,NR,NRMAX,KNMAX,P)


! ==================================================================================
! ==================================================================================
!     LISSAGE
! ==================================================================================
! ==================================================================================

      PRINT*, 'SMOOTHING OF GRID'
      CALL startChrono(chrono)
      CALL smoothMesh(MAILLE,NOEUD,NMAX,MMAX,NO,MA,N)
      CALL finishChrono(chrono)
      CALL printInformationChrono(chrono)

! ==================================================================================
! ==================================================================================
!     Dimensionalisation des données
! ==================================================================================
! ==================================================================================

      NOEUD(:,1:2) = NOEUD(:,1:2) * characteristicLength

! ==================================================================================
! ==================================================================================
!     SORTIE
! ==================================================================================
! ==================================================================================

      CALL exportToGMSH(MAILLE,NOEUD,NMAX,MMAX,NO,MA)
      CALL exportToScilab(MAILLE,NOEUD,NMAX,MMAX,NO,MA)
      CALL REORG(MAILLE,NOEUD,NMAX,MMAX,NO,MA,NTRI)
      PRINT*,'FINISHED mesh generation'

   CALL finaliseDIVAContext()

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Program procedures                  ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS

! Procedure 1 : subroutine : SORTIE
! ---------------------------------
!     SORTIE DES RESULTATS DANS DES FICHIERS 20 ET 21

SUBROUTINE SORTIE(MAILLE,NOEUD,NMAX,MMAX,NO,MA)

!     Declaration
!     - - - - - -
      INTEGER(KIND=4), INTENT(IN) :: NMAX, MMAX, NO, MA
      INTEGER(KIND=4), DIMENSION(MMAX,6), INTENT(IN) :: MAILLE
      INTEGER(KIND=4) :: I,J

      REAL(KIND=8), DIMENSION(NMAX,2), INTENT(IN) :: NOEUD

!     Body
!     - - -

      OPEN(UNIT=21,FILE='fort.21')
          WRITE(21,*) NO
          WRITE(21,*) MA
      CLOSE(21)


      OPEN(UNIT=20,file='fort.20')
         DO I=1,NO
            WRITE(20,*) I,NOEUD(I,1),NOEUD(I,2)
         ENDDO

         DO J=1,MA
            WRITE(20,*) J,MAILLE(J,1),MAILLE(J,2),MAILLE(J,3),MAILLE(J,4),MAILLE(J,5),MAILLE(J,6)
         ENDDO
      CLOSE(20)

END SUBROUTINE

! Procedure 2 : subroutine : NEWNO
! --------------------------------
!       INTRODUIT LE NOUVEAU NOEUD ( LOCALISATION DE CELUI-CI
!           ET INTRODUCTION DANS LE MAILLAGE )

SUBROUTINE NEWNO(MAILLE,NOEUD,NMAX,MMAX,NO,MA,XP,YP,I)

!     Declaration
!     - - - - - -
      INTEGER(KIND=4) NPOURR,jj
      PARAMETER(NPOURR=50000)
      INTEGER(KIND=4) NMAX,MMAX,NO,MA,R(NPOURR),NM,I,MAILLE(MMAX,6)

      real*8 NOEUD(NMAX,2),XP,YP


      IF(MA.GE.MMAX) STOP 'TROP DE MAILLES, REDEFINIR MMAX'
      IF(NO.GE.NMAX) STOP 'TROP DE NOEUDS, REDEFINIR NMAX'

!c     write(6,*) 'before '
!c      write(6,*) 'MLOC call',I,XP,YP
      CALL MLOC(MAILLE,NOEUD,NMAX,MMAX,R,NM,XP,YP,I)
!c      write(6,*) 'before newma'
!c      do jj=1,NM
!c      write(6,*) 'RR?',R(jj)
!c      write(6,*) noeud(maille(r(jj),1),1),noeud(maille(r(jj),1),2)
!c      write(6,*) noeud(maille(r(jj),2),1),noeud(maille(r(jj),2),2)
!c      write(6,*) noeud(maille(r(jj),3),1),noeud(maille(r(jj),3),2)
!c       enddo
      CALL NEWMA(MAILLE,NOEUD,NMAX,MMAX,NO,MA,R,NM,XP,YP)
      RETURN
      END subroutine

!C --------------------------------------
!C     SOUS-ROUTINE   MLOC
!C --------------------------------------

      SUBROUTINE MLOC(MAILLE,NOEUD,NMAX,MMAX,R,M,XP,YP,I)

!C     LOCALISE A QUELLE MAILLE APPARTIENT LE FUTUR NOEUD XP,YP
!C      ET DETERMINE LES MAILLES QUI SERONT AFFECTEES PAR SON
!C      INTRODUCTION

      IMPLICIT NONE
      INTEGER*4 NPOURR,NPOURT
      PARAMETER(NPOURR=50000)
      PARAMETER(NPOURT=50000)
      INTEGER*4 NMAX,MMAX,R(NPOURR),M,I,T(NPOURT),B(4),L,J,MAILLE(MMAX,6)
      real*8 NOEUD(NMAX,2),XP,YP,xi
      REAL*8 X1,Y1,Y2,X2,Z1,Z2,Z3,Z4,A1,A2,A3,C1
      INTEGER*4 S,A(NPOURT),N,K,F,P,SW,IIII,IOLD

      REAL*8 V,N1,N2,N3,N4,NX,NY,VOL,V2,V3
      REAL*8 XJ1,YJ1,XJ2,YJ2,XJ3,YJ3,VJ,XJC,YJC
      NX=XP
      NY=YP

!c      write(6,*) 'Into mloc',I,XP,YP
!C     I : MAILLE QUI CONTIENT LE PT XP,YP : A DETERMINER

10    B(1)=MAILLE(I,1)
      B(4)=B(1)
      B(2)=MAILLE(I,2)
      B(3)=MAILLE(I,3)
 
      DO 20 J=1,3
        N1=NOEUD(B(J),1)
        N2=NOEUD(B(J+1),1)
        N3=NOEUD(B(J),2)
        N4=NOEUD(B(J+1),2)
        V=(N1-N2)*(N3-NY)-(N1-NX)*(N3-N4)
!C is -xi of my notes
        VOL=(N2-N1)**2+(N4-N3)**2
        if(VOL.EQ.0) then
        write(6,*) 'DIVAERROR:MESH',N1,N2,N3,N4,J,B(J),B(J+1),I
        endif
!CJM Test added
        if(ABS(V).LE.1E-10*VOL) THEN
        If(V.LT.0) then
        xi=-V/VOL-3.E-5        
                   else
        xi=-V/VOL+3.E-5
        endif
        NX=NX-xi*(N4-N3)
        NY=NY+xi*(N2-N1)
!c        write(6,*) ' Moved point ',XP,YP,NX,NY,V
        XP=NX
        YP=NY
        V=(N1-N2)*(N3-NY)-(N1-NX)*(N3-N4)
!c        write(6,*) 'Now V=',V
        endif
!CJMB test
!C add test here for colinearity.
!C if colinear, displace NX,NY (and XP YP)
!C and recalculate V before continuing
!c        VOL=(N1-N2)**2+(N3-N4)**2+(N3-NY)**2+(N1-NX)**2
!c        VOL=VOL+(N2-NX)**2+(N4-NY)**2
!c        IF(abs(V).lt.1E-10*VOL) THEN
!c        V2=(N2-NX)*(N4-N3)-(N2-N1)*(N4-NY)
!c        V3=(NX-N1)*(NY-N4)-(NX-N2)*(NY-N3)
!c        write(6,*) 'Almost colinear?',N1,N2,NX,N3,N4,NY,V,VOL,V2,V3
!c        endif
!C test
        IF (V.LT.0.) THEN
           S=J+2
           
!c           write(6,*) '???',I,S,MAILLE(I,4),MAILLE(I,5),MAILLE(I,6),V
           IOLD=I
           IIII=0
 444       continue
           S=S+1
           IIII=IIII+1
           if(S.GT.6) S=S-3
           I=MAILLE(IOLD,S)
!c           write(6,*) 'a???',I,S
           if(I.NE.0) then
           GOTO 10
                      else
           write(6,*) 'a???',I,S
           if (iiii.gt.3) then
           write(6,*) '??????extremely strange'
           stop
           endif
           goto 444 
           
           endif
        ENDIF
20    CONTINUE

!C     UTILISATION DU TEST DU CERCLE CIRCONSCRIT A LA MAILLE
!C      POUR REPERTORIER LES MAILLES A MODIFIER
!C Chercher ici pour des probl�me d'arrondi et voir si l'on peut
!C chercher un peu plus loin pour etre sur? Probablement justement pas??

      M=1
      N=1
      L=2
      T(1)=0
      T(2)=I
      R(M)=I
30    A(N)=0
      DO 40 F=1,3
        S=F+3
        P=MAILLE(R(M),S)
        DO 50 K=1,L       
           IF (P.EQ.T(K)) GOTO 60
50        CONTINUE

        IF (N.EQ.1) GOTO 70

        DO 80 K=1,N-1
           IF(P.EQ.A(K)) GOTO 60
80        CONTINUE

70        A(N)=P
        N=N+1
60        CONTINUE

40    CONTINUE
      N=N-1
      IF(N.LT.1) THEN
!C      write(6,*) ' so what??',N
      goto 100
      endif
      IF (A(N).EQ.0) N=N-1
      IF (N.LE.0) GOTO 100
      L=L+1
      T(L)=A(N)
      
      XJ1=NOEUD(MAILLE(A(N),1),1)
      XJ2=NOEUD(MAILLE(A(N),2),1)
      XJ3=NOEUD(MAILLE(A(N),3),1)
      
      YJ1=NOEUD(MAILLE(A(N),1),2)
      YJ2=NOEUD(MAILLE(A(N),2),2)
      YJ3=NOEUD(MAILLE(A(N),3),2)
      
      VJ=(XJ2-XJ1)*(YJ3-YJ2)-(XJ3-XJ2)*(YJ2-YJ1)
      
      if((YJ2-YJ1)*(YJ2-YJ1).LT.1E-10*ABS(VJ)) then
      
      XJ1=NOEUD(MAILLE(A(N),2),1)
      XJ2=NOEUD(MAILLE(A(N),3),1)
      XJ3=NOEUD(MAILLE(A(N),1),1)
      
      YJ1=NOEUD(MAILLE(A(N),2),2)
      YJ2=NOEUD(MAILLE(A(N),3),2)
      YJ3=NOEUD(MAILLE(A(N),1),2)
      
      VJ=(XJ2-XJ1)*(YJ3-YJ2)-(XJ3-XJ2)*(YJ2-YJ1)
      
      if((YJ2-YJ1)*(YJ2-YJ1).LT.1E-10*ABS(VJ)) then
      
      XJ1=NOEUD(MAILLE(A(N),3),1)
      XJ2=NOEUD(MAILLE(A(N),1),1)
      XJ3=NOEUD(MAILLE(A(N),2),1)
      
      YJ1=NOEUD(MAILLE(A(N),3),2)
      YJ2=NOEUD(MAILLE(A(N),1),2)
      YJ3=NOEUD(MAILLE(A(N),2),2)
      
      VJ=(XJ2-XJ1)*(YJ3-YJ2)-(XJ3-XJ2)*(YJ2-YJ1)
      
      if((YJ2-YJ1)*(YJ2-YJ1).LT.1E-10*ABS(VJ)) then
      write(6,*) '????? Strange element'
!C      stop
      endif
      
      endif
      
      
      endif
      
      XJC=((XJ3-XJ2)*(XJ3+XJ2)+(YJ3+YJ2)*(YJ3-YJ2))*(YJ2-YJ1)/2-((XJ2-XJ1)*(XJ2+XJ1)+(YJ2+YJ1)*(YJ2-YJ1))*(YJ3-YJ2)/2
      
      XJC=-XJC/VJ
     
      YJC=-(XJ2-XJ1)*XJC+((XJ2-XJ1)*(XJ2+XJ1)+(YJ2+YJ1)*(YJ2-YJ1))/2
      YJC=YJC/(YJ2-YJ1) 
      A1=XJC
      A2=YJC     

      X1=NOEUD(MAILLE(A(N),1),1)
      Y1=NOEUD(MAILLE(A(N),1),2)

      A3=(X1-A1)**2+(Y1-A2)**2
      C1=(NX-A1)**2+(NY-A2)**2
      SW=0
      !write(59,910) a1,a2
      !IF (C1.LT.A3) THEN
      IF ((C1-A3).LT.(-1.D-6)*(ABS(C1)+ABS(A3))) THEN
         SW=1
      ENDIF
      IF(SW.EQ.1) THEN
         M=M+1
         R(M)=A(N)
      ENDIF 
      GOTO 30

100   CONTINUE
      RETURN
      END subroutine

!C --------------------------------------
!C     SOUS-ROUTINE   NEWMA
!C --------------------------------------

      SUBROUTINE NEWMA(MAILLE,NOEUD,NMAX,MMAX,NO,MA,R,NM,XP,YP)

      IMPLICIT NONE
!C       INTRODUIT LE NOUVEAU NOEUD EN AJUSTANT LA TOPOLOGIE.
      INTEGER*4 NPOURR,NPOURC
      PARAMETER(NPOURR=50000,NPOURC=50000)
      INTEGER*4 NMAX,MMAX,MA,NO,R(NPOURR),NM,T,MAILLE(MMAX,6)
      real*8 NOEUD(NMAX,2),XP,YP
      INTEGER*4 I,J,K,L,CE(NPOURC,2),MTEMP(NPOURC,5),A,B,C
      integer*4 MV(NPOURC,2)
      if((nm+2).gt.npourc) stop 'jmbtest failed, increase npourc'
!CJMB the problem is probably in this routine
      L=0
      NO=NO+1
      NOEUD(NO,1)=XP
      NOEUD(NO,2)=YP
      
!c      write(6,*) 'Working on NM triangles', NM

!C     BALAYAGE DES NM MAILLES POUR TROUVER LES BORDS DU SOUS-MAILLAGE NM

      DO 10 I=1,NM
       DO 20 K=4,6
             J=1
30           IF((MAILLE(R(I),K).NE.R(J)).AND.(J.LE.NM)) THEN
!C                  WRITE(*,*) I,R(I),MAILLE(R(I),K),K,J,R(J)
               J=J+1
               GOTO 30
             ENDIF
!CJMB Previous test makes no sense to me??? only stoppend when J.LT.NM???
             IF(J.GT.NM) THEN
               L=L+1
               CE(L,1)=I
               CE(L,2)=K
             ENDIF
20       CONTINUE
10    CONTINUE

!C     TEST POUR VOIR SI ...
!c      do i=1,L
!c      write(6,*) 'CE??',CE(i,1),CE(i,2)
!c      enddo

      IF(L.NE.(NM+2)) THEN
          WRITE(*,*) '??????',NO,R(1),L,NM+2,'??????'
          CALL SORTIE(MAILLE,NOEUD,NMAX,MMAX,NO,MA)
          STOP
      ENDIF

!C     CREATION DES NOUVELLES MAILLES DANS UN TABLEAU MTEMP (PROVISOIRE)
      if (L.GT.NPOURC) STOP 'INCREASE NPOURC'
!      if (check) l=3
      DO 40 I=1,L
       C=CE(I,1)
       IF(I.LE.NM) THEN
            MTEMP(I,1)=R(I)
       ELSE
            MA=MA+1
            MTEMP(I,1)=MA
       ENDIF
!C JMB MTEMP(I,1): mesh number
       A=CE(I,2)
       DO 50 K=1,2
            B=K-4
            IF((A+B).GT.3) B=B-3
            MTEMP(I,K+1)=MAILLE(R(C),A+B)
!C JMB MTEMP(I,2) and MTEMP(I,3) numbers of the nodes on the boundary segment?
50       CONTINUE
       MTEMP(I,4)=NO
!C Central node !
!c       write(6,*) 'NO',NO
       MTEMP(I,5)=MAILLE(R(C),A)
!C where to find the pointer to the external meshes
40    CONTINUE

!C     MISE A JOUR DE LA TOPOLOGIE

!C        PRIMO : REORGANISATION DE MTEMP
!C
      DO 60 I=1,(NM+1)
       A=I
       B=I+1
       IF(MTEMP(A,3).NE.MTEMP(B,2)) THEN
            J=I+2
70            IF(MTEMP(A,3).EQ.MTEMP(J,2)) THEN
              DO 80 K=1,5
                  C=MTEMP(B,K)
                  MTEMP(B,K)=MTEMP(J,K)
                  MTEMP(J,K)=C
80                CONTINUE
              C=CE(B,1)
              CE(B,1)=CE(J,1)
              CE(J,1)=C
              C=CE(B,2)
              CE(B,2)=CE(J,2)
              CE(J,2)=C

              GOTO 90
            ENDIF
            J=J+1
            GOTO 70
90            CONTINUE
       ENDIF
!C JMB OK, found the sequence of connecting segments (convex hull)
60    CONTINUE

!C       SECUNDO : REORGANISATION DES MAILLES VOISINES AU SOUS-MAILLAGE

      A=0
      DO 100 I=1,L
       C=CE(I,1)
       IF( MAILLE(R(C),CE(I,2)).NE.0) THEN
           DO 110 J=4,6
            IF( MAILLE(MTEMP(I,5) ,J).EQ.R(C) ) THEN
               T=0
               DO 120 K=1,A
                   IF((MV(K,1).EQ.MTEMP(I,5)).AND.(MV(K,2).EQ.J)) T=1
120                CONTINUE
               IF(T.EQ.0) THEN
                  A=A+1
                  MAILLE(MTEMP(I,5),J)=MTEMP(I,1)
                  MV(A,1)=MTEMP(I,5)
                  MV(A,2)=J
                  ENDIF
            ENDIF
110          CONTINUE
       ENDIF
100   CONTINUE

!C       TERTIO : REORGANISATION DES MAILLES EN FONCTION DE MTEMP
      if((nm+2).gt.npourc) stop 'jmbtestmaille failed'
      DO 130 I=1,(NM+2)
       A=MTEMP(I,1)
       DO 140 J=1,4
            MAILLE(A,J)=MTEMP(I,J+1)
140      CONTINUE
       IF(I.EQ.1) THEN
         MAILLE(A,6)=MTEMP(NM+2,1)
            ELSE
         MAILLE(A,6)=MTEMP(I-1,1)
       ENDIF
       IF(I.EQ.(NM+2)) THEN
            MAILLE(A,5)=MTEMP(1,1)
            ELSE
            MAILLE(A,5)=MTEMP(I+1,1)
       ENDIF
!c       write(6,*) '??? Central node',MAILLE(A,3),A
!c       write(6,*) 'central',NOEUD(MAILLE(A,3),1),NOEUD(MAILLE(A,3),2)
130    CONTINUE

      RETURN
      END subroutine
 


!C --------------------------------------
!C     SOUS-ROUTINE   DELMA
!C --------------------------------------

      SUBROUTINE DELMA(MAILLE,MMAX,MA,NM)

      IMPLICIT NONE

!C     SUPPRIME LA MAILLE NM DU TABLEAU

      INTEGER*4 MMAX,MA,NM,I,J
      INTEGER*4 MAILLE(MMAX,6)

      DO 10 I=4,6
       IF(MAILLE(NM,I).NE.0) THEN
!C JMB For meshes that had a pointer towards them from NM
!C the reverse pointer needs to be eliminated
       DO 20 J=4,6
            IF(MAILLE(MAILLE(NM,I),J).EQ.NM) THEN
                  MAILLE(MAILLE(NM,I),J)=0
            ENDIF
20       CONTINUE
       ENDIF
10    CONTINUE 

!C JMB only reorganise if NM is not the last mesh,
!C   otherwise MA->MA-1 is enough
      IF(NM.LT.MA) THEN
!C Move MA into NM position and modify pointers to MA
!C to pointers into NM
             DO 30 I=4,6
           IF(MAILLE(MA,I).NE.0) THEN
             DO 40 J=4,6
             IF(MAILLE(MAILLE(MA,I),J).EQ.MA) THEN
                  MAILLE(MAILLE(MA,I),J)=NM
             ENDIF
40           CONTINUE
           ENDIF
30        CONTINUE
!C move the triangle into NM position
        DO 50 I=1,6
            MAILLE(NM,I)=MAILLE(MA,I)
50        CONTINUE
      ENDIF
!c      write(6,*) 'number of elements',MA
      MA=MA-1
      RETURN
      END subroutine



!C --------------------------------------
!C     SOUS-ROUTINE   RAFFIN1
!C --------------------------------------

       SUBROUTINE RAFFIN1(MAILLE,NOEUD,NMAX,MMAX,NO,MA,S,G,KN,NR,NRMAX,KNMAX,P)

!C     RAFFINAGE DU MAILLAGES PAR INTRODUCTION DE NOUVEAU
!C      NOEUD AU CENTRE DE GRAVITE DES MAILLES DE SURFACE
!C      SUPERIEURES A LA SURFACE CARACTERISTIQUE A UN
!C      FACTEUR C PRES. 'C' EST LU DANS UN FICHIER 11

      
      IMPLICIT NONE
      INTEGER*4 I,P,NRMAX,NR
      INTEGER*4 NMAX,MMAX,MA,NO,KNMAX(NRMAX),MAILLE(MMAX,6)
!c      real*8 NOEUD(NMAX,2),XP,YP,XM,YM,X1,Y1,X2,Y2,X3,Y3
!c      real*8 A,B,C,D,E,F,AR,L,KN(NMAX,2),G(NRMAX),S
!CJMB
      REAL*8 X1,X2,X3,Y1,Y2,Y3,B,D,E,F,A,XM,YM,AR
      real*8 NOEUD(NMAX,2),XP,YP
      real*8 C,L,KN(NMAX,2),G(NRMAX),S

      READ(11,*) C
     
       I=0
10     I=I+1
!c       write(6,*) 'node ',i
       IF(I.EQ.MA) GOTO 20 
       X1=NOEUD(MAILLE(I,1),1)
       X2=NOEUD(MAILLE(I,2),1)
       X3=NOEUD(MAILLE(I,3),1)
       Y1=NOEUD(MAILLE(I,1),2)
       Y2=NOEUD(MAILLE(I,2),2)
       Y3=NOEUD(MAILLE(I,3),2)
       B=(X1-X3)**2+(Y1-Y3)**2
       D=(X2-X3)**2+(Y2-Y3)**2
       E=SQRT((X1-X2)**2+(Y1-Y2)**2)
       F=SQRT(D-((B-D)/2/E-E/2)**2)
       A=E*F/2
       XM=(X2+X1)/2
       YM=(Y1+Y2)/2
       XP=(X3+2*XM)/3
       YP=(Y3+2*YM)/3
       L=S
!c       write(6,*) '??L,A',L,A
       IF (P.EQ.1) CALL RAFLOC(KN,NR,NRMAX,KNMAX,G,XP,YP,L,NMAX) 
!c       write(6,*) '??Lafter,A',L,A
       AR=L*L*SQRT(3.D0)*C/4
       IF(A.GE.AR) THEN
!c          write(6,*) 'Adding node',AR,L,C,A
!c          write(6,*) 'Coord',X1,X2,X3,y1,y2,y3
          CALL NEWNO(MAILLE,NOEUD,NMAX,MMAX,NO,MA,XP,YP,I)
          I=1
       ENDIF
      GOTO 10
20      CONTINUE          
      RETURN
      END subroutine

!C --------------------------------------
!C     SOUS-ROUTINE   DELMACONC
!C --------------------------------------

      SUBROUTINE DELMACONC(MAILLE,PPTC,MMAX,NCMAX,MA,NC,MAC)

      IMPLICIT NONE

!C     SUPPRIME LES MAILLES DE CONCAVITE CORRESPONDANTS LA MAILLE D'ENTREE MAC
!C JMB: par methode de virus? en trouver un et puis verifier les mailles voisines?
!C

      integer jm1,jm2
      parameter(jm1=20000,jm2=40000)
      INTEGER*4 MMAX,MA,MAC,I,J,K,NT,NV,MAILLE(MMAX,6)
      INTEGER*4 ATEST(jm1),LMA(jm2),N1,N2,E,F,M,NCMAX,PPTC(NCMAX),NC,L
!c      write(6,*) 'into delmaconc',NC,MAC,PPTC(NC)
!c      do i=1,MA
!c      write(6,*) (MAILLE(I,J),j=1,6)
!c      enddo
      I=1
      L=1
      LMA(1)=MAC
!C JMBTEST brutal:
!C
!c      goto 100
      
!C end test
      ATEST(1)=MAC 
10    NT=ATEST(I)
!c      write(6,*) 'loop 10',i
      I=I-1
      J=1
20    IF(LMA(J).EQ.NT) GOTO 30
!c      write(6,*) 'loop 20',i,j,L,LMA(J),NT
      J=J+1
      IF(J.GT.L) THEN
        L=L+1
        LMA(L)=NT
        GOTO 30
      ENDIF
      GOTO 20
30    DO 40 J=4,6
      NV=MAILLE(NT,J)
      IF(NV.EQ.0) GOTO 50
      K=4
60    IF(MAILLE(NV,K).EQ.NT)  GOTO 70
      K=K+1 
      GOTO 60
!C  K MAILLE(NV,K) now is the place that points to NT
70    N1=MAILLE(NT,J-3)
      IF(J.LT.6) THEN 
        N2=MAILLE(NT,J-2)
       ELSE
        N2=MAILLE(NT,1)
      ENDIF
      E=1
      F=0
      DO 80 M=1,NC
      IF(N1.EQ.(F+1)) E=1-PPTC(M)
        F=F+PPTC(M)
80    CONTINUE
      IF(N1.EQ.(N2+E)) THEN
        GOTO 50
      ELSE
        I=I+1
!c        write(6,*) 'Loop',I
        ATEST(I)=NV
        MAILLE(NT,J)=0
        MAILLE(NV,K)=0
      ENDIF
50    CONTINUE
40    CONTINUE
90    IF(I.LT.1) GOTO 100
!C ????
!C      IF(I.LE.1) GOTO 100
!C????
      GOTO 10
100   DO 110 I=1,L
       DO 120 J=1,L
          IF(LMA(J).EQ.MA) LMA(J)=LMA(I)
120      CONTINUE
!c      write(6,*) 'into delma',I,MA,LMA(I)
      CALL DELMA(MAILLE,MMAX,MA,LMA(I))
110   CONTINUE
      RETURN
      END subroutine

! --------------------------------------
!     SOUS-ROUTINE   LISSAGE
! --------------------------------------

SUBROUTINE smoothMesh(cell,node,maxNbOfNode,maxNbOfCell,nbOfNode,nbOfCell,nbOfBoundaryNode)

!   Declaration
!   ===========
    INTEGER(KIND=4), INTENT(IN) :: maxNbOfNode,maxNbOfCell,nbOfNode, nbOfCell, nbOfBoundaryNode
    REAL(KIND=8), DIMENSION(maxNbOfNode,2), INTENT(INOUT), TARGET :: node
    INTEGER(KIND=4), DIMENSION(maxNbOfCell,6), INTENT(IN) :: cell

    TYPE(stencilDataBase) :: stencilDB, stencilMeshDB
    INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrNodeToCell
    TYPE(stencil), DIMENSION(:), POINTER :: ptrStencilDB
    TYPE(stencil), POINTER :: ptrStencil
    TYPE(vectorInteger4), POINTER :: ptrStencilRef
    INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrStencilValues
    INTEGER(KIND=4), DIMENSION(3,3) :: mat

    INTEGER :: i0, i1, i2, i3, i4, i5, iStart, iEnd, stencilSize, nbOfSmooth
    REAL(KIND=8) :: maxDValue, posXOld, posYOld
    REAL(KIND=8), POINTER :: ptrNodeX, ptrNodeY

!   Body
!   ====

    READ(11,*) nbOfSmooth
    CLOSE(11)

    mat(1,:) = (/1,2,3/)
    mat(2,:) = (/2,3,1/)
    mat(3,:) = (/3,1,2/)

    CALL stencilDBCreate(stencilDB,nbOfNode)
    CALL stencilDBInitialise(stencilDB)

    ptrStencilDB => stencilDBGetValues(stencilDB)

    DO i1 = 1, nbOfCell
       DO i0 = 1, 3
         i2 = mat(i0,1)
         i4 = mat(i0,2)
         i5 = mat(i0,3)
         ptrStencil => ptrStencilDB(cell(i1,i2))
         ptrStencilRef => ptrStencil%stencilNode
         i3 = ptrStencil%lastPositionNode
         i4 = cell(i1,i4)
         IF ( .NOT.(vectorIsAlreadyIn(ptrStencilRef,i4) )) THEN
            i3 = i3 + 1
            CALL vectorInsertValue(ptrStencilRef,i3,i4)
         ENDIF

         i4 = cell(i1,i5)
         IF ( .NOT.(vectorIsAlreadyIn(ptrStencilRef,i4) )) THEN
            i3 = i3 + 1
            CALL vectorInsertValue(ptrStencilRef,i3,i4)
         ENDIF

         ptrStencil%lastPositionNode = i3

         ptrStencilRef => ptrStencil%stencilCell
         i3 = ptrStencil%lastPositionCell
         IF ( .NOT.(vectorIsAlreadyIn(ptrStencilRef,i1) )) THEN
            i3 = i3 + 1
            CALL vectorInsertValue(ptrStencilRef,i3,i1)
         ENDIF

         ptrStencil%lastPositionCell = i3

      ENDDO
   ENDDO

   PRINT*, 'Maximum Displacement During Smoothing'

   DO i1 = 1, nbOfSmooth
    maxDValue = 0.
    DO i2 = nbOfBoundaryNode + 1 , nbOfNode
       ptrStencil => ptrStencilDB(i2)
       ptrStencilRef => ptrStencil%stencilNode

       iStart = vectorGetFirstIndex(ptrStencilRef)
       iEnd = vectorGetLastIndex(ptrStencilRef)
       stencilSize = iEnd - iStart + 1

       ptrStencilValues => vectorGetValues(ptrStencilRef)
       ptrNodeX => node(i2,1)
       ptrNodeY => node(i2,2)

       posXOld = ptrNodeX
       posYOld = ptrNodeY

       ptrNodeX = sum(node(ptrStencilValues(iStart:iEnd),1)) / stencilSize
       ptrNodeY = sum(node(ptrStencilValues(iStart:iEnd),2)) / stencilSize

       ptrStencilRef => ptrStencil%stencilCell

       IF ( checkNewPosition(ptrStencilRef,cell,node,maxNbOfNode,maxNbOfCell,nbOfNode,nbOfCell) ) THEN
          maxDValue = max(maxDValue, sqrt((ptrNodeX-posXOld)*(ptrNodeX-posXOld)+(ptrNodeY-posYOld)*(ptrNodeY-posYOld)) )
       ELSE
          ptrNodeX = posXOld
          ptrNodeY = posYOld
       ENDIF

    ENDDO
    PRINT*,i1,maxDValue
   ENDDO

   CALL stencilDBDestroy(stencilDB)

END SUBROUTINE

FUNCTION checkNewPosition(ptrStencilRef,cell,node,maxNbOfNode,maxNbOfCell,nbOfNode,nbOfCell) RESULT(check)

!   Declaration
!   ===========
    INTEGER(KIND=4), INTENT(IN) :: maxNbOfNode,maxNbOfCell,nbOfNode, nbOfCell
    REAL(KIND=8), DIMENSION(maxNbOfNode,2), INTENT(IN), TARGET :: node
    INTEGER(KIND=4), DIMENSION(maxNbOfCell,6), INTENT(IN) :: cell
    TYPE(vectorInteger4), POINTER :: ptrStencilRef
    INTEGER :: i1, iStart, iEnd, cellToConsider, node1, node2, node3

    LOGICAL :: check
    INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrStencilValues
    REAL(KIND=8) :: dxNode12, dyNode12, dxNode13, dyNode13, surface, surfaceLimit
    REAL(KIND=8), PARAMETER :: demi = 0.5D-1
    REAL(KIND=8), POINTER :: ptrNode1X, ptrNode1Y

!   Body
!   ====
    check = .TRUE.

    iStart = vectorGetFirstIndex(ptrStencilRef)
    iEnd = vectorGetLastIndex(ptrStencilRef)

    ptrStencilValues => vectorGetValues(ptrStencilRef)

    DO i1 = iStart, iEnd
       cellToConsider = ptrStencilValues(i1)
       node1 = cell(cellToConsider,1)
       node2 = cell(cellToConsider,2)
       node3 = cell(cellToConsider,3)
       ptrNode1X => node(node1,1)
       ptrNode1Y => node(node1,2)
       dxNode12 = node(node2,1) - ptrNode1X
       dyNode12 = node(node2,2) - ptrNode1Y
       dxNode13 = node(node3,1) - ptrNode1X
       dyNode13 = node(node3,2) - ptrNode1Y

       surface = dxNode12 * dyNode13 - dyNode12 * dxNode13
       surfaceLimit = dyNode12 * dyNode12
       surfaceLimit = max(surfaceLimit,dxNode13 * dxNode13)
       surfaceLimit = max(surfaceLimit,dxNode12 * dxNode12)
       surfaceLimit = max(surfaceLimit,dxNode13 * dxNode13)

       surfaceLimit = demi * surfaceLimit

       IF ( surface < surfaceLimit ) THEN
          check = .FALSE.
          RETURN
       ENDIF

    ENDDO

END FUNCTION


!C --------------------------------------
!C     SOUS-ROUTINE   RAFLOC
!C --------------------------------------

      SUBROUTINE RAFLOC(KN,NR,NRMAX,KNMAX,G,XP,YP,L,NMAX)

      IMPLICIT NONE

!C     LOCALISE A QUELLE REGION APPARTIENT LE PT XP,YP ET RETOURNE
!C      LA VALEUR DE L CORRESPONDANTE (INCHANGEE SI EN DEHORS D'UNE REGION)

      
      INTEGER*4 I,J,T,M,N,NR,NRMAX,NMAX,KNMAX(NRMAX)
      real*8 XP,YP,KN(NMAX,2),G(NRMAX),L
      REAL*8 X1,X2,Y1,Y2,V,A,B
      M=0
      DO 10 I=1,NR 
       N=M
       M=M+KNMAX(I)
       IF (I.EQ.NR) THEN 
          A=0
          B=0  
       ELSE
          A=KN(M+1,1)
          B=KN(M+1,2)
       ENDIF 
       KN(M+1,1)=KN(N+1,1)
       KN(M+1,2)=KN(N+1,2)
       T=0
       DO 20 J=N+1,M
          X1=KN(J,1)
          X2=KN(J+1,1)
          Y1=KN(J,2)
          Y2=KN(J+1,2)
          V=(X1-X2)*(Y1-YP)-(X1-XP)*(Y1-Y2)
          IF(V.GE.0) T=T+1
20       CONTINUE
       IF (T.EQ.KNMAX(I)) THEN
          L=G(I)
       ENDIF
       KN(M+1,1)=A
       KN(M+1,2)=B
10    CONTINUE
      RETURN
      END subroutine
 
!C --------------------------------------
!C     SOUS-ROUTINE   AJOUT
!C --------------------------------------

      SUBROUTINE AJOUT(MAILLE,NOEUD,NMAX,MMAX,NO,MA,N,NC,PPTC,NCMAX)

      IMPLICIT NONE

!C     RAJOUTE UN POINT SI DEUX POINTS DE CONTOUR NE SONT PAS RELIES

     
      INTEGER*4 I,J,K,M,NO,MA,NMAX,NCMAX,MMAX,MAILLE(MMAX,6)
      INTEGER*4 R,P,L,NC,PPTC(NCMAX),N,Q,MP,istop
      real*8 NOEUD(NMAX,2),XP,YP
      integer*4 ichange
      ichange=0
 777  continue
      MP=1
      P=4
      Q=-10000000
 
      L=1
!c      if (ichange.eq.0) goto 500
 888  continue
      
!c      DO 50 L=1,NC
       if(L.GT.NC) GOTO 50
       
       R=P
       P=PPTC(L)+P
       
       
 111   continue
        i=r+1
 112    if(i.gt.P) goto 40
        
!c       DO 40 I=R+1,P
5          DO 30 J=1,MA 
            DO 20 K=1,3
              IF (MAILLE(J,K).EQ.I) THEN
                  Q=I+1
                  IF(I.EQ.P) Q=R+1
                  DO 10 M=1,3           
                    IF (MAILLE(J,M).EQ.Q) GOTO 35
10                    CONTINUE
              ENDIF
20             CONTINUE
30         CONTINUE
         XP=(NOEUD(I,1)+NOEUD(Q,1))/2
         YP=(NOEUD(I,2)+NOEUD(Q,2))/2
         WRITE(*,'(A)') 'NEW CONTOUR POINT:'
         WRITE(*,*) XP,YP,I,Q,L
          CALL NEWNO(MAILLE,NOEUD,NMAX,MMAX,NO,MA,XP,YP,MP)
         write(6,*) 'Need to resort grid'
          CALL RENUM(MAILLE,NOEUD,NMAX,MMAX,NO,MA,I) 
!C Test
!c              write(6,*) 'stop?'
!c              read(5,*) istop
!c              if (istop.eq.1) then
!c              CALL SORTIE(MAILLE,NOEUD,NMAX,MMAX,NO,MA)
!c              stop
!c              endif
          PPTC(L)=PPTC(L)+1
          N=N+1
          P=P+1
          ichange=1
!c test
          i=r+1
         GOTO 5
35         J=1
!c       write(6,*) 'Contour test',I,N,P
       i=i+1
       goto 112
40     CONTINUE
      L=L+1 
      goto 888
50    CONTINUE 
      if (ichange.eq.1) then 
      write(6,*) 'another pass'
      ichange=0
      goto 777
      endif
500   continue
      RETURN
      END subroutine

!C --------------------------------------
!C     SOUS-ROUTINE   RENUM
!C --------------------------------------

      SUBROUTINE  RENUM(MAILLE,NOEUD,NMAX,MMAX,NO,MA,I)
      
      IMPLICIT NONE

!C     REORGANISE LES TABLEAUX NOEUD ET MAILLE DE FACON
!C     A CE QUE LES NOEUD SOIENT DANS L'ORDRE


      INTEGER*4 I,NO,MA,NMAX,MMAX,MAILLE(MMAX,6),M,J,K,S,L
      real*8 NOEUD(NMAX,2),A,B

      DO 50 L=NO,I,-1

        M=L
        IF(L.EQ.I) M=NO+1

        DO 20 J=1,MA
             DO 10 K=1,3
                IF(MAILLE(J,K).EQ.M) GOTO 30
10             CONTINUE               
20        CONTINUE
30        CONTINUE
 
        MAILLE(J,K)=L+1
        S=J
35         IF(MAILLE(S,K+3).NE.J) THEN
           S=MAILLE(S,K+3)
           DO 40 K=1,3
              IF (MAILLE(S,K).EQ.M) THEN
                MAILLE(S,K)=L+1
                GOTO 35
              ENDIF
40           CONTINUE
         ENDIF
50    CONTINUE
      A=NOEUD(NO,1)
      B=NOEUD(NO,2)   
      DO 60 L=NO,I+2,-1
         NOEUD(L,1)=NOEUD(L-1,1)
         NOEUD(L,2)=NOEUD(L-1,2)
60    CONTINUE
      NOEUD(I+1,1)=A
      NOEUD(I+1,2)=B
      RETURN 
      END subroutine

      subroutine REORG(MAILLE,NOEUD,NMAX,MMAX,NO,MA,NTRI)
!C ------------------------------------------------------------------
!C ---                                                            ---
!C ---   REORGANISATION DE LA LISTE DES NOEUDS ET DES MAILLES     ---
!C ---   ET CREATION DES NOEUDS D'INTERFACE                       ---
!C ---                                                            ---
!C ---                                                            ---
!C ---  ADAPTATION :  SCHOENAUEN 25/01/94                         ---
!C ---                                                            ---
!C ---   Version Officielle :                                     ---
!C ---       7 MARS 94 :  sortie fort.24 devient fort.23          ---
!C ------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER*4 NMAX,MMAX
      integer NW,JMBOPT, icoordchange
      PARAMETER(NW=1000000)
      REAL*8 XX(NW)
!C      PARAMETER(NMAX=00,MMAX=100000)

!C ---  NMAX: NOMBRE MAX DE NOEUDS
!C ---  MMAX: NOMBRE MAX DE MAILLES

      real*8 NOEUD(NMAX,2),X1,X2,Y1,Y2,X,Y

      INTEGER*4 MAILLE(MMAX,6),MA,NO,NS,JJ
      INTEGER*4 I,J,K,L,NTRI(NMAX),IND,I0,I1
      real*8 xxx,yyy

      REAL(KIND=8) :: meanPosX, meanPosY

!C --- LECTURE DES TABLEAUX DE MAILLES ET DE NOEUDS


      include'iodv.h'

      WRITE(*,'(A)') 'REORGANISATION OF GRID'
      
!C      OPEN(UNIT=21,FILE='fort.21')
!C      READ(21,*) NO
!C      READ(21,*) MA
!C      CLOSE(21)

      IF(NO.GT.NMAX) STOP 'Too many nodes increase NMAX'
      IF(MA.GT.MMAX) STOP 'Too many elements, increase MMAX'
      IF(NO.GT.NW) STOP 'Too many nodes increase NW'
 
!C      OPEN(UNIT=20,file='fort.20')
!C      DO 10 I=1,NO
!C        READ(20,*) JJ,NOEUD(JJ,1),NOEUD(JJ,2)
!C10    CONTINUE

!C      DO 20 I=1,MA
!C        READ(20,*) JJ,MAILLE(JJ,1),MAILLE(JJ,2),
!C     &   MAILLE(JJ,3),MAILLE(JJ,4),MAILLE(JJ,5),MAILLE(JJ,6)
!C20    CONTINUE
!C      CLOSE(20)

!C --- INTRODUCTION DES NOEUDS D'INTERFACE

      NS=NO
      DO 70 I=1,MA
         DO 60 J=1,3
            IF(MAILLE(I,J+3).GE.0) THEN 
                X1=NOEUD(MAILLE(I,J),1)
                Y1=NOEUD(MAILLE(I,J),2)

                IF(J.LE.2) THEN
                    X2=NOEUD(MAILLE(I,J+1),1)
                    Y2=NOEUD(MAILLE(I,J+1),2)
                ELSE
                    X2=NOEUD(MAILLE(I,1),1)
                    Y2=NOEUD(MAILLE(I,1),2)
                ENDIF

                X=(X1+X2)/2
                Y=(Y1+Y2)/2

                IF(NO.EQ.NMAX) THEN
                    STOP 'Too many nodes, increase NMAX'
                ENDIF

                NO=NO+1
                NOEUD(NO,1)=X
                NOEUD(NO,2)=Y

                DO 40 L=4,6
                   if (MAILLE(I,J+3).LE.0) then
!c                   write(6,*) '???JM',MAILLE(I,J+3),I,J
                   goto 40
                   endif
                   IF(MAILLE(MAILLE(I,J+3),L).EQ.I) MAILLE(MAILLE(I,J+3),L)=-NO
40              CONTINUE

                MAILLE(I,J+3)=-NO
            ENDIF
60       CONTINUE
70    CONTINUE

!C --- TRI DES NOEUDS,  DANS UN TABLEAU NTRI
      JMBOPT=1
      IF (JMBOPT.EQ.1) THEN
      DO  I=1,NO
         NTRI(I)=I
         XX(I)=NOEUD(I,1)+1D-7*NOEUD(I,2)
      ENDDO
      call QS2I1R(XX,NTRI,NO)
                       
!C JMB COULD BE optimised?
                       ELSE

      write(6,*) 'Going to sort nodes'
      DO 80 I=1,NO
         NTRI(I)=I
80    CONTINUE

!C ---  PREMIER TRI SUIVANT X

90    IND=0

      DO 100 I=1,NO-1
         I0=NTRI(I)
         I1=NTRI(I+1)
         X1=NOEUD(I0,1)
         X2=NOEUD(I1,1)

         IF(X1.GT.X2) THEN
             NTRI(I)=I1
             NTRI(I+1)=I0
             IND=1
         ENDIF

100   CONTINUE

      IF(IND.EQ.1) GOTO 90

!C --- TRI DES X IDENTIQUES SUIVANT Y

      I=1

110   continue
      if (ntri(i).eq.0) then
      write(6,*) '?xxx',i
      goto 1201
      endif
      X1=NOEUD(NTRI(I),1)
      if (ntri(i+1).eq.0) then
      write(6,*) '?xxxb',i+1
      goto 1201
      endif
      
      
      X2=NOEUD(NTRI(I+1),1)
 1201 continue
      IF(X1.EQ.X2) THEN
         J=I+1

120      continue
         if (ntri(j).eq.0) then
      write(6,*) '?xxxj',j
      goto 1202
      endif
      
         X1=NOEUD(NTRI(J),1)
!CJMB
      if(J+1.GT.NO) goto 130    
!C JMB
      if (ntri(j+1).eq.0) then
      write(6,*) '?xxxd',j+1
      goto 1202
      endif
      
         X2=NOEUD(NTRI(J+1),1)
 1202  continue
         IF(X1.EQ.X2) THEN
             J=J+1
             GOTO 120
         ENDIF

130      IND=0

         DO 140 K=I,J-1
            I0=NTRI(K)
            I1=NTRI(K+1)
            Y1=NOEUD(I0,2)
            Y2=NOEUD(I1,2)
            IF(Y1.GT.Y2) THEN
                 NTRI(K)=I1
                 NTRI(K+1)=I0
                 IND=1
            ENDIF
140      CONTINUE

         IF(IND.EQ.1) GOTO 130
            I=J
         ELSE
            I=I+1
         ENDIF

         IF(I.GT.NO) GOTO 150
!C??JMBB
         IF(I.GE.NO-1) GOTO 150
         GOTO 110
150   CONTINUE
!C
      ENDIF
      write(6,*) 'Finished sorting'

!C --- SORTIE DES RESULTATS DANS LE FORMAT ...
!c**rs
      if(iodv.eq.1) then
      open(22,file='fort.11')
!c**rs      OPEN(UNIT=22,file='fort.22')
      else
      OPEN(UNIT=22,file='fort.22')
      endif

      meanPosX = getMeanXCoordinate()
      meanPosY = getMeanYCoordinate()

      NOEUD(1:NS,1)=NOEUD(1:NS,1)+meanPosX
      NOEUD(1:NS,2)=NOEUD(1:NS,2)+meanPosY

      IF ( getIChangeCoordinate() /= 0 ) THEN
         CALL changeXYToLongLat(NOEUD(1:NS,1),NOEUD(1:NS,2),NS)
      END IF

      DO 160 I=1,NO
         IF(I.LE.NS) THEN
            WRITE(22,*) NTRI(I),NOEUD(I,1),NOEUD(I,2)
         ELSE
            WRITE(22,*) NTRI(I)
         ENDIF
160   CONTINUE

      DO 170 I=1,MA
         WRITE(22,*) MAILLE(I,1),MAILLE(I,4),MAILLE(I,2),MAILLE(I,5),MAILLE(I,3),MAILLE(I,6)
170   CONTINUE
      CLOSE(22)

      OPEN(UNIT=23,file='fort.23')
      WRITE(23,*) NS
      WRITE(23,*) (NO-NS)
      WRITE(23,*) MA
      CLOSE(23)
      WRITE(*,'(A)') 'INTERFACE NODES CREATED:'
      WRITE(*,*) NO-NS

      PRINT*,'Mesh information '
      PRINT*,'NS = ',NS, ' NS reel = 3078 '
      PRINT*,'NO-NS = ',NO-NS, ' NO-NS reel = 7306 '
      PRINT*,'MA = ',MA , ' MA reel = 4222 '
      return
      END subroutine

      
      subroutine QS2I1R (IA,JA,N)
!C=============================================================================
!C *** DESCRIPTION (from www.netlib.org)
!C     Written by Rondall E Jones
!C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
!C     algorithm. date 18 November 1976.
!C
!C     Further modified by David K. Kahaner
!C     National Bureau of Standards
!C     August, 1981
!C
!C     Even further modification made to bring the code up to the
!C     Fortran 77 level and make it more readable and to carry
!C     along one integer array and one real*8 array during the sort by
!C     Mark K. Seager
!C     Lawrence Livermore National Laboratory
!C     November, 1987
!C     This routine was adapted from the ISORT routine.
!C
!C     ABSTRACT
!C         This routine sorts an  array IA and makes the same
!C         interchanges in the integer array JA
!C         The array IA may be sorted in increasing order
!C         A slightly modified quicksort algorithm is used.
!C
!C     DESCRIPTION OF PARAMETERS
!C        IA -  array of values to be sorted.
!C        JA - Integer array to be carried along.
!C
!C         N - Number of values in integer array IA to be sorted.
!
!C     .. Scalar Arguments ..
      implicit none
      INTEGER N
!C     .. Array Arguments ..
      real*8  IA(N) 
      integer*4 JA(N)
!C     .. Local Scalars ..
      real*8 R 
      INTEGER*4 I,  IJ,  J, JJT, JT, K, L, M,NN
      REAL*8   IIT,  IT
!C     .. Local Arrays ..
      INTEGER*4 IL(21), IU(21)

!C --- FIRST EXECUTABLE STATEMENT  QS2I1R ---
      NN=N
      if (N.EQ.1) then
      write(6,*) 'No need to sort a single data point'
      return
      endif

!C     Sort IA and carry JA and A along.
!C     And now...Just a little black magic...
      M = 1
      I = 1
      J = NN
      R = .375E0
 210  IF( R.LE.0.5898437E0 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875E0
      ENDIF
 225  K = I

!C     Select a central element of the array and save it in location
!C     it, jt, at.
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)

!C     If first element of array is greater than it, interchange with it.
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
      ENDIF
      L=J

!C     If last element of array is less than it, swap with it.
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)

!C     If first element of array is greater than it, swap with it.
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
         ENDIF
      ENDIF

!C     Find an element in the second half of the array which is
!C     smaller than it.
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240

!C     Find an element in the first half of the array which is
!C     greater than it.
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245

!C     Interchange these elements.
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         GOTO 240
      ENDIF

!C     Save upper and lower subscripts of the array yet to be sorted.
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260

!C     Begin again on another portion of the unsorted array.
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      GO TO 265

 300  CONTINUE
      RETURN
!C=============================================================================
      END subroutine


SUBROUTINE changeLongLatToXY(xValue,yValue, nbOfData)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfData
      REAL(KIND=8), DIMENSION(nbOfData), INTENT(INOUT) :: xValue, yValue

      REAL(KIND=8) :: deltaXInKm, deltaYInKm, meanLongitude, meanLatitude, deltaY, pi

!     Body
!     - - -
      meanLongitude = getMeanLongitude()

      SELECT CASE (getISpheric())
         CASE (1)
            pi = getPi() / 180.
            deltaY = 0.001 * ( getMaximumLatitude() - getMinimumLatitude() )

            xValue = ( xValue - meanLongitude ) * max(cos(yValue*pi),cos(90.*pi-deltaY*pi))

         CASE DEFAULT
            deltaXInKm = getDeltaXInKm()
            deltaYInKm = getDeltaYInKm()

            meanLatitude = getMeanLatitude()

            xValue = ( xValue - meanLongitude ) * deltaXInKm
            yValue = ( yValue - meanLatitude ) * deltaYInKm

      END SELECT

END SUBROUTINE

SUBROUTINE changeXYToLongLat(xValue,yValue,nbOfData)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfData
      REAL(KIND=8), DIMENSION(nbOfData), INTENT(INOUT) :: xValue, yValue

      REAL(KIND=8) :: deltaXInKm, deltaYInKm, meanLongitude, meanLatitude, deltaY, pi

!     Body
!     - - -
      meanLongitude = getMeanLongitude()

      SELECT CASE (getISpheric())
         CASE (1)
            pi = getPi() / 180.
            deltaY = 0.001 * ( getMaximumLatitude() - getMinimumLatitude() )

            xValue = xValue /  max(cos(yValue*pi),cos(90.*pi-deltaY*pi)) + meanLongitude

         CASE DEFAULT
            deltaXInKm = getDeltaXInKm()
            deltaYInKm = getDeltaYInKm()

            meanLatitude = getMeanLatitude()

            xValue =  xValue / deltaXInKm + meanLongitude
            yValue =  yValue / deltaYInKm + meanLatitude

      END SELECT

END SUBROUTINE



! export vers gmsh
SUBROUTINE exportToGMSH(cell,node,nbOfNodeMax,nbOfCellMax,nbOfNode,nbOfCell)


! Declaration
! -----------
     IMPLICIT NONE
     INTEGER(KIND=4) :: nbOfCell, nbOfNode,nbOfNodeMax,nbOfCellMax
     REAL(KIND=8) :: node(nbOfNodeMax,2)
     INTEGER(KIND=4) :: cell(nbOfCellMax,6)

     INTEGER :: i1, i2

! Body
! ----
     OPEN(unit=51,file='diva.msh')
     WRITE(51,810)

!    3) write information
!    - - - - - - - - - - -

!       3.1) nodes
!       + + + + + +
     WRITE(51,820)
     WRITE(51,*) nbOfNode
     DO i1 = 1, nbOfNode
        WRITE(51,*) i1, node(i1,1), node(i1,2), 0.
     ENDDO
     WRITE(51,830)

!       3.2) cell topology
!       + + + + + + + + + +
     WRITE(51,840)
     WRITE(51,*) nbOfCell
     DO i1 = 1, nbOfCell
        WRITE(51,*) i1,2,2,99,2,cell(i1,1),cell(i1,2),cell(i1,3)
     ENDDO
     WRITE(51,850)
     CLOSE(51)

810  FORMAT("$MeshFormat",/,"2.1 0 8",/,"$EndMeshFormat")
820  FORMAT("$Nodes")
830  FORMAT("$EndNodes")
840  FORMAT("$Elements")
850  FORMAT("$EndElements")

END SUBROUTINE


RECURSIVE SUBROUTINE clockCounter(istart,xNode,yNode,check)

! Declaration
! -----------
     IMPLICIT NONE
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     INTEGER, INTENT(IN) :: istart

     REAL(KIND=8), DIMENSION(2) :: a,b
     REAL(KIND=8) :: checkValue
     REAL(KIND=8), PARAMETER :: tolerance = 1.D-10

     LOGICAL, INTENT(OUT) :: check

! Body
! ----

    a(1) = xNode(istart+2)-xNode(istart+1)
    a(2) = yNode(istart+2)-yNode(istart+1)
    b(1) = xNode(istart)-xNode(istart+1)
    b(2) = yNode(istart)-yNode(istart+1)

    checkValue = a(1)*b(2)-a(2)*b(1)
    IF ( abs(checkValue) < tolerance ) THEN
      CALL clockCounter(istart+1,xNode,yNode,check)
    ELSE
       IF ( checkValue < 0. ) THEN
          check = .TRUE.
       ELSE
          check = .FALSE.
       ENDIF
    ENDIF

END SUBROUTINE

SUBROUTINE meshWithGMSH(listOfContour,xNode,yNode,characteristicLength,nbOfContour)

! Declaration
! -----------
     IMPLICIT NONE
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     INTEGER(KIND=4), INTENT(IN) :: listOfContour(*)
     REAL(KIND=8), INTENT(IN) :: characteristicLength
     INTEGER, INTENT(IN) :: nbOfContour
     INTEGER :: nbOfLine

     REAL(KIND=8) :: length

! Body
! ----
     length = characteristicLength
     OPEN(UNIT=80,FILE='diva2.geo')
        CALL writeNodeToGeoFile(80,listOfContour,xNode,yNode,length,nbOfContour)
!        CALL writeSplineToGeoFile(80,listOfContour,nbOfContour)
        CALL writeLineToGeoFile(80,listOfContour,nbOfContour,nbOfLine)
!        CALL writeSurfaceToGeoFile(80,listOfContour,xNode,yNode,length,nbOfContour)
        CALL writeCommand(80,nbOfLine)
     CLOSE(80)

END SUBROUTINE

SUBROUTINE writeNodeToGeoFile(logicalUnit,listOfContour,xNode,yNode,characteristicLength,nbOfContour)

! Declaration
! -----------
     IMPLICIT NONE
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     INTEGER(KIND=4), INTENT(IN) :: listOfContour(*)
     REAL(KIND=8), INTENT(IN) :: characteristicLength
     INTEGER, INTENT(IN) :: nbOfContour, logicalUnit

     INTEGER :: i1, i2, istart, ifirstNode
     LOGICAL :: check

! Body
! ----
     istart = 1
     ifirstNode = 1
     DO i1 = 1, nbOfContour
        CALL writeANodeToGeoFile(80,istart,istart+listOfContour(i1)-1,xNode,yNode,characteristicLength,ifirstNode)
        istart = istart + listOfContour(i1)
     ENDDO

END SUBROUTINE

SUBROUTINE writeLineToGeoFile(logicalUnit,listOfContour,nbOfContour,nbOfLine)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER(KIND=4), INTENT(IN) :: listOfContour(*)
     INTEGER, INTENT(IN) :: nbOfContour, logicalUnit
     INTEGER, INTENT(OUT) :: nbOfLine

     INTEGER :: i1, i2, istart, ifirstNode, ifirstLine, ifirstLineLoop, startLine, endLine
     LOGICAL :: check

! Body
! ----
     istart = 1
     ifirstNode = 1
     ifirstLine = 1
     ifirstLineLoop = 1
     DO i1 = 1, nbOfContour
        startLine = ifirstLine
        CALL writeALineToGeoFile(80,istart,istart+listOfContour(i1)-1,ifirstNode,ifirstLine)
        endLine = ifirstLine-1
        CALL writeALineLoopToGeoFile(80,startLine,endLine,ifirstLineLoop)
        istart = istart + listOfContour(i1)
     ENDDO

     nbOfLine = ifirstLine - 1

END SUBROUTINE

SUBROUTINE writeANodeToGeoFile(logicalUnit,istart,iend,xNode,yNode,characteristicLength,ifirstNode)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, istart, iend
     INTEGER, INTENT(INOUT) :: ifirstNode
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     REAL(KIND=8), INTENT(IN) :: characteristicLength

     INTEGER :: i1

! Body
! ----
     DO i1 = istart, iend
        WRITE(logicalUnit,810) ifirstNode,xNode(i1),yNode(i1),0.,characteristicLength
        ifirstNode = ifirstNode + 1
     ENDDO

810  FORMAT("Point(",i20,") = {", f20.10,",", f20.10,",", f20.10,",", f20.10,"};")
END SUBROUTINE

SUBROUTINE writeALineToGeoFile(logicalUnit,istart,iend,ifirstNode,ifirstLine)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, istart, iend
     INTEGER, INTENT(INOUT) :: ifirstNode, ifirstLine

     INTEGER :: i1, startNode

! Body
! ----
     startNode = ifirstNode
     DO i1 = istart, iend - 1
        WRITE(logicalUnit,820) ifirstLine,ifirstNode,ifirstNode+1
        ifirstNode = ifirstNode + 1
        ifirstLine = ifirstLine + 1
!        CALL writeInfoLine(logicalUnit,ifirstLine)
     ENDDO
     WRITE(logicalUnit,820) ifirstLine,ifirstNode,startNode
     ifirstNode = ifirstNode + 1
     ifirstLine = ifirstLine + 1
!     CALL writeInfoLine(logicalUnit,ifirstLine)

820  FORMAT("Line(",i20,") = {",i20,",",i20,"};")
END SUBROUTINE

SUBROUTINE writeALineLoopToGeoFile(logicalUnit,startLine,endLine,ifirstLineLoop)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, startLine, endLine
     INTEGER, INTENT(INOUT) :: ifirstLineLoop

     INTEGER :: i1

! Body
! ----
     WRITE(logicalUnit,830) ifirstLineLoop,startLine
     DO i1 = startLine + 1 , endLine - 1
        WRITE(logicalUnit,840) i1
     ENDDO
     WRITE(logicalUnit,850) endLine
     ifirstLineLoop = ifirstLineLoop + 1

830  FORMAT("Line Loop(",i20,") = {",i20,",")
840  FORMAT("                      ",i20,",")
850  FORMAT("                      ",i20,"};")

END SUBROUTINE

SUBROUTINE writeSurfaceToGeoFile(logicalUnit,listOfContour,xNode,yNode,characteristicLength,nbOfContour)


! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     INTEGER(KIND=4), INTENT(IN) :: listOfContour(*)
     REAL(KIND=8), INTENT(IN) :: characteristicLength
     INTEGER, INTENT(IN) :: nbOfContour

     INTEGER :: i1, i2, istart, ifirstSurface
     INTEGER, PARAMETER :: nbOfContourMax = 1000
     LOGICAL, DIMENSION(nbOfContourMax) :: check
     INTEGER, DIMENSION(nbOfContourMax) :: checkIn
     INTEGER, DIMENSION(nbOfContourMax) :: sortContour
     INTEGER, DIMENSION(nbOfContourMax,2) :: sortBounds
     REAL(KIND=8), DIMENSION(nbOfContourMax) :: sortSize
     REAL(KIND=8) :: xCenter, yCenter
     LOGICAL :: check1

! Body
! ----
     IF ( nbOfContourMax < nbOfContour ) THEN
        PRINT*,'Maximum number of contours in writing surface to gmsh file is lower than the real number of contours'
     ENDIF

     istart = 1
     i2 = 0
     DO i1 = 1, nbOfContour
        sortBounds(i1,1)= istart
        CALL clockCounter(istart,xNode,yNode,check(i1))
        IF ( .NOT.(check(i1)) ) THEN
           i2 = i2 + 1
           sortContour(i2) = i1
           sortSize(i2) = REAL(listOfContour(i1),KIND=8)
        ENDIF
        istart = istart + listOfContour(i1)
        sortBounds(i1,2)= istart - 1
     ENDDO

     CALL QS2I1R (sortSize,sortContour,i2)

     DO i1 = 1, nbOfContour
        IF ( check(i1) ) THEN
           CALL evalCenterOfGravity(xNode,yNode,sortBounds(i1,1),sortBounds(i1,2),xCenter,yCenter)
           CALL searchSurroundingContour(i1,checkIn(i1),sortBounds,xCenter,yCenter,xNode,yNode,sortContour,&
                                                            1,i2,nbOfContourMax,check1)
        ENDIF
     ENDDO

     ifirstSurface = 1
     DO i1 = 1, nbOfContour
        IF ( .NOT.(check(i1)) ) THEN
           CALL writeASurfaceToGeoFile(logicalUnit,i1,check,checkIn,nbOfContour,ifirstSurface)
        ENDIF
     ENDDO

END SUBROUTINE

SUBROUTINE evalCenterOfGravity(xNode,yNode,istart,iend,xCenter,yCenter)

! Declaration
! -----------
     IMPLICIT NONE
     REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
     INTEGER, INTENT(IN) :: istart,iend
     REAL(KIND=8), INTENT(OUT) :: xCenter,yCenter

     INTEGER :: i1

! Body
! ----
     xCenter = 0.
     yCenter = 0.
     DO i1 = istart, iend
        xCenter = xCenter + xNode(i1)
        yCenter = yCenter + yNode(i1)
     ENDDO

     xCenter = xCenter / (iend - istart + 1 )
     yCenter = yCenter / (iend - istart + 1 )

END SUBROUTINE

RECURSIVE SUBROUTINE searchSurroundingContour(contourNb,checkIn,sortBounds,xCenter,yCenter, xNode,yNode, &
                                       sortContour,contourToCheck,maxContourToCheck,nbOfContourMax, check)

! Declaration
! -----------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nbOfContourMax
    INTEGER, INTENT(IN) :: contourNb, contourToCheck, maxContourToCheck
    INTEGER, INTENT(IN) :: sortBounds(nbOfContourMax,2), sortContour(nbOfContourMax)
    REAL(KIND=8), INTENT(IN) :: xCenter,yCenter
    REAL(KIND=8), INTENT(IN) :: xNode(*), yNode(*)
    LOGICAL, INTENT(OUT) :: check
    INTEGER, INTENT(OUT) :: checkIn

    INTEGER :: i1, istart, iend
    REAL(KIND=8), DIMENSION(2) :: a,b
    REAL(KIND=8) :: dotProduct, norm, vectProduct
    LOGICAL :: check2

! Body
! ----
    checkIn = 0

    istart = sortBounds(sortContour(contourToCheck),1)
    iend = sortBounds(sortContour(contourToCheck),2)

    check = .FALSE.
    check2 = .TRUE.

    DO i1 = istart, iend - 1
       a(1) = xNode(i1+1) -xNode(i1)
       a(2) = yNode(i1+1) -yNode(i1)
       b(1) = xCenter - xNode(i1)
       b(2) = yCenter - yNode(i1)
       dotProduct = a(1) * b(1) + a(2) * b(2)
       norm = a(1) * a(1) + a(2) * a(2 )
       IF ( (dotProduct >= 0.).AND.(dotProduct <= norm) ) THEN
          vectProduct = a(1) * b(2) - a(2) * b(1)
          IF ( vectProduct < 0. ) THEN
             check = .TRUE.
             check2 = .FALSE.
             GOTO 10
          ENDIF
       ENDIF
    ENDDO

     a(1) = xNode(istart) -xNode(iend)
     a(2) = yNode(istart) -yNode(iend)
     b(1) = xCenter - xNode(iend)
     b(2) = yCenter - yNode(iend)
     dotProduct = a(1) * b(1) + a(2) * b(2)
     norm = a(1) * a(1) + a(2) * a(2 )
     IF ( (dotProduct >= 0.).AND.(dotProduct <= norm) ) THEN
        vectProduct = a(1) * b(2) - a(2) * b(1)
        IF ( vectProduct < 0. ) THEN
           check2 = .FALSE.
           check = .TRUE.
           GOTO 10
        ENDIF
     ENDIF

    IF ( check ) THEN
        check = check2
        checkIn = sortContour(contourToCheck)
    ENDIF

10  CONTINUE

    IF ( contourToCheck + 1 == maxContourToCheck ) THEN
       checkIn = sortContour(contourToCheck + 1)
       check = .TRUE.
    ENDIF
    IF  ( .NOT.(check) ) THEN
       CALL searchSurroundingContour(contourNb,checkIn,sortBounds,xCenter,yCenter, xNode,yNode, &
                                       sortContour,contourToCheck+1,maxContourToCheck,nbOfContourMax, check)
    ENDIF

END SUBROUTINE

SUBROUTINE writeASurfaceToGeoFile(logicalUnit,i1,check,checkIn,nbOfContour,ifirstSurface)

! Declaration
! -----------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: logicalUnit,i1,nbOfContour
    INTEGER, INTENT(INOUT) :: ifirstSurface
    LOGICAL, INTENT(IN) :: check(*)
    INTEGER, INTENT(IN) :: checkIn(*)

    INTEGER :: i2, i3, nbOfHole
    INTEGER, PARAMETER :: nbOfHoleMax = 1000
    INTEGER, DIMENSION(nbOfHoleMax) :: iHole

! Body
! ----

     nbOfHole = 0
     i3 = 1
     DO i2 = 1, nbOfContour
        IF ( (check(i2)).AND.(checkIn(i2)==i1)) THEN
           nbOfHole = nbOfHole + 1
           iHole(i3) = i2
           i3 = i3 + 1
        ENDIF
     ENDDO

     IF ( nbOfHole == 0 ) THEN
        WRITE(logicalUnit,860) ifirstSurface,i1
     ELSE
        WRITE(logicalUnit,830) ifirstSurface,i1
        DO i2 = 1, nbOfHole - 1
           WRITE(logicalUnit,840) iHole(i2)
        ENDDO
        WRITE(logicalUnit,850) iHole(nbOfHole)
     ENDIF
     ifirstSurface = ifirstSurface + 1

830  FORMAT("Plane Surface(",i20,") = {",i20,",")
840  FORMAT("                          ",i20,",")
850  FORMAT("                          ",i20,"};")
860  FORMAT("Plane Surface(",i20,") = {",i20,"};")

END SUBROUTINE

SUBROUTINE writeSplineToGeoFile(logicalUnit,listOfContour,nbOfContour)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER(KIND=4), INTENT(IN) :: listOfContour(*)
     INTEGER, INTENT(IN) :: nbOfContour, logicalUnit

     INTEGER :: i1, i2, istart, ifirstSpline, ifirstLineLoop, startLine, endLine
     LOGICAL :: check

! Body
! ----
     istart = 1
     ifirstSpline = 1
     ifirstLineLoop = 1
     DO i1 = 1, nbOfContour
        CALL writeASplineToGeoFile(80,istart,istart+listOfContour(i1)-1,ifirstSpline)
        CALL writeASplineLoopToGeoFile(80,ifirstLineLoop,ifirstSpline-1)
        istart = istart + listOfContour(i1)
     ENDDO

END SUBROUTINE

SUBROUTINE writeASplineToGeoFile(logicalUnit,istart,iend,ifirstSpline)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, istart, iend
     INTEGER, INTENT(INOUT) :: ifirstSpline

     INTEGER :: i2, startNode

! Body
! ----

     WRITE(logicalUnit,830) ifirstSpline,istart
     DO i2 = istart + 1 , iend - 1
         WRITE(logicalUnit,840) i2
     ENDDO
     WRITE(logicalUnit,850) iend

     ifirstSpline = ifirstSpline + 1

830  FORMAT("Spline(",i20,") = {",i20,",")
840  FORMAT("                   ",i20,",")
850  FORMAT("                   ",i20,"};")

END SUBROUTINE

SUBROUTINE writeASplineLoopToGeoFile(logicalUnit,ifirstLineLoop,ifirstSpline)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, ifirstSpline
     INTEGER, INTENT(INOUT) :: ifirstLineLoop

     INTEGER :: i1

! Body
! ----
     WRITE(logicalUnit,830) ifirstLineLoop,ifirstSpline
     ifirstLineLoop = ifirstLineLoop + 1

830  FORMAT("Line Loop(",i20,") = {",i20,"};")

END SUBROUTINE

SUBROUTINE writeCommand(logicalUnit,nbOfLine)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit,nbOfLine
     INTEGER :: i1,ifield,i2

! Body
! ----
!     ifield = 2 * 1948
     WRITE(logicalUnit,810)
     WRITE(logicalUnit,830) 1
     DO i1 =2,nbOfLine-1
          WRITE(logicalUnit,840) i1
     ENDDO
     WRITE(logicalUnit,850) nbOfLine
     WRITE(logicalUnit,820)
     WRITE(logicalUnit,860)
!     WRITE(logicalUnit,910) ifield
!     WRITE(logicalUnit,920) ifield,2
!     DO i1=2,1947
!       i2 = 2 * i1
!       WRITE(logicalUnit,940) i2
!     ENDDO
!     i1 = 1948
!     i2 = 2 * i1
!     WRITE(logicalUnit,950) i2
!     WRITE(logicalUnit,960) ifield

810  FORMAT("Mesh.CharacteristicLengthExtendFromBoundary = 0;")
830  FORMAT("Field[1] = Attractor;",/,"Field[1].EdgesList = {",i20,",")
840  FORMAT("                      ",i20,",")
850  FORMAT("                      ",i20,"};")
820  FORMAT("Field[2] = Threshold;",/,"Field[2].DistMax = 0.1;",/,"Field[2].DistMin = 0.05;",/,"Field[2].IField = 1;")
860  FORMAT("Field[2].LcMax = 0.1;",/,"Field[2].LcMin = 0.05;",/,"Field[2].Sigmoid = 1;",/,"Background Field = 2;")

910  FORMAT("Field[",i20,"] = Min ;")
920  FORMAT("Field[",i20,"].FieldsList = {",i20,",")
940  FORMAT("                      ",i20,",")
950  FORMAT("                      ",i20,"};")
960  FORMAT("Background Field = ",i20,";")

END SUBROUTINE

SUBROUTINE writeInfoLine(logicalUnit,ifirstLine)

! Declaration
! -----------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: logicalUnit, ifirstLine
     INTEGER :: ifield,ifield2,iline

! Body
! ----
     CALL createDIVAContext()
     ifield = 2 * ifirstLine - 1
     ifield2 = ifield+1
     iline = ifirstLine - 1

     WRITE(logicalUnit,810) ifield
     WRITE(logicalUnit,830) ifield,iline
     WRITE(logicalUnit,820) ifield2,ifield2,ifield2
     WRITE(logicalUnit,840) ifield2,ifield
     WRITE(logicalUnit,860) ifield2,ifield2,ifield2

810  FORMAT("Field[",i20,"] = Attractor;")
830  FORMAT("Field[",i20,"].EdgesList = {",i20,"};")
820  FORMAT("Field[",i20,"] = Threshold;",/,"Field[",i20,"].DistMax = 0.05;",/,"Field[",i20,"].DistMin = 0.02;")
840  FORMAT("Field[",i20,"].IField = ",i20,";")
860  FORMAT("Field[",i20,"].LcMax = 0.1;",/,"Field[",i20,"].LcMin = 0.01;",/,"Field[",i20,"].Sigmoid = 1;")

END SUBROUTINE

SUBROUTINE exportToScilab(cell,node,nbOfNodeMax,nbOfCellMax,nbOfNode,nbOfCell)


! Declaration
! -----------
     IMPLICIT NONE
     INTEGER(KIND=4) :: nbOfCell, nbOfNode,nbOfNodeMax,nbOfCellMax
     REAL(KIND=8) :: node(nbOfNodeMax,2)
     INTEGER(KIND=4) :: cell(nbOfCellMax,6)

     INTEGER :: i1, i2

! Body
! ----
     OPEN(unit=51,file='diva.sce')
     WRITE(51,810)

!    3) write information
!    - - - - - - - - - - -

!       3.1) nodes
!       + + + + + +
     WRITE(51,820) nbOfNode
     DO i1 = 1, nbOfNode
        WRITE(51,830) i1, node(i1,1), node(i1,2)
     ENDDO

!       3.2) cell topology
!       + + + + + + + + + +
     WRITE(51,840) nbOfCell
     DO i1 = 1, nbOfCell
        WRITE(51,850) i1,cell(i1,1),cell(i1,2),cell(i1,3)
     ENDDO
     WRITE(51,860)
     WRITE(51,870)
     WRITE(51,880)
     CLOSE(51)

810  FORMAT("clear",/,"close")
820  FORMAT("node=zeros(",i10,",2);")
830  FORMAT("node(",i10,",:)=[",f20.10,",",f20.10,"];")
840  FORMAT("maille=zeros(",i10,",3);")
850  FORMAT("maille(",i10,",:)=[",i10,",",i10,",",i10,"];")
860  FORMAT("  plot([node(maille(:,1),1) node(maille(:,2),1)],[node(maille(:,1),2) node(maille(:,2),2)],'k')")
870  FORMAT("  plot([node(maille(:,2),1) node(maille(:,3),1)],[node(maille(:,2),2) node(maille(:,3),2)],'k')")
880  FORMAT("  plot([node(maille(:,3),1) node(maille(:,1),1)],[node(maille(:,3),2) node(maille(:,1),2)],'k')")

END SUBROUTINE

END PROGRAM divaGMSH
