

      SUBROUTINE SOL(VKGS,VKGD,VKGI,VFG,KLD,NEQ,MP,IFAC,ISOL,NSYM,ENERG)
C=======================================================================
C     RESOLUTION D'UN SYSTEME LINEAIRE SYMETRIQUE OU NON. LA MATRICE EST
C     STOCKEE PAR LIGNE DE CIEL, EN MEMOIRE DANS LES TABLES
C     VKGS,VKGD,VKGI
C                    AUTEURS: Dhatt et Touzot, 1985
C       ENTREES
C          VKGS,VKGD,VKGI    MATRICE DU SYSTEME : PARTIES SUPERIEURE,
C                            DIAGONALE, INFERIEURE
C          VFG               SECOND MEMBRE
C          KLD               POINTEURS VERS LES HAUTS DE COLONNE
C          NEQ               NOMBRE D'EQUATIONS
C          MP                UNITE LOGIQUE D'IMPRESSION
C          IFAC              SI IFAC.EQ.1 TRIANGULARISATION DE
C                            LA MATRICE
C          ISOL              SI ISOL.EQ.1 CALCUL DE LA SOLUTION A
C                            PARTIR DE LA MATRICE TRIANGULARISEE
C          NSYM              INDICE DE PROBLEME NON SYMETRIQUE
C       SORTIES
C          VKGS,VKGD,VKGI    MATRICE TRIANGULARISEE
C          VFG               SOLUTION (SI ISOL.EQ.1)
C          ENERG             ENERGIE DU SYSTEME (SI NSYM.EQ.0)
C=======================================================================
      include'divapre.h'
      DIMENSION VKGS(*),VKGI(*),VKGD(*),VFG(*),KLD(*)
      DATA ZERO/0.0D0/
C.......................................................................

#ifdef DIVAPARALLEL
      if(NSYM.NE.1) then
C New code for symmetric case here
      call newsol(VKGS,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
      return
      endif
C OK for non symmetric case use old code
#endif
#ifdef DIVAITERATIVE
      if(NSYM.NE.1) then
C New code for symmetric case here
      call itersol(VKGS,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
      return
      endif
C OK for non symmetric case use old code
#endif



c      write(6,*) 'into sol',neq,isol,nsym,ifac
      IK=1
      IF(VKGD(1).NE.ZERO) GO TO 10
      WRITE(MP,2000) IK
      STOP
 10   ENERG=ZERO
C
C.....  POUR CHAQUE COLONNE IK A MODIFIER
C
      JHK=1
C      write(6,*) 'into sol',JHK1
C?????? JHK1 not defined????
C      JHK1=0
C??????

      DO 100 IK=2,NEQ
C.....  POINTEUR DU HAUT DE LA COLONNE SUIVANTE IK+1
      JHK1=KLD(IK+1)
C.....  HAUTEUR DE LA COLONNE IK (HORS TERMES SUPERIEUR ET DIAGONAL)
      LHK=JHK1-JHK
      LHK1=LHK-1
C.....  LIGNE DU PREMIER TERME A MODIFIER DANS LA COLONNE IK
      IMIN=IK-LHK1
      IMIN1=IMIN-1
C.......  LIGNE DU DERNIER TERME A MODIFIER DANS LA COLONNE IK
      IMAX=IK-1
      IF(LHK1.LT.0) GO TO 100
      IF(IFAC.NE.1) GO TO 90
      IF(NSYM.EQ.1) VKGI(JHK)=VKGI(JHK)/VKGD(IMIN1)
      IF(LHK1.EQ.0) GO TO 40
C
C.....  MODIFIER LES TERMES NON DIAGONAUX DE LA COLONNE IK
C
      JCK=JHK+1
      JHJ=KLD(IMIN)
C.....  POUR CHAQUE TERME PLACE EN JCK, CORRESPONDANT A LA COLONNE IJ
      DO 30 IJ=IMIN,IMAX
      JHJ1=KLD(IJ+1)
C.....  NOMBRE DE TERMES MODIFICATIFS DU TERME PLACE EN JCK
      IC=MIN0(JCK-JHK,JHJ1-JHJ)
      IF(IC.LE.0.AND.NSYM.EQ.0) GO TO 20
      C1=ZERO
      IF(IC.LE.0) GO TO 17
      J1=JHJ1-IC
      J2=JCK-IC
      IF(NSYM.EQ.1) GO TO 15
      VKGS(JCK)=VKGS(JCK)-SCAL(VKGS(J1),VKGS(J2),IC)
      GO TO 20
 15   VKGS(JCK)=VKGS(JCK)-SCAL(VKGI(J1),VKGS(J2),IC)
      C1=SCAL(VKGS(J1),VKGI(J2),IC)
 17   VKGI(JCK)=(VKGI(JCK)-C1)/VKGD(IJ)
 20   JCK=JCK+1
 30   JHJ=JHJ1
C
C.....  MODIFIER LE TERME DIAGONAL
C
 40   JCK=JHK
      CDIAG=ZERO
      DO 70 IJ=IMIN1,IMAX
      C1=VKGS(JCK)
      IF(NSYM.EQ.1) GO TO 50
      C2=C1/VKGD(IJ)
      VKGS(JCK)=C2
      GO TO 60
 50   C2=VKGI(JCK)
 60   CDIAG=CDIAG+C1*C2
 70   JCK=JCK+1
      VKGD(IK)=VKGD(IK)-CDIAG
C      IF(VKGD(IK)) 90,80,90
      IF(VKGD(IK).LT.0) goto 90
      IF(VKGD(IK).GT.0) goto 90
 80   WRITE(MP,2000) IK
 2000 FORMAT(' *** ERREUR, PIVOT NUL EQUATION ',I5)
      STOP
C
C..... RESOLUTION DU SYSTEME TRIANGULAIRE INFERIEUR
C
 90   IF(ISOL.NE.1) GO TO 100
c      write(6,*) 'for sol',ik,jhk,imin1,lhk
      IF(NSYM.NE.1) VFG(IK)=VFG(IK)-SCAL(VKGS(JHK),VFG(IMIN1),LHK)
      IF(NSYM.EQ.1) VFG(IK)=VFG(IK)-SCAL(VKGI(JHK),VFG(IMIN1),LHK)
c      write(6,*) 'endsol'
 100  JHK=JHK1
C      write(6,*) 'JHK1',JHK1
      IF(ISOL.NE.1) RETURN
C
C.....  RESOLUTION DU SYSTEME DIAGONAL
C
      IF(NSYM.EQ.1) GO TO 120
      DO 110 IK=1,NEQ
      C1=VKGD(IK)
      C2=VFG(IK)/C1
      VFG(IK)=C2
 110  ENERG=ENERG+C1*C2*C2
C
C.....  RESOLUTION DU SYSTEME TRIANGULAIRE SUPERIEUR
C
 120  IK=NEQ+1
      JHK1=KLD(IK)
 130  IK=IK-1
      IF(NSYM.EQ.1) VFG(IK)=VFG(IK)/VKGD(IK)
      IF(IK.EQ.1) RETURN
      C1=VFG(IK)
      JHK=KLD(IK)
      JBK=JHK1-1
      IF(JHK.GT.JBK) GO TO 150
      IJ=IK-JBK+JHK-1
      DO 140 JCK=JHK,JBK
      VFG(IJ)=VFG(IJ)-VKGS(JCK)*C1
 140  IJ=IJ+1
 150  JHK1=JHK
      GO TO 130
      END



      FUNCTION SCAL(X,Y,N)
C=======================================================================
C     PRODUIT SCALAIRE DES VECTEURS X ET Y DE LONGUEUR N
C=======================================================================

      include'divapre.h'
      REAL*8 SUM
      DIMENSION X(*),Y(*)
      DATA ZERO/0.0D0/
C.......................................................................
      SCAL=ZERO
      SUM=0
c      write(6,*) 'Prod SCAL',N
C      if (N.GT.2000) then
C      write(6,*) 'worth trying omp',N
CC$OMP  PARALLEL DO
CC$OMP& REDUCTION(+:SUM)
      DO  I=1,N
       SUM=SUM+(X(I)*Y(I))
      ENDDO
CC$OMP  END PARALLEL DO
C                      else
CC Unrolled version
C         M = MOD(N,5)
C         IF (M.NE.0) THEN
C            DO I = 1,M
C               SUM = SUM + X(I)*Y(I)
C            END DO
C            IF (N.LT.5) THEN
C               SCAL=SUM
C            RETURN
C            END IF
C         END IF
C         MP1 = M + 1
C         DO I = MP1,N,5
C          SUM = SUM + X(I)*Y(I) + X(I+1)*Y(I+1) +
C     $            X(I+2)*Y(I+2) + X(I+3)*Y(I+3) + X(I+4)*Y(I+4)
C         END DO
C      endif
      SCAL=SUM
      RETURN
      END
