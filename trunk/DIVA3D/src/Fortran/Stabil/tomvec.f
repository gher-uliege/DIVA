C2345678901234567890123456789012345678901234567890123456789012345678901
      SUBROUTINE TOMVEC(A,B,C,F,X,IMAXD,IMIN,IMAX,KMAX)
C                ======
C>>>>>RESOUD DES SYSTEMES TRIDIAGONAUX D"UNE MANIERE VECTORISABLE<<<<<
C ATTENTION, LE SYTEME EST DE LA FORME  B Y + (1+A) Y + C Y
C
      IMPLICIT NONE
      INTEGER*4 i,k,km1,kp1,kmxm1,imin,imax,imaxd,kmax
      REAL*8 A(IMAXD,*),B(IMAXD,*),C(IMAXD,*),F(IMAXD,*),X(IMAXD,*)
C
C
      DO  20 K=2,KMAX
       KM1=K-1
       DO 10 I=IMIN,IMAX
          B(I,K)=B(I,K)/( A(I,KM1)+1.D0)
 10    CONTINUE
       DO 20 I=IMIN,IMAX
          A(I,K)=A(I,K)-B(I,K)*C(I,KM1)
 20   CONTINUE
C
C
      DO 30 K=2,KMAX
       KM1=K-1
       DO 30 I=IMIN,IMAX
          F(I,K)=F(I,K)-B(I,K)*F(I,KM1)
 30   CONTINUE
C
      DO 40 I=IMIN,IMAX
       X(I,KMAX)=F(I,KMAX)/( A(I,KMAX)+1.D0)
 40   CONTINUE
C
      KMXM1=KMAX-1
      DO 50 K=KMXM1,1,-1
       KP1=K+1
       DO 50 I=IMIN,IMAX
          X(I,K)=(F(I,K)-C(I,K)*X(I,KP1))/( A(I,K)+1.D0)
 50   CONTINUE
C
      RETURN
      END
