      SUBROUTINE POTMP(PRESS,TEMP,S,RP,POTEMP)
C
C     TITLE:
C     *****
C
C       POTMP  -- CALCULATE POTENTIAL TEMPERATURE FOR AN ARBITRARY
C                 REFERENCE PRESSURE
C
C     PURPOSE:
C     *******
C
C       TO CALCULATE POTENTIAL TEMPERATURE
C
C       REF: N.P. FOFONOFF
C            DEEP SEA RESEARCH
C            IN PRESS NOV 1976
C
C     PARAMETERS:
C     **********
C
C       PRESS  -> PRESSURE IN DECIBARS
C       TEMP   -> TEMPERATURE IN CELSIUS DEGREES
C       S      -> SALINITY PSS 78
C       RP     -> REFERENCE PRESSURE IN DECIBARS
C                 (0.0 FOR THE QUANTITY THETA)
C       POTEMP <- POTENTIAL TEMPERATURE (DEG C)
C
        REAL PRESS,TEMP,S,RP,POTEMP
C
C     VARIABLES:
C     *********
C
         INTEGER I,J,N
         REAL*4 DP,P,Q,R1,R2,R3,R4,R5,S1,T,X
C
C     CODE:
C     ****
C
      S1 = S-35.0
      P  = PRESS
      T  = TEMP
C
      DP = RP - P
      N  = IFIX(ABS(DP)/1000.) + 1
      DP = DP/FLOAT(N)
C
      DO 10 I=1,N
         DO 20 J=1,4
C
            R1 = ((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P
            R2 = (2.7759E-12*T-1.1351E-10)*S1
            R3 = ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T
            R4 = (R1+(R2+R3+1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*S1
            R5 = R4+((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5
C
            X  = DP*R5
C
            GO TO (100,200,300,400),J
C
  100       CONTINUE
            T = T+.5*X
            Q = X
            P = P + .5*DP
            GO TO 20
C
  200       CONTINUE
            T = T + .29298322*(X-Q)
            Q = .58578644*X + .121320344*Q
            GO TO 20
C
  300       CONTINUE
            T = T + 1.707106781*(X-Q)
            Q = 3.414213562*X - 4.121320344*Q
            P = P + .5*DP
            GO TO 20
C
  400       CONTINUE
            T = T + (X-2.0*Q)/6.0
  20      CONTINUE
  10    CONTINUE
C
        POTEMP = T
        RETURN
C
C       END POTMP
C
        END
