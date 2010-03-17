C ----------------------------------------------------
C     Change the order of contour points
C     generated by divacont
C
C     Based on generopt.f
C
C     INPUT:
C     coast.cont
C     OUTPUT
C     coastinv.cont
C
C ----------------------------------------------------

      IMPLICIT NONE

      INTEGER*4 NMAX,MMAX,NCMAX

      PARAMETER(NMAX=400000,MMAX=400000,NCMAX=1000)

C     NMAX: MAXIMAL NUMBER OF NODES
C     MMAX: MAXIMAL NUMBER OF MESHS
C     NCMAX: MAXIMAL NUMBER OF CONTOURS

      REAL*4 X(NMAX),Y(NMAX)

      INTEGER*4 I,J,K,N,NC
      INTEGER*4 MP,PPTC(NCMAX)
      INTEGER*4 NR,P,jj,nskip
      character*13 invcoastname
      
      invcoastname='coastinv.cont'

      J=0
      
C     READING COORDINATES IN fort.10 ( = input coast.cont)
      OPEN(UNIT=10,FILE='fort.10')
      OPEN(unit=20,file=invcoastname)
C     READING NUMBER OF CONTOURS
      READ(10,*) NC
      WRITE(20,*) NC
      
C     WRITE(*,*) 'total number of contours'
C     WRITE(*,*) NC
      
C     LOOP ON THE CONTOURS
C-------------------------
      nskip = 0
      DO 30 I=1,NC
C     READ THE NUMBER OF POINTS IN THE FIRST CONTOUR
       READ(10,*) N
       PPTC(I)=N
C       print *,'number of points in the contour n� ', I
C       print *,'=',N
       
       WRITE(20,*) N
C     LOOP ON N� CONTOUR POINTS
       DO 40 K=1,N
          J=J+1
C         READ COORDINATES POINTS
          READ(10,*) X(J),Y(J)
C          print *, J
C          print *, 'X =',X(J)  ,' Y = ',Y(J)
C
40     CONTINUE

       DO 50 K=1,N
C          WRITE(20,*) X(N-K+1+nskip),Y(N-K+1+nskip)
C          READ(10,*) X(J),Y(J)
       WRITE(20,*) X(N-K+1+nskip),Y(N-K+1+nskip)
50     CONTINUE

       nskip = N+nskip
C      print *,'nskip = ', nskip
30    CONTINUE
      CLOSE(10)
      CLOSE(20)

C      print *, 'contour points have been inverted'


      END

