      integer, parameter :: XM=10000000
      REAL(KIND=4) ::  A(XM),B(XM),C(XM)
      
      read(5,*,end=200) valex,M,N
      call RDMAT(A,M,N,M,20)
      call RDMAT(B,M,N,M,21)
      call msum(A,B,C,N,M,valex)
      call IMPMAT(C,M,N,M,22)
 200  continue
      stop
      end
      
      subroutine msum(A,B,C,N,M,valex)
!c      REAL(KIND=4) ::  A(N,M),B(N,M),C(N,M)
      REAL(KIND=4) ::  A(M,N),B(M,N),C(M,N)
      do i=1,M
      do j=1,N
      val=A(i,j)+B(i,j)
      if(abs(A(i,j)-valex).lt.0.00001*abs(valex)) val=valex
      if(abs(B(i,j)-valex).lt.0.00001*abs(valex)) val=valex
      C(i,j)=val
      enddo
      enddo
      return
      end
      

      SUBROUTINE IMPMAT(A,L,M,NDIM,IUCT)
!C     ==================================
      
      DIMENSION A(NDIM,M)
      K=1
 1    KK=K+7
      IF (KK.LE.M) goto 3
 2    KK=M
 3    WRITE(IUCT,200) (J,J=K,KK)
      DO 4 I=1,L
 4    WRITE(IUCT,201) I,(A(I,J),J=K,KK)
      K=K+8
      IF (K.LE.M) goto 1
 5    RETURN
 200  FORMAT('0',8(9X,I3,3X))
 201  FORMAT(1X,I3,8(E15.5))
      END
      

      SUBROUTINE RDMAT(A,L,M,NDIM,IUCT)
!C     ==================================
      
      DIMENSION A(NDIM,M)
      K=1
 1    KK=K+7
      IF (KK.LE.M) goto 3 
 2    KK=M
 3    READ(IUCT,200) 
      DO 4 I=1,L
 4    READ(IUCT,201) II,(A(I,J),J=K,KK)
      K=K+8
      IF (K.LE.M) goto 1
 5    RETURN
 200  FORMAT('0',8(9X,I3,3X))
 201  FORMAT(1X,I3,8(E15.5))
      END
