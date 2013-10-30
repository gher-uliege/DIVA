      SUBROUTINE CALDIVANORM(im,jm,km,dx,dy,VALEXC,var
     &                      ,Nabla_var,Delta_var,Norm_var)
!
      integer                            :: i,j,k,ic,klev,top_lev
!
      REAL*4                             :: VALEXC
     &                                    , dx,dy
!
      REAL*4 , DIMENSION(im)             :: XLON
      REAL*4 , DIMENSION(jm)             :: YLAT
      REAL*4 , DIMENSION(km)             :: CORLEN,SN
!
      REAL*4 , DIMENSION(im,jm,km)       :: var
     &                                    , Nabla_var,Delta_var,Norm_var
     &                                    , NablaX_var,NablaY_var
     &                                    , Nabla2X_var,Nabla2Y_var
     &                                    , DeltaX_var,DeltaY_var

!
!----------------------------------------------------------------------
!
      Nabla_var = VALEXC
      Delta_var = VALEXC
      Norm_var = VALEXC
      NablaX_var = VALEXC
      NablaY_var = VALEXC
!
      DO k = 1,km
!
      DO j= 2,jm-1
      DO i= 2,im-1
      IF(var(i,j,k) /= VALEXC) THEN
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i-1,j,k))/(2.*dx)
      ELSE
      IF(var(i-1,j,k) == VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i,j,k))/dx
      ELSE
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  == VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i,j,k) - var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j-1,k))/(2.*dy)
      ELSE
      IF(var(i,j-1,k) == VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j,k))/dy
      ELSE
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  == VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j,k) - var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Nabla_var(i,j,k) = SQRT(NablaX_var(i,j,k)**2+NablaY_var(i,j,k)**2)
      ENDIF
      ENDIF
      ENDDO
      ENDDO
!
      j= 1
      DO i= 2,im-1
      IF(var(i,j,k) /= VALEXC) THEN
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i-1,j,k))/(2.*dx)
      ELSE
      IF(var(i-1,j,k) == VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i,j,k))/dx
      ELSE
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  == VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i,j,k) - var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j,k))/dy
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Nabla_var(i,j,k) = SQRT(NablaX_var(i,j,k)**2+NablaY_var(i,j,k)**2)
      ENDIF
      ENDIF
      ENDDO
!
      j= jm
      DO i= 2,im-1
      IF(var(i,j,k) /= VALEXC) THEN
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i-1,j,k))/(2.*dx)
      ELSE
      IF(var(i-1,j,k) == VALEXC .AND. var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i,j,k))/dx
      ELSE
      IF(var(i-1,j,k) /= VALEXC .AND. var(i+1,j,k)  == VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i,j,k) - var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(var(i,j-1,k) /= VALEXC ) THEN
      NablaY_var(i,j,k) = ( var(i,j,k) - var(i,j-1,k))/dy
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Nabla_var(i,j,k) = SQRT(NablaX_var(i,j,k)**2+NablaY_var(i,j,k)**2)
      ENDIF
      ENDIF
      ENDDO
!
      i= 1
      DO j= 2,jm-1
      IF(var(i,j,k) /= VALEXC) THEN
      IF(var(i+1,j,k)  /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i+1,j,k) - var(i,j,k))/dx
      ENDIF
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j-1,k))/(2.*dy)
      ELSE
      IF(var(i,j-1,k) == VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j,k))/dy
      ELSE
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  == VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j,k) - var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Nabla_var(i,j,k) = SQRT(NablaX_var(i,j,k)**2+NablaY_var(i,j,k)**2)
      ENDIF
      ENDIF
      ENDDO
!
      i= im
      DO j= 2,jm-1
      IF(var(i,j,k) /= VALEXC) THEN
      IF(var(i-1,j,k) /= VALEXC) THEN
      NablaX_var(i,j,k) = ( var(i,j,k) - var(i-1,j,k))/dx
      ENDIF
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j-1,k))/(2.*dy)
      ELSE
      IF(var(i,j-1,k) == VALEXC .AND. var(i,j+1,k)  /= VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j+1,k) - var(i,j,k))/dy
      ELSE
      IF(var(i,j-1,k) /= VALEXC .AND. var(i,j+1,k)  == VALEXC) THEN
      NablaY_var(i,j,k) = ( var(i,j,k) - var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Nabla_var(i,j,k) = SQRT(NablaX_var(i,j,k)**2+NablaY_var(i,j,k)**2)
      ENDIF
      ENDIF
      ENDDO
!

!     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!
      DO j= 2,jm-1
      DO i= 2,im-1
      IF(NablaX_var(i,j,k) /= VALEXC) THEN
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) -
     & NablaX_var(i-1,j,k))/(2.*dx)
      ELSE
      IF(NablaX_var(i-1,j,k) == VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) - NablaX_var(i,j,k))/dx
      ELSE
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  == VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i,j,k) - NablaX_var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) -
     & NablaY_var(i,j-1,k))/(2.*dy)
      ELSE
      IF(NablaY_var(i,j-1,k) == VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) - NablaY_var(i,j,k))/dy
      ELSE
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  == VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j,k) - NablaY_var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(Nabla2X_var(i,j,k)/=VALEXC .AND. Nabla2Y_var(i,j,k)/=VALEXC)
     &THEN
      Delta_var(i,j,k) = Nabla2X_var(i,j,k) + Nabla2Y_var(i,j,k)
      ENDIF
      ENDIF
      ENDDO
      ENDDO
!
      j= 1
      DO i= 2,im-1
      IF(NablaX_var(i,j,k) /= VALEXC) THEN
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) -
     & NablaX_var(i-1,j,k))/(2.*dx)
      ELSE
      IF(NablaX_var(i-1,j,k) == VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) - NablaX_var(i,j,k))/dx
      ELSE
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  == VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i,j,k) - NablaX_var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) - NablaY_var(i,j,k))/dy
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Delta_var(i,j,k) = Nabla2X_var(i,j,k) + Nabla2Y_var(i,j,k)
      ENDIF
      ENDIF
      ENDDO
!
      j= jm
      DO i= 2,im-1
      IF(NablaX_var(i,j,k) /= VALEXC) THEN
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) -
     & NablaX_var(i-1,j,k))/(2.*dx)
      ELSE
      IF(NablaX_var(i-1,j,k) == VALEXC .AND.
     & NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) - NablaX_var(i,j,k))/dx
      ELSE
      IF(NablaX_var(i-1,j,k) /= VALEXC .AND.
     & NablaX_var(i+1,j,k)  == VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i,j,k) - NablaX_var(i-1,j,k))/dx
      ENDIF
      ENDIF
      ENDIF
      IF(NablaY_var(i,j-1,k) /= VALEXC ) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j,k) - var(i,j-1,k))/dy
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Delta_var(i,j,k) = Nabla2X_var(i,j,k) + Nabla2Y_var(i,j,k)
      ENDIF
      ENDIF
      ENDDO
!
      i= 1
      DO j= 2,jm-1
      IF(NablaX_var(i,j,k) /= VALEXC) THEN
      IF(NablaX_var(i+1,j,k)  /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i+1,j,k) - NablaX_var(i,j,k))/dx
      ENDIF
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) -
     & NablaY_var(i,j-1,k))/(2.*dy)
      ELSE
      IF(NablaY_var(i,j-1,k) == VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) - NablaY_var(i,j,k))/dy
      ELSE
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  == VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j,k) - NablaY_var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Delta_var(i,j,k) = Nabla2X_var(i,j,k) + Nabla2Y_var(i,j,k)
      ENDIF
      ENDIF
      ENDDO
!
      i= im
      DO j= 2,jm-1
      IF(NablaX_var(i,j,k) /= VALEXC) THEN
      IF(NablaX_var(i-1,j,k) /= VALEXC) THEN
      Nabla2X_var(i,j,k) = ( NablaX_var(i,j,k) - NablaX_var(i-1,j,k))/dx
      ENDIF
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) -
     & NablaY_var(i,j-1,k))/(2.*dy)
      ELSE
      IF(NablaY_var(i,j-1,k) == VALEXC .AND.
     & NablaY_var(i,j+1,k)  /= VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j+1,k) - NablaY_var(i,j,k))/dy
      ELSE
      IF(NablaY_var(i,j-1,k) /= VALEXC .AND.
     & NablaY_var(i,j+1,k)  == VALEXC) THEN
      Nabla2Y_var(i,j,k) = ( NablaY_var(i,j,k) - NablaY_var(i,j-1,k))/dy
      ENDIF
      ENDIF
      ENDIF
      IF(NablaX_var(i,j,k) /= VALEXC .AND. NablaY_var(i,j,k)  /= VALEXC)
     &THEN
      Delta_var(i,j,k) = Nabla2X_var(i,j,k) + Nabla2Y_var(i,j,k)
      ENDIF
      ENDIF
      ENDDO
!
!     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      DO j= 1,jm
      DO i= 1,im
      IF(var(i,j,k) /= VALEXC) THEN
      IF(Delta_var(i,j,k) /= VELEXC .AND. NablaX_var(i,j,k) /= VALEXC
     & .AND. NablaY_var(i,j,k) /= VALEXC) THEN
      Norm_var(i,j,k) = Nabla2X_var(i,j,k)**2 + Nabla2Y_var(i,j,k) **2
     &                + Nabla_var(i,j,k)**2   + var(i,j,k)**2
      ENDIF
      ENDIF
      ENDDO
      ENDDO
!
      ENDDO
!
      RETURN
      END
