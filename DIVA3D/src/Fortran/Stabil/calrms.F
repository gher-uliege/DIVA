!
      SUBROUTINE CALRMS(imax,jmax,nk,valexc,var,var1,var2,verr,reler
     &                 ,var_rms,ver_rms,var1_rms,var2_rms,verel_rms)
!
      implicit none
!
!---------------------------------------------------------------------!
!
      INTEGER                                :: i,j,k,ic
      INTEGER                                :: imax,jmax,nk
!
      REAL*4 , DIMENSION(imax,jmax,nk)       :: var, var1, var2
     &                                        , verr,reler
!
      REAL*4 , DIMENSION(nk)                 :: var_rms,ver_rms,var1_rms
     &                                        , var2_rms,verel_rms
!
      REAL*4                                 :: valexc
!
!--------------------------------------------------------------
!

      DO k = 1,NK
       ic = 0
       var_rms(k) = 0.
       do j = 1,jmax
        do i = 1,imax
           if (var(i,j,k) /= valexc ) then
              var_rms(k) = var_rms(k) + (var(i,j,k)*var(i,j,k))
              ic=ic+1
            end if
        end do
       end do
      if (ic.gt.0) then
         var_rms(k) = sqrt(var_rms(k)/ic)
      else
         var_rms(k) = valexc
      endif
      ENDDO
!
      DO k = 1,NK
       ic = 0
       ver_rms(k) = 0.
       do j = 1,jmax
        do i = 1,imax
           if (verr(i,j,k) /= valexc ) then
              ver_rms(k) = ver_rms(k) + (verr(i,j,k)*verr(i,j,k))
              ic=ic+1
            end if
        end do
       end do
       if (ic.gt.0) then
         ver_rms(k) = sqrt(ver_rms(k)/ic)
      else
         ver_rms(k) = valexc
      endif
      ENDDO
!
      DO k = 1,NK
       ic = 0
       var1_rms(k) = 0.
       do j = 1,jmax
        do i = 1,imax
           if (var1(i,j,k) /= valexc ) then
              var1_rms(k) = var1_rms(k) + (var1(i,j,k)*var1(i,j,k))
              ic=ic+1
            end if
        end do
       end do
       if (ic.gt.0) then
         var1_rms(k) = sqrt(var1_rms(k)/ic)
      else
         var1_rms(k) = valexc
      endif
      ENDDO
!
      DO k = 1,NK
       ic = 0
       var2_rms(k) = 0.
       do j = 1,jmax
        do i = 1,imax
           if (var2(i,j,k) /= valexc ) then
              var2_rms(k) = var2_rms(k) + (var2(i,j,k)*var2(i,j,k))
              ic=ic+1
            end if
        end do
       end do
       if (ic.gt.0) then
         var2_rms(k) = sqrt(var2_rms(k)/ic)
      else
         var2_rms(k) = valexc
      endif
      ENDDO
!
      DO k = 1,NK
       ic = 0
       verel_rms(k) = 0.
       do j = 1,jmax
        do i = 1,imax
           if (reler(i,j,k) /= valexc ) then
              verel_rms(k) = verel_rms(k) + (reler(i,j,k)*reler(i,j,k))
              ic=ic+1
            end if
        end do
       end do
       if (ic.gt.0) then
         verel_rms(k) = sqrt(verel_rms(k)/ic)
      else
         verel_rms(k) = valexc
      endif
      ENDDO
!
      RETURN
      END
