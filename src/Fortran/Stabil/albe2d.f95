!
      SUBROUTINE albesi(valexu,nx,ny,nk,mask,dep,psal,temp,alpha,beta,RHO)
      IMPLICIT NONE
!
      INTEGER                        :: nx,ny,nk,i,j,k,ISIG,wpnb
      REAL*4  , DIMENSION(nx,ny,nk)  :: psal,temp,SIG
      REAL*4                         :: SA,TP,SIG0,SSA,CR,TPP,SAP
      REAL*4  , DIMENSION(nx,ny,nk)  :: RHO
      REAL*4  , DIMENSION(nx,ny)     :: DRHOS,DRHOT
      INTEGER , DIMENSION(nx,ny,nk)  :: mask
      REAL*4  , DIMENSION(nx,ny)     :: alpha,beta
      REAL*4  , DIMENSION(nk)        :: DEP
      REAL                           :: valexu,PR,RSS
      REAL                           :: RHOG,VW,VW0,VSTP
      REAL                           :: T0,S0,ST2,ST1,SS1,SS2,RHOZERO,sig1,sig2,prsdb
!
      REAL, DIMENSION (15)           :: C
      REAL, DIMENSION (5)            :: C1
      REAL, DIMENSION (7)            :: C2
      REAL, DIMENSION (14)           :: C3
!
      DATA C/999.842594d0, .6793952e-1,-.9095290e-2, .1001685e-3, -.1120083e-5,.6536332e-8, .8244930e+0,-.4089900e-2, &
       .7643800e-4,-.8246700e-6, .5387500e-8,-.5724660e-2,.1022700e-3,-.1654600e-5, .4831400e-3/
      DATA C1/19652.21e0,148.4206e0,-2.327105e0,1.360477e-02,-5.155288e-05/
      DATA C2/54.6746e0,-0.603459e0,1.09987e-02,-6.1670e-05,7.944e-02,1.6483e-02,5.3009e-04/
      DATA C3/3.239908e0,1.43713e-03,1.16092e-04,-5.77905e-07,2.2838e-03,-1.0981e-05,-1.6078e-06,1.91075e-04,8.50935e-05,&
      -6.12293e-06,5.2787e-08,-9.9348e-07,2.0816e-08,9.1697e-10/
      DATA RHOG /0.09926209279/
!
       prsdb=0.
!!       PR=RHOG*DEP(1)
       call  pzcon(1,9.81,0.,pr,dep(1))
!
       RHO(:,:,2) =  valexu
       DO j = 1,ny 
       DO i = 1,nx 
         if (mask(i,j,1) == 1) then
!
            SA = psal(i,j,1)
            RSS=SQRT(SA)
            CALL POTMP(PR,temp(i,j,1),SA,prsdb,TP)
!
                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)        &
                      +TP*(C(5)+TP*C(6)))))                   &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)    &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)       &
                      +TP*(C(13)+TP*C(14))))
!
                  VW=C1(1)+                                    &
                      TP*C1(2)+                                &
                      TP*TP*C1(3)+                             &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)
!
                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))
!
                  VSTP=VW0+PR*(                                         &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)         &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))      &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                  &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))              &
                      )
!
                  SIG0 = SIG0/(1.-PR/VSTP)
!
                  RHO(i,j,1) =  SIG0
           else
                  RHO(i,j,1) =  valexu
          end if
       ENDDO
       ENDDO
!
       DO j = 1,ny 
       DO i = 1,nx 
         if (mask(i,j,1) == 1) then
!
                 SA = psal(i,j,1)
                 TPP = temp(i,j,1) +5.E-2
                 RSS=SQRT(SA)
                 CALL POTMP(PR,TPP,SA,prsdb,TP)
!
                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)        &
                      +TP*(C(5)+TP*C(6)))))                   &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)    &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)       &
                      +TP*(C(13)+TP*C(14))))
!
!
                  VW=C1(1)+                                    &
                      TP*C1(2)+                                &
                      TP*TP*C1(3)+                             &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)
!
                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))
!
                  VSTP=VW0+PR*(                                         &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)         &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))      &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                  &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))              &
                      )
!
                  SIG0 = SIG0/(1.-PR/VSTP)
!
                  DRHOT(i,j) =  SIG0
!
                 SA = psal(i,j,1)+1.E-2
                 RSS=SQRT(SA)
                 CALL POTMP(PR,temp(i,j,1),SA,prsdb,TP)
!
                  SIG0=C(1)+TP*(C(2)+TP*(C(3)+TP*(C(4)       &
                      +TP*(C(5)+TP*C(6)))))                  &
                      +SA*(SA*C(15)+C(7)+TP*(C(8)+TP*(C(9)   &
                      +TP*(C(10)+TP*C(11))))+RSS*(C(12)      &
                      +TP*(C(13)+TP*C(14))))
!
!
                  VW=C1(1)+                                   &
                      TP*C1(2)+                               &
                      TP*TP*C1(3)+                            &
                      TP*TP*TP*C1(4)+TP*TP*TP*TP*C1(5)
!
                  VW0=VW+SA*(C2(1)+TP*C2(2)+TP*TP*C2(3)+TP*TP*TP*C2(4)) &
                     +SA*RSS*(C2(5)+TP*C2(6)+TP*TP*C2(7))
!
                  VSTP=VW0+PR*(                                       &
                      C3(1)+TP*C3(2)+TP*TP*C3(3)+TP*TP*TP*C3(4)       &
                     +SA*(C3(5)+TP*C3(6)+TP*TP*C3(7)+TP*RSS*C3(8))    &
                     +PR*(C3(9)+TP*(C3(10)+TP*C3(11)))                &
                     +PR*SA*(C3(12)+TP*(C3(13)+TP*C3(14)))            &
                      )
!
                  SIG0 = SIG0/(1.-PR/VSTP)
!
                  DRHOS(i,j) =  SIG0
!
                  alpha(i,j) = (-1./RHO(i,j,1))*(DRHOT(i,j)-RHO(i,j,1))  &
                               /5.E-2
!
                   beta(i,j) = ( 1./RHO(i,j,1))*(DRHOS(i,j)-RHO(i,j,1))  &
                               /1.E-2
!
         else
!
                  DRHOT(i,j) =  valexu
                  DRHOS(i,j) =  valexu
                  alpha(i,j) = 0.0
                  beta(i,j)  = 0.0
!
         end if
       ENDDO
       ENDDO
!
      RETURN
      END 
!
