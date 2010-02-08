!C
PROGRAM lincom
USE ioInterface
        REAL(KIND=4) ::  U(5000000)
        REAL(KIND=4) ::  V(5000000)
        REAL(KIND=8) ::  W8(1)


        CALL UREADC(10,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        CALL UREADC(11,W8,V,VALEXW,IPR,IMAX,JMAX,KMAX,NW)
        read(20,*) a1,b1
        read(20,*) a2,b2
        read(20,*) a3,b3
        read(20,*) a4,b4
        ipr=4
        nw=imax*jmax
!C        valexw = valexu
        WRITE(6,*) ' VALEUR D EXCLUSION POUR B: ',VALEXU
        WRITE(6,*) ' VALEUR D EXCLUSION POUR X: ',VALEXW

        call usum(u,v,valexu,imax,jmax,kmax,a1,b1,a2,b2,a3,b3,a4,b4)
        CALL UWRITC(12,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        CALL UWRITC(13,W8,V,VALEXU,IPR,IMAX,JMAX,KMAX,NW)

 CONTAINS

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        subroutine usum(u,v,valexu,imax,jmax,kmax,a1,b1,a2,b2,a3,b3,a4,b4)
        REAL(KIND=4) ::  u(imax,jmax,kmax),v(imax,jmax,kmax)
        do k=1,kmax
         do j=1,jmax
          do i=1,imax
           if(u(i,j,k).ne.valexu) then
           rr=u(i,j,k)+a1*v(i,j,k)+b1
           tt=v(i,j,k)+a2*u(i,j,k)+b2
           u(i,j,k)=rr*b3+a3
           v(i,j,k)=tt*b4+a4
           endif
          enddo
         enddo
        enddo
        return
        end subroutine
END PROGRAM
