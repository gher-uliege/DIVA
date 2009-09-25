C
        REAL*4 U(5000000)
        REAL*4 V(5000000)
        REAL*4 W(5000000)
        REAL*8 W8


        character*100 in1,in2,out
        read(5,'(A)')in1
        read(5,'(A)')in2
        read(5,'(A)')out
C READING U,V FIELDS ---------------------------------------

        open (unit=10,file=in1,form='unformatted')
        open (unit=11,file=in2,form='unformatted')
        write(6,*) 'Going to read',in1
        CALL UREADC(10,W8,U,VALEXU,IPR,IMAX,JMAX,KMAX,NW)
        write(6,*) 'Going to read',in2
        CALL UREADC(11,W8,V,VALEXW,IPR,IMAX,JMAX,KMAX,NW)
        
        ipr=4
        nw=imax*jmax
C        valexw = valexu 
        WRITE(6,*) ' VALEUR D EXCLUSION POUR B: ',VALEXU
        WRITE(6,*) ' VALEUR D EXCLUSION POUR X: ',VALEXW

        call usum(u,v,w,valexu,imax,jmax,kmax)
        open (unit=12,file=out,form='unformatted')
        CALL UWRITC(12,W8,W,VALEXU,IPR,IMAX,JMAX,KMAX,NW)

        STOP
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        subroutine usum(u,v,w,valexu,imax,jmax,kmax)
        real*4 u(imax,jmax,kmax),v(imax,jmax,kmax)
        real*4 w(imax,jmax,kmax)
        do k=1,kmax
         do j=1,jmax
          do i=1,imax
          w(i,j,k)=u(i,j,k)
          if(u(i,j,k).ne.valexu) w(i,j,k)=u(i,j,k)+v(i,j,k)
          enddo
         enddo
        enddo
        return
        end
        INCLUDE 'ureadc.f'
        INCLUDE 'uwritc.f'
