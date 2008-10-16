      real*4 AA(1500000),A(50000)
      
      integer imax,jmax,kmax
      read(5,*)imax
      read(5,*)jmax
      read(5,*)kmax
      call concat2(A,AA,imax,jmax,kmax)
      end

      subroutine concat2(A,AA,imax,jmax,kmax)
      character*100 outname,name
      real*8 c8
      integer imax,jmax,kmax
      real*4 A(imax,jmax),AA(imax,jmax,kmax)

      read(5,'(A)')outname

      do k=kmax,1,-1
        read(5,'(A)')name
        OPEN (UNIT=10,FILE=name,FORM='UNFORMATTED')
        CALL UREADC (10,C8,A,VALEX,IPR,IM,JM,KM,NB)
        close(10)
        do i=1,imax
        do j=1,jmax
          AA(i,j,k)=A(i,j)
        enddo
        enddo
      enddo
   
      OPEN (UNIT=10,FILE=outname,FORM='UNFORMATTED')
      call uwritc (10,c8,AA,VALEX,IPR,imax,jmax,kmax,imax*jmax)
      close (10)
      end  
      include "ureadc.f"
      include "uwritc.f"
