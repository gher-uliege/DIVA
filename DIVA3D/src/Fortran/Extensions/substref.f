      program substref

      implicit none

      real*4 x,y,phi,x0,y0,phi0
      character*100 in1,in2,out
        read(5,'(A)')in1
        read(5,'(A)')in2
        read(5,'(A)')out

       open (unit=10,file=in1,form='formatted')
       open (unit=11,file=in2,form='formatted')
       open (unit=12,file=out,form='formatted')

 10   continue
        read (10,*,end=20) x ,y ,phi
        read (11,*)        x0,y0,phi0
        if (phi.eq.-9999.0.or.phi0.eq.-9999.0) then
         write(12,*)        x ,y, -9999.0 
C         write(6,*)        x ,y, -9999.0 
        else 
         phi = phi - phi0
         write(12,*)        x ,y ,phi
         write(6,*)        x ,y ,phi
        endif
        goto 10
 20   continue

      end

