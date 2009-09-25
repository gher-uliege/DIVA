      include'../Calc/divapre.h'
      parameter(npmax=1000000)
      real*8 tbess(40000)
      common/tabbess/tbess
      real*8 x(npmax),y(npmax),h(npmax)
      
      call tabess
        
      xscale=1
      read(5,*) rcoordchange,RL,icol
      write(6,*) 'parameters',rcoordchange,RL
      if (rcoordchange.lt.0) xscale=-rcoordchange
      np=0
 1    continue
      
      read(10,*,end=100,err=100) xxx,yyy,val,www
      np=np+1
      x(np)=xxx*xscale
      y(np)=yyy
      h(np)=www
      goto 1
      
 100  continue        
      write(6,*) 'working on grid points',np
!C First sum of Pf elements
      rsum=0
      do i=1,np
       do j=1,np
       dist=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2)
       dist=dist/RL
       ii=dist/0.0005
       if(ii.eq.0) then
       ccc=1
       goto 2
       endif
       if(ii.gt.40000) then
       ccc=0
       goto 2
       endif
       ccc=tbess(ii)
 2     continue
!c      write(6,*) 'Correlation',ccc
      rsum=rsum+ccc*h(i)*h(j)
      enddo
      enddo
!C finished

!C now on data points
 444  continue
      if(icol.ge.4) then
      read(11,*,end=111,err=111) xx,yy,val,ww
      else
      read(11,*,end=111,err=111) xx,yy,val
      ww=1
      endif
      
      rr=0
      do j=1,np
       dist=sqrt((xx*xscale-x(j))**2+(yy-y(j))**2)
       dist=dist/RL
       ii=dist/0.0005
       if(ii.eq.0) then
       ccc=1
       goto 22
       endif
       if(ii.gt.40000) then
       ccc=0
       goto 22
       endif
       ccc=tbess(ii)
       
 22     continue
      rr=rr+ccc*h(j)
      enddo
      write(12,*) xx,yy,rr,ww   
      
      
      
      goto 444
 111  continue
      write(14,*) rsum
      stop
      end
      subroutine tabess
!C=============================================================================
      include'../Calc/divapre.h'
      real*8 tbess(40000),eps,bessk1
      common/tabbess/tbess
      external bessk1
      eps=0
      do 10 i=1,40000
      eps=eps+0.0005
         tbess(i)=eps*bessk1(eps)
 10   continue
!C=============================================================================
      return                                                            
      end                                                               
!C=======================================================================
      function bessk1(X)
      
      include'../Calc/divapre.h'
      EXTERNAL BESSI1

      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,-0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/



      IF(X.LE.0.) STOP 'ERROR X <= 0' 

      IF(X.LE.2.0) THEN
         Y = X * X * 0.25
         BESSK1 = (LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         Y = 2.0 / X
         BESSK1 = (EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END

!C=========================================================================
      function bessi1(X)

      include'../Calc/divapre.h'

      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,-0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1, &
          -0.2895312D-1,0.1787654D-1,-0.420059D-2/

      IF(ABS(X).LT.3.75) THEN
         Y = X*X / (3.75*3.75)
         BESSI1 = X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         AX = ABS(X)
         Y = 3.75 / AX
         BESSI1 = (EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
         IF(X.LT.0.) BESSI1 = - BESSI1
      ENDIF

      RETURN
      END
