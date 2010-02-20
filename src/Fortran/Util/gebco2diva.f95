      integer, parameter :: idata=5000000
      real*4 c(idata)
      real*8 valex8
      valex8=-99999
      isgebco=0
      ired=1
      jred=1
      read(5,*,end=1313) isgebco,ired,jred
      
 1313 continue
      if(ired.ne.1) then
      write(6,*) 'Try to reduce X grid by',ired
      endif
      if(jred.ne.1) then
      write(6,*) 'Try to reduce Y grid by',jred
      endif
      
      write(6,*) 'Try to guess grid topology'
      read(10,*) x1,y1,d
      read(10,*) x2,y2,d
      
      if( abs(x2-x1).gt.1E-5*(abs(x1)+abs(x2))) then
       dx=x2-x1
       x0=x1
       write(6,*) 'x coordinates vary first',dx
       iorder=1
                                               else
       dy=y2-y1
       y0=y1
       write(6,*) 'y coordinates vary first',dy
       iorder=2                                
      endif
      rewind(10)
      read(10,*) x1,y1,d
      ii=1
 1    continue
      read(10,*) x2,y2,d
      if(iorder.eq.1) then
      if(abs(y2-y1).lt.1E-5*(abs(y1)+abs(y2))) then
      ii=ii+1
      x1=x2
      y1=y2
!c      write(6,*) 'ii',ii,x1,y1
      goto 1
                                              else
      imax=ii
      dy=y2-y1
      y0=y1
      if (dy.lt.0) then
      write(6,*) 'negative dy, will modify'
 2    continue
      read(10,*,end=888,err=888) x1,y1,d
      y0=min(y0,y1)
      goto 2
 888  continue
      dy=-dy
      write(6,*) 'y0',y0,dy
!C      rewind(10)
      endif
      goto 99
      endif
      
      
      else
      write(6,*) 'To be implemented'
      
      endif
 99   continue
      rewind(10)
      ii=0
      jmax=1
      if(abs(dx-1./60.).LT.(1./6000.)) then
      write(6,*) 'Probably 1 minute grid, rounding'
      dx=1./60.
      endif
      if(abs(dy-1./60.).LT.(1./6000.)) then
      write(6,*) ' Probably 1 minute grid, rounding'
      dy=1./60.
      endif
      jjjjj=0
      
 10   continue
      read(10,*,end=999,err=999) x,y,topo
      jjjjj=jjjjj+1
      if(isgebco.ne.0) then
      jmax=isgebco/imax
      j=(jjjjj-1)/imax
      j=jmax-j
      i=jjjjj-(jmax-j)*imax
!c      write(6,*),x,y,i,j,jjjjj,imax,jmax
      else
      i=nint((x-x0)/dx)+1
      j=nint((y-y0)/dy)+1
      endif
      jmax=max(j,jmax)
      
      imred=imax/ired
      ir=(i-1)/ired+1
      jr=(j-1)/jred+1
      jmred=jmax/jred
      if(ir.gt.imred) goto 10
      if(jr.gt.jmred) goto 10
      c(ir+(jr-1)*imred)=topo
      goto 10
 999  continue
      xs=0
      ys=0
      if (isgebco.eq.1) then
      xs=(ired-1)*dx
      ys=0
      endif
      write(20,*) x0+xs
      write(20,*) y0+ys
      write(20,*) dx*ired
      write(20,*) dy*jred
      write(20,*) imred
      write(20,*) jmred
      
      call uwritc(12,valex8,C,valex8,4,imred,jmred,1,imred)
      write(6,*) 'Finished writing binary file'
      stop
      END
