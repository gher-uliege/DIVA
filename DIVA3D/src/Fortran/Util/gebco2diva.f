      parameter(idata=5000000)
      real*4 c(idata)
      real*8 valex8,c8(1)
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
c      write(6,*) 'ii',ii,x1,y1
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
C      rewind(10)
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
      if(abs(dx-1./120.).LT.(1./12000.)) then
      write(6,*) 'Probably 30 seconds grid, rounding'
      dx=1./120.
      endif
      if(abs(dy-1./120.).LT.(1./12000.)) then
      write(6,*) ' Probably 30 seconds grid, rounding'
      dy=1./120.
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
c      write(6,*),x,y,i,j,jjjjj,imax,jmax
      else
      i=nint((x-x0)/dx)+1
      j=nint((y-y0)/dy)+1
      endif
      jmax=max(j,jmax)
      
      imred=imax/ired
      ir=(i-1)/ired+1
      jr=(j-1)/jred+1
      jmred=jmax/jred
c      write(0,*) imred,jmred,ired,jred,ir,jr,jjjjj
c       write(0,*) i,ired,ir,imred
c       write(0,*) j,jred,jr,jmred
c       write(0,*) dx,dy,ired,jred
c       write(0,*) dx*ired, dy*jred, x0, (ired-1)*dx
c      call sleep(1)
      if(ir.gt.imred) goto 10
      if(jr.gt.jmred) goto 10
      c(ir+(jr-1)*imred)=topo
c      write(0,*) ir, jr, topo
c      call sleep(1)
      goto 10
 999  continue
      xs=0
      ys=0
      if (isgebco.ge.1) then
      xs=(ired-1)*dx
      ys=0
      endif
      
ccc	More precise dx and dy - S. Watelet - 19/05/2015

      dx_old=dx
      dy_old=dy

      If ((dx/=1/60.).and.(dx/=1/120.)) then
      	rewind(10)
	If (iorder==1) then
		read(10,*,end=1000,err=1000) x1,y1,topo
		Do i=2,imax-1
		read(10,*,end=1000,err=1000) x,y,topo
		Enddo
		read(10,*,end=1000,err=1000) x2,y2,topo
		dx=abs(x2-x1)/real(imax)
	elseif (iorder==2) then
		read(10,*,end=1000,err=1000) x1,y1,topo
		Do i=2,isgebco-1
		read(10,*,end=1000,err=1000) x,y,topo
		Enddo
		read(10,*,end=1000,err=1000) x2,y2,topo
		dx=abs(x2-x1)/real(imax)
	Endif
      Endif 

 1000 continue

      If ((dy/=1/60.).and.(dy/=1/120.)) then
	rewind(10)
	If (iorder==2) then
		read(10,*,end=1001,err=1001) x1,y1,topo
		Do i=2,jmax-1
		read(10,*,end=1001,err=1001) x,y,topo
		Enddo
		read(10,*,end=1001,err=1001) x2,y2,topo
		dy=abs(y2-y1)/real(jmax)
	elseif (iorder==1) then
		read(10,*,end=1001,err=1001) x1,y1,topo
		Do i=2,isgebco-1
		read(10,*,end=1001,err=1001) x,y,topo
		Enddo
		read(10,*,end=1001,err=1001) x2,y2,topo
		dy=abs(y2-y1)/real(jmax)
	Endif
      Endif 

 1001 continue

	If ((abs(dx-dx_old)>(dx_old/10.)).or.
     &(abs(dy-dy_old)>(dy_old/10.))) then
	write(0,*) "WARNING : problem in gebco2diva.f, contact GHER !"
	write(0,*) "dx_old=",dx_old,"dy_old=",dy_old
	write(0,*) "dx=",dx,"dy=",dy
	Endif

	write(6,*) "more precise dx=",dx,"more precise dy=",dy

ccc

      write(20,*) x0+xs
      write(20,*) y0+ys
      write(20,*) dx*ired
      write(20,*) dy*jred
      write(20,*) imred
      write(20,*) jmred
      
      call uwritc(12,c8,C,valex8,4,imred,jmred,1,imred)
      write(6,*) 'Finished writing binary file'
      stop
      END
      Subroutine UWRITC(iu,c8,c4,valex8,ipre8,imaxc,jmaxc,kmaxc,nbmots)
c                ======
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c writes the field C(I,J,K)  into fortran unit iu 
c writes the field in the array c4 if iprecr=4
c writes the field in the array c8 if iprecr=8
c
c The KBLANC blank lines are at the disposal of the user
c JMB 6/3/92
c
c IF c(i,j,k)=NaN or infinity, it is replaced by VALEX! 
c
c 
c RS 12/1/93
c
c If nbmots = -1  then write only 1 data record
c     (only for non-degenerated data)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      real*8 valex8
      real*4 valexc
c in the calling routin you can specify the following equivalence to
c save memory space:
c      equivalence(c,c4)
c      equivalence(c,c8)
c
c Putting  Valex where not numbers
       z=0.
       un=1.
       ich=0
       ioff=1
       if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

       IF (NBMOTS.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

       do k=1,kmaxc
        do j=1,jmaxc
         do i=1,imaxc
c         if( c4(ioff).eq.(z/z) ) goto 1010 
c         if( c4(ioff).eq.(un/z) ) goto 1010 
c         if( c4(ioff).eq.(-z/z) ) goto 1010 
c         if( c4(ioff).eq.(-un/z) ) goto 1010 
         goto 1011
 1010     continue
          c4(ioff)=sngl(valex8)
          ich=ich+1
 1011    continue 
         ioff=ioff+1
         enddo
        enddo
       enddo
       if(ich.gt.0) then
       write(6,*) ' WARNING:',ich,' Values are not numbers'
       write(6,*) '   Changing them into VALEX'
       endif
       endif
       valexc=SNGL(valex8)
       iprec=4
c
c skip KBLANC lines
C        write(6,*) iu,imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
       do 1 kb=1,KBLANC
        write(iu,ERR=99)
 1     continue
c
        write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
c
c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
c
c if pathological case, write only four values C0 and DCI,DCJ,DCK found 
c as the two four elements of the array so that C(I,J,K) =
c C0 + I * DCI + J * DCJ + K * DCK
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
c
c
c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,ir)
                       else
c
c double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          write (iu,ERR=99) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          write (iu,ERR=99) (c8(ide+kc),kc=1,ir)
                       else
           goto 99
         endif
         endif
c
         return
 99      continue
         write(*,*) 'Data error in UWRITC, not a conform file'
        write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
        write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
         return
         end
         
