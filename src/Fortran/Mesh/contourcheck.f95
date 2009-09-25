      integer nmax,ncmax
      parameter(nmax=10000,ncmax=1000)
      
      include'iodv.h'

!C
      real*4 x(nmax,ncmax),y(nmax,ncmax),lc,lcm
      real*8 surf(ncmax)
      integer ip(ncmax),n,i
      integer ncr(nmax,4),isin(ncmax,ncmax)
      real rmin
      common/rminval/rmin
      
      if(iodv.eq.1) then
!c**rs
      write(6,*) 'Opening domain.raw'
      open(10,file='domain.raw')
!c**rs
      endif      
!C read the n contours (maximum number ncmax)
      read(10,*,err=997) n
      do i=1,n
       read(10,*,err=998) ip(i)
       j=1
 17     continue
       if(j.gt.ip(i)) goto 101
       read(10,*,err=999) x(j,i),y(j,i)
       if(j.gt.1) then
	    if(abs(x(j,i)-x(j-1,i)).lt.0.000001*abs(x(j,i)+x(j-1,i))) then
	    if(abs(y(j,i)-y(j-1,i)).lt.0.000001*abs(y(j,i)+y(j-1,i))) then
	    write(6,*) 'Found two succesive identical points ', I, J
	    j=j-1
	    ip(i)=ip(i)-1
	    endif
	    endif
	    endif
	    j=j+1
	    goto 17
 101    continue
      
      enddo
      
      close(10)
!C
      write(6,*) 'Check for splitted contours'
      
 135  continue
      nconn=0
      do ii=1,n
      do jj=1,n
      if(ii.ne.jj.and.ip(ii).ne.0.and.ip(jj).ne.0) then
       
      if(abs(x(1,jj)-x(ip(ii),ii)).lt.0.000001*abs(x(1,jj)+x(ip(ii),ii))) then
      if(abs(y(1,jj)-y(ip(ii),ii)).lt.0.000001*abs(y(1,jj)+y(ip(ii),ii))) then
     
      write(6,*) 'Try to connect contours',ii,jj,ip(ii),ip(jj)
       do i=2,ip(jj)
       x(ip(ii)+i-1,ii)=x(i,jj)
       y(ip(ii)+i-1,ii)=y(i,jj)
       enddo
       
       ip(ii)=ip(ii)+ip(jj)-1
       ip(jj)=0
       nconn=nconn+1
           
      endif
      endif
      
      endif
      enddo
      enddo
      if (nconn.ne.0) goto 135
      write(6,*) 'no splitted contour found anymore'     
 963  continue
      if(iodv.eq.1) then     
!c**rs
      open(11,file='meshgen.prm')
!c**rs
      endif


      read(11,*) lc
      read(11,*) lc
      read(11,*) rfact
      lcm=lc*min(rfact,1/rfact)
      rmin=0.05*lcm*lcm  
      RLLC=lc*sqrt(min(rfact,1/rfact))
    
      close(11)

!C Calculate bounding box
      xmin=x(1,1)
      xmax=x(1,1)
      ymin=y(1,1)
      ymax=y(1,1)
      do i=1,n
       do j=1,ip(i)
       xmin=min(xmin,x(j,i))
       xmax=max(xmax,x(j,i))
       ymin=min(ymin,y(j,i))
       ymax=max(ymax,y(j,i))
       enddo
      enddo
!C for contour i, there are ip(i) points on the contour (maximum nmax)
      nochange=0

      write(6,*) 'Read successfully ',n, ' contours'
      write(6,*) 'in the box for x in ',xmin,' to ',xmax
      write(6,*) 'and y between ', ymin, ' and', ymax

!C Need to think about closing or not contours... and/or last segment..
!C if not closed add point do close for easier calculation
      eps=0.000001
      do i=1,n
      isclosed=0
      if (abs(x(ip(i),i)-x(1,i)).lt.(eps*abs(x(ip(i),i)+x(1,i)))) then
      if (abs(y(ip(i),i)-y(1,i)).lt.(eps*abs(y(ip(i),i)+y(1,i)))) then
      isclosed=1
      endif
      endif
!C if not closed, add point
      if(isclosed.eq.0) then
      write(6,*) 'Closing contour ',i,' for convencience'
      write(6,*) ip(i), ' points originally'
      ip(i)=ip(i)+1
      endif
!C make sure it is closed
      x(ip(i),i)=x(1,i)
      y(ip(i),i)=y(1,i)
      enddo

!C Verify number of crossings of segments
      ncrossed=0
      
      do i=1,n
        do j=1,ip(i)-1
!C Check is segment of point (j,j+1) of contour (i) is crossing any other segment
!C look only at those not yet tested before
!C
           jjj=0
           if(j.eq.1) jjj=1
           do jj=j+2,ip(i)-1-jjj
           iscross=0
           ii=i
             call iscrossing(x(j,i),y(j,i),x(j+1,i),y(j+1,i),x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             ncrossed=ncrossed+iscross
           enddo

           
!C          on other contours
           do ii=i+1,n
             do jj=1,ip(ii)-1
             iscross=0
             call iscrossing(x(j,i),y(j,i),x(j+1,i),y(j+1,i),x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             ncrossed=ncrossed+iscross
             enddo
           enddo
        
        enddo
      enddo


      write(6,*) 'we found ',ncrossed, ' crossings'

!C Check for "idendical points" and "close" points
       identical=0
       nclose=0
      do i=1,n
       do j=1,ip(i)-1
        do ii=i,n
         do jj=1,ip(ii)-1
         if(i.eq.ii.and.j.eq.jj) goto 333
         if (abs(x(j,i)-x(jj,ii)).lt.(eps*abs(x(j,i)+x(jj,ii)))) then
         if (abs(y(j,i)-y(jj,ii)).lt.(eps*abs(y(j,i)+y(jj,ii)))) then
          identical=identical+1
          write(6,*) 'identical points: ',identical
          write(6,*)  'contour ', i, ' point ',j
          write(6,*)  'with contour ', ii, ' point ',jj
         endif
         endif
         dist=(x(j,i)-x(jj,ii))**2+(y(j,i)-y(jj,ii))**2
         if(dist.lt.rmin) nclose=nclose+1
 333     continue
         enddo
        enddo
       enddo
      enddo
      write(6,*) ' number of couple of points that are identical ',identical
      write(6,*) ' number of couples of close points', nclose
!C
!C
      write(6,*) 'Try to eliminate identical points'
      ntries=0
 1030 continue
      ntries=ntries+1
      if (identical.eq.0) goto 1135
      write(6,*) 'remaining identical',identical
      
      identical=0
      do i=1,n
       do j=1,ip(i)-1
        do ii=i+1,n
         do jj=1,ip(ii)-1
         if(i.eq.ii.and.j.eq.jj) goto 335
         if (abs(x(j,i)-x(jj,ii)).lt.(eps*abs(x(j,i)+x(jj,ii)))) then
         if (abs(y(j,i)-y(jj,ii)).lt.(eps*abs(y(j,i)+y(jj,ii)))) then
          identical=identical+1
          write(6,*) 'identical points: ',identical
          write(6,*)  'contour ', i, ' point ',j,ip(i)
          write(6,*)  'with contour ', ii, ' point ',jj,ip(ii)
          jm1=j-1
          if(jm1.lt.1) jm1=1
          ljm1=jj-1
          if(jjm1.lt.1) jjm1=1
          x(j,i)=(x(jm1,i)+2*x(j,i)+x(j+1,i))/4
          y(j,i)=(y(jm1,i)+2*y(j,i)+y(j+1,i))/4
          x(jj,ii)=(x(jjm1,ii)+2*x(jj,ii)+x(jj+1,ii))/4
          y(jj,ii)=(y(jjm1,ii)+2*y(jj,ii)+y(jj+1,ii))/4
          
          if (ntries.gt.100) then
!C Eliminate points
          do jjjj=j,ip(i)-1
          x(jjjj,i)=x(jjjj+1,i)
          y(jjjj,i)=y(jjjj+1,i)
          ip(i)=ip(i)-1
          enddo
          do jjjj=jj,ip(ii)-1
          x(jjjj,ii)=x(jjjj+1,ii)
          y(jjjj,ii)=y(jjjj+1,ii)
          ip(ii)=ip(ii)-1
          enddo

                    endif
          
         endif
         endif
 335     continue
         enddo
        enddo
       enddo
      enddo
      
      
      
       

       goto 1030
 1135  continue
  
       AREA=0
       do i=1,n
        surf(i)=0
        do j=1,ip(i)-1
         surf(i)=surf(i)+(y(j+1,i)-y(j,i))*(x(j+1,i)+x(j,i))
        enddo
        AREA=AREA+surf(i)
        write(6,*) 'Signed surface of contour ',i, ': ',surf(i)/2 
       enddo
       write(6,*) 'Total signed surface ',AREA/2,' (+: anticlockwise)'
       write(6,*) 'Estimated nb of elements : ',NINT(ABS(AREA/(RLLC*RLLC)))
       
          
!C
      
!C while there has been any change on the contour points loop over contours
 1    continue
!c      do i=1,n
!c      write(6,*) ip(i),i
!c      enddo
      if (nochange.eq.1) goto 99
      nochange=1
      
      i=0
 10   continue
      i=i+1
!c      write(6,*) 'loop over i',i,nochange

!C when i > n finished a loop over all contours, try again
      if(i.gt.n) goto 1
      if(ip(i).eq.0) goto 10
!C now look into contour i
      if(ip(i).le.4) then
!C if one segment too short, eliminate contour
         dist1=(x(1,i)-x(2,i))**2+(y(1,i)-y(2,i))**2
         dist2=(x(3,i)-x(2,i))**2+(y(3,i)-y(2,i))**2
         dist3=(x(1,i)-x(3,i))**2+(y(1,i)-y(3,i))**2
         if ((dist1.le.rmin).or.(dist2.le.rmin).or.(dist3.le.rmin)) then
         nochange=0
         ip(i)=0
         endif
!C next contour
      goto 10   
      endif


!C
      j=0
!C
 11   continue
      j=j+1
      if (j.gt.ip(i)-1) goto 10
      
      dist=(x(j,i)-x(j+1,i))**2+(y(j,i)-y(j+1,i))**2
!c      if(i.eq.9) write(6,*) 'dist??',dist,rmin
      if (dist.gt.rmin) goto 11
!C if small distance between points: candidate for elimination
      ifix1=0
      ifix2=0
       if(                                             &
        ((x(j,i).eq.xmin).or.(x(j,i).eq.xmax)).or.    &
        ((y(j,i).eq.ymin).or.(y(j,i).eq.ymax))        &
       ) ifix1=1
      if(                                                 &
        ((x(j+1,i).eq.xmin).or.(x(j+1,i).eq.xmax)).or.   &
        ((y(j+1,i).eq.ymin).or.(y(j+1,i).eq.ymax))       &
       ) ifix2=1
      
      if (j.eq.1) ifix1=1
      if (j.eq.(ip(i)-1)) ifix2=1    
      if ((ifix1+ifix2).eq.2) goto 11

      if (ifix1.eq.1) then
      xnew=x(j,i)
      ynew=y(j,i)
      goto 55
      endif
      if (ifix2.eq.1) then
      xnew=x(j+1,i)
      ynew=y(j+1,i)
      goto 55
      endif
      xnew=0.5*(x(j,i)+x(j+1,i))
      ynew=0.5*(y(j,i)+y(j+1,i))
 55   continue
!C
!C now check if when replacing point j with new values and
!C dropping j+1, the two new segments do not cross the others
      iscross=0
!C check on crossing


!C          on other contours
           do ii=1,n
             if (ii.eq.i) goto 33
             do jj=1,ip(ii)-1
             
             if((j.gt.1).and.(j.lt.(ip(i)-1))) then
             call iscrossing(x(j-1,i),y(j-1,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             if (iscross.eq.1) goto 33
             call iscrossing(x(j+2,i),y(j+2,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             if (iscross.eq.1) goto 33
             endif
             
             if(j.eq.1) then
             call iscrossing(x(3,i),y(3,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             if (iscross.eq.1) goto 33
             endif
             
             
             if(j.eq.(ip(i)-1)) then
             call iscrossing(x(j-1,i),y(j-1,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             if (iscross.eq.1) goto 33
             endif
             
             
             if (iscross.eq.1) goto 33
             
             enddo
 33        continue
           enddo
           if (icross.eq.1) then
           write(6,*) 'some crossing with other contour'
           endif
            if(icross.eq.1) goto 44
!C Now check on contour itself:
       ii=i
!c      write(6,*) 'need to add a test on contour itself'
!C if they cross, better not change
!c
!c For the left segment
      if (j.eq.1) then
             do jj=4,ip(i)-2
             call iscrossing(x(3,i),y(3,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'a',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo
                  else
             jjj=0
             if(j.eq.2) jjj=1
             do jj=j+2,ip(i)-1-jjj
             call iscrossing(x(j-1,i),y(j-1,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'b',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo
             jjj=0
             if(j.eq.ip(i)-1) jjj=1
             do jj=1+jjj,j-3
             call iscrossing(x(j-1,i),y(j-1,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'c',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo  
      endif
      
!C For right segment
      if (j.eq. (ip(i)-1)) then
             do jj=2,ip(i)-4
             call iscrossing(x(j-1,i),y(j-1,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'd',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo
                  else
             jjj=0
             if (j.eq.ip(i)-2) jjj=1
             do jj=1+jjj,j-2
             call iscrossing(x(j+2,i),y(j+2,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'e',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo
             jjj=0
             if(j.eq.1) jjj=1
             do jj=j+3,ip(i)-1-jjj
             call iscrossing(x(j+2,i),y(j+2,i),xnew,ynew,x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii), iscross)
!c             if(iscross.eq.1) then
!c             write(6,*) 'f',iscross,jj,j
!c             endif
             if (iscross.eq.1) goto 44
             enddo
      endif

 44   continue
!c       if (iscross.eq.1) then
!c        write(6,*) 'point cannot be eliminated otherwise new crossings'
!c       endif
      if (iscross.eq.1) goto 11
      
!C they do not cross


      

 
!C OK, point to eliminate
      nochange=0
      x(j,i)=xnew
      y(j,i)=ynew
      do jj=j+1,ip(i)-1
      x(jj,i)=x(jj+1,i)
      y(jj,i)=y(jj+1,i)
      enddo
      ip(i)=ip(i)-1
      
!C ok, point eliminated, goto next contour
      goto 10
      
      

!C try next point on same contour
      goto 11
!C
!C
 50   continue
!C try next contour
      goto 10      
      
      
 99   continue
!C no change in point positions anymore
!C perform final check on contour segments crossing
!C If no elimination (possible error in detection): uncomment line
!C     GOTO 4356
 8854 continue
!C Verify number of crossings of segments
      ncrossed=0
      write(6,*) 'Starting checks again'
      do i=1,n
        do j=1,ip(i)-1
!C Check is segment of point (j,j+1) of contour (i) is crossing any other segment
!C look only at those not yet tested before
!C
           jjj=0
           if(j.eq.1) jjj=1
           do jj=j+2,ip(i)-1-jjj
           iscross=0
           ii=i
             call iscrossing(x(j,i),y(j,i),x(j+1,i),y(j+1,i),x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             ncrossed=ncrossed+iscross
             if(iscross.eq.1) then
             write(6,*) 'scrossed',i,j,ii,jj,jjj,ip(i),ncrossed
             ncr(ncrossed,1)=i
             ncr(ncrossed,2)=j
             ncr(ncrossed,3)=ii
             ncr(ncrossed,4)=jj
             
             endif
           enddo
        enddo
      enddo

      
       write(6,*) 'On other contours'
       do i=1,n
        do j=1,ip(i)-1
!C
!c     on other contours
           do ii=i+1,n
             do jj=1,ip(ii)-1
             iscross=0
             call iscrossing(x(j,i),y(j,i),x(j+1,i),y(j+1,i),x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii),iscross)
             ncrossed=ncrossed+iscross
             if(iscross.eq.1) then 
             write(6,*) 'crossed',i,j,ii,jj,ncrossed
             write(6,*) x(j,i),y(j,i),x(j+1,i),y(j+1,i)
             write(6,*) x(jj,ii),y(jj,ii),x(jj+1,ii),y(jj+1,ii)
             
             ncr(ncrossed,1)=i
             ncr(ncrossed,2)=j
             ncr(ncrossed,3)=ii
             ncr(ncrossed,4)=jj
             
             endif
             enddo
           enddo
        
        enddo
      enddo


      write(6,*) 'we found ',ncrossed, ' crossings after eliminations'
      

      identical=0
       nclose=0
      do i=1,n
       do j=1,ip(i)-1
        do ii=i,n
         do jj=1,ip(ii)-1
         if(i.eq.ii.and.j.eq.jj) goto 336
         if (abs(x(j,i)-x(jj,ii)).lt.(eps*abs(x(j,i)+x(jj,ii)))) then
         if (abs(y(j,i)-y(jj,ii)).lt.(eps*abs(y(j,i)+y(jj,ii)))) then
          identical=identical+1
!c          write(6,*) 'identical points: '
!c          write(6,*)  'contour ', i, ' point ',j
!c          write(6,*)  'with contour ', ii, ' point ',jj
         endif
         endif
         dist=(x(j,i)-x(jj,ii))**2+(y(j,i)-y(jj,ii))**2
         if(dist.lt.rmin) nclose=nclose+1
 336     continue
         enddo
        enddo
       enddo
      enddo
      write(6,*) ' number of after rearranging are identical after elimination',identical
      write(6,*) ' number of couples of remaining close points', nclose

      AREA=0
       do i=1,n
        surf(i)=0
        do j=1,ip(i)-1
         surf(i)=surf(i)+(y(j+1,i)-y(j,i))*(x(j+1,i)+x(j,i))
        enddo
        AREA=AREA+surf(i)
!c        write(6,*) 'Signed surface of contour ',i, ': ',surf(i)/2
       enddo
       write(6,*) 'Total signed surface ',AREA/2,' (+: anticlockwise)'
       write(6,*) 'Estimated nb of elements : ',NINT(ABS(AREA/(RLLC*RLLC)))
      write(6,*) 'Eliminating areas less of 0.01 percent of area'
      do i=1,n
      if (abs(surf(i)).lt.0.0001*abs(AREA)) then
      ip(i)=0
      endif
      enddo
!C write out only contours with at least three points and
!C     GOTO 8421
      write(6,*) 'Now try to drop small areas with crossing',ncrossed
   
      ncrossec=0
      do jjj=1,ncrossed
      
      i=ncr(jjj,1)
      j=ncr(jjj,2)
      ii=ncr(jjj,3)
      jj=ncr(jjj,4)
      write(6,*) 'Areas of contours',i,ii,surf(i),surf(ii)
!C Better : for contours that cross, swith tails of contours (and copy last points)
!C For self-crossing: split the contour into two (add new contour at end)

       if(surf(ii).ne.0.and.surf(i).ne.0) then
        if(n+1.gt.ncmax) stop 'increase ncmax'
        ncrossec=ncrossec+1
         if(i.ne.ii) then
!C two different contours
          write(6,*) 'rearranging tails of contours',i,ii
          write(6,*) 'points',j,jj,ip(i),ip(ii)
          write(6,*) 'Should also cross again later:'
          write(6,*) ncr(jjj+1,1),ncr(jjj+1,3)
          write(6,*) ncr(jjj+1,2),ncr(jjj+1,4)
          
          j1=j
          j2=ncr(jjj+1,2)
          isj=1
          if(j1.gt.j2) then
          isj=-1
          endif
          
          
          jj1=jj
          jj2=ncr(jjj+1,4)
          isjj=1
          if(jj1.gt.jj2) then
          isjj=-1
          endif
          
          if(j1.gt.j2.and.jj1.gt.jj2) then
          jjjjjj=j1
          j1=j2
          j2=jjjjjj
          jjjjjj=jj1
          jj1=jj2
          jj2=jjjjjj
          isj=1
          isjj=1
          endif
          if(j2.eq.j1+1) then
!C just drop point j2
          do jjjjj=j2+1,ip(i)
          x(jjjjj-1,i)=x(jjjjj,i)
          y(jjjjj-1,i)=y(jjjjj,i)
          enddo
          ip(i)=ip(i)-1
          if(ip(i).le.3) ip(i)=3
          goto 8854
          
          endif
          if(jj2.eq.jj1+1) then
!C just drop point jj2
          do jjjjj=jj2+1,ip(ii)
          x(jjjjj-1,ii)=x(jjjjj,ii)
          y(jjjjj-1,ii)=y(jjjjj,ii)
          enddo
          ip(ii)=ip(ii)-1
          if(ip(ii).le.3) ip(ii)=3
          goto 8854
          
          endif
          if(j1.eq.j2+1) then
!C just drop point j2
          do jjjjj=j1+1,ip(i)
          x(jjjjj-1,i)=x(jjjjj,i)
          y(jjjjj-1,i)=y(jjjjj,i)
          enddo
          ip(i)=ip(i)-1
          if(ip(i).le.3) ip(i)=3
          goto 8854
          
          endif
          if(jj1.eq.jj2+1) then
!C just drop point jj2
          do jjjjj=jj1+1,ip(ii)
          x(jjjjj-1,ii)=x(jjjjj,ii)
          y(jjjjj-1,ii)=y(jjjjj,ii)
          enddo
          ip(ii)=ip(ii)-1
          if(ip(ii).le.3) ip(ii)=3
          goto 8854
          
          endif
          
          
                    
!C If both are at the end, reorder and hope it turns out next time:
!c          if(j.eq.ip(i)-1.) then
!c
!c          do jjjjj=1,ip(i)-1
!c          x(jjjjj,i)=x(jjjjj+1,i)
!c          y(jjjjj,i)=y(jjjjj+1,i)
!c          enddo
!c          x(ip(i),i)=x(1,i)
!c          y(ip(i),i)=y(1,i)
!c
!c
!c          ncrossec=1
!c          goto 8854
!c          endif
!c          if(jj.eq.ip(ii)-1.) then
!c          do jjjjj=1,ip(ii)-1
!c          x(jjjjj,ii)=x(jjjjj+1,ii)
!c          y(jjjjj,ii)=y(jjjjj+1,ii)
!c          enddo
!c          x(ip(ii),ii)=x(1,ii)
!c          y(ip(ii),ii)=y(1,ii)
!c
!c          ncrossec=1
!c          goto 8854
!c          endif

!C Save one tail if ii into working array
          if(isj.eq.1.and.isjj.eq.1) then
          
          
          do jjjj=jj1+1,ip(ii)-1
          x(jjjj,n+1)=x(jjjj,ii)
          y(jjjj,n+1)=y(jjjj,ii)
          enddo
!C now put tail of i on ii
          do jjjj=j1+1,ip(i)-1
          jjjjj=jj1+jjjj-j1
          x(jjjjj,ii)=x(jjjj,i)
          y(jjjjj,ii)=y(jjjj,i)
          enddo
!C then put saved tail on i
          do jjjj=jj1+1,ip(ii)-1
          jjjjj=j1+jjjj-jj1
          x(jjjjj,i)=x(jjjj,n+1)
          y(jjjjj,i)=y(jjjj,n+1)
          enddo
!C adapt number of points
          iold=ip(i)
          iiold=ip(ii)
          ip(i)=j1+iiold-jj1
          ip(ii)=jj1+iold-j1
          
          j2n=j1+jj2-jj1
          jj2n=jj1+j2-j1
          j2=j2n
          jj2=jj2n
          
          
          do jjjj=jj2+1,ip(ii)-1
          x(jjjj,n+1)=x(jjjj,ii)
          y(jjjj,n+1)=y(jjjj,ii)
          enddo
!C now put tail of i on ii
          do jjjj=j2+1,ip(i)-1
          jjjjj=jj2+jjjj-j2
          x(jjjjj,ii)=x(jjjj,i)
          y(jjjjj,ii)=y(jjjj,i)
          enddo
!C then put saved tail on i
          do jjjj=jj2+1,ip(ii)-1
          jjjjj=j2+jjjj-jj2
          x(jjjjj,i)=x(jjjj,n+1)
          y(jjjjj,i)=y(jjjj,n+1)
          enddo
!C adapt number of points
          iold=ip(i)
          iiold=ip(ii)
          ip(i)=j2+iiold-jj2
          ip(ii)=jj2+iold-j2
          

          write(6,*) 'New contours have', ip(i),ip(ii)
          if(ip(i).le.3) ip(i)=0
          if(ip(ii).le.3) ip(ii)=0
          endif

          if(isj.eq.-1.and.isjj.eq.1) then
          jjjjj=j1
          j1=j2
          j2=jjjjj
          jjjjj=jj1
          jj1=jj2
          jj2=jjjjj
          
          
          
          isj=1
          isjj=-1
          endif
          
          
          if(isj.eq.1.and.isjj.eq.-1) then
!C momentarely return contour
          write(6,*) 'Returning contour',i,ii,j,jj
          do jjjjj=1,ip(ii)/2
          tr=x(jjjjj,ii)
          x(jjjjj,ii)=x(ip(ii)-jjjjj+1,ii)
          x(ip(ii)-jjjjj+1,ii)=tr
          tr=y(jjjjj,ii)
          y(jjjjj,ii)=y(ip(ii)-jjjjj+1,ii)
          y(ip(ii)-jjjjj+1,ii)=tr
          enddo
          
          jj1=ip(ii)-jj1
          jj2=ip(ii)-jj2
          
          
          do jjjj=jj1+1,ip(ii)-1
          x(jjjj,n+1)=x(jjjj,ii)
          y(jjjj,n+1)=y(jjjj,ii)
          enddo
!C now put tail of i on ii
          do jjjj=j1+1,ip(i)-1
          jjjjj=jj1+jjjj-j1
          x(jjjjj,ii)=x(jjjj,i)
          y(jjjjj,ii)=y(jjjj,i)
          enddo
!C then put saved tail on i
          do jjjj=jj1+1,ip(ii)-1
          jjjjj=j1+jjjj-jj1
          x(jjjjj,i)=x(jjjj,n+1)
          y(jjjjj,i)=y(jjjj,n+1)
          enddo
!C adapt number of points
          iold=ip(i)
          iiold=ip(ii)
          ip(i)=j1+iiold-jj1
          ip(ii)=jj1+iold-j1
          
          j2n=j1+jj2-jj1
          jj2n=jj1+j2-j1
          j2=j2n
          jj2=jj2n
          
          do jjjj=jj2+1,ip(ii)-1
          x(jjjj,n+1)=x(jjjj,ii)
          y(jjjj,n+1)=y(jjjj,ii)
          enddo
!C now put tail of i on ii
          do jjjj=j2+1,ip(i)-1
          jjjjj=jj2+jjjj-j2
          x(jjjjj,ii)=x(jjjj,i)
          y(jjjjj,ii)=y(jjjj,i)
          enddo
!C then put saved tail on i
          do jjjj=jj2+1,ip(ii)-1
          jjjjj=j2+jjjj-jj2
          x(jjjjj,i)=x(jjjj,n+1)
          y(jjjjj,i)=y(jjjj,n+1)
          enddo
!C adapt number of points
          iold=ip(i)
          iiold=ip(ii)
          ip(i)=j2+iiold-jj2
          ip(ii)=jj2+iold-j2
          

          write(6,*) 'New contours have', ip(i),ip(ii)
          if(ip(i).le.3) ip(i)=0
          if(ip(ii).le.3) ip(ii)=0
          
          if(ip(ii).ne.0) then
          x(ip(ii),ii)=x(1,ii)
          y(ip(ii),ii)=y(1,ii)
!C  return contour
          do jjjjj=1,ip(ii)/2
          tr=x(jjjjj,ii)
          x(jjjjj,ii)=x(ip(ii)-jjjjj+1,ii)
          x(ip(ii)-jjjjj+1,ii)=tr
          tr=y(jjjjj,ii)
          y(jjjjj,ii)=y(ip(ii)-jjjjj+1,ii)
          y(ip(ii)-jjjjj+1,ii)=tr
          enddo
          endif
          
          endif
                    

!C close contours to make sure
          if(ip(i).ne.0) then
          x(ip(i),i)=x(1,i)
          y(ip(i),i)=y(1,i)
          endif
          if(ip(ii).ne.0) then
          x(ip(ii),ii)=x(1,ii)
          y(ip(ii),ii)=y(1,ii)
          endif
          else
!C Self crossing contour, eliminate for the moment.
          write(6,*) 'Cutting contour in two',i,j,jj
!c          ip(i)=0
!c          goto 8854
!C self crossing
         isj=1
         if(j.gt.jj) then
         jjjjj=j
         j=jj
         jj=jjjjj
         isj=-1
         write(6,*) '==================='
         write(6,*) '===========????===='
         endif
!C new contour
         do jjjjj=j+1,jj
          x(jjjjj-j,n+1)=x(jjjjj,i)
          y(jjjjj-j,n+1)=y(jjjjj,i)
         enddo
         ip(n+1)=jj-j+1
         x(ip(n+1),n+1)=x(1,n+1)
         y(ip(n+1),n+1)=y(1,n+1)
!C return new contour
          do jjjjj=1,ip(n+1)/2
          tr=x(jjjjj,n+1)
          x(jjjjj,n+1)=x(ip(n+1)-jjjjj+1,n+1)
          x(ip(n+1)-jjjjj+1,n+1)=tr
          tr=y(jjjjj,n+1)
          y(jjjjj,n+1)=y(ip(n+1)-jjjjj+1,n+1)
          y(ip(n+1)-jjjjj+1,n+1)=tr
          enddo
         if(ip(n+1).le.3) ip(n+1)=0
!C rest of contour remains, but needs to be copied
         write(6,*) 'Added a contour',ip(n+1)
         n=n+1
         write(6,*) 'n',n,ip(n)
!c         do jjjjj=1,ip(n)
!c         write(6,*) x(jjjjj,n),y(jjjjj,n)
!c         enddo
         
         do jjjjj=jj+1,ip(i)
         jjjjjj=j+jjjjj-jj
         x(jjjjjj,i)=x(jjjjj,i)
         y(jjjjjj,i)=y(jjjjj,i)
         enddo
         ip(i)=ip(i)-(jj-j)
         if(ip(i).le.3) ip(i)=0
         write(6,*) 'shortened contour',ip(i)        
!c         do jjjjj=1,ip(i)
!c         write(6,*) x(jjjjj,i),y(jjjjj,i)
!c         enddo
         endif
       goto 8854
       endif


!C      if(abs(surf(ii)).gt.abs(surf(i))) then
!C      ip(i)=0
!C      else
!C      ip(ii)=0
!C      endif
!C      if(surf(i).ne.0.and.surf(ii).ne.0) then
!c      ncrossec=ncrossec+1
!c      endif
      enddo
      if (ncrossec.gt.0) goto 8854
      
!C Finally, check sign ordering of contours contained within others:

!C
      nnt=0
 8421 continue
!C continue until no incoherence anymore
!C but stop after 10000 iterations?
      if(nnt.ge.10000) goto 4433
      nnt=nnt+1
      itopo=0
      do i=1,n
       do j=1,n
       isin(i,j)=0
       enddo
      enddo
      do i=1,n
!C Check if within any other contour
      iscont=0
      if (ip(i).ne.0) then
      
      do j=1,n
      iflag=0
      if(i.ne.j) then
      call checkin(x(1,i),y(1,i),x(1,j),y(1,j),ip(j),surf(j),iflag)
      if (iflag.eq.1) then
      if(abs(surf(i)).lt.abs(surf(j))) then
      iscont=iscont+1
      isin(i,iscont)=j
      isin(i,iscont+1)=0
      write(6,*) 'nested contour',i,j,iscont,ip(j),surf(i),surf(j)
      endif
      endif
      
      endif
      enddo
      endif
      enddo
      
!C start with those only with one parent
      iscont=0
 9991 continue
      iscont=iscont+1
      
      ifini=1
      do i=1,n
      if(isin(i,iscont).ne.0) then
      ifini=0
      endif
      enddo
      
      
      do i=1,n
      if(isin(i,iscont+1).eq.0.and.isin(i,iscont).ne.0) then
        iii=1
        do ii=1,iscont
        iii=iii*sign(1.D0,surf(isin(i,ii)))
        enddo
        iii=2*mod(iscont,2)-1
        if(surf(i)*iii.gt.0) then
        
        
        
        
        write(6,*) 'Incoherent ordering and signs of contours?',i
        do j=1,ip(i)/2
         trx=x(j,i)
         try=y(j,i)
         x(j,i)=x(ip(i)-j+1,i)
         y(j,i)=y(ip(i)-j+1,i)
         x(ip(i)-j+1,i)=trx
         y(ip(i)-j+1,i)=try
        enddo
        surf(i)=-surf(i)
        itopo=itopo+1
        endif
      
      endif
      enddo
      if (ifini.ne.1) goto 9991
      
      if(itopo.ne.0) goto 8421      
 4433 continue
!C do not write out the artificial closing point
      if(iodv.eq.1) then
!c**rs
      open(30,file='domain.checked')
!c**rs
      endif
      nc=0
      do i=1,n
      if (ip(i).ge.4) nc=nc+1
      enddo
      write(30,*) nc
      do i=1,n
      if (ip(i).ge.4) then
!C no last point
        write(30,*) ip(i)-1
        do j=1,ip(i)-1
         write(30,*) x(j,i),y(j,i)
        enddo
      endif
      enddo
      close(30)
      stop
 999  continue
      write(6,*) 'Sorry, your contour does not seem to have the'
      write(6,*) 'right format on contour', i
      stop
 998  continue
      write(6,*) 'Sorry, your contour does not seem to have the'
      write(6,*) 'right format or number of points on contour', i-1
      stop
 997  continue
      write(6,*) 'Sorry, your contour does not seem to have the'
      write(6,*) 'right format. Start with number of contours'
      stop


            end
      
      subroutine iscrossing(x1,y1,x2,y2,x3,y3,x4,y4,isc)
      
      real*8 a,b,c,d,e,f,xi,eta,det
      real rmin
      common/rminval/rmin
      
      isc=0
      
      if (min(x1,x2).gt.max(x3,x4)) return
      if (min(y1,y2).gt.max(y3,y4)) return
      if (max(x1,x2).lt.min(x3,x4)) return
      if (max(y1,y2).lt.min(y3,y4)) return
!C else look for intersection
!C (x,y)=x1+xi(x2-x1),y1+xi(y2-y1)
!C (x,y)=x3+eta(x4-x3),y3+eta(y4-y3)
      a= (x2-x1)
      b=-(x4-x3)
      e= x3-x1
      c= (y2-y1)
      d=-(y4-y3)
      f= y3-y1
      det=a*d-b*c
      vv=abs(a)+abs(b)+abs(c)+abs(d)
      if(abs(det).lt.0.0000001*vv) then
!C Parallel lines, calcute distance of point (x1,y1) to the second line
      den=((x4-x3)**2+(y4-y3)**2)
      if (den.eq.0) then
      dist=0 
                    else
      eta=((x1-x3)*(x4-x3)+(y1-y3)*(y4-y3))/den
      dist=(x1-x3-eta*(x4-x3))**2+(y1-y3-eta*(y4-y3))**2
      endif
      write(6,*) 'Distance ', dist
       if (dist.le.0.001*rmin) then
!C because alreadt checked to be in the same box, if zero distance,
       write(6,*) 'overlapping segments'
       write(6,*) x1,y1,x2,y2
       write(6,*) x3,y3,x4,y4
!C move one laterally
       dxxx=-(y2-y1)
       dyyy=(x2-x1)
       dist=sqrt(dxxx*dxxx+dyyy*dyyy)
       dxxx=dxxx/dist*0.01*rmin
       dyyy=dyyy/dist*0.01*rmin
!c       x1=x1+dxxx
!c       x2=x2+dxxx
!c       y1=y1+dyyy
!c       y2=y2+dyyy
       
       write(6,*) 'moved to',rmin,dxxx,dyyy
       write(6,*) x1,y1,x2,y2
       
       isc=1
       return
       else
       return
       endif
                                   else
      xi=(e*d-f*b)/det
      eta=(a*f-e*c)/det
      endif
      if(xi.lt.0) return
      if(xi.gt.1.0000001) return
      if(eta.lt.0) return
      if(eta.gt.1.000001) return
      isc=1
      return
      end
      
      subroutine checkin(x,y,xc,yc,ip,surf,iflag)
      real*4 xc(*),yc(*)
      real*8 surf,r,wangle,dx,dy,dxt,dyt
      wangle=0
      iflag=1
      r=0
      if(surf.eq.0) then
      iflag=0
      return
      endif
      do i=1,ip-1
      dx=(xc(i)-x)
      dy=(yc(i)-y)
      dxt=(xc(i+1)-x)
      dyt=(yc(i+1)-y)
      
      III=sign(1.D0,dx*dyt-dxt*dy)
      wangle=wangle+III*dacos((dx*dxt+dy*dyt)/(dsqrt((dx*dx+dy*dy)*(dxt*dxt+dyt*dyt))))
      
      enddo
!c      write(6,*) 'Surf',surf,ip,wangle
      
      if(abs(wangle).le.0.1) iflag=0
      return
      end
      
      
      
      
