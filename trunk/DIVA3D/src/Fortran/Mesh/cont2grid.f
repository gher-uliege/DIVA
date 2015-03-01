C To go from ODV .coa files (see documentation of ODV)
C to DIVA coast.cont files
C using a resolution comparable to the specified gridded output of divacalc
C In: fort.13 grid definition. This grid is slightly increased for the contour retained
C   : fort.10 ODV coast
C Out: fort.67: original ODV contour in diva format but with crossings 
C    : fort.99: Diva contour to be used by divamesh
      
      
      parameter(np=1000,nm=np*np,nmp=(np+2)*(np+2))
      parameter(ncmax=1000,npmax=1500)
      
      real*4 topo(nm),x(npmax,ncmax),y(npmax,ncmax)
      real*4 xcorn(4,ncmax)
      integer*2 ic(nmp*3)
      integer ip(ncmax)
      real*4 surf(ncmax)
      
      
      
      nc=0
      nct=0
      read(13,*) x1
      read(13,*) y1
      read(13,*) dx
      read(13,*) dy
      read(13,*) M
      read(13,*) N
      xmin=x1-dx/50
      ymin=y1-dy/50
      xmax=x1+(M-1)*dx+dx/50
      ymax=y1+(N-1)*dy+dy/50
      nx=M
      ny=N
      if(M*N.GT.NP*NP) stop 'Increase np'

        write(6,*) 'Global box to use'
c        read(60,*) xmin,xmax,ymin,ymax
        write(6,*) xmin,xmax,ymin,ymax
c        write(11,*) xmin, ymin
c        write(11,*) xmax, ymin
c        write(11,*) xmax,ymax
c        write(11,*) xmin, ymax
      read(10,*,err=999) n
      do i=1,n
       read(10,*,err=999) ip(i)
       j=1
 17     continue
       if(j.gt.ip(i)) goto 101
       read(10,*,err=999) x(j,i),y(j,i)
       if(j.gt.1) then
	    if(abs(x(j,i)-x(j-1,i)).lt.
     &     0.000001*abs(x(j,i)+x(j-1,i))) then
	    if(abs(y(j,i)-y(j-1,i)).lt.
     &     0.000001*abs(y(j,i)+y(j-1,i))) then
	    write(6,*) 'Found two succesive identical points ', I, J
	    j=j-1
	    ip(i)=ip(i)-1
	    endif
	    endif
	    endif
	    j=j+1
	    goto 17
 101    continue
C close contour for convenience
      x(j+1,i)=x(1,i)
      y(j+1,i)=y(1,i)
      enddo
      
      close(10)

        do nc=1,n
        surf(nc)=0
        xcorn(1,nc)=x(1,nc)
        xcorn(2,nc)=x(1,nc)
        xcorn(3,nc)=y(1,nc)
        xcorn(4,nc)=y(1,nc)
        do j=1,ip(nc)
        surf(nc)=surf(nc)+(y(j+1,nc)-y(j,nc))*(x(j+1,nc)+x(j,nc))
        xcorn(1,nc)=min(xcorn(1,nc),x(j,nc))
        xcorn(2,nc)=max(xcorn(2,nc),x(j,nc))
        xcorn(3,nc)=min(xcorn(3,nc),y(j,nc))
        xcorn(4,nc)=max(xcorn(4,nc),y(j,nc))
        
        enddo
c        surf(nc)=-surf(nc)
        write(6,*) 'Surf',nc,surf(nc),dx*dy
        enddo
        
        
C Deal with signs too: allow only positive values : already done by tests
C Eliminate contours that are too small or fall outside the box
        
        surfmin=dx*dy
        nnc=0
        nc=n
        do nn=1,n
        nnc=nnc+1
        
        if(surf(nn).le.surfmin) then
        surf(nn)=0
        nnc=nnc-1
        write(6,*) 'Contour ',nn, ' too small'
        goto 444
        endif
        if(xcorn(2,nn).le.xmin) then
        surf(nn)=0
        nnc=nnc-1
        goto 444
        endif
        if(xcorn(1,nn).ge.xmax) then
        surf(nn)=0
        nnc=nnc-1
        goto 444
        endif
        if(xcorn(4,nn).le.ymin) then
        surf(nn)=0
        nnc=nnc-1
        goto 444
        endif
        if(xcorn(3,nn).ge.ymax) then
        surf(nn)=0
        nnc=nnc-1
        goto 444
        endif
        
        
 444    continue       
        enddo
        write(6,*) 'Will use ', nnc,' out of ',n, '  contours'

        write(6,*) 'Now trying to make grid'
        
        call makecont(x,y,ip,npmax,nc,topo,nx,ny,
     & xmin,xmax,ymin,ymax,ic,surf,xcorn)
        write(6,*) 'Finished'
        
      stop
 999  continue
      write(6,*) 'Unable to read contour file correctly'
      stop
      end
      
      
C23456      
      subroutine makecont(x,y,ip,npmax,nc,topo,nx,ny,
     & xmin,xmax,ymin,ymax,ic,surf,xcorn)
      real*4 x(npmax,*),y(npmax,*),xcorn(4,*)
      integer*2 ic(*)
      integer ip(*)
      real*8 c8(1)
      real*4 valex
      real*4 surf(*)
      real*4 topo(nx,ny),xmin,xmax,ymin,ymax
      character*10 coastname
      dx=(xmax-xmin)/(nx-1)
      dy=(ymax-ymin)/(ny-1)
      x1=xmin
      y1=ymin
      ifirst=1
      do i=1,nx
       
       xx=xmin+(i-1)*dx
c       write(6,*) 'x ',xx
       do j=1,ny
       yy=ymin+(j-1)*dy
       topo(i,j)=1
C now loop on all contours, as soon as point is in a contour go out
C and put topo(i,j)=0 (not part of the meshing domain)   
C speed can maybe be increased by checking first on the biggest contours (with
C the highest probability of having the point)
C An even better optimization would use scan-line filling of the grid as for colouring
C a polygon
C
C  First try the polygon for for wich the last point was in (neighbor probably)
       nn=ifirst
       iflag=0
        if(surf(nn).eq.0) goto 129
        if(xx.le.xcorn(1,nn)) goto 129
        if(xx.ge.xcorn(2,nn)) goto 129
        if(yy.le.xcorn(3,nn)) goto 129
        if(yy.ge.xcorn(4,nn)) goto 129
c       call checkin(xx,yy,x(1,nn),y(1,nn),ip(nn),surf(nn),iflag)
       call PNPOLY(xx,yy,x(1,nn),y(1,nn),ip(nn),iflag)
  
        if (iflag.eq.1) then
c        write(6,*) 'Point',xx,yy, ' in contour', nn
        topo(i,j)=0
        goto 123
        endif
 129   continue      
C Then look at other contours
       do nn=1,nc
       iflag=0
        if(nn.eq.ifirst) goto 130
        if(surf(nn).eq.0) goto 130
        if(xx.le.xcorn(1,nn)) goto 130
        if(xx.ge.xcorn(2,nn)) goto 130
        if(yy.le.xcorn(3,nn)) goto 130
        if(yy.ge.xcorn(4,nn)) goto 130

c       call checkin(xx,yy,x(1,nn),y(1,nn),ip(nn),surf(nn),iflag)
       call PNPOLY(xx,yy,x(1,nn),y(1,nn),ip(nn),iflag)
        if (iflag.eq.1) then
c        write(6,*) 'Point',xx,yy, ' in contour', nn
        topo(i,j)=0
        ifirst=nn
        goto 123
        endif
 130   continue      
       enddo       


       
 123   continue
       enddo
      enddo
       
      
      valex=99999.
      Z=0.5
      M=nx
      N=ny
      iu=99
      c8=0
      iprec=4
      imaxc=nx
      jmaxc=ny
      kmaxc=1
      nbmots=nx*ny
      open(99,form='unformatted')
      call uwritc(iu,c8,topo,valex,iprec,imaxc,jmaxc,kmaxc,nbmots)

      return
      end
      
      
      

C     ..................................................................
C                                                                       
C        SUBROUTINE PNPOLY                                              
C                                                                       
C        PURPOSE                                                        
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON            
C                                                                       
C        USAGE                                                          
C           CALL PNPOLY (PX, PY, XX, YY, N, INOUT )                     
C                                                                       
C        DESCRIPTION OF THE PARAMETERS                                  
C           PX      - X-COORDINATE OF POINT IN QUESTION.                
C           PY      - Y-COORDINATE OF POINT IN QUESTION.                
C           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF         
C                     VERTICES OF POLYGON.                              
C           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF           
C                     VERTICES OF POLYGON.                              
C           N       - NUMBER OF VERTICES IN THE POLYGON.                
C           INOUT   - THE SIGNAL RETURNED:                              
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,        
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,     
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.         
C                                                                       
C        REMARKS                                                        
C           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.      
C           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY           
C           OPTIONALLY BE INCREASED BY 1.                               
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING      
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX    
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING   
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.              
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.         
C           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM      
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.   
C                                                                       
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  
C           NONE                                                        
C                                                                       
C        METHOD                                                         
C           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT  
C           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE        
C           POINT IS INSIDE OF THE POLYGON.                             
C                                                                       
C     ..................................................................
C                                                                       
      SUBROUTINE PNPOLY(PX,PY,XX,YY,N,INOUT)                            
      REAL*4 XX(N),YY(N),PX,PY
      REAL*8 X(2000),Y(2000)
      LOGICAL MX,MY,NX,NY                                               
      INTEGER O                                                         
C      OUTPUT UNIT FOR PRINTED MESSAGES                                 
      DATA O/6/                                                         
      MAXDIM=2000                                                        
      IF(N.LE.MAXDIM)GO TO 6                                            
      WRITE(O,7)                                                        
7     FORMAT('0WARNING:',I5,' TOO GREAT FOR THIS VERSION OF PNPOLY.     
     1RESULTS INVALID')                                                 
      RETURN                                                            
6     DO 1 I=1,N                                                        
      X(I)=XX(I)-PX                                                     
1     Y(I)=YY(I)-PY                                                     
      INOUT=-1                                                          
      DO 2 I=1,N                                                        
      J=1+MOD(I,N)                                                      
      MX=X(I).GE.0.0                                                    
      NX=X(J).GE.0.0                                                    
      MY=Y(I).GE.0.0                                                    
      NY=Y(J).GE.0.0                                                    
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GO TO 2       
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GO TO 3  
      INOUT=-INOUT                                                      
      GO TO 2            
3     IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))).LT.0) goto 2
      IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))).EQ.0) goto 4   
      IF(((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))).GT.0) goto 5      
C replaced depreciated style
C     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5                       
4     INOUT=0                                                           
      RETURN                                                            
5     INOUT=-INOUT                                                      
2     CONTINUE                                                          
      RETURN                                                            
      END                                             
      Subroutine UWRITC(iu,c8,c4,valexc,iprec,imaxc,jmaxc,kmaxc,nbmots)
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

       endif
c
c skip KBLANC lines
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
          write(iu,ERR=99) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) (c4(ide+kc),kc=1,ir)
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
