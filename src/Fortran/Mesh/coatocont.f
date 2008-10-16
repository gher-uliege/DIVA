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
 1      continue
        read(10,*,end=99,err=99) I1,I2
        
        if (i1.ne.0.and.i2.ne.0) then
         nc=nc+1
         nct=nct+1
         ip(nc)=I1
         if(nc.gt.ncmax) then
          write(6,*) 'Too many contours'
          write(6,*) 'Increase paramter ncmax'
          stop
         endif
         if(I1.gt.npmax) then
          write(6,*) 'Too many points on contour'
          write(6,*) 'Increase paramter npmax'
          stop
         endif
        do i=1,I1
        read(10,*,end=999,err=999) y(i,nc),x(i,nc)
        enddo
        if(I1.lt.3) then
        nc=nc-1
        goto 1
        endif
        surf(nc)=0
        xcorn(1,nc)=x(I1,nc)
        xcorn(2,nc)=x(I1,nc)
        xcorn(3,nc)=y(I1,nc)
        xcorn(4,nc)=y(I1,nc)
        do j=1,I1-1
        surf(nc)=surf(nc)+(y(j+1,nc)-y(j,nc))*(x(j+1,nc)+x(j,nc))
        xcorn(1,nc)=min(xcorn(1,nc),x(j,nc))
        xcorn(2,nc)=max(xcorn(2,nc),x(j,nc))
        xcorn(3,nc)=min(xcorn(3,nc),y(j,nc))
        xcorn(4,nc)=max(xcorn(4,nc),y(j,nc))
        
        enddo
        
        write(6,*) 'Contour ', nc ,' surface', surf(nc)
c        write(6,*) 'Corners',xcorn(1,nc),xcorn(2,nc),
c     &    xcorn(3,nc),xcorn(4,nc)
        if(surf(nn).lt.0) then
        write(6,*) '??????',nn
        endif
        surf(nc)=abs(surf(nc))
        surfmax=max(surf(nc),surfmax)
CC Optimization possibility: it current contour is outside the box of interest
C nc=nc-1





                                 else
        goto 99
        endif
        goto 1
 99     continue
C write out original contours (including crossings etc in diva format
        write(67,*) nc
        do ii=1,nc
        write(67,*) ip(ii)-1
        do jj=ip(ii),2,-1
         write(67,*) x(jj,ii),y(jj,ii)
        enddo
        enddo
 
c        rr=(xmax-xmin)/(ymax-ymin)
c        nx=np*sqrt(rr)
c        ny=np/sqrt(rr)
c        dx=(xmax-xmin)/(nx-1)
c        dy=(ymax-ymin)/(ny-1)
C Eliminate contours that are too small or fall outside the box
        
        surfmin=dx*dy*2
        nnc=0
        do nn=1,nc
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
        write(6,*) 'Will use ', nnc,' out of ',nct, ' ODV contours'

        write(6,*) 'Now trying to make DIVA contours'
        
        call makecont(x,y,ip,npmax,nc,topo,nx,ny,
     & xmin,xmax,ymin,ymax,ic,surf,xcorn)
        write(6,*) 'Finished'
        
      stop
 999  continue
      write(6,*) 'Unable to read ODV contour file correctly'
      stop
      end
      
      
C23456      
      subroutine makecont(x,y,ip,npmax,nc,topo,nx,ny,
     & xmin,xmax,ymin,ymax,ic,surf,xcorn)
      real*4 x(npmax,*),y(npmax,*),xcorn(4,*)
      integer*2 ic(*)
      integer ip(*)
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
      call contourg(topo,
     &   ic(1),ic(1+(M+2)*(N+2)),ic(1+2*(M+2)*(N+2)),M+2,N+2,Z,
     &   x1,y1,dx,dy,valex)

      return
      end
      
      
      
      subroutine contourg(topo,ic,ix,iy,M,N,z,x1,y1,dx,dy,valex)
      parameter(nmax=100000,ncm=2000)
      integer*2 IC(M,N),IX(M,N),IY(M,N)
      real*4 xx(nmax,ncm),yy(nmax,ncm)
      real*4 TOPO(M-2,N-2)
      integer npc(ncm)
      
      
C 
C Add border of zeros around the real grid
      do i=1,M
      do j=1,N
      IX(i,j)=0
      IY(i,j)=0
      IC(i,j)=0
      enddo
      enddo
      
C Read depth for mask (here negative value for etopo5)
c      read(70,*) DD
      write(6,*) 'Contour on z=',z
      DD=z
C Create mask
      do i=2,M-1
       do j=2,N-1
       if (TOPO(I-1,J-1).EQ.VALEX) then
       IC(I,J)=0
       else
      if(TOPO(I-1,J-1).GT.DD) IC(I,J)=1
c        IC(I,J)=1
       endif
       enddo
      enddo
      
      do j=N,1,-1
      NNN=75
      IF(M.LT.NNN) NNN=M
      write(6,111) (IC(i,j),i=1,NNN)
      enddo
 111  format(75(I1))
      
      write(6,*) 'Starting'
      call allcontours(IC,IX,IY,M,N,xx,yy,npc,nc,nmax,ncm)

C
C For points with equal last point equals first point: islands or interior basin.

      do jj=1,nc
       npp=abs(npc(jj))
       if (abs(xx(1,jj)-xx(npp,jj)).lt.0.00001
     & *abs(xx(1,jj)+xx(npp,jj))) then
       if (abs(yy(1,jj)-yy(npp,jj)).lt.0.00001
     & *abs(yy(1,jj)+yy(npp,jj))) then
C island
       if(npc(jj).gt.1) then
       npc(jj)=npc(jj)-1
       endif
       if(npc(jj).lt.-1) then
       npc(jj)=npc(jj)+1
       endif
       goto 19
       endif
       endif


     
     
C23456
C for contour arriving at the boundary: need to close with corners of grid:
C if positive contour at those corners that are missing

 19   continue
      enddo
C For dimensional coordinates, finally add origin and delta x.
C xx and yy are 1,1 for the first grid point center
C
      write(99,*) nc
      do jj=1,nc
       write(99,*) abs(npc(jj))
       do i=1,abs(npc(jj))
       ii=i
       if (npc(jj).gt.0) then
       ii=abs(npc(jj))-i+1
       endif
       write(99,*) x1+(xx(ii,jj)-2)*DX,y1+(yy(ii,jj)-2)*DY
       enddo
      enddo
      return
      end

      SUBROUTINE READTOPO (A,IMAX,JMAX)
C ----------------------------------------
      INTEGER IMAX,JMAX
      REAL*4 A(IMAX,JMAX)


      
      DO 20 J = JMAX,1,-1
         write(6,*) 'Read',I
         READ (10,101) (A(I,J),I=1,IMAX)

20    CONTINUE
10    CONTINUE

101   FORMAT (10F8.2)
      return
      END

      
            
      subroutine allcontours(IC,IX,IY,M,N,xx,yy,npc,nc,nmax,ncm)
      integer*2 IC(M,N),IX(M,N),IY(M,N)
      real*4 xx(nmax,ncm),yy(nmax,ncm)
      integer npc(ncm)
C Need to check seeds and signs of seeds..
      nc=0
C Now interior cases only
      
      do j=2,N-1
      do i=1,M-1
      icase=0
      if (IX(i,j).eq.0) then
      if ((IC(i,j).eq.0).and.(IC(i+1,j).EQ.1)) then
C found a new seed
       icase=1
       isig=1
       endif
      if ((IC(i,j).eq.1).and.(IC(i+1,j).EQ.0)) then
       icase=1
       isig=-1
      endif
      endif
      
C23456
      if (icase.ne.0) then
c       write(6,*) 'new seed',i,j,icase
       iin=i
       jin=j
       call nextcontour(iin,jin,icase,xx(1,nc+1),yy(1,nc+1),np,
     & IC,M,N,IX,IY,nmax)
       nc=nc+1
       if(nc.gt.ncm) STOP 'increase ncm'
       npc(nc)=np*isig
       
      endif
      
      enddo
      enddo
      
      do i=2,M-1
      do j=1,N-1
      
      icase=0
      if (IY(i,j).eq.0) then
      if ((IC(i,j).eq.0).and.(IC(i,j+1).EQ.1)) then
C found a new seed
       icase=3
       isig=-1
       endif
      if ((IC(i,j).eq.1).and.(IC(i,j+1).EQ.0)) then
       icase=3
       isig=1
      endif
      endif
      
C23456
      if (icase.ne.0) then
c       write(6,*) 'new seed',i,j,icase
       iin=i
       jin=j
       call nextcontour(iin,jin,icase,xx(1,nc+1),yy(1,nc+1),np,
     & IC,M,N,IX,IY,nmax)
       nc=nc+1
       npc(nc)=np*isig
       
      endif
      
      enddo
      enddo

      
      return
      end
      
      subroutine nextcontour(i,j,icase,xc,yc,np,IC,M,N,IX,IY,nmax)
      integer*2 ic(M,N),IX(M,N),IY(M,N)
C IX(i,j) mask to mark that interface i,j -i+1,j was treated
      real*4 xc(nmax),yc(nmax)
      np=0
 
 1    continue
      call nextpoint(i,j,icase,xn,yn,in,jn,icasen,IC,M,N)
C Stop if new point is on a boarder or already treated
      np=np+1
      xc(np)=xn
      yc(np)=yn
c      write(6,*) xn,yn,i,j,icase,icasen,in,jn
      if(icasen.eq.1) then
      if(IX(in,jn).eq.1) return
      IX(in,jn)=1
      if (jn.eq.N) then
      return
      endif
      endif
      if(icasen.eq.2) then
      if(IX(in,jn).eq.1) return
      IX(in,jn)=1
      if (jn.eq.1) then
      return
      endif
      endif
      
      if(icasen.eq.3) then
      if(IY(in,jn).eq.1) return
      IY(in,jn)=1
      if (in.eq.M) then
      return
      endif
      endif
      if(icasen.eq.4) then
      if(IY(in,jn).eq.1) return
      IY(in,jn)=1
      if (in.eq.1) then
      return
      endif
      endif
      i=in
      j=jn
      icase=icasen
      goto 1
      end
      
      
      subroutine nextpoint(i,j,icase,xn,yn,in,jn,icasen,IC,M,N)
      integer*2 ic(M,N)
C 4 situations 
C
C    between i and i+1
C     
C      from  below
      if (icase.eq.1) then
      call nextiter(IC(i,j),IC(i+1,j),IC(i+1,j+1),IC(i,j+1),IY)
       if(IY.EQ.5) then
         icasen=3
         in=i+1
         jn=j
         xn=i+1
         if(IC(i,j).eq.0) then
         yn=j+0.75
         yn=j+0.5
                          else
         yn=j+0.25
         yn=j+0.5
         
         endif
         return
       endif
       if(IY.EQ.6) then
         icasen=1
         in=i
         jn=j+1
         yn=j+1
         if(IC(i,j).eq.0) then
         xn=i+0.25
         xn=i+0.5
                          else
         xn=i+0.75
         xn=i+0.5
         endif
         return
       endif
       if(IY.EQ.7) then
         icasen=4
         in=i
         jn=j
         xn=i
         if(IC(i,j).eq.0) then
         yn=j+0.25
         yn=j+0.5
                          else
         yn=j+0.75
         yn=j+0.5
         endif
         return
       endif
      endif

C  
      if (icase.eq.2) then
C      from above
      call nextiter(IC(i+1,j),IC(i,j),IC(i,j-1),IC(i+1,j-1),IY)

       if(IY.EQ.5) then
         icasen=4
         in=i
         jn=j-1
         xn=i
         if(IC(i,j).eq.0) then
         yn=j-0.25
         yn=j-0.5
                          else
         yn=j-0.75
         yn=j-0.5
         endif
         return
       endif
       if(IY.EQ.6) then
         icasen=2
         in=i
         jn=j-1
         yn=j-1
         if(IC(i,j).eq.0) then
         xn=i+0.25
         xn=i+0.5
                          else
         xn=i+0.75
         xn=i+0.5
         endif
         return
       endif
       if(IY.EQ.7) then
         icasen=3
         in=i+1
         jn=j-1
         xn=i+1
         if(IC(i,j).eq.0) then
         yn=j-0.25
         yn=j-0.5
                          else
         yn=j-0.75
         yn=j-0.5
         endif
         return
       endif


        
       endif 
C   between j and j+1        
C
       if (icase.eq.3) then
c       from left
       call nextiter(IC(i,j+1),IC(i,j),IC(i+1,j),IC(i+1,j+1),IY)
 
       
       if(IY.EQ.5) then
         icasen=2
         in=i
         jn=j
         yn=j
         if(IC(i,j).eq.0) then
         xn=i+0.25
         xn=i+0.5
                          else
         xn=i+0.75
         xn=i+0.5
         endif
         return
       endif
       if(IY.EQ.6) then
         icasen=3
         in=i+1
         jn=j
         xn=i+1
         if(IC(i,j).eq.0) then
         yn=j+0.25
         yn=j+0.5
                          else
         yn=j+0.75
         yn=j+0.5
         endif
         return
       endif
       if(IY.EQ.7) then
         icasen=1
         in=i
         jn=j+1
         yn=j+1
         if(IC(i,j).eq.0) then
         xn=i+0.25
         xn=i+0.5
                          else
         xn=i+0.75
         xn=i+0.5
         endif
         return
       endif

       
             
       
       
       endif
       
       if (icase.eq.4) then
c       from right
       call nextiter(IC(i,j),IC(i,j+1),IC(i-1,j+1),IC(i-1,j),IY)
        
       
       if(IY.EQ.5) then
         icasen=1
         in=i-1
         jn=j+1
         yn=j+1
         if(IC(i,j).eq.0) then
         xn=i-0.75
         xn=i-0.5
                          else
         xn=i-0.25
         xn=i-0.5
         endif
         return
       endif
       if(IY.EQ.6) then
         icasen=4
         in=i-1
         jn=j
         xn=i-1
         if(IC(i,j).eq.0) then
         yn=j+0.25
         yn=j+0.5
                          else
         yn=j+0.75
         yn=j+0.5
         
         endif
         return
       endif
       if(IY.EQ.7) then
         icasen=2
         in=i-1
         jn=j
         yn=j
         if(IC(i,j).eq.0) then
         xn=i-0.25
         xn=i-0.5
                          else
         xn=i-0.75
         xn=i-0.5
         endif
         return
       endif

       
       
        endif
        stop '???????'
        end
C
        subroutine nextiter(I1,I2,I3,I4,IY)
        integer*2 I1,I2,I3,I4

C  4   6    3
C
C   7      5
C
C  1   x   2
C
C We are in x and come from below.
C output says if to go to interface 5, 6 or 7
C 
C either zero is to left coming from below:
       if(I1.EQ.0) THEN

       If  (I4.EQ.1) then
        IY=7
        GOTO 100
       endif
       if  (I3.EQ.1) then
        IY=6
        GOTO 100
       endif
       if  (I2.EQ.1) then
        IY=5
        GOTO 100
       else
       stop '???' 
       endif
       
       ELSE
C or zero is to right coming from below:
       
       If  (I3.EQ.1) then
        IY=5
        GOTO 100
       endif
       if  (I4.EQ.1) then
        IY=6
        GOTO 100
       endif
       if  (I1.EQ.1) then
        IY=7
        GOTO 100
       else
       stop '???' 
       endif
       
       endif
 100   continue
       return
       end

      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
C                ======
C-----------------------------------------------------------------------
C Reads the field C(I,J,K) from fortran unit iu
C returns the field in the array c4 if the returned iprecr=4
C returns the field in the array c8 if the returned iprecr=8
C returns the values if imaxr,jmaxr,kmaxr found in the file
C
C JMB 6/3/91
C-----------------------------------------------------------------------
C
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)

C in the calling routin you can specify the following equivalence to
C save memory space:
C      equivalence(c,c4)

C      equivalence(c,c8)
C
C skip KBLANC lines
       do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1     continue
C
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
C
C pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc

C      print *, 'iprecr=', iprec
C      print *, 'imaxr=', imaxc
C      print *, 'jmaxr=', jmaxc
C      print *, 'kmaxr=', kmaxc
C      print *, 'nbmotr=', nbmots
C      print *, 'valexr=', valexc


C
C compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
C

C if pathological case, read only four values C0 and DCI,DCJ,DCK
C and return
C them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
C
C
C single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
C
C double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,ir)
                       else
         goto 99
         endif
         endif
C
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

         return
         end
         
      subroutine checkin(x,y,xc,yc,ip,surf,iflag)
      real*4 xc(*),yc(*)
      real*8 r,wangle,dx,dy,dxt,dyt
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
      
      RIII=sign(1.D0,dx*dyt-dxt*dy)
      wangle=wangle+RIII*dacos(
     &(dx*dxt+dy*dyt)/(dsqrt((dx*dx+dy*dy)*(dxt*dxt+dyt*dyt))))
      
      enddo
c      write(6,*) 'Surf',surf,ip,wangle
      
      if(abs(wangle).le.0.1) iflag=0
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
3     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5                       
4     INOUT=0                                                           
      RETURN                                                            
5     INOUT=-INOUT                                                      
2     CONTINUE                                                          
      RETURN                                                            
      END                                             
