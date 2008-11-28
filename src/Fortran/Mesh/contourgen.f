      parameter(nm=5000000)
      real*4 topo(nm)
      real*8 c8
      integer*2 ic(nm*3)
      character*10 coastname
      character*16 cn
      character*5 depth
      read(10,*) x1
      read(10,*) y1
      read(10,*) dx
      read(10,*) dy
      read(10,*) M
      read(10,*) N
      coastname='coast.cont'
      
      
      
      if ((M+2)*(N+2).GT.NM) stop 'increase NM'
      write(6,*) 'into ureadc',M,N
      call ureadc(12,c8,topo,valex,iprecr,imax,jmax,kmax,nbmotr)
      write(6,*) 'out of reading',imax,jmax
      
      
      
      if ((M.NE.IMAX).OR.(N.NE.JMAX)) stop 'incoherent files'
      z=0
      nl=0
 1    continue
      read(13,*,err=99,end=99) z
      nl=nl+1
      write(depth,88) 10000+nl
 88   format(I5)
      cn=coastname//"."//depth
      open(file=cn,unit=99)
      call contourg(topo,
     &   ic(1),ic(1+(M+2)*(N+2)),ic(1+2*(M+2)*(N+2)),M+2,N+2,Z,
     &   x1,y1,dx,dy,valex)
      close(99)
      
      goto 1
 99   continue
C output at $z=0$ anyway
      z=0
      open(file=coastname,unit=99)
      call contourg(topo,
     &   ic(1),ic(1+(M+2)*(N+2)),ic(1+2*(M+2)*(N+2)),M+2,N+2,Z,
     &   x1,y1,dx,dy,valex)
      close(99)
      
      stop
      end
      
      subroutine contourg(topo,ic,ix,iy,M,N,z,x1,y1,dx,dy,valex)
      parameter(nmax=20000,ncm=2000)
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
c      write(6,*) 'icase',i,j,icase
C      from  below
      if (icase.eq.1) then
      call nextiter(IC(i,j),IC(i+1,j),IC(i+1,j+1),IC(i,j+1),IY)
c       write(6,*) '???',i,j,IY
       if(IY.EQ.5) then
         icasen=3
         in=i+1
         jn=j
         xn=i+1
         if(IC(i,j).eq.0) then
         yn=j+0.75
                          else
         yn=j+0.25
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

                          else
         xn=i+0.75

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
                          else
         yn=j+0.75
         endif
         return
       endif
      endif

C  
      if (icase.eq.2) then
C      from above
      call nextiter(IC(i+1,j),IC(i,j),IC(i,j-1),IC(i+1,j-1),IY)
c        write(6,*) '!!!??',i,j,IY
       if(IY.EQ.5) then
c         write(6,*) '!!!'
         icasen=4
         in=i
         jn=j-1
         xn=i
         if(IC(i,j).eq.0) then
         yn=j-0.25
                          else
         yn=j-0.75
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
                          else
         xn=i+0.75
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
         yn=j-0.75
                          else
         yn=j-0.75
         yn=j-0.25
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
                          else
         xn=i+0.75
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
                          else
         yn=j+0.75
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
         xn=i+0.75
                          else
         xn=i+0.75
         xn=i+0.25
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
c         xn=i-0.25
                          else
         xn=i-0.25
c         xn=i-0.75
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
                          else
         yn=j+0.75
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
                          else
         xn=i-0.75
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
         
