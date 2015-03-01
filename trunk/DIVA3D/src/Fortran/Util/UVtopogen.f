      parameter(nm=5000000)
      real*4 topo(nm),u(nm),v(nm)
      real*4 unn(nm),vnn(nm)
      real*8 c8(1)
      
      character*4 uname,vname
      character*10 un,vn
      character*5 depth
      read(10,*) x1
      read(10,*) y1
      read(10,*) dx
      read(10,*) dy
      read(10,*) M
      read(10,*) N
      uname='Uvel'
      vname='Vvel'
      
      
      
      if ((M+0)*(N+0).GT.NM) stop 'increase NM'
      write(6,*) 'into ureadc',M,N
      call ureadc(12,c8,topo,valex,iprecr,imax,jmax,kmax,nbmotr)
      write(6,*) 'out of reading',imax,jmax
      read (5,*) icoordchange,ifull
C NEED TO READ IN ICOORDCHANGE if ONE, change DX and DY, otherwise leave as is
      if (icoordchange.eq.1) then
      rlonmin=X1
      rlonmax=X1+(M-1)*DX
      rlatmin=Y1
      rlatmax=Y1+(N-1)*DY
      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=(4*asin(1.)*6360.)/360.
      dxkm=asin(1.)*rlatmean/90.
      dxkm=6360.*cos(dxkm)
      dxkm=(4*asin(1.)*dxkm)/360.
      dx=dx*dxkm
      dy=dy*dykm
      endif

      if ((M.NE.IMAX).OR.(N.NE.JMAX)) stop 'incoherent files'
      z=0
      nl=0
 1    continue
      read(13,*,err=99,end=99) z
      nl=nl+1
      write(depth,88) 10000+nl
 88   format(I5)
      un=uname//"."//depth
      vn=vname//"."//depth
      open(file=un,unit=98,form="unformatted")
      open(file=vn,unit=99,form="unformatted")
C call the velocity generation hre
      call uvg(topo,U,V,UNN,VNN,M,N,z,dx,dy,valex)
      close(99)
      close(98)
      goto 1
 99   continue
C output info file: no, just copy the topoinfo file !
      stop
      end
      
      subroutine uvg(topo,U,V,UN,VN,M,N,z,dx,dy,valex)
      real*4 TOPO(M,N),U(M,N),V(M,N),UN(M,N),VN(M,N)
      real*8 c8(1),DMEAN
      real*4 valex
      
C Calculate mean depth
      DMEAN=0
      IMEAN=0
      DMIN=0
      imin=0
      do j=1,N
      do i=1,M
      if(topo(i,j).gt.0) then
      DMEAN=DMEAN+topo(i,j)
      imean=imean+1
                         else
      DMIN=DMIN-topo(i,j)
      imin=imin+1
      endif
      enddo
      enddo
C Take D to be a fraction of this mean depth
C   
      if(imean.gt.0) then
      D=DMEAN/imean/5.
      else
      D=DMIN/imin/5.
      endif

C Calculate centered gradients, reduced by distance to level
      do j=2,N-1
      do i=2,M-1
      xi=(z-topo(i,j))/D
      FACTEUR=0
      if(abs(xi).lt.5) then
      FACTEUR=exp(-xi*xi)
      endif
      
      if (ifull.eq.1) FACTEUR=1
      
      u(i,j)=(topo(i,j+1)-topo(i,j-1))*FACTEUR/DY
      v(i,j)=-(topo(i+1,j)-topo(i-1,j))*FACTEUR/DX
      enddo
      enddo
C fill boundaries
       do i=1,M
       U(i,1)=U(i,2)
       U(i,N)=U(i,N-1)
       V(i,1)=V(i,2)
       V(i,N)=V(i,N-1)
       enddo
       do j=1,N
       U(1,j)=U(2,j)
       U(M,j)=U(M-1,j)
       V(1,j)=V(2,j)
       V(M,j)=V(M-1,j)
       enddo
       
C now filter
                    NTIMES=sqrt(sqrt(M*N*1.)+1.)/2.+1.
C             NTIMES=0
             do nn=1,NTIMES
             
             
              do i=2,M-1
               do j=2,N-1
               UN(i,j)=(U(I+1,j)+U(I-1,j)+U(i,j+1)+U(i,j-1))/4.
               VN(i,j)=(V(I+1,j)+V(I-1,j)+V(i,j+1)+V(i,j-1))/4.
               enddo
              enddo
              do i=2,M-1
               do j=2,N-1
               U(i,j)=UN(i,j)
               V(i,j)=VN(i,j)
               enddo
              enddo
       do i=1,M
       U(i,1)=U(i,2)
       U(i,N)=U(i,N-1)
       V(i,1)=V(i,2)
       V(i,N)=V(i,N-1)
       enddo
       do j=1,N
       U(1,j)=U(2,j)
       U(M,j)=U(M-1,j)
       V(1,j)=V(2,j)
       V(M,j)=V(M-1,j)
       enddo
       
       
             enddo
       
       
C save in fort.98 and .99
       MN=M*N
       KM=1
       IPRE=4
       VALEX8=valex
       call UWRITC(98,c8,U,valex8,ipre,M,N,KM,MN)
       call UWRITC(99,c8,V,valex8,ipre,M,N,KM,MN)

      
      
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
      real*4 valex8
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
          c4(ioff)=valex8
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
       valexc=valex8
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
         
