      integer, parameter :: nm=5000000
      REAL(KIND=4) ::  topo(nm),u(nm),v(nm)
      REAL(KIND=4) ::  unn(nm),vnn(nm)
      REAL(KIND=8) ::  c8
      
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
      read (5,*) icoordchange
!C NEED TO READ IN ICOORDCHANGE if ONE, change DX and DY, otherwise leave as is
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
!C call the velocity generation hre
      call uvg(topo,U,V,UNN,VNN,M,N,z,dx,dy,valex)
      close(99)
      close(98)
      goto 1
 99   continue
!C output info file: no, just copy the topoinfo file !
      stop
      end
      
      subroutine uvg(topo,U,V,UN,VN,M,N,z,dx,dy,valex)
      REAL(KIND=4) ::  TOPO(M,N),U(M,N),V(M,N),UN(M,N),VN(M,N)
      REAL(KIND=8) ::  c8,DMEAN
      REAL(KIND=4) ::  valex
      
!C Calculate mean depth
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
!C Take D to be a fraction of this mean depth
!C
      if(imean.gt.0) then
      D=DMEAN/imean/5.
      else
      D=DMIN/imin/5.
      endif

!C Calculate centered gradients, reduced by distance to level
      do j=2,N-1
      do i=2,M-1
      xi=(z-topo(i,j))/D
      FACTEUR=0
      if(abs(xi).lt.5) then
      FACTEUR=exp(-xi*xi)
      endif
      
      
      u(i,j)=(topo(i,j+1)-topo(i,j-1))*FACTEUR/DY
      v(i,j)=-(topo(i+1,j)-topo(i-1,j))*FACTEUR/DX
      enddo
      enddo
!C fill boundaries
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
       
!C now filter
                    NTIMES=sqrt(sqrt(M*N*1.)+1.)/2.+1.
!C             NTIMES=0
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
       
       
!C save in fort.98 and .99
       MN=M*N
       KM=1
       IPRE=4
       VALEX8=valex
       call UWRITC(98,c8,U,valex8,ipre,M,N,KM,MN)
       call UWRITC(99,c8,V,valex8,ipre,M,N,KM,MN)

      
      
       return
       end
