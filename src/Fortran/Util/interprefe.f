C Template for calculating relative length scale based on data coverage
C
             PARAMETER(IW=15000000)
             REAL*4 C(IW)
             REAL*8 C8
             REAL*4 RRR
             
C Read data (just coordinates, not value)
      READ(21,*) x1
      READ(21,*) y1
      READ(21,*) dx
      READ(21,*) dy
      READ(21,*) NX
      READ(21,*) NY
      
               x0f=x1-dx
               dxxf=dx
               dyxf=0
               y0f=y1-dy
               dxyf=0
               dyyf=dy
               imaxf=NX
               jmaxf=NY
      
      call ureadc(20,c8,C,valex,IPR,NXX,NYY,NZZ,NWW)
      if(NX.NE.NXX.OR.NY.NE.NYY) then
      write(6,*) '???? Incoherent files???',NX,NY,NXX,NYY
      stop
      endif
        call ufill(C,VALEX,NX,NY,1)
      
             NDATA=0
 5           continue
             read(44,*,END=99,ERR=99) XX,YY
             NDATA=NDATA+1
               
               call bilininl(C,x0f,dxxf,dyxf,y0f,dxyf,dyyf
     &                      ,imaxf,jmaxf,
     &                     RRR,XX,YY)
             
             write(81,*) XX,YY,RRR
             goto 5
            

 99          continue
             write(6,*) ' Data read:',NDATA
             
                   
             NDATA=0
 55           continue
             read(45,*,END=999,ERR=999) XX,YY
             NDATA=NDATA+1
               
               call bilininl(C,x0f,dxxf,dyxf,y0f,dxyf,dyyf
     &                      ,imaxf,jmaxf,
     &                     RRR,XX,YY)
             
             write(82,*) XX,YY,RRR
             goto 55
            

 999          continue
             write(6,*) ' Valatxycoord read:',NDATA

             
      stop
      end

      subroutine UFILL(c,valexc,imax,jmax,kmax)
c                =====
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Fills in the standart field C by interpolating the field into the
c points where there was a exclusion value valexc.
c The interpolation is continued until the complete field contains
c regular values
c
c JMB 3/4/91
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       real*4 C(imax,jmax,kmax),valexc
       parameter(iwork=1000000)
       real*4 work(iwork),work2(iwork)
       integer*2 ic(iwork)
       integer*2 ic2(iwork)
       if( (imax+2)*(jmax+2)*(kmax+2).gt.iwork) then
         write(6,*) ' Sorry, UFILL needs more working space'
         return
       endif
c
       call JM0000(c,valexc,imax,jmax,kmax,work,work2,ic,ic2)
       return
       end
c
       subroutine JM0000(c,valexc,imax,jmax,kmax,work,work2,ic,ic2)
c      parameter(A1=5,A2=2,A3=4)
       parameter(A1=5,A2=0,A3=0)
       real*4 C(imax,jmax,kmax),valexc
       real*4 work(0:imax+1,0:jmax+1,0:kmax+1)
       real*4 work2(0:imax+1,0:jmax+1,0:kmax+1)
       integer*2 ic(0:imax+1,0:jmax+1,0:kmax+1)
       integer*2 ic2(0:imax+1,0:jmax+1,0:kmax+1)
c fill the borders...
       write(6,*) ' Filling in',valexc,imax,jmax,kmax
       do 11 j=0,jmax+1
        do 11 i=0,imax+1
        work(i,j,0)=valexc
        ic(i,j,0)=0
        work(i,j,kmax+1)=valexc
        ic(i,j,kmax+1)=0
 11     continue
       do 12 k=0,kmax+1
        do 12 i=0,imax+1
        work(i,0,k)=valexc
        ic(i,0,k)=0
        work(i,jmax+1,k)=valexc
        ic(i,jmax+1,k)=0
 12     continue
       do 13 k=0,kmax+1
        do 13 j=0,jmax+1
        work(0,j,k)=valexc
        ic(0,j,k)=0
        work(imax+1,j,k)=valexc
        ic(imax+1,j,k)=0
 13     continue
c
c copy interior field
        do 100 k=1,kmax
         do 100 j=1,jmax
          do 100 i=1,imax
          work(i,j,k)=c(i,j,k)
          ic(i,j,k)=1
          if(work(i,j,k).eq.valexc) ic(i,j,k)=0
 100    continue
c
 1000  continue 
       icount=0
       do 500 k=1,kmax
        do 500 j=1,jmax
         do 500 i=1,imax
         work2(i,j,k)=work(i,j,k)
         ic2(i,j,k)=ic(i,j,k)
         if(ic(i,j,k).eq.0 ) then
         work2(i,j,k)=valexc
          icount=icount+1
          isom1=
c    &      ic(i,j,k+1)+ic(i,j,k-1)
     &      0. 
     &      +ic(i+1,j,k)+ic(i-1,j,k)
     &      +ic(i,j+1,k)+ic(i,j-1,k)
          isom2=
     &      ic(i+1,j+1,k+1)+ic(i+1,j+1,k-1)
     &      +ic(i+1,j-1,k+1)+ic(i+1,j-1,k-1)
     &      +ic(i-1,j+1,k+1)+ic(i-1,j+1,k-1)
     &      +ic(i-1,j-1,k+1)+ic(i-1,j-1,k-1)
          isom3=
     &       ic(i,j+1,k+1)+ic(i,j+1,k-1)
     &     + ic(i,j-1,k+1)+ic(i,j-1,k-1)
     &     + ic(i+1,j,k+1)+ic(i+1,j,k-1)
     &     + ic(i-1,j,k+1)+ic(i-1,j,k-1)
     &     + ic(i+1,j+1,k)+ic(i+1,j-1,k)
     &     + ic(i-1,j+1,k)+ic(i-1,j-1,k)
           isom=isom1*a1+isom2*a2+isom3*a3
           if(isom.ne.0) then
c interpolate
           rsom1= 
c    &      ic(i,j,k+1)*work(i,j,k+1)
c    &     +ic(i,j,k-1)*work(i,j,k-1)
     &      0.
     &     +ic(i+1,j,k)*work(i+1,j,k)
     &     +ic(i-1,j,k)*work(i-1,j,k)
     &     +ic(i,j+1,k)*work(i,j+1,k)
     &     +ic(i,j-1,k)*work(i,j-1,k)
          rsom2=
     &      ic(i+1,j+1,k+1)*work(i+1,j+1,k+1)
     &     +ic(i+1,j+1,k-1)*work(i+1,j+1,k-1)
     &     +ic(i+1,j-1,k+1)*work(i+1,j-1,k+1)
     &     +ic(i+1,j-1,k-1)*work(i+1,j-1,k-1)
     &     +ic(i-1,j+1,k+1)*work(i-1,j+1,k+1)
     &     +ic(i-1,j+1,k-1)*work(i-1,j+1,k-1)
     &     +ic(i-1,j-1,k+1)*work(i-1,j-1,k+1)
     &     +ic(i-1,j-1,k-1)*work(i-1,j-1,k-1)
          rsom3=
     &      ic(i,j+1,k+1)*work(i,j+1,k+1)
     &     +ic(i,j+1,k-1)*work(i,j+1,k-1)
     &     +ic(i,j-1,k+1)*work(i,j-1,k+1)
     &     +ic(i,j-1,k-1)*work(i,j-1,k-1)
     &     +ic(i+1,j,k+1)*work(i+1,j,k+1)
     &     +ic(i+1,j,k-1)*work(i+1,j,k-1)
     &     +ic(i-1,j,k+1)*work(i-1,j,k+1)
     &     +ic(i-1,j,k-1)*work(i-1,j,k-1)
     &     +ic(i+1,j+1,k)*work(i+1,j+1,k)
     &     +ic(i+1,j-1,k)*work(i+1,j-1,k)
     &     +ic(i-1,j+1,k)*work(i-1,j+1,k)
     &     +ic(i-1,j-1,k)*work(i-1,j-1,k)
          work2(i,j,k)=(a1*rsom1+a2*rsom2+a3*rsom3)/float(isom)
          ic2(i,j,k)=1
           endif
         endif
 500   continue
       do 501 k=1,kmax
        do 501 j=1,jmax
         do 501 i=1,imax
          work(i,j,k)=work2(i,j,k)
          ic(i,j,k)=ic2(i,j,k)
 501   continue
       if(icount.gt.0) goto 1000
c
c fini
       do 99 k=1,kmax
        do 99 j=1,jmax
         do 99 i=1,imax
         c(i,j,k)=work(i,j,k)
 99    continue

       return
       end
       
       Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,
     &    jmaxr,kmaxr,nbmotr)
c23456                ======
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Reads the field C(I,J,K) from fortran unit iu 
c returns the field in the array c4 if the returned iprecr=4
c returns the field in the array c8 if the returned iprecr=8
c returns the values if imaxr,jmaxr,kmaxr found in the file
c
c JMB 6/3/91 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c23456
       PARAMETER(KBLANC=10)
       real*4 c4(*)
       real*8 c8(*)
c in the calling routin you can specify the following equivalence to
c save memory space:
c      equivalence(c,c4)
c      equivalence(c,c8)
c
c skip KBLANC lines
       write(6,*) ' ureadc in'
       do 1 kb=1,KBLANC
        read(iu,end=99,ERR=99)
 1     continue
c
        read(iu,end=99,err=99) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
c
c pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc
c
c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
c
c if pathological case, read only four values C0 and DCI,DCJ,DCK
c and return
c them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
c
c
c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
c
c double precision
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
c
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         imaxr=1
         jmaxr=1
         kmaxr=1
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1
         imaxr=0
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
         

        
        
               subroutine bilininl(UF,x0f,dxxf,dyxf,y0f,dxyf,dyyf
     &                      ,imaxf,jmaxf,
     &                     UT,xt,yt)
c                 ========
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Interpolates from a regular full field into xt,yt 
C regular full field
C Input geometry:
C   x= x0f + I dxxf + J dyxf
C   y= y0f + I dxyf + J dyyf
C 
C 
C 
C Interpolated field Tt computed from field Tf
C
C
C JMB 15/5/93
C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      integer imaxf,jmaxf,imaxt,jmaxt,i,j,ii,jj
      real*4 UF(imaxf,jmaxf)

      real*4 UT,VT
      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf
      real*4 ri,rj,det,xt,yt,xi,yi,xj,yj
      real*4 xbt,ybt
c     write(6,*) 'Interpolating',x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf
c     write(6,*) ' to',x0t,dxxt,dyxt,y0t,dxyt,dyyt,imaxt,jmaxt
c
      det=dxxf*dyyf-dxyf*dyxf
      XI=dyyf/det
      YI=-dyxf/det
      XJ=-dxyf/det
      YJ=dxxf/det
c
       Ri = XI * ( xt - x0f ) + YI * ( yt - y0f )
       Rj = XJ * ( xt - x0f ) + YJ * ( yt - y0f )
       ii = Ri
       jj = Rj
       Ri = Ri - ii
       Rj = Rj - jj
C
       if( ii.lt.1 ) then
        if(jj.lt.1 ) jj=1
        if(jj.ge.jmaxf) jj=jmaxf
        UT=UF(1,jj)

                     else
          if(ii.ge.imaxf) then
             if(jj.lt.1 ) jj=1
             if(jj.ge.jmaxf) jj=jmaxf
             UT=UF(imaxf,jj)

                          else
             if(jj.lt.1 ) then          
                 UT=UF(ii,1)

                          else
                 if(jj.ge.jmaxf) then          
                  UT=UF(ii,jmaxf)

                                  else
C Interpolate...
                  UT= rj* ( ri * UF(ii+1,jj+1) 
     &                           + ( 1 - ri )*UF(ii,jj+1) )
     &            + (1 -rj) * ( ri * UF(ii+1,jj) 
     &                           + ( 1 - ri )*UF(ii,jj) )


                      endif
             endif
          endif
        endif
 99     continue
        return
        end
