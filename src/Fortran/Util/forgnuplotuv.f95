       parameter(nm=30000000)
       integer*4 inode(nm)
       real*4 u(nm),v(nm)
       real*8 c8
       
       call ureadc(30,c8,u,valex,ipr,imax,jmax,kmax,nb)
       call ureadc(31,c8,v,valex,ipr,imax,jmax,kmax,nb)
       call gognuv(u,v,imax,jmax,valex)
       stop
       end
       subroutine gognuv(u,v,imax,jmax,valex)
       real*4 u(imax,jmax)
       real*4 v(imax,jmax)
       
       read(32,*) x0
       read(32,*) y0
       read(32,*) dx
       read(32,*) dy
       read(32,*) im
       read(32,*) jm
       write(6,*) 'valex',valex,im,jm,dx,dy
       if (im.ne.imax) stop 'incoherent files'
       if (jm.ne.jmax) stop 'incoherent files'
       vscale=0
       iscale=0
       do i=1,im
       do j=1,jm
       if (v(i,j).ne.valex) then
       iscale=iscale+1
       vscale=vscale+u(i,j)*u(i,j)+v(i,j)*v(i,j)
       endif
       enddo
       enddo
       vscale=sqrt(vscale/iscale)
       scale=sqrt(dx*dy)/vscale*2
       do i=1,im
       do j=1,jm
       if (v(i,j).ne.valex) then
       
       x1=x0+(i-1)*dx
       y1=y0+(j-1)*dy
       x2=x1+u(i,j)*scale
       y2=y1+v(i,j)*scale
       write(55,*) x1,y1,u(i,j)*scale,v(i,j)*scale
!C       write(55,*) x2,y2
!C       write(55,*)
       endif
       enddo
       enddo
       stop
       end
       
      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
!c23456                ======
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Reads the field C(I,J,K) from fortran unit iu
!c returns the field in the array c4 if the returned iprecr=4
!c returns the field in the array c8 if the returned iprecr=8
!c returns the values if imaxr,jmaxr,kmaxr found in the file
!c
!c JMB 6/3/91
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c23456
       PARAMETER(KBLANC=10)
       real*4 c4(*)
       real*8 c8(*)
!c in the calling routin you can specify the following equivalence to
!c save memory space:
!c      equivalence(c,c4)
!c      equivalence(c,c8)
!c
!c skip KBLANC lines
       do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1     continue
!c
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!c
!c pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc
!c
!c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!c
!c if pathological case, read only four values C0 and DCI,DCJ,DCK
!c and return
!c them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!c
!c
!c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
!c
!c double precision
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
!c
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

         return
         end
         

       
