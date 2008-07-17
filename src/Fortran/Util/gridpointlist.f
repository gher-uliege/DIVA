       parameter(nm=50000000)
       
       real*4 c(nm)
       real*8 c8
       write(6,*) 'Into reading'
       call ureadc(20,c8,c,valex,ipr,imax,jmax,kmax,nb)
       write(6,*) 'Have read a gridded field'
       if(kmax.gt.1) then
       write(6,*) '3D files; using k=1'
       endif
       call gognu(c,imax,jmax,valex)
       stop
       end
       subroutine gognu(c,imax,jmax,valex)
       real*4 c(imax,jmax)
       
       
       read(21,*) x0
       read(21,*) y0
       read(21,*) dx
       read(21,*) dy
       read(21,*) im
       read(21,*) jm
       write(6,*) 'valex',valex,im,jm,dx,dy
       if (im.ne.imax) stop 'incoherent files'
       if (jm.ne.jmax) stop 'incoherent files'
       do j=1,jmax
        do i=1,imax
        if(c(i,j).ne.valex) then
        write(22,*) x0+(i-1)*dx,y0+(j-1)*dy,c(i,j),1
        endif
       enddo
       enddo
       stop
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
         

       
