       parameter(nm=50000000)
       
       real*4 c(nm),ce(nm)
       real*8 c8
       
       call ureadc(20,c8,c,valex,ipr,imax,jmax,kmax,nb)
       write(6,*) 'Have read a gridded field'
       call ureadc(19,c8,ce,valexe,ipr,imaxe,jmaxe,kmaxe,nb)
       if (imax.eq.imaxe.and.jmax.eq.jmaxe.and.kmax.eq.kmaxe) then
       write(6,*) 'Found a second gridded field'
       do i=1,imax*jmax*kmax
       if (ce(i).eq.valexe) ce(i)=valex
       enddo
       else
       do ii=1,imax*jmax*kmax
       ce(ii)=0
       enddo
       endif
       if(kmax.gt.1) then
       write(6,*) '3D files; using k=1'
       endif
       call gognu(c,ce,imax,jmax,valex)
       stop
       end
       subroutine gognu(c,ce,imax,jmax,valex)
       real*4 c(imax,jmax)
       real*4 ce(imax,jmax)
       
       read(21,*) x0
       read(21,*) y0
       read(21,*) dx
       read(21,*) dy
       read(21,*) im
       read(21,*) jm
       write(6,*) 'valex',valex,im,jm,dx,dy
       if (im.ne.imax) stop 'incoherent files'
       if (jm.ne.jmax) stop 'incoherent files'
       cmin=1E36
       cmax=-cmin
       do i=1,im
       do j=1,jm
       if(c(i,j).ne.valex) then
       cmin=min(cmin,c(i,j))
       cmax=max(cmax,c(i,j))
       endif
       enddo
       enddo
       
       
       do i=1,im-1
       do j=1,jm-1
       icc=0
       if (c(i,j).ne.valex) then
       icc=icc+1
       endif
       if (c(i+1,j).ne.valex) then
       icc=icc+1
       endif
       if (c(i,j+1).ne.valex) then
       icc=icc+1
       endif
       if (c(i+1,j+1).ne.valex) then
       icc=icc+1
       endif
       if(icc.ge.3) then
       
       
       x1=x0+(i-1)*dx
       x2=x1+dx
       x3=x2
       x4=x1
       y1=y0+(j-1)*dy
       y2=y1
       y3=y2+dy
       y4=y3
       vala1=c(i,j)
       vale1=ce(i,j)
       vala2=c(i+1,j)
       vale2=ce(i+1,j)
       vala3=c(i+1,j+1)
       vale3=ce(i+1,j+1)
       vala4=c(i,j+1)
       vale4=ce(i,j+1)
       
       if(icc.eq.4) then
       write(68,*) '#'
       write(68,*) x1,y1,vala1,vale1
       write(68,*) x2,y2,vala2,vale2
       write(68,*)
       write(68,*) x4,y4,vala4,vale4
       write(68,*) x3,y3,vala3,vale3
       write(68,*)
       write(68,*)
       endif
       
       if(icc.eq.3) then
       if(c(i,j).eq.valex) then
       write(68,*) '#'
       write(68,*) x2,y2,vala2,vale2
       write(68,*) x2,y2,vala2,vale2
       write(68,*)
       write(68,*) x4,y4,vala4,vale4
       write(68,*) x3,y3,vala3,vale3
       write(68,*)
       write(68,*)
       endif
       if(c(i+1,j).eq.valex) then
       write(68,*) '#'
       write(68,*) x1,y1,vala1,vale1
       write(68,*) x1,y1,vala1,vale1
       write(68,*)
       write(68,*) x4,y4,vala4,vale4
       write(68,*) x3,y3,vala3,vale3
       write(68,*)
       write(68,*)
       endif
       if(c(i+1,j+1).eq.valex) then
       write(68,*) '#'
       write(68,*) x1,y1,vala1,vale1
       write(68,*) x2,y2,vala2,vale2
       write(68,*)
       write(68,*) x4,y4,vala4,vale4
       write(68,*) x4,y4,vala4,vale4
       write(68,*)
       write(68,*)
       endif
       if(c(i,j+1).eq.valex) then
       write(68,*) '#'
       write(68,*) x1,y1,vala1,vale1
       write(68,*) x2,y2,vala2,vale2
       write(68,*)
       write(68,*) x3,y3,vala3,vale3
       write(68,*) x3,y3,vala3,vale3
       write(68,*)
       write(68,*)
       endif
       
       
       endif
       
       
       
       endif
       enddo
       enddo
       
       xmin=x0
       ymin=y0
       xmax=x0+(im-1)*dx
       ymax=y0+(jm-1)*dy
       write(41,*) 'set cbrange[',cmin,':',cmax,']'
       write(41,*) 'set xrange[',xmin,':',xmax,']'
       write(41,*) 'set yrange[',ymin,':',ymax,']'
       write(40,*) 'longref=',x0+(im/2-1)*dx
C box for .klm files
       write(47,*) '      <north>',ymax,'</north>'
       write(47,*) '      <south>',ymin,'</south>'
       write(47,*) '      <east>',xmax,'</east>'
       write(47,*) '      <west>',xmin,'</west>'
       stop
       end
       
      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
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
         imaxr=0
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1
         imaxr=0
         return
         end
         

       
