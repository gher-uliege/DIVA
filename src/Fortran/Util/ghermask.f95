       integer, parameter :: nm=50000000
       
       real*4 c(nm),ce(nm)
       real*8 c8
       write(6,*) 'Into reading'
       call ureadc(99,c8,c,valex,ipr,imax,jmax,kmax,nb)
       write(6,*) 'Have read a gridded field'
       call ureadc(98,c8,ce,valexe,ipr,imaxe,jmaxe,kmaxe,nb)
       if (imax.eq.imaxe.and.jmax.eq.jmaxe.and.kmax.eq.kmaxe) then
       write(6,*) 'Successfully read two files',imax,jmax
       call gognu(c,ce,imax,jmax,valex,valexe)
       else
       write(6,*) ' Problem: Found only one gridded file'
       endif
       
       
       call uwritc(97,c8,ce,valexe,ipr,imaxe,jmaxe,kmaxe,nb)
       stop
       end
       subroutine gognu(c,ce,imax,jmax,valex,valexe)
       real*4 c(imax,jmax)
       real*4 ce(imax,jmax)
       do i=1,imax
       do j=1,jmax
       if (c(i,j).eq.0) ce(i,j)=valexe
!c       write(6,*) '?',c(i,j),ce(i,j)
       enddo
       enddo
       return
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
       write(6,*) ' ureadc in'
       do 1 kb=1,KBLANC
        read(iu,end=99,ERR=99)
 1     continue
!c
        read(iu,end=99,err=99) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
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
!c! double precision
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
!c                ======
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c writes the field C(I,J,K)  into fortran unit iu
!c writes the field in the array c4 if iprecr=4
!c writes the field in the array c8 if iprecr=8
!c
!c The KBLANC blank lines are at the disposal of the user
!c JMB 6/3/92
!c
!c IF c(i,j,k)=NaN or infinity, it is replaced by VALEX!
!c
!c
!c RS 12/1/93
!c
!c If nbmots = -1  then write only 1 data record
!c     (only for non-degenerated data)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
      real*4 valex8
      real*4 valexc
!c in the calling routin you can specify the following equivalence to
!c save memory space:
!c      equivalence(c,c4)
!c      equivalence(c,c8)
!c
!c Putting  Valex where not numbers
       z=0.
       un=1.
       ich=0
       ioff=1
       if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

       IF (NBMOTS.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

       do k=1,kmaxc
        do j=1,jmaxc
         do i=1,imaxc
!c         if( c4(ioff).eq.(z/z) ) goto 1010
!c         if( c4(ioff).eq.(un/z) ) goto 1010
!c         if( c4(ioff).eq.(-z/z) ) goto 1010
!c         if( c4(ioff).eq.(-un/z) ) goto 1010
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
!c
!c skip KBLANC lines
!C        write(6,*) iu,imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
       do 1 kb=1,KBLANC
        write(iu,ERR=99)
 1     continue
!c
        write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
!c
!c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
!c
!c if pathological case, write only four values C0 and DCI,DCJ,DCK found
!c as the two four elements of the array so that C(I,J,K) =
!c C0 + I * DCI + J * DCJ + K * DCK
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
!c
!c
!c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) ((c4(ide+kc)),kc=1,ir)
                       else
!c
!c double precision
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
!c
         return
 99      continue
         write(*,*) 'Data error in UWRITC, not a conform file'
        write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
        write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
         return
         end
         

