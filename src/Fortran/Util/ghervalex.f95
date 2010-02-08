!C Template for calculating relative length scale based on data coverage
!C
             integer, PARAMETER :: IW=15000000
             REAL*4 C(IW)
             REAL*8 C8,RRR
             read(5,*) nx,ny,valex
             do i=1,nx*ny
             c(i)=valex
             enddo
            call UWRITC(84,c8,C,valex,4,NX,NY,1,NX*NY)
!C Close
      stop
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
         
