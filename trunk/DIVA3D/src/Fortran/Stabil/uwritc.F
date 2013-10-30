!
      Subroutine UWRITC(iu,c8,c4,valexc,iprec,imaxc,jmaxc,kmaxc,nbmots)
c                ======
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c writes the field C(I,J,K)  into fortran unit iu 
c writes the field in the array c4 if iprecr=4
c writes the field in the array c8 if iprecr=8
c
c The KBLANC blank lines are at the disposal of the user
c JMB 6/3/91 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER(KBLANC=10)
      include'divapre.h'
      real*4 c4(*)
      real*8 c8(*)

      integer*4 iprec,imaxc,jmaxc,kmaxc,nbmots

c in the calling routin you can specify the following equivalence to
c save memory space:
c      equivalence(c,c4)
c      equivalence(c,c8)
c
c skip KBLANC lines
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
          write(iu,ERR=99) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) (c4(ide+kc),kc=1,ir)
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
         return
         end


	 subroutine uwbimg4(iu,c4,nx,ny,nz,spval,dx,dy,xori,yori)
	 implicit none


	 integer nx,ny,nz,nt,ndim,icod
	 integer i,j,k,l,iu
	 real*4 c4(nx,ny,nz)
	 real*4 spval,dx,dy,xori,yori
	 character*80 record

         nt=1
	 ndim=1 
	 icod=1

	 write(iu) record
	 write(iu) record
	 write(iu) record
	 write(iu) record

	 xori=xori+dx
         yori=yori+dy

	 write(iu) nx,ny,nz,nt,ndim,icod
	 write(iu) xori,yori,dx,dy,spval
	 write(iu)(float(k),k=1,nz)

	 do l=1,nt
	   write(iu) float(l)
	   do k=1,nz
	      write(iu) ((c4(i,j,k),i=1,nx),j=1,ny)
           enddo
	 enddo
 
	 return
	 end
