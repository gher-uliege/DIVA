      subroutine repeltest2 (tcoog,kconn,kntc,ncamax)
C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(3),jkntc(3),xn(3),yn(3)

C MAKING THE KNTC 3D TABLE 
C (1) INITIALISE THE FIRST ELEMENTS OF KNTC
      ncamax=0
c      write(6,*) 'into repel2'
      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue
cmr      write(6,*)ncax,ncay,ncaz
cmr      write(6,*)tmix,tmax,tmiy,tmay
C (2) TAKE EACH TRIANGLE
      iel=0
 300  iel=iel+1

C (2.1) TAKE THE THREE NODES OF THIS TRIANGLE
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)

C (2.2) IN WHICH ELEMENT OF KNTC IS THIS TRIANGLE?
      do 400 i=1,3     
cmr      ikntc(i)=((xn(i)-tmix-mod(xn(i),tlcx))/tlcx)+1
cmr      jkntc(i)=((yn(i)-tmiy-mod(yn(i),tlcy))/tlcy)+1
c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1
      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=max(1,min(jkntc(i),ncay))
 400  continue     
 
C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.3) then
         goto 500 
      endif
cmr      write(6,*) imii,imai,imij,imaj,iel
      if (imii.lt.0.or.
     &    imai.lt.0.or.
     &    imij.lt.0.or.
     &    imaj.lt.0.or.
     &    imii.gt.ncax.or.
     &    imai.gt.ncax.or.
     &    imij.gt.ncay.or.
     &    imaj.gt.ncay) then
        write(6,*) 'wrong index', imii,imai,imij,imaj
        write(6,*)  imii,imai,imij,imaj
cmr        stop
      endif
C (4)  PUT THE NUMBER OF THE TRIANGLE IN THE FIRST FREE ELEMENTS CORRESPONDING 
C      TO THE BIGGEST RECTANGLE THAT CONTAINS THE TRIANGLE.  

      do 800 i=imii,imai
         do 700 j=imij,imaj
         kntc(i,j,1)=kntc(i,j,1)+1
 700     continue
 800  continue

      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
      ncamax=1
      do i=1,ncax
      do j=1,ncay
        if(kntc(i,j,1).gt.ncamax) ncamax=kntc(i,j,1)
      enddo
      enddo
C============================================================================
      return
      end 

      subroutine repeltest3 (tcoog,kconn,kntc,ncamax)
C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(4),jkntc(4),xn(4),yn(4)
      ncamax=0
      write(6,*) 'into repel3'
C MAKING THE KNTC 3D TABLE 
C (1) INITIALISE THE FIRST ELEMENTS OF KNTC

      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue

C (2) TAKE EACH ELEMENT
      iel=0
 300  iel=iel+1

C (2.1) TAKE THE FOUR NODES OF THIS ELEMENT
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)
      xn(4)=tcoog(kconn(iel,7),1)
      yn(4)=tcoog(kconn(iel,7),2)

C (2.2) IN WHICH ELEMENT OF KNTC IS IT ?
      do 400 i=1,4
c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1

      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=min(1,min(jkntc(i),ncay))
 400  continue     
 
C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.4) then
         goto 500 
      endif

C  (4) PUT THE NUMBER OF THE ELEMENT IN THE FIRST FREE PLACE CORRESPONDING 
C      TO THE BIGGEST RECTANGLE THAT CONTAINS IT  

      do 800 i=imii,imai
         do 700 j=imij,imaj
               kntc(i,j,1)=kntc(i,j,1)+1
 700     continue
 800  continue
      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
      ncamax=1
      do i=1,ncax
      do j=1,ncay
        if(kntc(i,j,1).gt.ncamax) ncamax=kntc(i,j,1)
      enddo
      enddo
C============================================================================
      return
      end 

