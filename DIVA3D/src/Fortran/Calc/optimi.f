C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  The subroutines in this file are used for optimisaton
C
C   SUBROUTINE LIST:
C     -  DIVESP : Subdivises the space for optimisation
C     -  SIZES2 : Computes the size of the space for elements of type 2
C     -  SIZES3 : Computes the size of the space for elements of type 3
C     -  REPEL2 : Distributes the elements of type 2 in kntc table
C     -  REPEL3 : Distributes the elements of type 2 in kntc table
C     -  FINDCA : Finds in which region is one point
C     -  LOCPT2OPTI : locates the (x,y) point in the structure (for ityp=2)
C     -  LOCPT3OPTI : locates the (x,y) point in the structure (for ityp=3)
C     -  SORTDTOPTI : sorts the data according to the sequence of elements
C     -  QS2I1R : Quick Sort algorithm for SORDTOPTI (from www.netlib.org)
C     -  CALPSOOPTI : computes pseudo data sets for error estimates
C     -  FCORROPTI : part of calpsoopti  
C     -  TABESS : Tabulates the Bessel function for the calculation of error
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      subroutine divesp(tcoog,kconn)
C============================================================================
      include'divapre.h'
#include "divainc.h"
      dimension kconn(nelt,nnel),tcoog(nnt1,2)

C COMPUTE THE SIZE OF THE SPACE
      if(ityp.eq.2) then
         call sizes2(s(ltcoog),l(lkconn))
      endif
      if(ityp.eq.3) then
         call sizes3(s(ltcoog),l(lkconn))
      endif

C COMPUTE CHARACTERISTIC SIZE OF ELEMENTS 
      carax=0
      caray=0
c      neltp=(nelt-mod(nelt,20))/20
      
      neltp=0
      jjstel=nint(nelt/1000.)+1
      iel=1
 300  iel=iel+jjstel
C JMB addded
      if(iel.gt.nelt) iel=nelt
C JMB
         carax=carax+(abs(tcoog(kconn(iel,1),1)-tcoog(kconn(iel,3),1))+
     &                abs(tcoog(kconn(iel,1),1)-tcoog(kconn(iel,5),1))+         
     &                abs(tcoog(kconn(iel,3),1)-tcoog(kconn(iel,5),1)))
     &/3
         caray=caray+(abs(tcoog(kconn(iel,1),2)-tcoog(kconn(iel,3),2))+
     &                abs(tcoog(kconn(iel,1),2)-tcoog(kconn(iel,5),2))+         
     &                abs(tcoog(kconn(iel,3),2)-tcoog(kconn(iel,5),2)))
     &/3
      neltp=neltp+1
      if (iel.lt.nelt-jjstel) then
          goto 300
      endif
      carax=carax/neltp 
      caray=caray/neltp
c      write(6,*) 'carax',carax,caray
C COMPUTE THE NUMBERS OF ELEMENTS IN THE KNTC TABLE
      tlex=tmax-tmix      
      tley=tmay-tmiy      
 
      ncax=int(tlex/carax)
      ncay=int(tley/caray)
Cjmb added 1
      ncax=int(tlex/carax)+1
      ncay=int(tley/caray)+1
      ncat=ncax*ncay
      tlcx=tlex/ncax
      tlcy=tley/ncay
cmr      write(6,*) ncax,ncay,ncat
cmr      write(6,*) tlcx,tlcy
cmr      write(6,*) neltp
C============================================================================
      return
      end


      subroutine sizes2 (tcoog,kconn)
C============================================================================
C COMPUTE THE SIZE OF THE SPACE FOR ELEMENTS OF TYPE 2
      include'divapre.h'
#include "divainc.h"
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      tmax=tcoog(kconn(1,1),1)
      tmix=tcoog(kconn(1,1),1)
      tmay=tcoog(kconn(1,1),2)
      tmiy=tcoog(kconn(1,1),2)
      iel=0
      j=-1

 100  iel=iel+1
 200     j=j+2   
            if (tcoog(kconn(iel,j),1).gt.tmax) then 
               tmax=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),1).lt.tmix) then 
               tmix=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),2).gt.tmay) then 
               tmay=tcoog(kconn(iel,j),2)
            endif
            if (tcoog(kconn(iel,j),2).lt.tmiy) then 
               tmiy=tcoog(kconn(iel,j),2)
            endif
         if (j.lt.5) then
            goto 200 
         endif
      if (iel.lt.nelt) then
         j=-1
         goto 100
      endif
C============================================================================
      return
      end 


      subroutine sizes3 (tcoog,kconn)
C============================================================================
      include'divapre.h'
#include "divainc.h"
      dimension kconn(nelt,nnel),tcoog(nnt1,2)

C COMPUTE THE SIZE OF THE SPACE FOR ELEMENTS OF TYPE 3
      tmax=tcoog(kconn(1,1),1)
      tmix=tcoog(kconn(1,1),1)
      tmay=tcoog(kconn(1,1),2)
      tmiy=tcoog(kconn(1,1),2)
      iel=0
      j=-1

 100  iel=iel+1
 200     j=j+2   
            if (tcoog(kconn(iel,j),1).gt.tmax) then 
               tmax=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),1).lt.tmix) then 
               tmix=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),2).gt.tmay) then
               tmay=tcoog(kconn(iel,j),2)
            endif
            if (tcoog(kconn(iel,j),2).lt.tmiy) then 
               tmiy=tcoog(kconn(iel,j),2)
            endif
         if (j.lt.7) then
            goto 200 
         endif
      if (iel.lt.nelt) then
         j=-1
         goto 100
      endif
C============================================================================
      return
      end 


      subroutine repel2 (tcoog,kconn,kntc,ncamax)
C============================================================================
      include'divapre.h'
#include "divainc.h"
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
            kkntc=0
  600       kkntc=kkntc+1
cmr               if(kkntc.gt.ncaz) then ! otherwise crashes because kntc(i,j,kkntc+1)=0 fails 
               if(kkntc.ge.ncamax) ncamax=kkntc
               if(kkntc.ge.ncaz) then
               write (6,*) 'NCAZ OF TABLE KNTC IS TO SMALL'
     &                     ,kkntc,ncaz,iel,i,j,ncamax
               stop
               endif
            if (kntc(i,j,kkntc).eq.0) then
               kntc(i,j,kkntc)=iel
               kntc(i,j,kkntc+1)=0
               goto 700
            endif
            goto 600
 700     continue
 800  continue

      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
C============================================================================
      return
      end


      subroutine repel3 (tcoog,kconn,kntc,ncamax)
C============================================================================
      include'divapre.h'
#include "divainc.h"
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
            kkntc=0
  600       kkntc=kkntc+1
cmr               if(kkntc.gt.ncaz) then ! otherwise crashes because kntc(i,j,kkntc+1)=0 fails
               if(kkntc.ge.ncamax) ncamax=kkntc
               if(kkntc.ge.ncaz) then
               write (6,*) 'NCAZ OF TABLE KNTC IS TO SMALL'
     &                     ,kkntc,ncaz,iel,i,j,nelt
               stop
               endif
            if (kntc(i,j,kkntc).eq.0) then
               kntc(i,j,kkntc)=iel
               kntc(i,j,kkntc+1)=0
               goto 700
            endif
            goto 600
 700     continue
 800  continue
      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
C============================================================================
      return
      end





      subroutine locpt2opti(x,y,tcoog,kconn,ielem,isub,kntc,ipr)
C============================================================================
C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 2)

      include'divapre.h'
#include "divainc.h"
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      ielem=-1
      isub=-1


C  IN WHICH AREA IS THE (X,Y) POINT ?
c      write(6,*) 'strange',x,y
      call findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)
c            write(6,*) 'stranger',x,y
c      write(6,*) 'bound ',ikn,jkn
C  WHICH ELEMENTS ARE CAPABLE OF CONTAINING DATA ID ?
      kkn=0

  100 kkn=kkn+1
      if (kntc(ikn,jkn,kkn).le.0) then
          goto 20
      endif
      iel=kntc(ikn,jkn,kkn)
c      write(6,*) kkn,iel

C  DOES DATA ID BELONG TO ELEMENT IEL ?
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
c      if(iel.eq.500) then
c      write(6,*) 'loc2opt???',x1,y1,x2,y2,x3,y3
c      endif
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) then
         goto 100
      endif
c      write(6,*) 'sub-el?'
C  WHICH SUB-ELEMENT IN ELEMENT IEL ?
      ielem=iel
      isub=itria
      itria=1
c      if(itria.eq.0) then
c         write(6,*) '%%% ERROR  LOCPT2 IN THE LOCALIZATION %%%'
c         stop
c      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',
     &        i4,'(',i1,')')
 20   continue
c      if(ielem.eq.-1) then
c      write(6,*) 'Locopti problem',x,y,ikn,jkn,tmix,tmiy,tlcx,tlcy
c      write(6,*) 'kntc table'
c      kkn=0
c 9987 continue
c
c      write(6,*),kkn+1,kntc(ikn,jkn,kkn+1)
c      kkn=kkn+1
c      if (kntc(ikn,jkn,kkn).ne.0) goto 9987
c      call locpt2(x,y,tcoog,kconn,ielem,isub,ipr)
c      write(6,*) 'Locpt2 found',ielem,isub
c      write(6,*) 'of corners'
c      xx1=tcoog(kconn(ielem,1),1)
c      yy1=tcoog(kconn(ielem,1),2)
c      xx2=tcoog(kconn(ielem,3),1)
c      yy2=tcoog(kconn(ielem,3),2)
c      xx3=tcoog(kconn(ielem,5),1)
c      yy3=tcoog(kconn(ielem,5),2)
c      write(6,*) xx1,yy1,xx2,yy2,xx3,yy3
c      endif
C============================================================================
      return
      end


      subroutine locpt3opti(x,y,tcoog,kconn,tcele,ielem,isub,
C============================================================================
     &                      kntc,ipr)
C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 3)
      include'divapre.h'
#include "divainc.h"
      dimension kconn(nelt,nnel),tcoog(nnt1,2),tcele(nelt,2)
      dimension kntc(ncax,ncay,*)
      ielem=-1
      isub=-1

C  IN WHICH AREA IS THE (X,Y) POINT ?
      call findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)

C  WHICH ELEMENTS ARE CAPABLE OF CONTAINING DATA ID ?
      kkn=0
    
  100 kkn=kkn+1
      if (kntc(ikn,jkn,kkn).le.0) then
          goto 20
      endif
      iel=kntc(ikn,jkn,kkn)

C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 1 or 2)?
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      x4=tcoog(kconn(iel,7),1)
      y4=tcoog(kconn(iel,7),2)
      x0=tcele(iel,1)
      y0=tcele(iel,2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 50

C  WHICH SUB-ELEMENT IN ELEMENT IEL (1 or 2 ) ?
      ielem=iel
      call istria(x,y,x1,y1,x2,y2,x0,y0,itria)
      if(itria.ge.1) then
         isub=1
                     else
         isub=2
      endif
      goto 22
 50   continue

C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 3 or 4)?
      call istria(x,y,x1,y1,x3,y3,x4,y4,itria)
      if(itria.eq.0) goto 100

C  WHICH SUB-ELEMENT IN ELEMENT IEL (3 or 4 ) ?
      ielem=iel
      call istria(x,y,x1,y1,x0,y0,x4,y4,itria)
      if(itria.ge.1) then
         isub=4
      else
         isub=3
      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',
     &        i4,'(',i1,')')
 20   continue
C============================================================================
      return
      end

CJMB2012 added parameters to be able to use it for data AND sources
      subroutine
     & sortdtopti(kindt,kdata,kelos,kelos1,ipr,ndatas,nonlocs)
C============================================================================
      include'divapre.h'
#include "divainc.h"
      dimension kdata(ndatas),kelos(ndatas,2),kindt(nelt)
      dimension kelos1(ndatas)

C INITIALISE THE ARRAYS
      imaxd=ndatas-nonlocs
C      ndatl=imaxd
      do 10 iel=1,nelt
         kindt(iel)=0
  10  continue
      do 20 idata=1,ndatas
         kelos1(idata)=kelos(idata,1)
	 kdata(idata)=idata
  20  continue
C       write(6,*) '?? QUICK',ndata
C CALL THE QUICK SORT ROUTINE
      call QS2I1R(kelos1,kdata,ndatas)
c       write(6,*) 'ended'
C REMOVE THE IGNORED DATAS
      do 30 idata=1,ndatas-nonlocs
         kdata(idata)=kdata(idata+nonlocs)
  30  continue

C       call jmkelostest(ndatas,kelos,kindt,nelt)
C      call jmkelostest(ndatas,nonlocs,kelos,kindt,nelt,kdata)
      call jmkelostest(ndatas,nonlocs,kelos1,kindt,nelt,kdata)



C COMPUTE THE KINDT ARRAY
c      do 40 idata=1,ndatas
c         iel=kelos(idata,1)
cc         if(iel.lt.0) then
cc            goto 40
cc         endif
c         if(iel.ge.0) then
c         do 41 ie=iel+1,nelt
c            kindt(ie)=kindt(ie)+1
c  41     continue
c         endif
c  40  continue

C storage of number of data located in the mesh
      if (imaxd.le.0) then
       write(6,*) ' Will create valex grid'
       write(43,*) imaxd
      endif
      if(ipr.ge.1) then
         write(6,910) imaxd
 910     format(/,t2,60('%'),/,' There are ',i9,
     &' data localized in the mesh (and resorted)'
     &   ,/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   ORDERED SEQUENCE OF DATA IN ELEMENTS '
         do 50 id=1,imaxd
            write(6,*) id,kdata(id)
 50      continue
         write(6,*)'   VECTOR KINDT   '
         do 60 iel=1,nelt
            write(6,*) iel,kindt(iel)
 60      continue
      endif
      return
C============================================================================
      end
      subroutine jmkelostest(ndatas,nonlocs,kelos,kindt,nelt,kdata)
CCCC NICE PLACE TO OPTIMIZE HERE
CCCC Normally no need to make a full double loop !
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCccc
      include'divapre.h'
      dimension kelos(*),kindt(*),kdata(*)
C KELOS(ID) : LOCALIZATON OF DATA ID IN THE ELEMENT MESH ;
C KELOS(ID) = IEL where data is located
C KINDT(IEL)  : INDEX OF THE LAST DATA BELONGING TO (IEL-1)
C
C real data: ndatas-nonlocs
      iii=1+nonlocs
      ielold=kelos(iii)
      do ii=1,ielold
      kindt(ii)=0
      enddo

c      write(6,*) 'starting',kelos(1),kelos(iii)
      do 40 idata=iii+1,ndatas
         iel=kelos(idata)
         if(iel.ne.ielold) then
C            write(6,*) 'Trying',iel,ielold,idata-nonlocs,iii
            kindt(ielold+1)=idata-nonlocs-1
            do kk=ielold+2,iel
            kindt(kk)=kindt(ielold+1)
            enddo


c            write(6,*) iel,ielold,kindt(ielold),kindt(ielold+1)
c            write(6,*) iel,idata,kindt(iel), kindt(iel+1)
            ielold=iel
c                           else
c           indexold=index
c           index=max(index,idata)
         endif
C        write(6,*) 'in element', iel, kdata(idata),idata,nelt
c         write(6,*) 'sorted', kelos((idata)),kdata(idata),idata
cc         if(iel.lt.0) then
cc            goto 40
cc         endif
c         if(iel.ge.0) then
c         do 41 ie=iel+1,nelt
c            kindt(ie)=kindt(ie)+1
c  41     continue
c         endif
   40  continue
c      write(6,*) 'Ending',ielold+1,nelt
      if ((ielold+1).le.nelt) then
      kindt(ielold+1)=ndatas-nonlocs
      do kk=ielold+2,nelt
       kindt(kk)=kindt(ielold+1)
      enddo
      endif
c      do iii=1,nelt
c      write(90,*) iii,kindt(iii)
c      enddo
c      write(6,*) '???',kindt(300),kindt(301),kindt(302),kindt(303)
c      write(6,*) '????',kelos(1),kelos(2),kelos(3)
      return
      end

      subroutine QS2I1R (IA,JA,N)
C=============================================================================
C *** DESCRIPTION (from www.netlib.org)
C     Written by Rondall E Jones
C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
C     algorithm. date 18 November 1976.
C
C     Further modified by David K. Kahaner
C     National Bureau of Standards
C     August, 1981
C
C     Even further modification made to bring the code up to the
C     Fortran 77 level and make it more readable and to carry
C     along one integer array and one real array during the sort by
C     Mark K. Seager
C     Lawrence Livermore National Laboratory
C     November, 1987
C     This routine was adapted from the ISORT routine.
C
C     ABSTRACT
C         This routine sorts an integer array IA and makes the same
C         interchanges in the integer array JA and the real array A.
C         The array IA may be sorted in increasing order or decreasing
C         order.  A slightly modified quicksort algorithm is used.
C
C     DESCRIPTION OF PARAMETERS
C        IA - Integer array of values to be sorted.
C        JA - Integer array to be carried along.
C         A - Real array to be carried along.
C         N - Number of values in integer array IA to be sorted.

C     .. Scalar Arguments ..
      INTEGER N
C     .. Array Arguments ..
      INTEGER IA(N), JA(N)
C     .. Local Scalars ..
      REAL R 
      INTEGER I, IIT, IJ, IT, J, JJT, JT, K, KK, L, M, NN
C     .. Local Arrays ..
      INTEGER IL(21), IU(21)

C --- FIRST EXECUTABLE STATEMENT  QS2I1R ---
      NN=N
      if (N.EQ.1) then
      write(6,*) 'No need to sort a single data point'
      return
      endif

C     Sort IA and carry JA and A along.
C     And now...Just a little black magic...
      M = 1
      I = 1
      J = NN
      R = .375E0
 210  IF( R.LE.0.5898437E0 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875E0
      ENDIF
 225  K = I

C     Select a central element of the array and save it in location
C     it, jt, at.
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)

C     If first element of array is greater than it, interchange with it.
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
      ENDIF
      L=J

C     If last element of array is less than it, swap with it.
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)

C     If first element of array is greater than it, swap with it.
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
         ENDIF
      ENDIF

C     Find an element in the second half of the array which is
C     smaller than it.
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240

C     Find an element in the first half of the array which is
C     greater than it.
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245

C     Interchange these elements.
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         GOTO 240
      ENDIF

C     Save upper and lower subscripts of the array yet to be sorted.
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260

C     Begin again on another portion of the unsorted array.
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      GO TO 265

 300  CONTINUE
      RETURN
C=============================================================================
      END


      subroutine calpsoopti(xob,yob,tdata,ipr,ikfull)
C=============================================================================
C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
#include "divainc.h"
      dimension tdata(ndata,4)

C JMB FOR FULL KERNEL CALCULATION
      if(ikfull.eq.1) then
      include'kernelbis.inc'
                      else
      if(ikfull.eq.-1) then
       do i=1,ndata
       tdata(i,3)=varbak
       enddo
       write(6,*) 'Poor man''s error uses 1 for correlation'
                       else
C JME
C  INPUT OF DATA SET DESCRIPTION
      do 10 i=1,ndata
         call fcorropti(xob,tdata(i,1),yob,tdata(i,2),corre)
         tdata(i,3)=corre
 10   continue
C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',
     &               xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
c=======================================================================
      endif
      endif
      return
      end


      subroutine fcorropti(x1,x2,y1,y2,corre)
C=============================================================================
      include'divapre.h'
#include "divainc.h"

      external euclidist
      external bessk1

      r=euclidist(x1,x2,y1,y2)
      eps=r/rl0

      if (eps.le.0.001) then
         corre=varbak
      else
         if(eps.lt.20.) then
            corre=tbess(nint((eps-mod(eps,0.0005D0))/0.0005D0))
	 else
	    corre=0
	 endif
      endif
C=============================================================================
      return                                                            
      end                                                               
                                                      

      subroutine tabess
C=============================================================================
      include'divapre.h'
#include "divainc.h"
C      dimension tbess(40000)
      external bessk1
      eps=0
      do 10 i=1,40000
      eps=eps+0.0005
         tbess(i)=varbak*eps*bessk1(eps)
 10   continue
C=============================================================================
      return                                                            
      end                                                               
