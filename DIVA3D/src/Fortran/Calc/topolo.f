C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  TOPOLO (MODULE)
C     -  RDTOPO (read the topology of the finite element mesh: unit 11)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             TOPOL0 MODULE                            C
C       Description of the topology of the finite element grid         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine topolo(ipr)
      include'divapre.h'
      include'divainc.h'
C
C  INPUT OF GENERAL DATA
C
      read(10,*) nnt1
      if(ipr.gt.0) write(6,*) ' Total number of vertex nodes :',nnt1
      read(10,*) nnint
      if(ipr.gt.0) write(6,*) ' Total number of interfaces   :',nnint
      nnt=nnt1+nnint
      if(ipr.gt.0) write(6,*) ' Total number of nodes        :',nnt
      read(10,*) nelt
      if(ipr.gt.0) write(6,*) ' Total number of elements     :',nelt
C
C ALLOCATION OF STORAGE TABLES:
C  ==> KLINK(I)    : OPTIMAL POSITION OF NODE (I) IN THE SEQUENCE OF DOF
C  ==> KSORT(IPOSI): NODE ID IN POSITION (I) IN THE SEQUENCE OF NODES
C  ==> TCOOG(I,*)  : TABLE OF ABSLUTE COORDINATES OF NODE (I)
C
      call allody(nnt,0,'klink',lklink,ipr)
      call allody(nnt,0,'ksort',lksort,ipr)
      call allody(2*nnt1,1,'tcoog',ltcoog,ipr)
C
C ALLOCATION OF STORAGE TABLES:
C  ==> KSKYH (I)   :CUMULATED HEIGHT OF STIFFNESS MATRIX COLUMN
C  ==> KCONN (I,*) :CONNECTIVITY TABLE BETWEEN ELEMENTS AND NODES
C  ==> KLOCE (I)   :LOCALIZATION OF ONE ELEMENT IN THE STRUCTURE
C
C FINITE ELEMENT ITYP=2 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
C
      if(ityp.eq.2) then
         nnel=6
         nddle=12
         nddlt=3*nnt1+nnint
      endif
C
C FINITE ELEMENT ITYP=3 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
C
      if(ityp.eq.3) then
         nnel=8
         nddle=16
         nddlt=3*nnt1+nnint
C SPACE ALLOCATION FOR CENTER OF ELEMENT (CALCULATED ONCE)
         call allody(2*nelt,1,'tcele',ltcele,ipr)
      endif
C
C WHATEVER THE TYPE OF ELEMENT ...
C
      if(ipr.gt.0) write(6,*) ' Total number of deg. of frd. :',nddlt
      call allody(nddlt+1,0,'kskyh',lkskyh,ipr)
      call allody(nnel*nelt,0,'kconn',lkconn,ipr)
      if (ltcoog.le.0) then
      write(6,*) 'ltcoog',ltcoog
      goto 321
      endif
      if (ltcele.le.0) then
      if(ityp.eq.3) then
      write(6,*) 'ltcele??',ltcele
      endif
      bidon=0
      
      call rdtopo(s(ltcoog),l(lkconn),l(lklink),l(lksort),
     &            bidon2,ipr)
      
                       else
      
      call rdtopo(s(ltcoog),l(lkconn),l(lklink),l(lksort),
     &            s(ltcele),ipr)
      endif
 321  continue
      if (icoordchange.ne.0) call topollxy(s(ltcoog),ipr)
      call allody(nddle,0,'kloce',lkloce,ipr)
      call calsky(l(lkskyh),l(lkconn),l(lklink),l(lkloce),ipr)

      return
      end

      subroutine rdtopo(tcoog,kconn,klink,ksort,tcele,ipr)
C
C  I/O OF TOPOLOGIC DATA SET
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),klink(nnt),ksort(nnt),
     &          tcele(nelt,2)
C
C  INPUT OF ID and COORDINATES of VERTEX NODES
C

      do 10 i=1,nnt1  
#ifdef DIVABINARYFILESMESH
       read(11) ksort(i),tcoog(i,1),tcoog(i,2)
#else
       read(11,*) ksort(i),tcoog(i,1),tcoog(i,2)
#endif

 10   continue
C
C  INPUT OF ID of INTERFACE NODES
C
      do 20 i=1+nnt1,nnt
#ifdef DIVABINARYFILESMESH
       read(11) ksort(i)
#else
       read(11,*) ksort(i)
#endif

 20   continue
C
C  INPUT OF CONNECTIVITY TABLE
C
      do 30 i=1,nelt
#ifdef DIVABINARYFILESMESH
       read(11) (kconn(i,j),j=1,nnel)
#else
       read(11,*) (kconn(i,j),j=1,nnel)
#endif
         
 30   continue
C
C  RE-DEFINE KCONN IF ELEMENTS ARE NOT LEFT-AREA ORIENTED
C              NECESSARY WHEN ITYP = 2 OR 3
C
      if(ityp.eq.2) then
         zero=0.
         do 35 iel=1,nelt
            x1=tcoog(kconn(iel,1),1)
            x2=tcoog(kconn(iel,3),1)
            x3=tcoog(kconn(iel,5),1)
            y1=tcoog(kconn(iel,1),2)
            y2=tcoog(kconn(iel,3),2)
            y3=tcoog(kconn(iel,5),2)
            dotvz=(x2-x1)*(y3-y2)-(x3-x2)*(y2-y1)
            if(dotvz.lt.zero) then
               intf1=kconn(iel,6)
               intf3=kconn(iel,2)
               iver2=kconn(iel,5)
               iver3=kconn(iel,3)
               kconn(iel,2)=intf1
               kconn(iel,6)=intf3
               kconn(iel,3)=iver2
               kconn(iel,5)=iver3
               if (ipr.ge.3) then
                  write(6,*) ' !!! ELEMENT ',IEL,' WAS LEFT-ORIENTED !'
               endif
            endif
 35      continue
      endif
      if(ityp.eq.3) then
         zero=0.
         do 36 iel=1,nelt
            x1=tcoog(kconn(iel,1),1)
            x2=tcoog(kconn(iel,3),1)
            x3=tcoog(kconn(iel,5),1)
            x4=tcoog(kconn(iel,7),1)
            y1=tcoog(kconn(iel,1),2)
            y2=tcoog(kconn(iel,3),2)
            y3=tcoog(kconn(iel,5),2)
            y4=tcoog(kconn(iel,7),2)
            call intsec(x1,x2,x3,x4,y1,y2,y3,y4,x0,y0)
            tcele(iel,1)=x0
            tcele(iel,2)=y0
            dotvz=(x2-x1)*(y3-y2)-(x3-x2)*(y2-y1)
            if(dotvz.lt.zero) then
               intf1=kconn(iel,8)
               intf2=kconn(iel,6)
               intf3=kconn(iel,4)
               intf4=kconn(iel,2)
               iver2=kconn(iel,7)
               iver4=kconn(iel,3)
               kconn(iel,2)=intf1
               kconn(iel,4)=intf2
               kconn(iel,6)=intf3
               kconn(iel,8)=intf4
               kconn(iel,3)=iver2
               kconn(iel,7)=iver4
               if (ipr.ge.3) then
                  write(6,*) ' !!! ELEMENT ',IEL,' WAS LEFT-ORIENTED !'
               endif
            endif
 36      continue
      endif
C
C  CONSTRUCTION OF KLINK VECTOR; FOR TYPE 2 OR 3: FDV ELEMENT
C
      if(ityp.eq.2.or.ityp.eq.3) then
         ilink=1
         do 40 iposi=1,nnt
            inod=ksort(iposi)
            klink(inod)=ilink
            ilink=ilink+1
            if(inod.le.nnt1) ilink=ilink+2
 40      continue
      endif
C
C OUTPUT OF TOPOLOGICAL DATA
C
      if(ipr.ge.3) then
         write(6,*)' List of vertex nodes id, X and Y positions'
         write(6,*)' ------------------------------------------'
         do 100 i=1,nnt1
           write(6,*) i,tcoog(i,1),tcoog(i,2)
 100     continue
      endif
      if(ipr.ge.3) then
         if(ityp.eq.2) then
            write(6,*)' List of element id and connectivities '
            write(6,*)' ------------------------------------- '
            do 110 i=1,nelt
              write(6,910) i,(kconn(i,j),j=1,nnel)
 910          format(' Element :',i5,'  Nodes :',10(i5))
 110        continue
         endif
         if(ityp.eq.3) then
            write(6,*)' List of element id , center and connectivities'
            write(6,*)' ----------------------------------------------'
            do 115 i=1,nelt
              write(6,915) i,(tcele(i,j),j=1,2),(kconn(i,j),j=1,nnel)
 915          format(' Element:',i4,' Center:',2(f7.1),
     &               ' Nodes:',10(i4))
 115        continue
         endif
      endif
      if(ipr.ge.4) then
         write(6,*)' I   ,    KSORT(I)    ,     KLINK(I)      '
         write(6,*)' ---------------------------------------- '
         do 120 i=1,nnt
           write(6,*) i,ksort(i),klink(i)
 120     continue
      endif
      return
      end
