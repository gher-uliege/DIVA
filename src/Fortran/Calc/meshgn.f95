!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  MESHGN (MODULE)
!C     -  COUNTN (count the number of nodes, interfaces .. to be created
!C     -  GENTPO (generate the topology of the finite element mesh;
!C                then, create the topological file on unit 11)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             MESHGN MODULE                            C
!C       Generates a square finite element mesh on a regular grid       C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine meshgn(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  INPUT OF GENERAL DATA
!C
      read(10,*) deltax
      read(10,*) deltay
      read(10,*) nex
      read(10,*) ney
      read(10,*) imatrx
      if(ipr.gt.0) write(6,*) ' X-step of the regular mesh :',deltax
      if(ipr.gt.0) write(6,*) ' Y-step of the regular mesh :',deltay
      if(ipr.gt.0) write(6,*) ' X-number of finite elements :',nex
      if(ipr.gt.0) write(6,*) ' Y-number of finite elements :',ney
      if(ipr.gt.0) write(6,*) ' Read/creat topology matrix ? :',imatrx
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KTPOL   : MATRIX FOR TOPOLOGICAL INFORMATION
!C
      ll=(2+nex)*(2+ney)
      call allody(ll,0,'ktpol',lktpol,ipr)
      call countn(l(lktpol))
      if(ipr.gt.0) write(6,*) ' Total number of vertex nodes :',nnt1
      if(ipr.gt.0) write(6,*) ' Total number of interfaces   :',nnint
      if(ipr.gt.0) write(6,*) ' Total number of nodes        :',nnt
      if(ipr.gt.0) write(6,*) ' Total number of elements     :',nelt
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KLINK(I)    : OPTIMAL POSITION OF NODE (I) IN THE SEQUENCE OF DOF
!C  ==> KSORT(IPOSI): NODE ID IN POSITION (I) IN THE SEQUENCE OF NODES
!C  ==> TCOOG(I,*)  : TABLE OF ABSLUTE COORDINATES OF NODE (I)
!C
      call allody(nnt,0,'klink',lklink,ipr)
      call allody(nnt,0,'ksort',lksort,ipr)
      call allody(2*nnt1,1,'tcoog',ltcoog,ipr)
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KSKYH (I)   :CUMULATED HEIGHT OF STIFFNESS MATRIX COLUMN
!C  ==> KCONN (I,*) :CONNECTIVITY TABLE BETWEEN ELEMENTS AND NODES
!C  ==> KLOCE (I)   :LOCALIZATION OF ONE ELEMENT IN THE STRUCTURE
!C
!C FINITE ELEMENT ITYP=2 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.2) then
         nnel=6
         nddle=12
         nddlt=3*nnt1+nnint
      endif
!C
!C FINITE ELEMENT ITYP=3 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.3) then
         nnel=8
         nddle=16
         nddlt=3*nnt1+nnint
!C SPACE ALLOCATION FOR CENTER OF ELEMENT (CALCULTED ONCE)
         call allody(2*nelt,1,'tcele',ltcele,ipr)
      endif
!C
!C WHATEVER THE TYPE OF ELEMENT ...
!C
      if(ipr.gt.0) write(6,*) ' Total number of deg. of frd. :',nddlt
      call allody(nddlt+1,0,'kskyh',lkskyh,ipr)
      call allody(nnel*nelt,0,'kconn',lkconn,ipr)
      call gentpo(s(ltcoog),l(lkconn),l(lklink),l(lksort),s(ltcele),l(lktpol),ipr)
      call allody(nddle,0,'kloce',lkloce,ipr)
      call calsky(l(lkskyh),l(lkconn),l(lklink),l(lkloce),ipr)
      return
      end



      subroutine countn(ktpol)
      include'divapre.h'
      include'divainc.h'
      dimension ktpol(0:nex+1,0:ney+1)
!C
!C  READ OR CREATE THE TOPOLOGY MATRIX KTPOL:
!C    ktpol(i,j) = -1   ==> no element has to be created;
!C                  0   ==> an element must be created;
!C                  iel ==> element iel has been created;
!C
      do 20 i=0,nex+1
         ktpol(i,0)=-1
         ktpol(i,ney+1)=-1
 20   continue
      do 25 j=1,ney
         ktpol(0,j)=-1
         ktpol(nex+1,j)=-1
 25   continue
      if(imatrx.eq.0) then
         do 10 i=1,nex
            do 15 j=1,ney
               ktpol(i,j)=0
 15         continue
 10      continue
      endif
      if(imatrx.eq.1) then
         do 30 j=ney,1,-1
            read(14,400)(ktpol(i,j),i=1,nex)
 30      continue
      endif
 400  format(100(i2))
!C
!C  INSPECT EVERY POSSIBILITY
!C
      nelt=0
      nnt1=0
      nnint=0
      do 100 i=1,nex
         do 110 j=1,ney
            if(ktpol(i,j).eq.0) then
            nelt=nelt+1
            ktpol(i,j)=nelt
!C               FIRST NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j-1).le.0.and.ktpol(i,j-1).le.0) nnt1=nnt1+1
!C               FIRST INTERFACE
            if(ktpol(i,j-1).le.0) nnint=nnint+1
!C               SECONT NODE
            if(ktpol(i+1,j).le.0.and.ktpol(i+1,j-1).le.0.and.ktpol(i,j-1).le.0) nnt1=nnt1+1
!C               SECOND INTERFACE
            if(ktpol(i+1,j).le.0) nnint=nnint+1
!C               THIRD NODE
            if(ktpol(i+1,j).le.0.and.ktpol(i+1,j+1).le.0.and.ktpol(i,j+1).le.0) nnt1=nnt1+1
!C               THIRD INTERFACE
            if(ktpol(i,j+1).le.0) nnint=nnint+1
!C               FOURTH NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j+1).le.0.and.ktpol(i,j+1).le.0) nnt1=nnt1+1
!C               FOURTH INTERFACE
            if(ktpol(i-1,j).le.0) nnint=nnint+1
            endif
 110     continue
 100  continue
      if(ityp.eq.3) then
         nnt=nnt1+nnint
      endif
      return
      end



      subroutine gentpo(tcoog,kconn,klink,ksort,tcele,ktpol,ipr)
!C
!C  I/O OF TOPOLOGIC DATA SET
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),klink(nnt),ksort(nnt),tcele(nelt,2),ktpol(0:nex+1,0:ney+1)
!C
!C  RE-INITIATE THE TOPOLOGY MATRIX
!C
      do 10 i=1,nex
        do 15 j=1,ney
           if(ktpol(i,j).gt.0) ktpol(i,j)=0
 15     continue
 10   continue
      nelt=0
      nns=0
      nni=0
      ipos=0
      do 20 i=1,nex
         do 25 j=1,ney
            if(ktpol(i,j).eq.0) then
            nelt=nelt+1
            tcele(nelt,1)=(i-1)*deltax+deltax*0.5
            tcele(nelt,2)=(j-1)*deltay+deltay*0.5
            ktpol(i,j)=nelt
!C               FIRST NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j-1).le.0.and.ktpol(i,j-1).le.0) then
               nns=nns+1
               ipos=ipos+1
               ksort(ipos)=nns
               tcoog(nns,1)=deltax*(i-1)
               tcoog(nns,2)=deltay*(j-1)
               kconn(nelt,1)=nns
               goto 112
            endif
            if(ktpol(i-1,j).gt.0) then
               iel=ktpol(i-1,j)
               kconn(nelt,1)=kconn(iel,3)
               goto 112
            endif
            if(ktpol(i-1,j-1).gt.0) then
               iel=ktpol(i-1,j-1)
               kconn(nelt,1)=kconn(iel,5)
               goto 112
            endif
            if(ktpol(i,j-1).gt.0) then
               iel=ktpol(i,j-1)
               kconn(nelt,1)=kconn(iel,7)
               goto 112
            endif
!C               FIRST INTERFACE
 112         if(ktpol(i,j-1).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,2)=-(nni+nnt1)
                goto 113
             endif
             if(ktpol(i,j-1).gt.0) then
                iel=ktpol(i,j-1)
                kconn(nelt,2)=kconn(iel,6)
                goto 113
             endif
!C               SECOND NODE
 113         if(ktpol(i,j-1).le.0.and.ktpol(i+1,j-1).le.0.and.ktpol(i+1,j).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*i
                tcoog(nns,2)=deltay*(j-1)
                kconn(nelt,3)=nns
                goto 114
             endif
             if(ktpol(i,j-1).gt.0) then
                iel=ktpol(i,j-1)
                kconn(nelt,3)=kconn(iel,5)
                goto 114
             endif
             if(ktpol(i+1,j-1).gt.0) then
                iel=ktpol(i+1,j-1)
                kconn(nelt,3)=kconn(iel,7)
                goto 114
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,3)=kconn(iel,1)
                goto 114
             endif
!C               SECOND INTERFACE
 114         if(ktpol(i+1,j).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,4)=-(nnt1+nni)
                goto 115
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,4)=kconn(iel,8)
                goto 115
             endif
!C               THIRD NODE
 115         if(ktpol(i+1,j).le.0.and.ktpol(i+1,j+1).le.0.and.ktpol(i,j+1).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*i
                tcoog(nns,2)=deltay*j
                kconn(nelt,5)=nns
                goto 116
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,5)=kconn(iel,7)
                goto 116
             endif
             if(ktpol(i+1,j+1).gt.0) then
                iel=ktpol(i+1,j+1)
                kconn(nelt,5)=kconn(iel,1)
                goto 116
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,5)=kconn(iel,3)
                goto 116
             endif
!C               THIRD INTERFACE
 116         if(ktpol(i,j+1).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,6)=-(nnt1+nni)
                goto 117
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,6)=kconn(iel,2)
                goto 117
             endif
!C               FOURTH NODE
 117         if(ktpol(i,j+1).le.0.and.ktpol(i-1,j+1).le.0.and.ktpol(i-1,j).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*(i-1)
                tcoog(nns,2)=deltay*j
                kconn(nelt,7)=nns
                goto 118
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,7)=kconn(iel,1)
                goto 118
             endif
             if(ktpol(i-1,j+1).gt.0) then
                iel=ktpol(i-1,j+1)
                kconn(nelt,7)=kconn(iel,3)
                goto 118
             endif
             if(ktpol(i-1,j).gt.0) then
                iel=ktpol(i-1,j)
                kconn(nelt,7)=kconn(iel,5)
                goto 118
             endif
!C               FOURTH INTERFACE
 118         if(ktpol(i-1,j).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,8)=-(nnt1+nni)
                goto 119
             endif
             if(ktpol(i-1,j).gt.0) then
                iel=ktpol(i-1,j)
                kconn(nelt,8)=kconn(iel,4)
                goto 119
             endif
 119         continue
             endif
 25      continue
 20   continue

!C
!C  CONSTRUCTION OF KLINK VECTOR; FOR TYPE 2 OR 3: FDV ELEMENT
!C
      if(ityp.eq.2.or.ityp.eq.3) then
         ilink=1
         do 40 iposi=1,nnt
            inod=ksort(iposi)
            klink(inod)=ilink
            ilink=ilink+1
            if(inod.le.nnt1) ilink=ilink+2
 40      continue
      endif
!C
!C OUTPUT OF TOPOLOGICAL DATA / CREATE THE FORT.11 FILE
!C
      if(ipr.ge.3) then
         write(6,*)' List of vertex nodes id, X and Y positions'
         write(6,*)' ------------------------------------------'
      endif
      do 100 i=1,nnt1
         if(ipr.ge.3) then
           write(6,*) i,tcoog(i,1),tcoog(i,2)
         endif
         write(11,*) ksort(i),tcoog(i,1),tcoog(i,2)
 100  continue
      do 105 i=nnt1+1,nnt
         write(11,*) ksort(i)
 105  continue
      if(ipr.ge.3) then
         write(6,*)' List of element id , center and connectivities'
         write(6,*)' ----------------------------------------------'
      endif
      do 195 i=1,nelt
         if(ipr.gt.3) then
           write(11,916) (kconn(i,j),j=1,nnel)
           write(6,915) i,(tcele(i,j),j=1,2),(kconn(i,j),j=1,nnel)
         endif
 915     format(' Elt:',i5,' Center:',2(f7.1),' Nodes:',10(i5))
 916     format(10(i6,' '))
 195  continue
      close(11)
      open(unit=11,file='fort.11')
      if(ipr.ge.4) then
         write(6,*)' I   ,    KSORT(I)    ,     KLINK(I)      '
         write(6,*)' ---------------------------------------- '
         do 120 i=1,nnt
           write(6,*) i,ksort(i),klink(i)
 120     continue
      endif
      return
      end
