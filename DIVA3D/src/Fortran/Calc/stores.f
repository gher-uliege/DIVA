C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  STORES (MODULE)
C     -  EXTRT2 (extract the solution from an ITYP=2 element)
C     -  EXTRT3 (extract the solution from an ITYP=3 element)
C     -  GENF80 (create fort.80 file, with localization of solution
C               points in the finite element mesh
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             STORES MODULE                            C
C                         Storage of the solution                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine stores(ipr)
      include'divapre.h'
      real*8 c8,gcvaln,vargcv
      include'divainc.h'
      real*8 d0d,d1d,d2d,d3d,d4d,atr,btr,ctr
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      GCVALN=0
      vargcv=0
      d0d=0
      d1d=0
      d2d=0
      d3d=0
      d4d=0
      atr=0
      btr=0
      ctr=0
      ifirst=1
 1661 format(3(E22.9))
C
C  PROBLEM P2: OPEN THE FILE CONTAINING THE CONDENSATION VECTOR
C
c      if(ityp.eq.2.or.ityp.eq.3) then
c         open(32,file='kele2.cnd',recl=nddle*3*iprc,form='unformatted',
c     &        access='direct')
C         write(6,*) 'JMB: depreciated file output'
c         if (IJMB.EQ.0) STOP 'KELE OPTIMISATION NOT WORKING'
c      endif
C
C  input of general data
C
      read(10,*) ispec
C      if(ipr.gt.0) write(6,*) ' Specification of output =',ispec
      ispec0=ispec
      ispec=0
      
CJMBB Produce analysis at data points into fort.71
         rewind(13)
         read(13,*,end=1234) xori,yori
         read(13,*)dx,dy
         read(13,*)nx,ny
         read(13,*)valex
         rewind(13)
 1234 continue
      if(ispec0.eq.0.or.ispec0.eq.3.or.ispec0.eq.4) then
C      write(6,*) 'valatxy'
         ireclu=0
#ifdef DIVABINARYFILES
 6       read(79,end=8) x,y
#else
 6       read(79,*,end=8) x,y
#endif
 
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,
     &                         l(lkntc),ipr)
c               if (iel.eq.-1) then
c                write(6,*) 'sauve qui store',x_ll,y_ll
c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
C               if (icoordchange.ne.0) call xyll(x,y)
c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
#ifdef DIVABINARYFILES
        write(82) valex
#else
        write(82,*) x_ll,y_ll,valex
#endif
              
               goto 6
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &                  l(lklocs),s(ltrhsg),val,s(1),ipr,s(lrkele))
C            if (icoordchange.ne.0) call xyll(x,y)
#ifdef DIVABINARYFILES
        write(82) val
#else
        write(82,*) x_ll,y_ll,val
#endif

         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,
     &                      ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
#ifdef DIVABINARYFILES
        write(82) val
#else
        write(82,*) x_ll,y_ll,val
#endif

            goto 6
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &       l(lklocs),s(ltrhsg),s(ltcele),val,s(1),ipr,s(lrkele))
C            if (icoordchange.ne.0) call xyll(x,y)
#ifdef DIVABINARYFILES
        write(82) val
#else
        write(82,1661) x_ll,y_ll,val
#endif
            
         endif
         goto 6
 8       continue
C
C          write(6,*) 'data'
 123     continue
         ireclu=0
         ijmbval=0
         rewind(20)
CCC         open(71,file='fort.71',form='formatted',buffered='yes')
#ifdef DIVABINARYFILES
 66    read(20,end=86) x,y,tttt,wwww
#else
 66    read(20,*,end=86)  x,y,tttt,wwww
#endif
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
C         ijdat=l(lkdata+ireclu-1)
C JMB2013
         iel=l(lkelos+ireclu-1)
         isub=l(lkelos+ndata+ireclu-1)
C         ijki=l(lkelos+ijdat-1)
C         write(6,*) 'Why not use known element?',ireclu,iel,isub
C
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
c               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,
c     &                         l(lkntc),ipr)
c            write(6,*) 'Since it is found in ',iel,isub
c               if (iel.eq.-1) then
c                write(6,*) 'sauve qui store',x_ll,y_ll
c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
c               endif
            endif
c            if (opti.eq.0) then
c               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
c            endif
C JMB2013END
            if(iel.le.0.or.isub.le.0) then
C               if (icoordchange.ne.0) call xyll(x,y)
c              write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
C               write(82,*) x_ll,y_ll,-9999.0
                val=valex
CTESTSPEEDJMB   
#ifdef DIVABINARYFILES
           write(71) x_ll,y_ll,val
#else
           write(71,1661) x_ll,y_ll,val
#endif
              
               goto 66
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &       l(lklocs),s(ltrhsg),val,s(1),ipr,s(lrkele))
C            if (icoordchange.ne.0) call xyll(x,y)
CTESTSPEEDJMB
#ifdef DIVABINARYFILES
           write(71) x_ll,y_ll,val
#else
           write(71,1661) x_ll,y_ll,val
#endif

         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,
     &                      ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
#ifdef DIVABINARYFILES
           write(71) x_ll,y_ll,val
#else
           write(71,1661) x_ll,y_ll,val
#endif

            goto 66
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &        l(lklocs),s(ltrhsg),s(ltcele),val,s(1),ipr,s(lrkele))
C            if (icoordchange.ne.0) call xyll(x,y)
#ifdef DIVABINARYFILES
           write(71) x_ll,y_ll,val
#else
           write(71,1661) x_ll,y_ll,val
#endif

         endif
         ijmbval=ijmbval+1
         jmboff=ndata*2+ireclu-1
         valb=val
         IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            rewind(22)
            CLOSE (22)
            
            IFIRST = 0
         ENDIF
         VALb = VAL - XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            rewind(22)
            IFIRST = 0
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

c         write(6,*) 'datasto',s(ltdata+jmboff),jmboff,ireclu,val
         GCVALN=GCVALN+(s(ltdata+jmboff)-valb)*
     &     wwww/hmmu*(s(ltdata+jmboff)-valb)
         vargcv=vargcv+(s(ltdata+jmboff))*(s(ltdata+jmboff))
         d0d=d0d+wwww/hmmu*
     &     s(ltdata+jmboff)*s(ltdata+jmboff)
         d1d=d1d+wwww/hmmu*
     &     s(ltdata+jmboff)*valb
         d2d=d2d+wwww/hmmu*
     &     valb*valb
         goto 66
  86     continue
         GCVALN=sqrt(GCVALN/ijmbval)
         vargcv=(vargcv/ijmbval)
         d0d=d0d/ijmbval
         d1d=d1d/ijmbval
         d2d=d2d/ijmbval
         nntr=ijmbval
         write(33,*) vargcv,ijmbval
CJMBE
 
 
      endif
      ispec=ispec0
      if(ispec.ge.1) then
         read(13,*)xori,yori
         read(13,*)dx,dy
         read(13,*)nx,ny
         read(13,*)valex
c                  write(6,*) 'xorib',xori, yori,dx,dy
         
         iix=1
         iiy=0
         if (icoordchange.ne.0) then
          xorio=xori
          call llxy(xori,yori)
          dx=dx*dxkm 
          dy=dy*dykm 
         endif
c         write(6,*) 'xori',xori, yori,dx,dy
C
C  ALLOCATION OF SPACE FOR THE GRIDDED SOLUTION
C
         call allody(nx*ny,1,'tgrds',ltgrds,ipr)
         do 5 i=1,nx*ny
            s(ltgrds+i-1)=valex
 5       continue
      endif

C
C  construction of file 80
C
      if(ispec.eq.1.or.ispec.eq.3) then
         if(ltcele.eq.0) then
         bidon=0
         call genf80(s(ltcoog),l(lkconn),l(lkntc),bidon,ipr)
         else
         call genf80(s(ltcoog),l(lkconn),l(lkntc),s(ltcele),ipr)
      endif
         close(80)
         open(unit=80,form='unformatted',file='fort.80')
      endif
      if(ispec.gt.0) then
      index=0
C JMB (change for the problem in extre2 for regular grid...
       write(6,*) 'grid'
 10    continue
       val=valex
       if(ityp.eq.2) then
    
         read(80,end=100)x,y,iel,isub
cmr         write(6,*)x,y,iel,isub
C         if (icoordchange.ne.0) call llxy(x,y)
         if(iel.gt.0) then
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &       l(lklocs),s(ltrhsg),val,s(ltgrds),ipr,s(lrkele))
         endif
      endif
      if(ityp.eq.3) then
         read(80,end=100)x,y,iel,isub
C         if (icoordchange.ne.0) call llxy(x,y)
         if(iel.gt.0) then
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &      l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrds),ipr,s(lrkele))
         endif
      endif
      index=index+1
C JMB to replace the regular grid output
      ix=nint((x-xori)/dx)
      iy=nint((y-yori)/dy)

      iiy=iiy+1
      if(iiy.gt.ny) then
      iiy=1
      iix=iix+1
      endif
      
c      if (isspheric.eq.1) then
c      xori=xorio
c      yyy=y
c      call llxy(xori,yyy)
c      write(6,*) 'Spheric',xori,dx
c      xxx=max(cos(y*RPI/180),1E-10)
c      ix=nint((x-xori)/dx/xxx)
c      endif
c      if(iix.ne.ix.or.iiy.ne.iy) then
c      write(6,*) '????',ix,iix,iy,iiy
c      endif
       ix=iix
       iy=iiy
       jmboff=(iy-1)*nx+ix-1
c      write(6,*) 'gridded',ix,iy,jmboff,val
      s(ltgrds+jmboff)=val
C JMBend??
      goto 10
 100  nptsol=index
      write(6,*)'Total nb. of pts where gridded solution is asked ='
     &  ,nptsol
      endif
      if(ispec.ge.1) then
         call impmat(s(ltgrds),nx,ny,nx,83)
         iu=84
        nbm=-1
c        write(6,*) 'storing', iprc
         if(iprc.eq.4) then
           call uwritc(iu,c8,s(ltgrds),valex,iprc,nx,ny,1,nbm)
           write(6,*) 'Storing field estimate in real*4'
                       else
c           write(6,*) 'Storing field estimate in real*8'
c,nx,ny,nbm
           call uwrit2(iu,c8,s(ltgrds),valex,iprc,nx,ny,1,nbm)  
c           write(6,*)' % Writing gridded field in single precision % '
c           write(6,*) 'Storing field estimate in real*8'
         endif
         write(6,*) 'Finished storing'
      endif
      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
      return
      end



      subroutine extrt2(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,val,
     &                  tgrds,ipr,trkele)
C
C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=2
C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
C            IS COMPULSORY                               !!!!!!!!!!!
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(3,10),
     &          sol(nddlt),tr(3,12),ddl(15),x(0:3),y(0:3),ddlsub(10),
     &          tjac(2,2),tjaci(2,2),wk(10,10),tgrds(nx,ny),ep(10)
     &          ,TRKELE(3,12,*)
      REAL*4 XMEAN, A0, A1, A2
      INTEGER IFIRST
      SAVE IFIRST, XMEAN, A0, A1, A2

      DATA IFIRST / 1 /
cmr      write(6,*) 'element bef: ',iel

      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
cmr      write(6,*) 'element : ',iel
C JMB
c      read(32,rec=iel) tr
c            if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c         endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
C JMB
      do 10 i=1,12
        ddl(i)=sol(kloce(i))
 10   continue
C
C  CORRECTION FOR NORMAL REFERENCE
C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(10)=-ddl(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(11)=-ddl(11)
      endif
C mr BIG BUG      if(kconn(iel,1).lt.kconn(iel,3)) then
      if(kconn(iel,1).lt.kconn(iel,5)) then
         ddl(12)=-ddl(12)
      endif
      do 20 i=1,3
         ddl(12+i)=zero
         do 25 k=1,12
            ddl(12+i)=ddl(12+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
C
C calculate the (XI,ETA) coordinates from (X,Y)
C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
C
C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
C
C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue

C --- Code Added for Version 2.2  (RS 22 March 94) ---

      IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            IFIRST = 0
         ENDIF
c          val=val
         VAL = VAL + A0 + A1*XP + A2*YP
C test: only reference valye
c         VAL=A0 + A1*XP + A2*YP
      ENDIF

C --- End of Added Code ---
C JMB???????? what does this added code mean?????? check for ispec history
C when putting out also in discrete locations. There is no need to fill in 
C the regular grid when asking for local values only (effet de bord assure...)
c      if(ispec.ge.1) then
c        ix=nint((xp-xori)/dx)
c        iy=nint((yp-yori)/dy)
c        tgrds(ix,iy)=val
c      endif
      return
      end



      subroutine extrt3(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,
     &                 tcele,val,tgrds,ipr,trkele)
C
C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=3
C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
C            IS COMPULSORY                               !!!!!!!!!!!
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(4,10),
     &          sol(nddlt),tr(3,16),ddl(19),x(0:4),y(0:4),ddlsub(10),
     &          tjac(2,2),tjaci(2,2),wk(10,10),tgrds(nx,ny),
     &          tcele(nelt,2),ep(10)
     &          ,TRKELE(3,12,*)

      REAL*4 XMEAN, A0, A1, A2
      INTEGER IFIRST
      SAVE IFIRST, XMEAN, A0, A1, A2

      DATA IFIRST / 1 /

      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
c      read(32,rec=iel) tr
C JMB
c      read(32,rec=iel) tr
c            if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
C JMB
      do 10 i=1,16
        ddl(i)=sol(kloce(i))
 10   continue
C
C  CORRECTION FOR NORMAL REFERENCE
C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(13)=-ddl(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(14)=-ddl(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
         ddl(15)=-ddl(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
         ddl(16)=-ddl(16)
      endif
      do 20 i=1,3
         ddl(16+i)=zero
         do 25 k=1,16
            ddl(16+i)=ddl(16+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
C
C calculate the (XI,ETA) coordinates from (X,Y)
C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(4)
         y2=y(4)
      endif
      if(isub.eq.4) then
         x1=x(4)
         y1=y(4)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
C
C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
C
C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue

C --- Code Added for Version 2.2  (RS 22 March 94) ---

      IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + A0 + A1*XP + A2*YP
      ENDIF

C --- End of Added Code ---


C Also strange when extraction in localized points... need to add some test here
c      if(ispec.ge.1) then
c        ix=nint((xp-xori)/dx)
c        iy=nint((yp-yori)/dy)
c        tgrds(ix,iy)=val
c      endif
      return
      end




      subroutine genf80(tcoog,kconn,kntc,tcele,ipr)
C
C  THIS ROUTINE GENERATES A LIST OF POINTS (X,Y,IEL,ISUB) ON A REGULAR
C  GRID, WHERE THE SOLUTION IS REQUIRED
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),tcele(nelt,2)
C
C  FOR ITYP = 2 ...
C
      if(ityp.eq.2) then
      do 10 i=1,nx
         do 15 j=1,ny
      
            x=xori+i*dx
            y=yori+j*dy
            if (isspheric.eq.1) then
            xori=xorio
            yyy=y
            call llxy(xori,yyy)
            x=xori+i*dx*cos(y*RPI/180.)
            endif
Cmr            if (icoordchange.ne.0) call llxy(x,y)
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,tcoog,kconn,iel,isub,kntc,
     &                         ipr)
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
            endif
            write(80)x,y,iel,isub
cmr            write(6,*)'create 80 ',x,y,iel,isub
 15      continue
 10   continue
      endif
C
C  FOR ITYP = 3 ...
C
      if(ityp.eq.3) then
      do 110 i=1,nx
         do 115 j=1,ny
            x=xori+i*dx
            y=yori+j*dy
            if (isspheric.eq.1) then
            xori=xorio
            yyy=y
            call llxy(xori,yyy)
            x=xori+i*dx*cos(y*RPI/180.)
            endif
Cmr            if (icoordchange.ne.0) call llxy(x,y)
            if (opti.eq.1) then 		! (SvL)
               call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,
     &                         kntc,ipr)
            endif
            if (opti.eq.0) then 
               call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif
            write(80)x,y,iel,isub
 115     continue
 110  continue
      endif
      return
      end
