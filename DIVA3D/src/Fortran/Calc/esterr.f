C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  ESTERR (MODULE)
C     -  CALGEL : calculating and assembling elementary rhs
C     -  CGELE2 : integrating elementary rhs when ITYP=2
C     -  CGELE3 : integrating elementary rhs when ITYP=3
C     -  CGSEL2 : integrating sub-elementary matrices when ITYP=3
C     -  CALPSO : computes pseudo data sets for error estimates
C     -  EXTRE2 : extract error estimate when ITYP=2
C     -  EXTRE3 : extract error estimate when ITYP= 3
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             ESTERR MODULE                            C
C       Estimates the analysis error (same grid as the analysis)       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine esterr(ipr)
      include'divapre.h'
      include'divainc.h'
 1661 format(3(E22.9))
      zero=0.
      read(10,*) ispec
      write(6,*) 'Please be patient'
CJMB TO BE OPTIMIZED: calculate error only if point in mesh; use locopt
C for regular grid should already be done?? Yes done; so just do it on 
C data points and valatxy with locopt.
      ifkern=0
      JMRELERR=1
      ipmerror=0
      if (ispec.gt.10) then
      ispec=ispec-10
      ipmerror=1
      igdone=0
      ifkern=-1
      write(6,*) 'Poor man''s error field'
      endif
      if (ispec.lt.0) then
      ifkern=1
      ispec=-ispec
      
      if (ispec.gt.10) then
      ispec=ispec-10
      JMRELERR=0
      endif
      write(6,*) 'Full covariance calculation, be VERY patient'
      ipipe=0
      read(59,*) ipipe
      if(ipipe.ne.1) then
      open(61,file='divapipe',form='unformatted')
      else
      call pingpong(0,0,1)
c     call system('sleep 1')
       call pingpong(0,1,1)
       iillmm=0
      endif
      endif
      iix=1
      iiy=0
C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP - Error estimation not validated for
     &               this mathematical problem '
	 stop
      endif
      if(alpha0.eq.zero) then
      if (ifkern.ne.1) then
	 write(6,*) ' STOP - Simple error estimation not valid for
     &               alpha0 = 0 '
	 stop
	 endif
      endif 

      accur=1.e-2
      rl0=sqrt(sqrt(1./alpha0))
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.
      if(abs(atwo).gt.accur) then
       if (ifkern.ne.1.and.ipmerror.ne.1) then
       write(6,*) ' STOP - Simple error estimation not valid for
     &               this choice of (alpha0,alpha1) '
	 stop
	endif
      endif 

c      read(10,*)varbak
      read(15,*)varbak
      if(ipr.ge.2) write(6,*)' Variance of bakground =',varbak

C Tabulate the bessel function
      if(opti.eq.1) then
         call tabess
      endif

C  ALLOCATION OF SPACE FOR THE GRIDDED ERROR ESTIMATE
C

c Boucle sur les points de grille ou l'erreur doit etre calculee
      close(80)
      rewind(80)
      open(unit=80,form='unformatted',file='fort.80')
      index=0
      jmcount=0
C Only if ispec=1 3 5 or 7
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 100
      call allody(nx*ny,1,'tgrde',ltgrde,ipr)
       do 5 i=1,nx*ny
          s(ltgrde+i-1)=valex
 5     continue

 10   read(80,end=100)xob,yob,iel,isub
      val=valex
C      write(6,*) 'Errors in',xob,yob,iel,isub
      jmcount=jmcount+1
      
      if(mod(jmcount,max(nx*ny/10,1)).eq.0) 
     &  write(6,*) 'proceeded gridded points: ',
     & nint(jmcount*100./(nx*ny)), ' percent'
      if(iel.gt.0) then
      if(ipmerror.eq.1.and.igdone.eq.1) goto 963
         if(opti.eq.1) then			!	(SvL)
         if(ipr.gt.2) write(6,*) 'calpsoopti'
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif

         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue


C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
C                    IN THE GLOBAL SYSTEM

         if(ipr.gt.2) write(6,*) 'calgel'
         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         if (ipr.gt.2) write(6,*) 'sol'
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),
     &            nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

c ... extraction de la solution au point observe
         if(ipmerror.eq.1) igdone=1
 963     continue

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &              l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &       l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif

      endif
C JMB to replace the regular grid output
      iiy=iiy+1
      if(iiy.gt.ny) then
      iiy=1
      iix=iix+1
      endif
      



C      ix=nint((xob-xori)/dx)
C      iy=nint((yob-yori)/dy)
C      if (isspheric.eq.1) then
C      xori=xorio
C      yyy=yob
C      call llxy(xori,yyy)
c       write(6,*) 'Spherice',xori,dx,xob,yob
C      xxx=max(cos(yob*RPI/180),1E-10)
C      ix=nint((xob-xori)/dx/xxx)
C      endif
C      if(iix.ne.ix.or.iiy.ne.iy) then
C       write(6,*) '????',ix,iix,iy,iiy
C      endif
      ix=iix
      iy=iiy     
      
      jmboff=(iy-1)*nx+ix-1
c      write(6,*) 'gridded',ix,iy,jmboff,val
      s(ltgrde+jmboff)=val
C JMBend??
      goto 10

 100  continue
C
CJMBB now try to put out error field at data points....
C Only if ispec=2 3 6 or 7
         if((ispec.eq.1).or.(ispec.eq.4).or.(ispec.eq.5)) goto 866
         write(6,*) 'now errors at data locations'

         ireclu=0
         rewind(20)
#ifdef DIVABINARYFILES
 666   read(20,end=866) x,y,ttt,www
#else
 666   read(20,*,end=866)  x,y
#endif
         x_ll=x
         y_ll=y
         if(mod(ireclu,max(ndata,10)/10).eq.0) write(6,*) 'proceeded',
     & nint(ireclu*100./(ndata)), ' percent'


         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
C JMB2013
         iel=l(lkelos+ireclu-1)
         isub=l(lkelos+ndata+ireclu-1)
         if(ityp.eq.2) then
c            if (opti.eq.1) then 		! (SvL)
c               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,
c     &                         l(lkntc),ipr)
c               if (iel.eq.-1) then
c                write(6,*) 'sauve qui store',x_ll,y_ll
c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
c               endif
c            endif
c            if (opti.eq.0) then
c               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
c            endif
C JMB2013END
            if(iel.le.0.or.isub.le.0) then
C               if (icoordchange.ne.0) call xyll(x,y)
c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
#ifdef DIVABINARYFILES
           write(72) valex
#else
           write(72,1661) x_ll,y_ll,valex
#endif
              
               goto 666
            endif
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
           write(72) valex
#else
           write(72,1661) x_ll,y_ll,valex
#endif

            goto 666
            endif
         endif
         
C now calculate the error in x,y         
       xob=x
       yob=y


      if(iel.gt.0) then
         if(ipmerror.eq.1.and.igdone.eq.1) goto 964
         if(opti.eq.1) then			!	(SvL)
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif
	       
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
156        continue


C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),
     &            nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif
         igdone=1
 964     continue
c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &            l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,1661) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &      l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,1661) xob,yob,val
	    endif 
         endif

      endif
#ifdef DIVABINARYFILES
           write(72) val
#else
           write(72,1661) x_ll,y_ll,val
#endif

      goto 666
         


  866     continue


CJMBE

CJMBB now try to put out error field at other discrete locations...
C only of ispec= 4 5 6 7
         if((ispec.eq.1).or.(ispec.eq.2).or.(ispec.eq.3)) goto 867
         write(6,*) 'Finally error at desired discrete locations'
         ireclu=0
         rewind(79)
#ifdef DIVABINARYFILES
 667       read(79,end=867) x,y
#else
 667       read(79,*,end=867) x,y
#endif

         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(mod(ireclu,100).eq.0) 
     & write(6,*) 'proceeded',ireclu, ' points'

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
C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
#ifdef DIVABINARYFILES
           write(73) valex
#else
           write(73,1661) x_ll,y_ll,valex
#endif
               goto 667
            endif
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
           write(73) valex
#else
           write(73,1661) x_ll,y_ll,valex
#endif


            goto 667
            endif
         endif
         
C now calculate the error in x,y         
       xob=x
       yob=y
      
      if(iel.gt.0) then
      if(ipmerror.eq.1.and.igdone.eq.1) goto 965
         if(opti.eq.1) then			!	(SvL)
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif
	       
         do 157 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
157        continue


C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),
     &            nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

         igdone=1
965    continue
c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &        l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &       l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif

      endif
#ifdef DIVABINARYFILES
           write(73) val
#else
           write(73,1661) x_ll,y_ll,val
#endif


      goto 667
         


  867     continue


CJMBE
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 1991
      if(ispec.ge.1) then
         call impmat(s(ltgrde),nx,ny,nx,86)
c            write(6,*) 'uwritc 87', iprc
           nbm=-1
	 if(iprc.eq.4) then
            call uwritc(87,c8,s(ltgrde),valex,iprc,nx,ny,1,nbm)
c	    call uwbimg4(87,s(ltgrde),nx,ny,1,valex,dx,dy,xori,yori)
	    write(6,*) 'Storing error estimate in gher format (real*4)'
		       else
            call uwrit2(87,c8,s(ltgrde),valex,iprc,nx,ny,1,nbm)
c	    call uwbimg8(87,s(ltgrde),nx,ny,1,valex,dx,dy,xori,yori)
	    write(6,*) 'Storing error estimate in gher format (real*4)'
         endif
c            write(6,*) 'end uwritc 87', iprc
      endif

      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
 1991 continue
      return
      end



      subroutine calgel(trhsg,tcoog,trhse,kconn,ipr)
C
C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY RHS AND
C  ASSEMBLING IN THE GLOBAL SECOND MEMBER OF THE SYSTEM
C
      include'divapre.h'
      include'divainc.h'
      dimension trhsg(nddlt),tcoog(nnt1,2),trhse(nddle),
     &          kconn(nelt,nnel)
      one=1.0D0
      three=3.0D0
      five=5.0D0

      do 5 i=1,nddle
	 trhse(i)=0.
 5    continue

C
C  OPEN FILE FOR READING CONDENSATION VECTORS (FOR RHS CONDENSATION)
C
c      if(ityp.eq.2.or.ityp.eq.3) then
c         open(32,file='kele2.cnd',recl=nddle*3*iprc,
c     &         form='unformatted',access='direct')
c      endif
      do 10 iel=1,nelt
         if(ityp.eq.2) then
           call cgele2 (iel,tcoog,kconn,trhse,l(lklocs),ipr,s(lrkele))
         endif
         if(ityp.eq.3) then
           call cgele3 (iel,tcoog,kconn,trhse,l(lklocs),s(ltcele),
     &                   ipr,s(lrkele))
         endif

         IF(IPR.GE.4) then
            WRITE (6,406) iel
         endif
 406     FORMAT(///,T10,' ELEMENTARY  RHS  FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(trhse,nddle,1,nddle,6)
C
C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
C
c         if (iel.eq.500) then
c         write(6,*) 'into calloc',nddle,lkloce,lkconn
c         endif
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         ikg=0
         ifg=1
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),
     &              trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue
      return
      end




      subroutine cgele2 (iel,tcoog,kconn,trhse,loces,ipr,trkele)
C
C  INTEGRATE ELEMENTARY RHS WHEN ITYP = 2 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(12),kconn(nelt,nnel),
     &          x(0:3),y(0:3)
     &          ,TRKELE(3,12,*)
C
C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
C
      dimension tge(15),tgse(10),loces(3,10),tr(3,12)
C
C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      zero=0.D0
      do 10 i=1,15
         tge(i)=zero
 10   continue
C
C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
C
      x0=x(0)
      y0=y(0)
C
C  First sub-element
C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
c      if(iel.eq.500) then
c      write(6,*) 'another cgsel2',iel,x0,y0,x1,y1,x2,y2
c      write(6,*) 'another  k',kconn(iel,1),kconn(iel,3),kconn(iel,5)
c      write(6,*) '??',tcoog(kconn(iel,1),2),nnt1
c      endif
      call cgsel2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
C
C  Second sub-element
C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      call cgsel2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
C
C  Third sub-element
C
      x1=x(3)
      y1=y(3)
      x2=x(1)
      y2=y(1)
      call cgsel2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
C
C             AND ELIMINATING THE D.O.F AT THE CENTER
C                       MATRIX CONDENSATION
C       (see method in: Brasseur, 1993, Ph. D. dissertation)
C
      if(ipr.ge.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (15) '
         call impmat(tge,15,1,15,6)
	 write(6,*) 'iel = ',iel
      endif
c      read(32,rec=iel) tr
C JMB
c      read(32,rec=iel) tr
c            if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c        endif
c        IF (IJMB.EQ.0) STOP "KELE OPTI CRASHED"
C        if(ijmbw(iel).ne.1) stop 'bizzare'
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
C JMB
C
C  REDUCTION OF ELEMENTARY RHS (15) ==> (12)
C
      do 160 i=1,12
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+12)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
C
C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
C  WITH number(nod2) > number(nod1)
C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(10)=-trhse(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(11)=-trhse(11)
      endif
      if(kconn(iel,1).lt.kconn(iel,5)) then
            trhse(12)=-trhse(12)
      endif
      return
      end



      subroutine cgele3(iel,tcoog,kconn,trhse,loces,tcele,ipr,trkele)
C
C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(16),kconn(nelt,nnel),
     &          x(0:4),y(0:4),tcele(nelt,2)
     &         ,TRKELE(3,12,*)
C
C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
C
      dimension tge(19),tgse(10),loces(4,10),tr(3,16)
C
C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
C
      zero=0.d0
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
      do 10 i=1,19
         tge(i)=zero
 10   continue
C
C  INTEGRATION OF THE 4 (10) SUB-ELEMENT RHS
C
      x0=x(0)
      y0=y(0)
C
C  First sub-element
C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(iel.eq.500) then
      write(6,*) 'into cgsel',iel,x0,x0,x1,y1,x2,y2
      endif
      call cgsel2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
C
C  Second sub-element
C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      call cgsel2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
C
C  Third sub-element
C
      x1=x(3)
      y1=y(3)
      x2=x(4)
      y2=y(4)
      call cgsel2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
C
C  Fourth sub-element
C
      x1=x(4)
      y1=y(4)
      x2=x(1)
      y2=y(1)
      call cgsel2(4,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),ipr)
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
 131  continue
C
C             AND ELIMINATING THE D.O.F AT THE CENTER
C                       MATRIX CONDENSATION
C       (see method in: Brasseur, 1993, Ph. D. dissertation)
C
      if(ipr.gt.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (19) '
         call impmat(tge,19,1,19,6)
	 write(6,*) 'iel = ',iel
      endif
c      read(32,rec=iel) tr
C JMB
c      read(32,rec=iel) tr
c            if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
C        if(ijmbw(iel).eq.0) stop 'etrange'
c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
C JMB
C
C  REDUCTION OF ELEMENTARY RHS (19) ==> (16)
C
      do 160 i=1,16
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+16)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
C
C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
C  WITH number(nod2) > number(nod1)
C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(13)=-trhse(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(14)=-trhse(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
            trhse(15)=-trhse(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
            trhse(16)=-trhse(16)
      endif
      return
      end




      subroutine cgsel2(isub,iel,tgse,x0,y0,x1,y1,x2,y2,kindt,
     &                  kdata,kelos,tdata,ipr)
C
C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tgse(10),kindt(nelt),kdata(ndata),
     &          kelos(ndata,2),tdata(ndata,4),
     &          tjaci(2,2),tjac(2,2),tr(10,10),gtemp(10),ep(10)
      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      do 10 i=1,10
         tgse(i)=zero
         gtemp(i)=zero
        do 15 j=1,10
            tr(i,j)=zero
 15      continue
 10   continue

C
C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CGSEL2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
c         write(6,*) 'Detj now',detj
        
        detj=max(abs((x2-x1)*(y0-y1)),abs((x0-x1)*(y2-y1)))
        detj=max(detj,abs((x1-x0)*(y2-y0)),abs((x2-x0)*(y1-y0)))
        detj=max(detj,abs((x1-x2)*(y0-y2)),abs((x0-x2)*(y1-y2)))
        detj=max(detj,abs((x1-x2)*(x1-x2)),abs((x0-x2)*(x0-x2)))
        detj=max(detj,abs((y1-y2)*(y1-y2)),abs((y0-y2)*(y0-y2)))
        detj=max(detj,abs((x1-x0)*(x1-x0)),abs((y0-y1)*(y0-y1)))
        write(6,*) 'changed detj to ',detj
      endif
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      IF(IPR.GE.5) then
         WRITE (6,405) iel,isub
      endif
 405  FORMAT(///,T10,'JACOBIAN MATRIX FOR ELEMENT ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjac,2,2,2,6)
      IF(IPR.GE.5) then
         WRITE (6,406) iel,isub
      endif
 406  FORMAT(///,T10,'INVERSE JAC. MATRIX FOR EL. ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjaci,2,2,2,6)
C
C  CONTRIBUTION OF THE DATA POINTS
C
      ifirst=kindt(iel)+1
      if(iel.eq.nelt) then
         ilast=ndatl
                      else
         ilast=kindt(iel+1)
      endif
      do 150 id=ifirst,ilast
         idata=kdata(id)
         if(kelos(idata,1).ne.iel) then
            write(6,*)' %%% ERROR - cgsel2 : CHECK DATA SORTING %%% '
            stop
         endif
         isel=kelos(idata,2)
         if(isel.ne.isub) goto 150
         xo=tdata(idata,1)
         yo=tdata(idata,2)
         do=tdata(idata,3)
         wo=tdata(idata,4)
         if(ipr.ge.4) then
             write(6,151)xo,yo,do,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';yo=',f8.2,';do=',
     &          f6.4,';wo=',f10.4)
C
C Transformation of the data position in reference element
C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
C
C  CONTRIBUTION FROM PSEUDO-OBSERVATIONS
C
CJMB TODO if profiling show too many calculations just above:
C Store here ep(i) for each data point (idata) and sub-element into big matrix and once stored, skip all the stuff above
C when contribution at data point is needed
C right store ep(i,iel,isubelem) should be enought?? Store it from analysis step already
C
         do 160 i=1,10
C         ep(i)=TRKELEE(i,idata)
            tgse(i)=tgse(i)+wo*do*ep(i)
 160     continue
 150  continue
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
C
C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
C  TR IS THE TRANSFORMATION MATRIX
C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 400 k=1,3
         is=(k-1)*3+1
         tr(is,is)=un
         do 410 i=1,2
            do 420 j=1,2
               tr(is+i,is+j)=tjac(i,j)
 420        continue
 410     continue
 400  continue
      tr(10,10)=p1
      tr(10,4)=-trois*p2/(deux*dist12)
      tr(10,7)=-tr(10,4)
      tr(10,5)=p2*(x1-x2)/(quatre*dist12)
      tr(10,8)=tr(10,5)
      tr(10,6)=p2*(y1-y2)/(quatre*dist12)
      tr(10,9)=tr(10,6)
C
C TRANSFORMATION OF TGSE
C JMB TRY TO SAVE TR as for condensation ?? Or is it already this matrix? CHECK if written only 
C only once and used in the same way; if so, try to use it here and not calculate it. Should work
C since data location invariant during whole process??, not really the same matrix.
C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
C         gtemp(i)=gtemp(i)+TRKELEB(k,i,isub,iel)*tgse(k)   
         gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue

      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end


      subroutine calpso(xob,yob,tdata,ipr)

C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

C  INPUT OF DATA SET DESCRIPTION
      do 10 i=1,ndata
         call fcorr(xob,tdata(i,1),yob,tdata(i,2),varbak,rl0,corre)
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
      return
      end

c=======================================================================
      subroutine fcorr(x1,x2,y1,y2,rm0,rl,corre)        
      
      include'divapre.h'
      
      external euclidist                                                
      external bessk1

      r=euclidist(x1,x2,y1,y2)                                        
      eps=r/rl    
                                                                        
      if (eps.le.0.001) then                                                 
         corre=rm0                                                     
      else
         corre=rm0*eps*bessk1(eps)                           
      endif                                                             
      return                                                            
      end                                                               
                                                                       
c=======================================================================
      function euclidist(x1,x2,y1,y2)                                   
                                                                       
      include'divapre.h'

      euclidist=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))             
      return                                                            
      end                                                               

c=======================================================================
      function bessk1(X)
      
      include'divapre.h'
      EXTERNAL BESSI1

      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,
     &     -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,
     &     0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/



      IF(X.LE.0.) STOP 'ERROR X <= 0' 

      IF(X.LE.2.0) THEN
         Y = X * X * 0.25
         BESSK1 = (LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+Y*(P3+
     &             Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         Y = 2.0 / X
         BESSK1 = (EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     &             Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END

C=========================================================================
      function bessi1(X)

      include'divapre.h'

      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,
     &     0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,
     &     -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,
     &     -0.2895312D-1,0.1787654D-1,-0.420059D-2/

      IF(ABS(X).LT.3.75) THEN
         Y = X*X / (3.75*3.75)
         BESSI1 = X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         AX = ABS(X)
         Y = 3.75 / AX
         BESSI1 = (EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+
     &             Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
         IF(X.LT.0.) BESSI1 = - BESSI1
      ENDIF

      RETURN
      END

c=======================================================================


      subroutine extre2(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,val,
     &                  tgrde,ipr,trkele)
C
C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=2
C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
C            IS COMPULSORY                               !!!!!!!!!!!
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(3,10),
     &          sol(nddlt),tr(3,12),ddl(15),x(0:3),y(0:3),ddlsub(10),
c     &          tjac(2,2),tjaci(2,2),wk(10,10),tgrde(nx,ny),ep(10)
     &          tjac(2,2),tjaci(2,2),wk(10,10),ep(10)
     &          ,trkele(3,12,*)
     


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
c      read(32,rec=iel) tr
C JMB
c      read(32,rec=iel) tr
c            if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c        endif
C        if(ijmbw(iel).ne.1) stop 'zut'
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
      if (ipr.ge.3) write(6,*) 'detj ', detj
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
      if (ipr.ge.3) write(6,*) 'dist12 ', dist12
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
      if (ipr.ge.2) write(6,*) 'val, varbak ', val, varbak
      if (val.gt.varbak) then
       val=0.
      else
       val=sqrt(1.D0*varbak-val)
      endif
CCCCC  
       if(C0C0C0.NE.0) then
       val=sqrt(max(val*val/(1.D0-C0C0C0)-varbak,0.D0))
C       JMRELERR=1
       if (JMRELERR.EQ.1) then
       val=val*sqrt(1/C0C0C0-1)
       endif
       endif
CCCCC 
      if (ipr.ge.3) write(6,*) 'val, varbak ', val, varbak
CJMBB
c      if(ispec.ge.1) then
c        ix=nint((xp-xori)/dx)
c        iy=nint((yp-yori)/dy)
c        tgrde(ix,iy)=val
c      endif
CJMBE
      return
      end



      subroutine extre3(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,
     &                 tcele,val,tgrde,ipr,trkele)
C
C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=3
C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
C            IS COMPULSORY                               !!!!!!!!!!!
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(4,10),
     &          sol(nddlt),tr(3,16),ddl(19),x(0:4),y(0:4),ddlsub(10),
     &          tjac(2,2),tjaci(2,2),wk(10,10),tgrde(nx,ny),
     &          tcele(nelt,2),ep(10)
     &          ,trkele(3,12,*)

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

      val=sqrt(varbak-val)
CJMBB
c      if(ispec.ge.1) then
c        ix=nint((xp-xori)/dx)
c        iy=nint((yp-yori)/dy)
c        tgrde(ix,iy)=val
c      endif
C JMBE
      return
      end


