!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             COVAR  MODULE                            C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine covar(ipr)
      include'divapre.h'
      include'divainc.h'
      zero=0.
      read(10,*) ispec
      isumcovar=0
      if(ispec.lt.-100) then
      write(6,*) 'Summing on covariances'
      isumcovar=1
      ispec=ispec+100
      write(6,*) 'ispec',ispec
      endif
      write(6,*) 'Please be patient'
!CJMB TO BE OPTIMIZED: calculate error only if point in mesh; use locopt
!C for regular grid should already be done?? Yes done; so just do it on
!C data points and valatxy with locopt.
      ifkern=0
      JMRELERR=0
      if (ispec.lt.0) then
      ifkern=1
      ispec=-ispec
      if (ispec.gt.10) then
      ispec=ispec-10
      JMRELERR=1
      endif
      write(6,*) 'Full covariance calculation, be patient'
      
      ipipe=0

      if(isumcovar.ne.1) then

            read(59,*) ipipe
      
      if(ipipe.ne.1) then
      close(61)
      open(61,file='../divapipe',form='unformatted')
      else
      
      call pingpong(1,0,1)
!c      call system('sleep 2')
      call pingpong(1,1,1)
      endif
!C only when no summing on covariances
      endif
      
      endif

!C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP -  not validated forthis mathematical problem '
	 stop
      endif
      if(alpha0.eq.zero) then
	 write(6,*) ' STOP - Covariance estimation not valid for alpha0 = 0 '
	 stop
      endif 

      accur=1.e-2
      rl0=sqrt(sqrt(1./alpha0))
      RJMMU=4*3.141592/RL0/RL0
!c      write(6,*) 'rlam',RJMMU
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.
!c      if(abs(atwo).gt.accur) then
!c	 write(6,*) ' STOP - Error estimation not valid for
!c     &               this choice of (alpha0,alpha1) '
!c	 stop
!c      endif


!C  ALLOCATION OF SPACE FOR Covariance infos on location and elements,
!C  MEME HACK Qu'avant, dimensionner avec 1 puis après lecture augmenter
!C  abandonne, plus simple de lire le nombre de données
       read(55,*) ncvdatar
!c      call allody(1,0,'cvpoii',lcvpoii,ipr)
!c      call allody(1,1,'cvpoir',lcvpoir,ipr)
       call allody(2*ncvdatar,0,'cvpoii',lcvpoii,ipr)
       call allody(2*ncvdatar,1,'cvpoir',lcvpoir,ipr)
       

 
      call cvread(l(lcvpoii),s(lcvpoir),ncvdata)
      write(6,*) 'ncvdata?',ncvdata,ncvdatar
!c      call allody(2*ncvdata-1,0,'cvpoii',jjjjj,99)
!c      call allody(2*ncvdata-1,1,'cvpoir',jjjjj,99)
      call allody(nddlt,1,'cvg',lcvg,ipr)
      call allody(ncvdata,1,'lcova',lcova,ipr)
!C      do i=1,ncvdata
!C      write(6,*) 'xy',s(lcvpoir+2*i-2),s(lcvpoir+2*i-1)
!C      write(6,*) 'ei',l(lcvpoii+2*i-2),l(lcvpoii+2*i-1)
!C      enddo
!C
       jjjjco=0
       if(isumcovar.eq.1) then
            doublesum=0
            read(55,*) npxy
            write(6,*) 'npxy',npxy
            call allody(2*npxy,1,'lxycova',lxycova,ipr)
            jjjjco=0
            rewind(79)
            do i=1,npxy
            read(79,*,END=8822,ERR=8822) xxxcov,yyycov
            jjjjco=jjjjco+1
            s(lxycova+2*i-2)=xxxcov
            s(lxycova+2*i-1)=yyycov
            enddo
 8822       continue
            npxy=jjjjco
            write(6,*) 'npxy',npxy
            call allody(npxy,1,'lsumcova',lsumcova,ipr)
            do i=1,npxy
            s(lsumcova+i-1)=0
            enddo
            call allody(ncvdata,1,'lwcova',lwcova,ipr)
!C read here weights
            do i=1,ncvdata
            s(lwcova+i-1)=1
            enddo
            jjjjco=0
       endif
       
       
       
!C if sum to be calculated, loop here over all "data" points; no not needed !
 3344  continue
      jjjjco=jjjjco+1

!c Boucle sur les points de grille ou l'erreur doit etre calculee
      close(80)
      rewind(80)
      open(unit=80,file='fort.80')
      index=0
      jmcount=0
!C Only if ispec=1 3 5 or 7
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 100

      
 10   read(80,*,end=100)xob,yob,iel,isub
      val=0
!C      write(6,*) 'Errors in',xob,yob,iel,isub
      jmcount=jmcount+1
      
      if(mod(jmcount,max(nx*ny/10,1)).eq.0) write(6,*) 'proceeded gridded points:', nint(jmcount*100./(nx*ny)), ' percent'

!C IF Point in mesh
      if(iel.gt.0) then
!C	       write(6,*) 'Point in element',iel,isub,xob,yob
         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         if(ipr.gt.2) write(6,*) 'calgel'
         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)
!C Save right hand side
         do i=1,nddlt
         s(lcvg+i-1)=s(ltrhsg+i-1)
         enddo
         STSB=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         if (ipr.gt.2) write(6,*) 'sol'
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh), nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
	    endif 
         endif
!C Ici boucle sur le reste des points et decision sur fichier fortran de sortie
!C 72, points eux memes, 73 avec les points du fichier 45
!C
      valcv=val
!CJMB BUG??17/7/8 NO, OK
!c      valcv=0
!C JMBBUGEND?
      if(isumcovar.ne.1) then
      if(ipipe.eq.1) then
      open(61,file='../divapipe',form='unformatted')
      rewind(61)
      endif
      
       
      write(61) xob,yob,val/(1+valcv)
      endif
      rjmc0=val/(1+valcv)
!c               write(6,*) '?jmc0a',rjmc0,xob,yob
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
       if (ipipe.eq.1) then
       close(61)
       call pingpong(1,1,0)
!c       write(6,*) 'Now waiting covar'
       call pingpong(1,0,0)
       endif
                         else
      write(6,*) 'Why should I be here?'
      endif
       
       
      endif
!C END if point in mesh

      
      goto 10

 100  continue
!C
!CJMBB now try to put out error field at data points....: DO NOT USE FORT.20
!C USE FORT 45 or directly coordinates already stored... (take care of coordinate change?)
!C Maybe the problem with marina ? Check with and without coordinate change...
!C Only if ispec=2 3 6 or 7
         if((ispec.eq.1).or.(ispec.eq.4).or.(ispec.eq.5)) goto 866
         write(6,*) 'now covariance at data locations'

         
         
         do 1866 iiiii=1,ncvdata
         x=s(lcvpoir+2*iiiii-2)
         y=s(lcvpoir+2*iiiii-1)
         iel=l(lcvpoii+2*iiiii-2)
         isub=l(lcvpoii+2*iiiii-1)
         if(mod(iiiii,max(ncvdata,10)/10).eq.0) write(6,*) 'proceeded', nint(iiiii*100./(ncvdata)), ' percent'

       val=0         
!C now calculate the error in x,y
       xob=x
       yob=y
!C is in mesh?
      if(iel.gt.0) then
	       
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 156        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         valcv=val
!C JMBBUG: NO
!c         valcv=0
!C
         rjmc0=val/(1+valcv)
!c                  write(6,*) '?jmc0b',rjmc0,val,xob,yob
         if(isumcovar.ne.1) then
         if(ipipe.eq.1) then
         open(61,file='../divapipe',form='unformatted')
       rewind(61)
         endif
      write(61) xob,yob,val/(1+valcv)
         endif
      
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
      if (ipipe.eq.1) then
      close(61)
       call pingpong(1,1,0)
       call pingpong(1,0,0)
      endif
                        else
       do i=1,ncvdata
       wwwjco=s(lwcova+iiiii-1)
       wwwico=s(lwcova+i-1)
       doublesum=doublesum+s(lcova+i-1)*wwwjco*wwwico*1./(1.-rjmc0)
       enddo
      
      endif
      
      
      endif
      
!C      goto 666
         

 1866  continue
  866     continue


!CJMBE

!CJMBB now try to put out error field at other discrete locations...
!C only of ispec= 4 5 6 7
         if((ispec.eq.1).or.(ispec.eq.2).or.(ispec.eq.3)) goto 867
         write(6,*) 'Finally covariance at desired discrete locations'
         ireclu=0
         rewind(79)
         jjiijj=0
 667      read(79,*,end=867) x,y
         jjiijj=jjiijj+1
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(mod(ireclu,100).eq.0) write(6,*) 'proceeded',ireclu, ' points'

         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=0
               write(73,*) x_ll,y_ll,valex
               goto 667
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=0
            write(73,*) x_ll,y_ll,valex
            goto 667
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y
!C point in mesh
      if(iel.gt.0) then
	       
         do 157 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
157        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         valcv=val
!C JMBBUG? NO see paper on efficient calculation
!c         valcv=0
!C END
         rjmc0=val/(1+valcv)
!c         write(6,*) '?jmc0',rjmc0,xob,yob
         if(isumcovar.ne.1) then
         if (ipipe.eq.1) then
         open(61,file='../divapipe',form='unformatted')
       rewind(61)
       endif
      write(61) xob,yob,val/(1+valcv)
         endif
      
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
      if (ipipe.eq.1) then
      close(61)
       call pingpong(1,1,0)
       call pingpong(1,0,0)
      endif
                         else
      do i=1,ncvdata
      wwwiii=s(lwcova+i-1)*1./(1.-rjmc0)
      s(lsumcova+jjiijj-1)=s(lsumcova+jjiijj-1)+wwwiii*s(lcova+i-1)
      enddo
      endif
      
      
      endif
!C end point in mesh
      
      goto 667
         


  867     continue

 1991 continue
 
      if(isumcovar.ne.1) then
      call flush(61)
      if (ipipe.eq.1) then
      write(6,*) 'Last call'
!c      call pingpong(1,1,0)
      write(6,*) 'Getting out'
      endif
      endif
      
      
      if (isumcovar.eq.1) then

!c      if (jjjjco.lt.ncvdata) then
!c      write(6,*) 'next point'
!c      goto 3344
!c                          else
      write(6,*) 'Saving summed covariances',npxy,ncvdata
      
      do i=1,npxy
      write(65,*) s(lsumcova+i-1)
      enddo
      write(64,*) doublesum
!C Save here
!c      endif
      endif
      
      
      
      return
      end

      subroutine cvread(lcvelem,cvxy,ncvdata)
      include'divapre.h'
      include'divainc.h'
      dimension cvxy(2,*)
      dimension lcvelem(2,*)
      ncvdata=0   
 10   continue
      read(45,*,end=99) xxx,yyy
      ncvdata=ncvdata+1
      if (icoordchange.ne.0) call llxy(xxx,yyy)
      cvxy(1,ncvdata)=xxx
      cvxy(2,ncvdata)=yyy
!c      write(6,*) 'in cvread',cvxy(1,ncvdata),cvxy(2,ncvdata)
         ipr=1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
            endif
            if (opti.eq.0) then 
               call locpt2(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(xxx,yyy,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(xxx,yyy,tcoog,kconn,tcele,iel,isub,ipr)
            endif


         endif
         lcvelem(1,ncvdata)=iel
         lcvelem(2,ncvdata)=isub
!c         write(6,*) 'in cvread',cvxy(1,ncvdata),cvxy(2,ncvdata)
!c         write(6,*) 'in cvread',lcvelem(1,ncvdata),lcvelem(2,ncvdata)
         goto 10
  99     continue
         write(6,*) ' Covariance with ',ncvdata, ' points'
         return
         end
      

      subroutine calgelcv(trhsg,tcoog,trhse,kconn,ipr,xob,yob,iel,isub)
!C
!C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY RHS AND
!C  ASSEMBLING IN THE GLOBAL SECOND MEMBER OF THE SYSTEM
!C
      include'divapre.h'
      include'divainc.h'
      dimension trhsg(nddlt),tcoog(nnt1,2),trhse(nddle),kconn(nelt,nnel)
      one=1.0D0
      three=3.0D0
      five=5.0D0
      
      do  i=1,nddlt
       trhsg(i)=0
      enddo
      do 5 i=1,nddle
	 trhse(i)=0.
 5    continue

!C
!C  OPEN FILE FOR READING CONDENSATION VECTORS (FOR RHS CONDENSATION)
!C      do 10 iel=1,nelt
 

!C JMB Only do it for the element iel and subelement isub of xob, yob ...
        xxx=xob
        yyy=yob
!c        if(ityp.eq.2) then
!c            if (opti.eq.1) then 		! (SvL)
!c               call locpt2opti(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,
!c     &                         l(lkntc),ipr)
!c            endif
!c            if (opti.eq.0) then
!c               call locpt2(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,ipr)
!c            endif
!c         endif
!c         if(ityp.eq.3) then
!c            if (opti.eq.1) then 		! (SvL)
!c            call locpt3opti(xxx,yyy,tcoog,kconn,tcele,iel,isub,kntc,
!c     &                      ipr)
!c            endif
!c            if (opti.eq.0) then
!c            call locpt3(xxx,yyy,tcoog,kconn,tcele,iel,isub,ipr)
!c            endif
!cc
!c
!c         endif
!cC
         if(ityp.eq.2) then
           call cgelecv2 (iel,tcoog,kconn,trhse,l(lklocs),ipr,s(lrkele),xob,yob,isub,s(ltprop))
         endif
         if(ityp.eq.3) then
           call cgelecv3 (iel,tcoog,kconn,trhse,l(lklocs),s(ltcele),ipr,s(lrkele),xob,yob,isub,s(ltprop))
         endif

         IF(IPR.GE.4) then
            WRITE (6,406) iel
         endif
 406     FORMAT(///,T10,' ELEMENTARY  RHS  FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(trhse,nddle,1,nddle,6)
!C
!C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
!C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
!C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
!C
!c         if (iel.eq.500) then
!c         write(6,*) 'into calloc',nddle,lkloce,lkconn
!c         endif
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         ikg=0
         ifg=1
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue

      return
      end





      subroutine cgelecv2 (iel,tcoog,kconn,trhse,loces,ipr,trkele,xob,yob,isub,tprop)
!C
!C  INTEGRATE ELEMENTARY RHS WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(12),kconn(nelt,nnel),x(0:3),y(0:3),rll(0:3),TRKELE(3,12,*)
      dimension tprop(nnt1,nnpr)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(15),tgse(10),loces(3,10),tr(3,12)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      zero=0.D0
      do 10 i=1,15
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
!C
      x0=x(0)
      y0=y(0)
      rll0=rll(0)
!C  First sub-element
      if(isub.eq.1) then
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
!c      if(iel.eq.500) then
!c      write(6,*) 'another cgsel2',iel,x0,y0,x1,y1,x2,y2
!c      write(6,*) 'another  k',kconn(iel,1),kconn(iel,3),kconn(iel,5)
!c      write(6,*) '??',tcoog(kconn(iel,1),2),nnt1
!c      endif
      call cgselcv2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
      endif
!C  Second sub-element
!C
      if(isub.eq.2) then
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cgselcv2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
      endif
!C
!C  Third sub-element
!C
      if(isub.eq.3) then
      x1=x(3)
      y1=y(3)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(1)
      endif
      call cgselcv2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
      endif
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.ge.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (15) '
         call impmat(tge,15,1,15,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
!c        IF (IJMB.EQ.0) STOP "KELE OPTI CRASHED"
!C        if(ijmbw(iel).ne.1) stop 'bizzare'
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (15) ==> (12)
!C
      do 160 i=1,12
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+12)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
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



      subroutine cgelecv3(iel,tcoog,kconn,trhse,loces,tcele,ipr,trkele,xob,yob,isub,tprop)
     
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(16),kconn(nelt,nnel),x(0:4),y(0:4),tcele(nelt,2),rll(0:4),TRKELE(3,12,*)
      dimension tprop(nnt1,nnpr)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(19),tgse(10),loces(4,10),tr(3,16)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
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
      
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(4)=tprop(kconn(iel,7),1)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(4)=tprop(kconn(iel,7),3)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      
      do 10 i=1,19
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 4 (10) SUB-ELEMENT RHS
!C
      x0=x(0)
      y0=y(0)
      rll0=rll(0)
!C
!C  First sub-element
!C
      if(isub.eq.1) then
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
      if(iel.eq.500) then
      write(6,*) 'into cgsel',iel,x0,x0,x1,y1,x2,y2
      endif
      call cgselcv2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
      endif
!C
!C  Second sub-element
!C
      if(isub.eq.2) then
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cgselcv2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
      endif
!C
!C  Third sub-element
!C
      if(isub.eq.3) then
      x1=x(3)
      y1=y(3)
      x2=x(4)
      y2=y(4)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(4)
      endif
      call cgselcv2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
      endif
!C
!C  Fourth sub-element
!C
      if(isub.eq.4) then
      x1=x(4)
      y1=y(4)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(4)
         rll2=rll(1)
      endif
      call cgselcv2(4,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
 131  continue
      endif
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.gt.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (19) '
         call impmat(tge,19,1,19,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!C        if(ijmbw(iel).eq.0) stop 'etrange'
!c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (19) ==> (16)
!C
      do 160 i=1,16
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+16)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
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

      subroutine cgselcv2(isub,iel,tgse,x0,y0,x1,y1,x2,y2,kindt,kdata,kelos,tdata,ipr,xob,yob,rll0,rll1,rll2)
!C
!C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tgse(10),kindt(nelt),kdata(ndata),kelos(ndata,2),tdata(ndata,4),tjaci(2,2),tjac(2,2),tr(10,10),gtemp(10),ep(10)
!C       rlrel,rll0,rll1,rll2
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

!C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
!C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CGSELcv2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
!c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
!c         write(6,*) 'Detj now',detj
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
!C
!C  CONTRIBUTION OF THE DATA POINTS
!C
      rlrel=1
      if(itcs.eq.2.or.itcs.eq.3) then
      rlrel=(rll0+rll1+rll2)/trois
      rlrel=(1.D0/(rll0*rll0)+1.D0/(rll1*rll1)+1.D0/(rll2*rll2))/trois
      rlrel=1.D0/sqrt(rlrel)
      endif

         xo=xob
         yo=yob
         dodo=1
!c         write(6,*) '???covar',iel,RL0
         wo=4*3.141592/RL0/RL0/rlrel/rlrel
         if(ipr.ge.4) then
             write(6,151)xo,yo,dodo,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';yo=',f8.2,';do=',f6.4,';wo=',f10.4)
!C
!C Transformation of the data position in reference element
!C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
!C
!C  CONTRIBUTION FROM PSEUDO-OBSERVATIONS
!C
!CJMB TODO if profiling show too many calculations just above:
!C Store here ep(i) for each data point (idata) and sub-element into big matrix and once stored, skip all the stuff above
!C when contribution at data point is needed
!C right store ep(i,iel,isubelem) should be enought?? Store it from analysis step already
!C
         do 160 i=1,10
!C         ep(i)=TRKELEE(i,idata)
            tgse(i)=tgse(i)+wo*dodo*ep(i)
 160     continue
 150  continue
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
!C
!C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
!C  TR IS THE TRANSFORMATION MATRIX
!C
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
!C
!C TRANSFORMATION OF TGSE
!C JMB TRY TO SAVE TR as for condensation ?? Or is it already this matrix? CHECK if written only
!C only once and used in the same way; if so, try to use it here and not calculate it. Should work
!C since data location invariant during whole process??, not really the same matrix.
!C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
!C         gtemp(i)=gtemp(i)+TRKELEB(k,i,isub,iel)*tgse(k)
         gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue

      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end


         subroutine pingpong(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

!c          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           
!c           call system('sleep 0.001')
           read(89,100) istherea
!c           write(6,*) 'w: waiting',istherea
           
           irwtd=irwtd+1
           
           
           
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           
!c           call system('sleep 0.001')
           read(88,100) isthere
!c           write(6,*) 'r: waiting',isthere
           
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
!c          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end
          
          
         subroutine pingpongd(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           rewind(88)
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           rewind(89)
!c           call system('sleep 0.001')
           read(89,100,end=11,err=11) istherea
!c           write(6,*) 'w: waiting',istherea
           if(istherea.eq.0) goto 11
           irwtd=irwtd+1
           
           rewind(88)
           write(88,100) 0
           
           call flush(88)
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           rewind(89)
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           rewind(88)
!c           call system('sleep 0.001')
           read(88,100,end=111,err=111) isthere
!c           write(6,*) 'r: waiting',isthere
           if(isthere.eq.0) goto 111
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           rewind(89)
           write(89,100) 0
           
           call flush(89)
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end
