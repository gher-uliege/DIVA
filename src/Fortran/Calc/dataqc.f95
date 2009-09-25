!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  DATAQC (MODULE)
!C     -  CALPSOK : computes pseudo data sets for quality check
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             DATQC  MODULE                            C
!C       Estimates of expected data-analysis differences                C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine dataqc(ipr)
      include'divapre.h'
      include'divainc.h'
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      zero=0.
      AIIAVER=0
      IIAVER=0
      write(6,*) 'Please be patient'

!C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP - Error estimation not validated for this mathematical problem '
	 stop
      endif

      accur=1.e-2
!c      write(6,*) 'divaqc, rl0',rl0
      rl0=sqrt(sqrt(1./alpha0))
!c      write(6,*) 'divaqc, rl0b',rl0
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.

!c      read(10,*)varbak
       rewind(15)
       read(15,*)varbak
       rewind(15)
!c      if(ipr.ge.2) write(6,*)' Variance of bakground =',varbak

 100  continue
!CJMBB now try to put out error field at data points....
         write(6,*) 'now expected analysis-data value at data locations'

         ireclu=0
         rewind(20)
 666      read(20,*,end=866) x,y,tttttt,wwwwww
         x_ll=x
         y_ll=y
         if(mod(ireclu,max(ndata,10)/10).eq.0) write(6,*) 'proceeded',nint(ireclu*100./(ndata)), ' percent'

         
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
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
               val=valex
               write(76,76) x_ll,y_ll,valex,valex
               goto 666
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
            val=valex
            write(76,76) x_ll,y_ll,valex,valex
            goto 666
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y
       
       if(iel.gt.0) then
            call calpsok(xob,yob,s(ltdata),ipr,ireclu)
!c	 write(6,*) 'exit calpsok',ireclu,ndata,ltrhsg
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 156        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)
!c           write(6,*) 'exit calgel',ireclu,ndata,ltrhsg
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
!c         write(6,*) 'try extracting from',iel,isub

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
!c               write(76,*) xob,yob,val,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
!c               write(76,*) xob,yob,val,val
	    endif 
         endif

      endif
!c esterror provides      sqrt(1.D0*varbak(1-val))
      val=1-val*val/varbak
!c      write(6,*) '???',x_ll,y_ll,val
!C VAL contains Aii ??
      errstd=max(1.D0-val,0.D0)
!c      write(6,*) '???',errstd
      rlm=rl0
      if (icoordchange.ne.0) rlm=rlm/dykm
      errstd=sqrt(4*3.1415*varbak/rlm/rlm/wwwwww*errstd)
      AIIAVER=AIIAVER+val
      IIAVER=IIAVER+1
      write(76,76) x_ll,y_ll,errstd,val
      goto 666
         


  866     continue

      write(6,*) "Exact trace average",AIIAVER/IIAVER,IIAVER
!c      write(76,*) AIIAVER/IIAVER,IIAVER

 76   FORMAT(4(E19.7))
      return
      end





      subroutine calpsok(xob,yob,tdata,ipr,idatap)

!C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

!C  INPUT OF DATA SET DESCRIPTION
      iiii=0
!c      write(6,*) '???',xob,yob,tdata(idatap,1),tdata(idatap,2)
      do 10 i=1,ndata
!c         call fcorrk(xob,tdata(i,1),yob,tdata(i,2),varbak,rl0,corre)
!c         tdata(i,3)=corre
!c         if(corre.gt.0) iiii=iiii+1
          tdata(i,3)=0
 10   continue
!c      if(iiii.gt.0) then
!c      do 11 i=1,ndata
!c      if(tdata(i,3).gt.0) write(6,*) 'corre',tdata(i,3),iiii
!c         tdata(i,3)=tdata(i,3)/iiii
!c 11   enddo
!c                    else
!c      write(6,*) ' ??? Strange'
!c      endif
       tdata(idatap,3)=varbak
!C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end

!c=======================================================================
      subroutine fcorrk(x1,x2,y1,y2,rm0,rl,corre)        
      
      include'divapre.h'
      
      external euclidist                                                
      external bessk1

      r=euclidist(x1,x2,y1,y2)                                        
      eps=r/rl    
                                                                        
      if (eps.le.0.001) then                                                 
         corre=rm0                                                     
      else
         corre=0
      endif                                                             
      return                                                            
      end                                                               
                                                                       
