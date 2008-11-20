C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  GCVFAC (MODULE)
C     -  GCVRAN : constructing random data
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             GCVFAC MODULE                            C
C       Estimates the analysis error (same grid as the analysis)       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine gcvfac(ipr)
      include'divapre.h'
      include'divainc.h'
      real*8 d0d,d1d,d2d,d3d,d4d,atr,btr,ctr
      real*8 aaagcv,bbbgcv,ggggcv,lll1,lll2,lll3,lll4,ll1,ll2,ll3,ll4
      real*8 llll1,llll2,llll3,llll4
      real*8 gcvaln,vargcv,gcvala,gcvalb,gcval,zval,terr,terra
      real*8 lamnew(12)
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      zero=0.
      gcvala=0
      gcvalb=0
      terr=0
      terra=0
      
      read(10,*) jmbgcv
      ifac=0
      ifirst=1

         do 1009 jjj=1,jmbgcv
         call GCVRAN(s(ltdata),ipr,jjj)
	     zval=0
	     gcval=0
	     terr=0
         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue


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

c ... extraction de la solution au point observe

         ireclu=0
         rewind(20)
 666      read(20,*,end=866) x,y,tttt,wwww
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
c         write(6,*) 'gcval.?',ityp,opti
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
c               write(72,*) x_ll,y_ll,valex
               goto 666
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &               l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
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
c            write(72,*) x_ll,y_ll,valex
            goto 666
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &   l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
         endif
C only add up points that are located in the mesh
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
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

         jmboff=ndata*2+ireclu-1
c         write(6,*) 'data',s(ltdata+jmboff),jmboff,ireclu,ndata
         GCVAL=GCVAL+s(ltdata+jmboff)*(valb)
         terr=terr+valb*valb
         zval=zval+(s(ltdata+jmboff))*(s(ltdata+jmboff))
         goto 666
 866     continue
      gcval=gcval/(zval)
      terr=terr/zval
      gcvala=gcvala+gcval
      terra=terra+terr
 1009 continue
      gcvala=gcvala/jmbgcv
      terra=terra/jmbgcv
      atr=gcvala
      btr=terra
      ctr=atr*btr
      write(6,*) 'Trace average estimate:',gcvala
      write(6,*) 'rms of misfits',GCVALN
      
      
      
      
      
      
      
      
      
C Factor 1.4 to avoir understmoothing (see???)
      write(77,999) GCVALN/(1-1.0*GCVala)
     &           ,vargcv,GCVALA,GCVALN,(vargcv/gcvaln**2*(1-gcvala)-1)
     &           ,float(NDATL)
 999  format(6(E12.5))
 
 
 
 
 
C read input from fort.71
C subtract reference field (make sure coordinates are correct) 
C analyse
C calculate anomaly of analysis and finally calculate the diagnostics:
c
C prepare input data (anomalies)
         call GCVRDT(s(ltdata),ipr,jjj)
c
         do 415 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 415        continue


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

c ... extraction de la solution au point observe

         ireclu=0
         ijmbval=0
         rewind(20)
 466      read(20,*,end=566) x,y,tttt,wwww
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
c         write(6,*) 'gcval.?',ityp,opti
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
c               write(72,*) x_ll,y_ll,valex
               goto 466
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &                  l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
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
c            write(72,*) x_ll,y_ll,valex
            goto 466
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),
     &    l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
         endif
C only add up points that are located in the mesh
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
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF
         ijmbval=ijmbval+1
         jmboff=ndata*2+ireclu-1

         d2db=d2db+wwww/hmmu*
     &     s(ltdata+jmboff)*s(ltdata+jmboff)
         d3d=d3d+wwww/hmmu*
     &     s(ltdata+jmboff)*valb
         d4d=d4d+wwww/hmmu*
     &     valb*valb

         goto 466
 566     continue
         d2db=d2db/ijmbval
         d3d=d3d/ijmbval
         d4d=d4d/ijmbval
C now compute the diagnostics for new value of lambda
      RLAMJM=sqrt(1./alpha0)/(4*3.1415)*hmmu
      if(icoordchange.eq.1) RLAMJM=RLAMJM/dykm/dykm
      aaagcv=(d0d-d1d)
      bbbgcv=d0d
      cccgcv=(1-atr)*d0d
      lll1=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll1=max(1D-6,lll1)
      aaagcv=(d0d-2*d1d+d2d)
      bbbgcv=cccgcv
      cccgcv=(1-2*atr+btr)*d0d
      lll2=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll2=max(1D-6,lll2)
      aaagcv=(d0d-3*d1d+3*d2d-d3d)
      bbbgcv=cccgcv
      cccgcv=(1-3*atr+3*btr-ctr)*d0d
      lll3=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll3=max(1D-6,lll3)
      aaagcv=(d0d-4*d1d+6*d2d-4*d3d+d4d)
      bbbgcv=cccgcv
      cccgcv=(1-4*atr+6*btr-4*ctr+ctr*d3d/d2d)*d0d
      cccgcv=(1-4*atr+6*btr-4*ctr+btr*d4d/d2d)*d0d
      lll4=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll4=max(1D-6,lll4)
      ll1= d0d/(d0d-d1d)-1
      ll2= d0d/(d0d-2*d1d+d2d)*(1-atr)-1
      ll3= d0d/(d0d-3*d1d+3*d2d-d3d)*(1-2*atr+btr)-1
      ll4= d0d/(d0d-4*d1d+6*d2d-4*d3d+d4d)*(1-3*atr+3*btr-ctr)-1
      ll1=max(1D-6,ll1)
      ll2=max(1D-6,ll2)
      ll3=max(1D-6,ll3)
      ll4=max(1D-6,ll4)
      if(ll1.lt.rlamjm) then
        llll1=ll1
                        else
                 if(lll1.gt.rlamjm) then
                 llll1=lll1
                 else
                 llll1=(ll1+lll1)/2
                 endif
      endif
      if(ll2.lt.rlamjm) then
        llll2=ll2
                        else
                 if(lll2.gt.rlamjm) then
                 llll2=lll2
                 else
                 llll2=(ll2+lll2)/2
                 endif
      endif
      if(ll3.lt.rlamjm) then
        llll3=ll3
                        else
                 if(lll3.gt.rlamjm) then
                 llll3=lll3
                 else
                 llll3=(ll3+lll3)/2
                 endif
      endif
      if(ll4.lt.rlamjm) then
        llll4=ll4
                        else
                 if(lll4.gt.rlamjm) then
                 llll4=lll4
                 else
                 llll4=(ll4+lll4)/2
                 endif
      endif
      
      lamnew(1)=ll1
      lamnew(2)=ll2
      lamnew(3)=ll3
      lamnew(4)=ll4
      lamnew(5)=lll1
      lamnew(6)=lll2
      lamnew(7)=lll3
      lamnew(8)=lll4
      lamnew(9)=llll1
      lamnew(10)=llll2
      lamnew(11)=llll3
      lamnew(12)=llll4
      call sellam(lamnew,bestguess,guessflag)           
      write(27,*) 'S/N'
      write(27,*) bestguess
      write(27,*) 'Varbak'
      write(27,*) d0d/(1+bestguess)
      write(27,*) 'Quality of guess'
      write(27,*) guessflag
      write(27,*) 'Old value'
      write(27,999)     rlamjm
      write(27,*) 'proposed new values'
      write(27,999)     ll1,ll2,ll3,ll4
      write(27,999)     lll1,lll2,lll3,lll4
      write(27,999)     llll1,llll2,llll3,llll4
 
 
      return
      end
      subroutine sellam(lamnew,bestguess,qualflag)
      real*8 lamnew(12),bestguess,qualflag
      
      rm=0
      rvar=0
      do i=1,12
      lamnew(i)=log10(lamnew(i))
      rm=rm+lamnew(i)
      rvar=rvar+lamnew(i)*lamnew(i)
      enddo
      std=sqrt(rvar/12.-(rm/12.)**2)
      
      qualflag=1-std/(rm/12.)
      
      ii=12
 1    continue
      bestguessl=lamnew(ii)
      bestguess=10**lamnew(ii)
      
      if (abs(bestguessl-rm/12.).le.2*std) then
       return
      else
       if (ii.eq.1) then
          bestguess=10**(rm/12.)
          return
       else
         ii=ii-1
         goto 1
       endif
      endif
      return
      end

      subroutine GCVRAN(tdata,ipr,iii)

C RANDOM PSEUDO-DATA USED TO COMPUTE GCV ESTIMATION
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
      integer iseed
      COMMON /CSEED/ ISEED
      iseed=1000000+iii
C  INPUT OF DATA SET DESCRIPTION
            
      do 10 i=1,ndata/2
          call GRNF(x,y)
          tdata(i,3)=x
          tdata(ndata/2+i,3)=y
c          write(6,*) 'random',i,x
 10   continue
c if ndata is odd
          call grnf(x,y)
          tdata(ndata,3)=x
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
      SUBROUTINE GRNF(X,Y)
      include'divapre.h'
      integer iseed
      COMMON /CSEED/ ISEED
        PI = 4.0*ATAN(1.0)
        R1 = -LOG(1.0-RANF())
        R2 = 2.0*PI*RANF()
        R1 = SQRT(2.0*R1)
        X  = R1*COS(R2)
        Y  = R1*SIN(R2)
      RETURN
      END
C
      FUNCTION RANF()
      include'divapre.h'
      integer iseed,ia,ic,iq,ir
      COMMON /CSEED/ ISEED
      DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
      
        IH = ISEED/IQ
        IL = MOD(ISEED,IQ)
        IT = IA*IL-IR*IH
        IF(IT.GT.0) THEN
          ISEED = IT
        ELSE
          ISEED = IC+IT
        END IF
        RANF = ISEED/FLOAT(IC)
      RETURN
      END
      
      subroutine GCVRDT(tdata,ipr,iii)

C READ ANALYSED DATA AT DATA LOCATION
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
      
      rewind(71)
C  INPUT OF DATA SET DESCRIPTION
       IFIRST=1
       

            
      do 10 i=1,ndata
          read(71,*) x,y,val
          if (icoordchange.ne.0) call llxy(x,y)
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
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

          tdata(i,3)=valb
  10   continue
      return
      end
