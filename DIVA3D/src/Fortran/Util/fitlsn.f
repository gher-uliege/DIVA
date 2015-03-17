        integer nm
        parameter(nm=5000000)
        
        real*8 x(nm),y(nm),d(nm),work(nm)
        real*8 iww(nm)
        integer iflagw
        real*8 w2(nm),w3(nm)
        real*8 iw(nm),dmin,dmax,rqual
        write(6,*) ' Into fitting '

        icoord=0
        ireg=0
C2015JMB 
C       Put zero to go back to non weighted fittign
        iflagw=1
C
C Testing if 4 columns are there
        iread=4
         read(10,*,end=123,err=123) xxx,yyy,zzz,www
         write(6,*) 'Four data columns present'
         goto 124
 123    continue
        write(6,*) 'No weigth present will use ones'
        iread=3
 124    continue
        rewind(10)
        if (iflagw.eq.0) iread=3




        nsamp=0
        read(5,*,end=888,err=888) nsamp
        write(6,*) 'Will try to use', nsamp*(nsamp-1)/2, ' couples'
 888    continue
        

        read(11,*,end=11,err=11) rcoord
        read(11,*,end=11,err=11) ireg
        if (rcoord.lt.0) icoord=-1
        if (rcoord.gt.0) icoord=1
        if (rcoord.gt.1.5) icoord=2
 11     continue
        write(6,*) 'Icoord',icoord
        sn=1
        rl=1
        n=0
        dmin=10D30
        dmax=-dmin
 1      continue
        if (iread.eq.4) then
        read(10,*,end=99) x(n+1),y(n+1),d(n+1),iww(n+1)
                        else
        read(10,*,end=99) x(n+1),y(n+1),d(n+1)
        iww(n+1)=1
        endif

        dmin=min(dmin,d(n+1))
        dmax=max(dmax,d(n+1))
        n=n+1
        if (n.ge.nm) then
        write(6,*) 'Open fitsnl.f and increase parameter nm'
        stop
        endif
        goto 1
 99     continue
        write(6,*) 'Data range :',dmin,dmax
        if (icoord.eq.1) then
        write(6,*) 'Into coordinate change'
        call changec(x,y,n,icoord)
        endif
        if (ireg.eq.2) then
        write(6,*) 'Linear regression '
        call linreg(x,y,d,n)
        endif
        call lfit(x,y,d,iww,
     &  n,rl,sn,varbak,work,w2,w3,iw,icoord,rcoord
     & ,nsamp,rqual)
        if(icoord.eq.1) then
        RL=RL/6400*180/3.14
        endif

        write(6,*) 'RL,SN,VARBAK',RL,SN,VARBAK
        if (icoord.eq.1) then
        write(66,*) 'Correlation length (in degrees latitude)'
                         else
        write(66,*) 'Correlation length'
        endif
        write(66,*) RL
        write(66,*) 'Signal to noise ratio'
        write(66,*) SN
        write(66,*) 'VARBAK'
        write(66,*) VARBAK
        write(66,*) 'Quality of the fit (0: bad 1: good)'
        write(66,*) rqual
        if (icoord.ge.1) then
        write(66,*) 'For information: correlation length in km is',
     &      RL*6400.*3.14/180.
        write(6,*) 'For information: correlation length in km is',
     &      RL*6400.*3.14/180.

             endif
        stop
        end
        subroutine changec(x,y,n,icoord)
        real*8 x(n),y(n)
        xmin=1E30
        xmax=-xmin
        ymin=xmin
        ymax=-ymin
        
        do i=1,n
        if (x(i).lt.xmin) xmin=x(i)
        if (x(i).gt.xmax) xmax=x(i)
        if (y(i).lt.ymin) ymin=y(i)
        if (y(i).gt.ymax) ymax=y(i)
        enddo
        ylat=(ymax+ymin)/2.
        dx=cos(ylat*3.1415/180.)
        write(6,*) 'dx',dx,ylat,xmin,xmax,ymin,ymax
        
        do i=1,n
        x(i)=(x(i)-xmin)*3.1415/180.*6400.*dx
        y(i)=(y(i)-ymin)*3.1415/180.*6400.
        enddo
        return
        end
        
        subroutine lfit(x,y,d,iww,n,rl,sn,varbak,work
C23456
     &    ,w2,w3,iw,icoord,rcoord,nsamp,rqual)
c
        real*8 x(n),y(n),d(n),iww(n)
        real*8 work(n),maxdist
        real*8 w2(n),w3(n)
        real*8 variance,datavar
        real*8 iw(n)
        real*8 meandist,rjjj,datamean,rjjjbis
        real*8 dist,rnbins,rn,rqual
        real*4 randf
        parameter(nopt=64000)
        parameter(nop=10000)
        real*4 distcouples(nop*nop/2)
        integer*4 icouples(nop*nop/2)
        integer*4 jcouples(nop*nop/2)
       if(nsamp.ne.0) then
       write(6,*) 'will generate random couples'
       if (nsamp.gt.n) then
       write(6,*) 'Strange to ask for more samples than available'
       write(6,*) 'from data; will proceed'

       endif
       if (nsamp.gt.nop) then
       write(6,*) 'too many asked, will only make',nop*(nop-1)/2
       nsamp=nop
       endif
       ii=n
       call mysrand(ii)
       endif
       j=nint((n-1)*randf())+1
        maxdist=0
        meandist=0
        datamean=0
        datavar=0
        dist=0
        rjjj=0
        if (n.gt.10000.and.nsamp.ne.0) then
        write(6,*) 'Be patient big data set: ',n
        endif
        rn=0
        do i=1,n
        datamean=datamean+d(i)
     & *iww(i)
        datavar=datavar+d(i)*d(i)
     & *iww(i)
        rn=rn+iww(i)
        enddo
        datamean=datamean/rn
C        rn=n
        variance=datavar/rn - datamean**2
        write(6,*) 'Number of data points : ', n
        write(6,*) 'data mean', datamean
        write(6,*) 'data variance ',variance
        write(6,*) 'Now calculating distance distribution'
        if(nsamp.eq.0) then
        iiii=0
        do i=1,n

        nww=max((n/10),1)
        if(mod(i,nww).eq.0) write(6,*) i,' out of', n
        do j=i+1,n
        dist=0
         if(icoord.eq.0.or.icoord.eq.1) then
         dist=(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         if(icoord.eq.2) then
         call greatarc(x(i),y(i),x(j),y(j),dist)
         endif
         if(icoord.eq.-1) then
         dist=rcoord*rcoord*(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         if(n.le.nop) then
         iiii=iiii+1
         distcouples(iiii)=dist
         icouples(iiii)=i
         jcouples(iiii)=j
         endif

         meandist=meandist+dist
         if (dist.gt.maxdist) maxdist=dist
         enddo
        enddo
        rjjjbis=rn*(rn-1.D0)*0.5
        rjjj=rjjjbis
                      else
        do iiii=1,nsamp*(nsamp-1)/2
         j=nint((n-1)*randf())+1
 4444    continue
         i=nint((n-1)*randf())+1
C         write(6,*) '??',i,j
         if(i.eq.j) goto 4444
         if(icoord.eq.0.or.icoord.eq.1) then
         dist=(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         if(icoord.eq.2) then
         call greatarc(x(i),y(i),x(j),y(j),dist)
         endif
         if(icoord.eq.-1) then
         dist=rcoord*rcoord*(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         
         distcouples(iiii)=dist
         icouples(iiii)=i
         jcouples(iiii)=j
         

         meandist=meandist+dist
         if (dist.gt.maxdist) maxdist=dist
         enddo
        rjjj=nsamp*(nsamp-1)/2

        
        
        
                      
                      
        endif
        
        write(6,*) 'Number of data couples considered',rjjj
        meandist=meandist/rjjj


                
        write(6,*) 'maximum distance between points: ',maxdist
        
        write(6,*) 'Mean distance between points :',meandist
        if (nsamp.eq.0) then
        rnbins=(min(80.D0,rn*rn/maxdist*meandist/20.D0))
        else
        rnbins=(min(80.D0,nsamp*nsamp/maxdist*meandist/20.D0))
        endif
        write(6,*) 'Number of probable active bins:',rnbins
        
        ddist=meandist/rnbins
        nbmax=maxdist/ddist+1
        write(6,*) 'distance for binning: ',ddist
        write(6,*) 'maximum number of bins: ',nbmax
        if(nsamp.eq.0) then
        write(6,*) 'Average number of pairs in each bin', rn*rn/nbmax/2
        else
        write(6,*) 'Average number of pairs in each bin', 
     &      nsamp*nsamp/nbmax/2
        endif
        do ii=1,nbmax
        work(ii)=0
        w2(ii)=0
        iw(ii)=0
        enddo
        if(nsamp.eq.0) then
        iiii=0
        do i=1,n                           
        nww=max((n/10),1)
        if(mod(i,nww).eq.0) write(6,*) i,' out of', n
         do j=i+1,n
         if(n.le.nop) then
         iiii=iiii+1
         dist=distcouples(iiii)
                      else
         if (icoord.eq.0.or.icoord.eq.1) then
         dist=(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         if(icoord.eq.2) then
         call greatarc(x(i),y(i),x(j),y(j),dist)
         endif
         if(icoord.eq.-1) then
         dist=rcoord*rcoord*(x(i)-x(j))**2+(y(i)-y(j))**2
         dist=sqrt(dist)
         endif
         endif
         nb=dist/ddist+1
         work(nb)=work(nb)+(d(i)-datamean)*(d(j)-datamean)
     & *iww(i)*iww(j)
         w2(nb)=w2(nb)+((d(i)-datamean)*(d(j)-datamean))**2
     & *iww(i)*iww(j)
         iw(nb)=iw(nb)+1
     & *iww(i)*iww(j)
         enddo                             
        enddo           
                   else
        do iiii=1,nsamp*(nsamp-1)/2
        dist=distcouples(iiii)
        i=icouples(iiii)
        j=jcouples(iiii)
        nb=dist/ddist+1
         work(nb)=work(nb)+(d(i)-datamean)*(d(j)-datamean)
     & *iww(i)*iww(j)
         w2(nb)=w2(nb)+((d(i)-datamean)*(d(j)-datamean))**2
     & *iww(i)*iww(j)
         iw(nb)=iw(nb)+1
     & *iww(i)*iww(j)
        
        enddo
        endif
        ifo=0
        w3mean=0
        do nn=1,nbmax
         w3(nn)=0
        if (iw(nn).gt.0) then
         work(nn)=work(nn)/iw(nn)
         w2(nn)=w2(nn)/iw(nn)-work(nn)**2
         if(w2(nn).gt.1E-8*work(nn)**2) w3(nn)=1/w2(nn)**2*(iw(nn)-1)
C Uniform weight
         w3mean=w3mean+w3(nn)
        endif
        enddo
        do nn=1,nbmax
c        write(6,*) '??',nn,w3(nn),w3mean,nbmax
        w3(nn)=w3mean/nbmax+w3(nn)
        if(iw(nn).lt.1) w3(nn)=0

c        w3(nn)=1./nn
c        w3(nn)=exp(-(nn-10)**2/100))
        enddo
        
        do jj=1,3
        workm=work(1)   
        do nn=1,nbmax
        nnp=min(nbmax,nn+1)
        workf=work(nn)+0.25*(work(nnp)+workm-2*work(nn))
        workm=work(nn)
        work(nn)=workf
        enddo
        enddo
        
        do nn=1,nbmax
        nnp=min(nbmax,nn+1)
C if not working force simple use of variance
        ncross=5
C
        write(99,147) (nn-1)*ddist,work(nn),iw(nn),2*w2(nn)/
     &    sqrt((max(iw(nn),1.D0))),w3(nn)
 147    format(5(E14.6))
        if ((ifo.eq.0).and.(iw(nn).ne.0).and.(work(nn).lt.0).
     & and.(nn.gt.4)) then
        write(6,*) 'First zero crossing',nn,ddist,nn*ddist

        rlz=ddist*nn
        ifo=1
        ncross=nn
        endif
        enddo   
        
        write(55,*) 'set xrange[',0,':',1.2*rlz,']'
        
C Now try to fit Bessel function using only the data from ddist to zero-crossing.
C Then extrapolate to zero to get S/N ratio                
        write(6,*) 'Now trying to fit Bessel covariance function'
        errmin=1.E35
        VAR=variance
        RL=rlz
        
        x0=RLz/20
        dx=ddist
        nstart=max(x0/dx+1,2.)
        x0=(nstart-1)*dx
        np=ncross*0.95-0*nstart
        
        write(6,*) 'Nstart',nstart,np,x0,RLz,x0+(np-1)*ddist
        
        do jj=1,1
         do ii=1,1000
	 VARtest=variance ! 17/03/2015
         RLtest=RLz/10+(ii-1)*RLz/500.
C         VARTEST=Variance*jj/200.

        if (np.lt.10) then
        write(6,*) '!!!!!!!!!!!!!!!!!!'
        write(6,*) '?? too few data ??'
        write(6,*) ' will use guesses '
        write(6,*) '!!!!!!!!!!!!!!!!!!'
        RL=RLz
        VAR=0.01*Variance
        SN=VAR/(Variance-VAR+1.E-10)
        iwr=2


!! mo patch
        if(np.eq.0) then
        iwr=2
        call forfit(x0,dx,work(nstart),w2(nstart),1,RL,VAR,err,iwr,
     &    w3(nstart))
        return
        endif
!! mo patch

        call forfit(x0,dx,work(nstart),w2(nstart),np,RL,VAR,err,iwr,
     &    w3(nstart))
        return
        
        endif
        iwr=0
        call forfit(x0,dx,work(nstart),w2(nstart),np,RLtest,
     & VARtest,err,iwr,w3(nstart))
C        write(6,*) 'RL??',RLtest,VARtest,err,errmin
        if (err.lt.errmin) then
               
        RL=RLtest
        VAR=VARtest
        errmin=err
        endif
        
        enddo
        enddo

        write(6,*) 'Best fit:',RL,VAR
        if (VAR.GT.0.9999*Variance) then
        VAR=Variance
        SN=10000
                             else
        SN=VAR/(Variance-VAR+1.E-10)
        endif
        write(6,*) 'S/N',SN
        write(6,*) 'Relative misfit of fit',sqrt(errmin)/VAR
        rqual=1-sqrt(errmin)/VAR
        varbak=var
        iwr=1
        call forfit(x0,dx,work(nstart),w2(nstart),np,RL,VAR,err,iwr,
     &    w3(nstart))
        return
        end
        
        subroutine forfit(x0,dx,c,w2,n,RL,VAR,err,iwr,w3)
        real*8 c(n)
        real*8 w2(n),w3(n),ww3
        real*8 bessk1

C        write(6,*) 'forfit .... ',RL,dx,n!,var

        err=0
        errb=0
        do i=1,n
        eps=(x0+(i-1)*dx)/RL
        errb=errb+eps*bessk1(eps)*w3(i)
        err=err+c(i)*w3(i)
        
        enddo
C        write(6,*) 'TestVAR',err/errb

        if(iwr.lt.2) then
        VAR=err/errb
c       
        endif
        
        err=0
        errb=0
        ww3=0
        do i=1,n
        eps=(x0+(i-1)*dx)/RL
        errb=c(i)-var*eps*bessk1(eps)
        err=err+errb*errb*w3(i)
        ww3=ww3+w3(i)
        if (iwr.ge.1) then
        write(98,147) eps*RL,c(i),var*eps*bessk1(eps),w3(i)
        endif
        enddo
c        write(6,*) 'during fitting',err,RL,VAR,n
        
C       
 147    format(4(E16.7))
        err=err/ww3
        return
        end
        
      function bessk1(X)
      
      implicit real*8 (a-h,o-z)
      real*4 x
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,
     &     -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,
     &     0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/

      EXTERNAL BESSI1

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

            implicit real*8 (a-h,o-z)
            real*4 x

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

      SUBROUTINE LINREG (x,y,dd,ndata)

      PARAMETER (NP = 5)

      real*8 x(NDATA),y(ndata),dd(ndata)
      REAL*8 XMEAN,TOTDAT,SX,SY,SXY,SX2,SY2,SV,SXV,SYV
      REAL*4 A(NP,NP), B(NP)

      INTEGER*4 INDX(NP)
C JMB I put D as REAL??
      REAL*4 D
        
C Compute Mean Value
         TOTDAT = 0.
         SX  = 0.
         SY  = 0.
         SXY = 0.
         SX2 = 0.
         SY2 = 0.
         SV  = 0.
         SXV = 0.
         SYV = 0.
         DO 20 I = 1,NDATA
               TOTDAT = TOTDAT + 1.
               SX  = SX  + x(I)
               SY  = SY  + y(I)
               SXY = SXY + x(I)*y(I)
               SX2 = SX2 + x(I)*x(I)
               SY2 = SY2 + y(I)*y(I)
               SV  = SV  + dd(I)             
               SXV = SXV + x(I)*dd(I)
               SYV = SYV + y(I)*dd(I)
 20      CONTINUE

         A(1,1) = TOTDAT
         A(1,2) = SX
         A(1,3) = SY
         A(2,2) = SX2
         A(2,3) = SXY
         A(3,3) = SY2
         A(2,1) = A(1,2)
         A(3,1) = A(1,3)
         A(3,2) = A(2,3)
         B(1) = SV
         B(2) = SXV
         B(3) = SYV

         CALL LUDCMP (A,3,NP,INDX,D)
         IF (D.NE.0) THEN
         CALL LUBKSB (A,3,NP,INDX,B)
                     ELSE
         
         B(1)=B(1)/TOTDAT
         B(2)=0
         B(3)=0
         write(6,*) 'Using average value as reference',B(1)
         ENDIF
         
         
         DO 21 I = 1,NDATA
            dd(I)=dd(I) 
     &                  -B(1) - B(2) * x(I) - B(3) * y(I)
21       CONTINUE
         DO 22 I=1,3
            D = D*A(I,I)
22       CONTINUE
         WRITE (22,*) B(1),B(2),B(3)
         WRITE (22,*) 'Total Nb  of data : '
         WRITE (22,*) NDATA
         WRITE (22,*) 'Nb of Inside data : '
         WRITE (22,*) TOTDAT
         WRITE (22,*) 'Determinant of the Matrix :'
         WRITE (22,*) D
         CLOSE (22)
      return
      END
              

C -------------------------------------------------
C --- LUDCMP & LUBKSB :
C ---                   LU Matrix Decomposition
C ---                   and Backward Substitution
C ---
C --- Numerical Recipies (c)
C -------------------------------------------------

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) then
        D=0
        return
        endif
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
C JMB???
        imax=N
c        write(6,*) 'ludcmp',imax
C JMBE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      AAMAX=ABS(A(1,1))
      AAMIN=AAMAX
      DO I=1,N
      AAMAX=MAX(AAMAX,ABS(A(I,I)))
      AAMIN=MIN(AAMIN,ABS(A(I,I)))
      ENDDO
      IF(AAMIN.LE.1E-6*AAMAX) then
      write(6,*) 'Probably ill posed fit'
      D=0
      endif
      IF(A(N,N).EQ.0.) then
      A(N,N)=TINY
      D=0
      endif
      write(6,*) 'LUDCMP',AAMIN,AAMAX
      
      RETURN
      END

C ----------------------------------------------

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
      
       subroutine greatarc(rlon1,rlat1,rlon2,rlat2,dist)
       real*8 lon1,lon2,lat1,lat2,torad
       real*8 rlon1,rlon2,rlat1,rlat2,dist
       torad=3.14159/180.
       lon1=rlon1*torad
       lat1=rlat1*torad
       lon2=rlon2*torad
       lat2=rlat2*torad        
       dlon = lon2 - lon1
       dlat = lat2 - lat1
       a = (sin(dlat/2))**2 + cos(lat1) * cos(lat2) * (sin(dlon/2))**2
       c = 2 * asin(min(1.,sqrt(a)))
       dist=c/torad
       return
       end
      FUNCTION RANDF()
      
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
        RANDF = ISEED/FLOAT(IC)
      RETURN
      END
      subroutine mysrand(i)
      COMMON /CSEED/ ISEED
      iseed=i
      return
      end
