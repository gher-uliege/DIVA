      include'../Calc/divapre.h'
      parameter(npmax=1000000)
      parameter(mmax=100000000)
      
      real*8 tbess(40000)
      
      real*8 x(npmax),y(npmax),d(npmax)
      real*8 work(mmax)
      common/tabbess/tbess
      call tabess
        
      xscale=1
      read(5,*) rcoordchange,ireg,RL
      write(6,*) 'command line parameters',rcoordchange,ireg,RL
      if (rcoordchange.lt.0) xscale=-rcoordchange
      np=0
 
      xmin=1E36
      ymin=1E36
      xmax=-1E36
      ymax=-1E36
      rm=0
 1    continue
      read(10,*,end=100,err=100) xxx,yyy,val
      np=np+1
      rm=rm+val
      if(xxx.lt.xmin) xmin=xxx
      if(yyy.lt.ymin) ymin=yyy
      if(xxx.gt.xmax) xmax=xxx
      if(yyy.gt.ymax) ymax=yyy

      x(np)=xxx
      y(np)=yyy
      d(np)=val
      goto 1
      
 100  continue        
      write(6,*) 'working on grid points',np
      if (ireg.eq.1) then
      rm=rm/np
      do i=1,np
            d(i)=d(i)-rm
      enddo
      endif
      if (ireg.eq.2) then
      call linreg(x,y,d,np)
      endif
      xmin=xmin-abs(xmin)/1000
      ymin=ymin-abs(ymin)/1000
      xmax=xmax+abs(xmax)/1000
      ymax=ymax+abs(ymax)/1000
      x0=xmin
      y0=ymin
      dy=RL/2
      dx=RL/2/xscale
      nx=(xmax-xmin)/dx+2
      ny=(ymax-ymin)/dy+2
      ns=nx*ny
      if(6*ns.gt.mmax) then
      write(6,*) 'severe error snbyrid needs more space'
      stop 'severe error snbyrid needs more space'
      endif
      write(6,*) 'Average number of points per bin',np/(nx*1.*ny)
      Mmin=30
      call sncalc(work(1),work(1+ns),work(1+2*ns),work(1+3*ns)   &
         ,work(1+4*ns),work(1+5*ns)                              &
         ,nx,ny,np,x,y,d,x0,dx,y0,dy,xscale,Mmin,RL)
      stop
      end
      subroutine sncalc(rnd,var,rmean,alpha,xmass,ymass,nxd,nyd,np,x,y,d,x0,dx,y0,dy,scale,Mmin,RL)
      include'../Calc/divapre.h'
      real*8 rnd(nxd,nyd)
      real*8 var(nxd,nyd)
      real*8 rmean(nxd,nyd)
      real*8 alpha(nxd,nyd)
      real*8 xmass(nxd,nyd)
      real*8 ymass(nxd,nyd)
      real*8 x(np),y(np),d(np)
      real*8 aa,bb,cc,dd,ee,ff,rnoise,rsignal
      parameter(nemax=10000)
      real*8 w(nemax),e(nemax,2),b(nemax)
      real*8 tbess(40000)
      common/tabbess/tbess
      nx=nxd
      ny=nyd
 2222 continue
      if(nx*ny.lt.1) then
      write(6,*) 'Too few bins, will stop'
      stop 'Severe error, unable to fit'
      endif
      
      do i=1,nx
      do j=1,ny
      var(i,j)=0
      rnd(i,j)=0
      rmean(i,j)=0
      alpha(i,j)=0
      xmass(i,j)=0
      ymass(i,j)=0
      enddo
      enddo
      
      
      do i=1,np
      ig=(x(i)-x0)/dx+1
      jg=(y(i)-y0)/dy+1
!c      write(6,*) ig,jg,x(i),y(i),x0,y0
      if(ig.lt.1) goto 999
      if(jg.lt.1) goto 999
      if(ig.gt.nx) goto 999
      if(jg.gt.ny) goto 999
      rmean(ig,jg)=rmean(ig,jg)+d(i)
      xmass(ig,jg)=xmass(ig,jg)+x(i)
      ymass(ig,jg)=ymass(ig,jg)+y(i)
      rnd(ig,jg)=rnd(ig,jg)+1
      
       do j=1,np
      igb=(x(j)-x0)/dx+1
      jgb=(y(j)-y0)/dy+1
      if((igb.eq.ig).and.(jgb.eq.jg)) then
!C on same bin
       
       
       dist=sqrt((x(i)-x(j))**2*scale**2+(y(i)-y(j))**2)
       dist=dist/RL
       ii=dist/0.0005
       if(ii.eq.0) then
       ccc=1
       goto 2
       endif
       if(ii.gt.40000) then
       ccc=0
       goto 2
       endif
       ccc=tbess(ii)
 2     continue
       var(ig,jg)=var(ig,jg)+(d(i)-d(j))**2
       alpha(ig,jg)=alpha(ig,jg)+ccc
       endif

       enddo
 999   continue
      enddo
!C Use all boxes with more than Mmin points
      NREL=0
      VARBAK=0
      VARTOT=0
      do i=1,nx
      do j=1,ny
      if(rnd(i,j).ge.1) then
      VARTOT=VARTOT+var(i,j)
      var(i,j)=var(i,j)/(2.*rnd(i,j)*rnd(i,j))
      rmean(i,j)=rmean(i,j)/rnd(i,j)
      xmass(i,j)=xmass(i,j)/rnd(i,j)
      ymass(i,j)=ymass(i,j)/rnd(i,j)
      alpha(i,j)=1-alpha(i,j)/(1*rnd(i,j)*rnd(i,j))

      endif
      if(rnd(i,j).ge.Mmin) then
      NREL=NREL+1
      if (NREL.gt.(nemax-2)) stop 'severe error increase please'
      W(NREL)=1./RND(i,j)
      b(NREL)=var(i,j)
      E(NREL,1)=1
      E(NREL,2)=alpha(i,j)
      VARBAK=VARBAK+rmean(i,j)*rmean(i,j)
!c      write(6,*) var(i,j),i,j,rnd(i,j),rmean(i,j)
      endif
      enddo
      enddo
!C Other approach, double loop and double sum as in note
      varvar=0
      alpalp=0
      do i=1,nx
      do j=1,ny
      do ii=1,nx
      do jj=1,ny
      if(rnd(i,j).ge.Mmin.and.rnd(ii,jj).ge.Mmin) then
      varvar=varvar+(rmean(i,j)-rmean(ii,jj))**2
      xxi=xmass(i,j)
      xxj=xmass(ii,jj)
      yyi=ymass(i,j)
      yyj=ymass(ii,jj)
      dist=sqrt((xxi-xxj)**2*scale**2+(yyi-yyj)**2)
       dist=dist/RL
       iii=dist/0.0005
       if(iii.eq.0) then
       ccc=1
       goto 223
       endif
       if(iii.gt.40000) then
       ccc=0
       goto 223
       endif
       ccc=tbess(iii)
 223     continue
      alpalp=alpalp+ccc
      endif
      enddo
      enddo
      enddo
      enddo
      
      
      
      if(NREL.eq.0) then
      write(6,*) 'Not enought data in bins'
      write(6,*) '      will try to increase bins'
      dx=dx*2
      dy=dy*2
      nx=nx/2+1
      ny=ny/2+1
      goto 2222
      VARBAK=0
      
                    else
      VARBAK=VARBAK/NREL
      varvar=varvar/(2.*NREL*NREL)
      alpalp=1-alpalp/(1.*NREL*NREL)
      endif
      
      VARTOT2=0
      rm=0
      do i=1,np
      vartot2=vartot2+d(i)*d(i)
      rm=rm+d(i)
      enddo
      vartot2=vartot2/np-(rm/np)**2
!c      write(6,*) 'Check',varbak,NREL,varvar
      
      
      VARTOT=VARTOT/(2*np*np)
      NREL=NREL+1
      W(NREL)=1./(1.*(NREL-1.))
      b(NREL)=VARVAR
      E(NREL,1)=0
      E(NREL,2)=alpalp
      
      
      
      
      NREL=NREL+1
      W(NREL)=1./(1.*np)
      b(NREL)=VARTOT2
      E(NREL,1)=1
      E(NREL,2)=1
 
      
      


      
      
      aa=0
      bb=0
      cc=0
      dd=0
      ee=0
      ff=0
      do l=1,NREL
!c      write(6,*) '?',W(l),E(l,1),E(l,2),b(l)
      aa=aa+E(l,1)/W(l)*E(l,1)
      bb=bb+E(l,1)/W(l)*E(l,2)
      cc=cc+E(l,2)/W(l)*E(l,1)
      dd=dd+E(l,2)/W(l)*E(l,2)
      ee=ee+E(l,1)/W(l)*b(l)
      ff=ff+E(l,2)/W(l)*b(l)
      ENDDO
      
      
      rnoise=(dd*ee-bb*ff)/(aa*dd-bb*cc)
      if(rnoise.le.0) rnoise=vartot2/1000.
      rsignal=(-cc*ee+aa*ff)/(aa*dd-bb*cc)
      if(rsignal.lt.0) rsignal=vartot2/1000.
      rms=0
      sm=0
      do l=1,NREL
      rms=rms+(b(l)-E(l,1)*rnoise-E(l,2)*rsignal)**2/W(l)
      sm=sm+1./W(l)
      enddo
      rms=sqrt(rms/sm)

        write(6,*) 'Length scale'
        write(6,*) RL
        write(6,*) 'Signal to noise ratio'
        write(6,*) rsignal/rnoise
        write(6,*) 'VARBAK'
        write(6,*) rsignal
        write(6,*) 'Quality of the fit (0: bad 1: good)'
        write(6,*) (vartot2-rms)/vartot2
        write(6,*) 'Noise variance'
        write(6,*) rnoise
      
        write(14,*) 'Length scale'
        write(14,*) RL
        write(14,*) 'Signal to noise ratio'
        write(14,*) rsignal/rnoise
        write(14,*) 'VARBAK'
        write(14,*) rsignal
        write(14,*) 'Quality of the fit (0: bad 1: good)'
        write(14,*) (vartot2-rms)/vartot2
        write(14,*) 'Noise variance'
        write(14,*) rnoise
   
 
      return
      end
      subroutine tabess
!C=============================================================================
      include'../Calc/divapre.h'
      real*8 tbess(40000),eps,bessk1
      common/tabbess/tbess
      external bessk1
      eps=0
      do 10 i=1,40000
      eps=eps+0.0005
         tbess(i)=eps*bessk1(eps)
 10   continue
!C=============================================================================
      return                                                            
      end                                                               
!C=======================================================================
      function bessk1(X)
      
      include'../Calc/divapre.h'
      EXTERNAL BESSI1

      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,-0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/



      IF(X.LE.0.) STOP 'ERROR X <= 0' 

      IF(X.LE.2.0) THEN
         Y = X * X * 0.25
         BESSK1 = (LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         Y = 2.0 / X
         BESSK1 = (EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END

!C=========================================================================
      function bessi1(X)

      include'../Calc/divapre.h'

      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,-0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1, &
          -0.2895312D-1,0.1787654D-1,-0.420059D-2/

      IF(ABS(X).LT.3.75) THEN
         Y = X*X / (3.75*3.75)
         BESSI1 = X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         AX = ABS(X)
         Y = 3.75 / AX
         BESSI1 = (EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
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
!C JMB I put D as REAL??
      REAL*4 D
        
!C Compute Mean Value
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
            dd(I)=dd(I) -B(1) - B(2) * x(I) - B(3) * y(I)
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
              

!C -------------------------------------------------
!C --- LUDCMP & LUBKSB :
!C ---                   LU Matrix Decomposition
!C ---                   and Backward Substitution
!C ---
!C --- Numerical Recipies (c)
!C -------------------------------------------------

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
!C JMB???
        imax=N
!c        write(6,*) 'ludcmp',imax
!C JMBE
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

!C ----------------------------------------------

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
