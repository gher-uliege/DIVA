      real*4 BAT(3000000),IN(8000000)
      character*100 batname,inname
      real*8 c8(1)

c     batname='/home/walrave/Modb/Travail/DATA/adr.GHER.bat'
c     inname='/home/walrave/Modb/Travail/RESULTS/adr.res'
      read(5,'(A)')batname
      read(5,'(A)')inname

      open (unit=9,file=batname,form='unformatted')
      call ureadc(9,C8,BAT,valexbat,iprbat,imaxbat,jmaxbat,kmaxbat,
     +            nbbat)
      close (9)

      open (unit=10,file=inname,form='unformatted')
      call ureadc(10,C8,IN,valexin,iprin,imaxin,jmaxin,kmaxin,nbin)
      close (10)

      call MASK2(BAT,IN,imaxbat,jmaxbat,valexbat,
     +           valexin,iprin,imaxin,jmaxin,kmaxin,nbin)

      end

C***************************************************************

      subroutine MASK2(BAT,IN,imaxbat,jmaxbat,valexbat,
     +           valexin,iprin,imaxin,jmaxin,kmaxin,nbin)

      integer*4 nbdepth,i,j,k
      real*4 indepth(300),BAT(imaxbat,jmaxbat),IN(imaxin,jmaxin,kmaxin),
     +       xbatmin,xbatmax,
     +       ybatmin,ybatmax,xinmin,xinmax,yinmin,yinmax,dxbat,dybat,
     +       dxin,dyin,xin,yin,zin
      character*100 batinfo,ininfo,outname
            real*8 c8(1)


c     batinfo='/home/walrave/Modb/Travail/DATA/adr.GHER.bat.info'
c     ininfo='/home/walrave/Modb/Travail/RESULTS/adr.res.info'
c     outname='/home/walrave/Modb/Travail/RESULTS/adr.res.tmp'
      read(5,'(A)')batinfo
      read(5,'(A)')ininfo
      read(5,'(A)')outname

      open (unit=11,file=batinfo)
      read (11,*)xbatmin
      read (11,*)xbatmax
      read (11,*)ybatmin
      read (11,*)ybatmax
      close (11)

      open (unit=12,file=ininfo)
      read (12,*)xinmin
      read (12,*)xinmax
      read (12,*)yinmin
      read (12,*)yinmax
      read (12,*)nbdepth
      if (nbdepth .NE. 0) then
        do k=1,nbdepth
        read (12,*)indepth(k)
        enddo
      endif
      close (12)

      if ((xinmin .LT. xbatmin) .OR. (yinmin .LT. ybatmin) .OR.
     +    (xinmax .GT. xbatmax) .OR. (yinmax .GT. ybatmax)) then
c        write(6,*)'ERROR 1'
        write(6,*)'WARNING 1'
c mr
	write(6,*) xinmin,xbatmin,yinmin,ybatmin
     +             xinmax,xbatmax,yinmax,ybatmax
      endif

      dxbat=(xbatmax-xbatmin)/float(imaxbat-1)
      dybat=(ybatmax-ybatmin)/float(jmaxbat-1)

      dxin=(xinmax-xinmin)/float(imaxin-1)
      dyin=(yinmax-yinmin)/float(jmaxin-1)

      do k=1,kmaxin
      do i=1,imaxin 
      do j=1,jmaxin 

      xin=xinmin+(float(i-1)*dxin)
      yin=yinmin+(float(j-1)*dyin)

      if ((xin .LE. xbatmin) .OR. (yin .LE. ybatmin) .OR.
     +    (xin .GE. xbatmax) .OR. (yin .GE. ybatmax)) then
c        write(6,*)'WARNING 1',xin,yin
        IN(i,j,k)=valexin
c mr
      endif

      if (IN(i,j,k) .NE. valexin) then
        call CALDEPTH (xin,yin,zin,BAT,imaxbat,jmaxbat,valexbat,
     +                 dxbat,dybat,xbatmin,ybatmin)
        if (zin .LT. indepth(kmaxin-k+1)) then
          IN(i,j,k)=valexin
        endif
      endif

      enddo
      enddo
      enddo

      open (unit=13,file=outname,form='unformatted')
      call uwritc(13,c8,IN,valexin,iprin,imaxin,jmaxin,kmaxin,nbin)
      close(13)

      end

      include "ureadc.f"
      include "uwritc.f"

C***************************************************************

      SUBROUTINE CALDEPTH(x,y,z,BAT,imax,jmax,valex,dxbat,dybat,
     +                    xbatmin,ybatmin)

      real*4 ireal,jreal,ifrac,jfrac,x1,y1,z1,x2,y2,z2,x3,y3,z3,
     +       XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,I,z,
     +       BAT(imax,jmax)
C    +       dxbat,dybat,xbatmin,ybatmin,BAT(*),valex,x,y

      integer ip1,jp1,ip2,jp2,ip3,jp3,iint,jint

C     calcul des 3 points de BAT les plus proches du point a calculer

      ireal = 1.+(x-xbatmin)/dxbat
      jreal = 1.+(y-ybatmin)/dybat

      iint=INT(ireal)
      jint=INT(jreal)

      ifrac=ireal-iint
      jfrac=jreal-jint

      if (ifrac .GT. .5) then
        ip1=iint+1
        jp1=jint
        ip2=iint+1
        jp2=jint+1
        if (jfrac . GT. .5) then
          ip3=iint
          jp3=jint+1
        else
          ip3=iint
          jp3=jint
        endif
      else
        ip1=iint
        jp1=jint
        ip2=iint
        jp2=jint+1
        if (jfrac .GT. .5) then
          ip3=iint+1
          jp3=jint+1
        else
          ip3=iint+1
          jp3=jint
        endif
      endif

C     calcul de la profondeur du point dans
C     le plan definit par les 3 points de BAT

      x1=xbatmin+(ip1-1)*dxbat
      y1=ybatmin+(jp1-1)*dybat
      if (BAT(ip1,jp1).EQ.valex) then
        z1=0.
      else
        z1=BAT(ip1,jp1)
      endif
      x2=xbatmin+(ip2-1)*dxbat
      y2=ybatmin+(jp2-1)*dybat
      if (BAT(ip2,jp2).EQ.valex) then
        z2=0.
      else
        z2=BAT(ip2,jp2)
      endif
      x3=xbatmin+(ip3-1)*dxbat
      y3=ybatmin+(jp3-1)*dybat
      if (BAT(ip3,jp3).EQ.valex) then
        z3=0.
      else
        z3=BAT(ip3,jp3)
      endif

      XX1=y2*z3-y3*z2
      YY1=x2*z3-z2*x3
      ZZ1=x2*y3-y2*x3
      XX2=y1*z3-z1*y3
      YY2=x1*z3-z1*x3
      ZZ2=x1*y3-y1*x3
      XX3=y1*z2-z1*y2
      YY3=x1*z2-z1*x2
      ZZ3=x1*y2-y1*x2
      I=x1*XX1-y1*YY1+z1*ZZ1

      z=(x*(XX1-XX2+XX3)+y*(YY2-YY1-YY3)-I)/(ZZ2-ZZ1-ZZ3)

      end

