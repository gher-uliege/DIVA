C =====================================================================
C =  fem3d.f  is based upon a mesh file (tpo extension) and a 
C =           bathymetry file (regular grid in the GHER format)
C =           and creates a result file containing informations 
C =           if a mesh is in land or is sea for a given depth.
C =
C =
C =
C =  Walrave Stephane (GHER - University of Liege, 25/11/95)
C =====================================================================

      implicit none
      integer n1max
      parameter(n1max=1000000)
C     dimension nsom(n1max),xel(n1max),yel(n1max)
      character*12 str3
      character*20 form
      character*100 mh4,tpo,batinfo,depthname,outname,batname
      integer kmax,nb,ipr,i,k,zero,one
      integer imax,jmax,nbdepth,nsom,nd1(n1max),nd2(n1max),
     +        nd3(n1max),nnt1,nnint,nelt,inter,val,line(200)
      real*4 BAT(5000000),valex,depth(200),xel(n1max),
     +       yel(n1max),zel(n1max),xsom(n1max),ysom(n1max),zsom(n1max),
     +       xbatmin,xbatmax,ybatmin,ybatmax,xmax,ymax,xmin,ymin,
     +       dxbat,dybat
      real*8 c8(1)
      zero=0
      one=1

      read(5,'(A)')batname
      read(5,'(A)')mh4
      read(5,'(A)')tpo
      read(5,'(A)')batinfo
      read(5,'(A)')depthname
      read(5,*)nbdepth
      read(5,'(A)')outname

      open (unit=9,file=batname,form='unformatted')
      call ureadc(9,C8,BAT,valex,ipr,imax,jmax,kmax,nb)
      close (9)

      open (unit=10,file=mh4)
      read(10,*)nnt1
      read(10,*)nnint
      read(10,*)nelt
      close(10)

      if(nnt1.gt.n1max) then
         write(6,*) ' error stif: n1max is too small '
         stop
      endif

      open (unit=12,file=batinfo)
      read (12,*)xbatmin
      read (12,*)xbatmax
      read (12,*)ybatmin
      read (12,*)ybatmax
      close (12)

      dxbat=(xbatmax-xbatmin)/float(imax-1)
      dybat=(ybatmax-ybatmin)/float(jmax-1)

      open (unit=13,file=depthname)
      do i=1,nbdepth
        read(13,*)depth(i)
        write(82,*)depth(i)
      enddo
      close(13)

      xmax = -10E6
      ymax = -10E6
      xmin = 10E6
      ymin = 10E6

      open(unit=11,file=tpo)
      do 10 i=1,nnt1
        read(11,*) nsom,xsom(i),ysom(i)
        xmin=min(xmin,xsom(i))
        ymin=min(ymin,ysom(i))
        xmax=max(xmax,xsom(i))
        ymax=max(ymax,ysom(i))
10    continue

      if ((xmin .LT. xbatmin) .OR. (ymin .LT. ybatmin) .OR. 
     +    (xmax .GT .xbatmax) .OR. (ymax .GT. ybatmax)) then
c        write(6,*)'ERROR 1'
        write(6,*)'ERROR 1',xmin,ymin,xmax,ymax,
     + xbatmin,ybatmin,xbatmax,ybatmax
        stop
      endif

      do 20 i=1,nnint
        read(11,*)
20    continue

      do 30 i=1,nelt
        read(11,*)nd1(i),inter,nd2(i),inter,nd3(i)
        xel(i)=(xsom(nd1(i))+xsom(nd2(i))+xsom(nd3(i)))/3.
        yel(i)=(ysom(nd1(i))+ysom(nd2(i))+ysom(nd3(i)))/3.
30    continue
      close(11)

      do i=1,nnt1
      call CALDEPTH(xsom(i),ysom(i),zsom(i),BAT,imax,jmax,valex,
     +              dxbat,dybat,xbatmin,ybatmin)
      enddo

      do i=1,nelt
      call CALDEPTH(xel(i),yel(i),zel(i),BAT,imax,jmax,valex,
     +              dxbat,dybat,xbatmin,ybatmin)
      enddo

      open (unit=14,file=outname)

      do 100 k=1,nelt
      do i=1,nbdepth
      if (zel(k) .GE. depth(i)) then
        line(i)=one
      else
        val=0
        if (zsom(nd1(k)) .GE. depth(i)) val=val+1
        if (zsom(nd2(k)) .GE. depth(i)) val=val+1
        if (zsom(nd3(k)) .GE. depth(i)) val=val+1
        if (val .GE. 2) then 
          line(i)=one   
        else
          line(i)=zero
        endif 
      endif 
      enddo

      write(str3,*)nbdepth
      form='('//str3//'i2)'
      write(14,form)(line(i),i=1,nbdepth)

 100  continue

      close(14)

      end

C***************************************************************

      SUBROUTINE CALDEPTH(x,y,z,BAT,imax,jmax,valex,dxbat,dybat,
     +                    xbatmin,ybatmin)

c     BUG IF SOME VAR NOT IN REAL*8 !!!!
 
      real*4 ireal,jreal,ifrac,jfrac,BAT(imax,jmax),x,y,z
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,
     +       XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,I
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
        if (jfrac . GT. .5) then
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

C***************************************************************

      Subroutine UREADC(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
c                ======
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Reads the field C(I,J,K) from fortran unit iu 
c returns the field in the array c4 if the returned iprecr=4
c returns the field in the array c8 if the returned iprecr=8
c returns the values if imaxr,jmaxr,kmaxr found in the file
c
c JMB 6/3/91 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER(KBLANC=10)
      real*4 c4(*)
      real*8 c8(*)
c in the calling routin you can specify the following equivalence to
c save memory space:
c      equivalence(c,c4)
c      equivalence(c,c8)
c
c skip KBLANC lines
       do 1 kb=1,KBLANC
        read(iu,ERR=99)
 1     continue
c
        read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
c
c pass the values read to the calling routine
        iprecr=iprec
        imaxr=imaxc
        jmaxr=jmaxc
        kmaxr=kmaxc
        nbmotr=nbmots
        valexr=valexc
c
c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
c
c if pathological case, read only four values C0 and DCI,DCJ,DCK
c and return
c them as the two four elements of the array
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
c
c
c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          read(iu,ERR=99,END=100) (c4(ide+kc),kc=1,ir)
                       else
c
c double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          read(iu,ERR=99,END=100) (c8(ide+kc),kc=1,ir)
                       else
           goto 99
         endif
         endif
c
         return
 99      continue
         write(*,*) 'Data error in UREADC, not a conform file'
         return
100      continue
         write(*,*) 'Data error in UREADC, EOF reached'
         write(*,*)' number of values retrieved:', (kl-1)*nbmots+kc-1

         return
         end
