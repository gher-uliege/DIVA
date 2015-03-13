         parameter(ndatamax=10000000)
         parameter(ndim=3)
         real*8 coord(ndim,ndatamax),x(ndatamax),Rx(ndatamax),w(ndatamax)
         real(kind=8)::tmp1(ndatamax),tmp2(ndatamax),tmp3(ndatamax),tmp4(ndatamax)
	 character(len=250)::yc,mc,dc,time1,tmp5_1
         character(len=50),dimension(ndatamax)::time,tmp5 ! remark for developpers : too big size (len*dim) creates compilation errors.
	 character(len=50)::c1,c2,c3,c4,c5,c6,c7,c8,c9,c10
	 integer::y,m,d
	 real(kind=8)::mjd

         real*8 LS(ndim),rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,RPI
         real*8 rcoordchange
       COMMON/COORDCH/rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,RPI, &
       rcoordchange,isspheric,icoordchange

        RPI=2*ASIN(1.)
        dxkm=1
        dykm=1
        i=1
 1      continue
       read(20,*,end=999,err=999) coord(1,i),coord(2,i),x(i),w(i),tmp1(i),tmp2(i),tmp3(i),tmp4(i),time1,tmp5_1
	time(i)=time1
	tmp5(i)=tmp5_1
	yc=time(i)(1:4)
	mc=time(i)(6:7)
	dc=time(i)(9:10)
	read(yc,'(I4.4)') y
	read(mc,'(I2.2)') m
	read(dc,'(I2.2)') d
	coord(3,i)=mjd(y,m,d,0.)
!	write(*,*) coord(3,i)
!	call sleep(1)
        i=i+1
        goto 1
 999    continue
	x=1. ! applying R to unit vector to create the weights
        ndata=i-1
        write(6,*) 'Data read',ndata
        xmin=minval(coord(1,:))
        xmax=maxval(coord(1,:))
        ymin=minval(coord(2,:))
        ymax=maxval(coord(2,:))
        write(6,*) 'in box',xmin,xmax,ymin,ymax


! Now implement coordinate change on the N points and Density regions if ncessessary

      icoordchange=0
      isspheric=0
      read(10,*,err=9911,end=9911) RL,rcoordchange,LS(3)

      if (rcoordchange.gt.0) icoordchange=1
      if (rcoordchange.gt.1.5) then
      write(6,*) 'Pseudo-spherical'
      isspheric=1
      endif
      if (rcoordchange.lt.0) icoordchange=-1

 9911 continue
      write(6,*) 'Coordinates',icoordchange,isspheric,rcoordchange


!
! FIND LAT MIN, LAT MAX
      if (icoordchange.lt.0) then
      write(6,*) 'Anisotropic case'
      rlonmin=Xmin
      rlonmax=Xmax
      rlatmin=Ymin
      rlatmax=Ymax
      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=1
      dxkm=-rcoordchange
      endif


      if (icoordchange.eq.1) then
      rlonmin=Xmin
      rlonmax=Xmax
      rlatmin=Ymin
      rlatmax=Ymax
      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=(4*asin(1.)*6360.)/360.
      dxkm=asin(1.)*rlatmean/90.
      dxkm=6360.*cos(dxkm)
      dxkm=(4*asin(1.)*dxkm)/360.
      if(isspheric.ne.1) then
      RL=RL*dykm
      endif
      endif


      if (isspheric.eq.1) then
      write(6,*) 'Spherical case'
      rlonmin=Xmin
      rlonmax=Xmax
      rlatmin=Ymin
      rlatmax=Ymax
      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=0
      dykm=1
      dxkm=1
      write(6,*) 'Mean longitude',rlonmean
      endif

      if(icoordchange.ne.0) then
      XMIN=1E30
      XMAX=-1E30
      YMIN=XMIN
      YMAX=XMAX
      do I=1,ndata
      call llxy(coord(1,I),coord(2,I))
      xmin=min(coord(1,i),xmin)
      xmax=max(coord(1,i),xmax)
      ymin=min(coord(2,i),ymin)
      ymax=max(coord(2,i),ymax)
      enddo

      endif
! end coordinate change
         LS(1)=RL
         LS(2)=RL


         call Rtimesx(coord,LS,x,ndim,ndata,Rx)
	 rewind(20)
         do i=1,ndata
         write(21,*) rx(i)
	 read(20,*) c1,c2,c3,c4,c5,c6,c7,c8,c9,c10
	 write(22,'(3(a,x),f4.2,x,6(a,x))') trim(c1),trim(c2),trim(c3),w(i)/rx(i),trim(c5),trim(c6),trim(c7),trim(c8),trim(c9),trim(c10)
         enddo
         stop
         end





         subroutine Rtimesx(coord,LS,x,ndim,ndata,Rx)
!                   =======
! Gaussian type R matix in ndim dimensions applied to vector x of length ndata
! Gaussian scale differs in each direction k : LS(k)
! Coordinates of  point i are coord(i,1),coord(i,2),coord(i,...),coord(i,ndim)
! To avoid an ndata^2 complexity a grid is set up first so as to allow only to calculate
! covarances when distances are smaller than 3*L
!
!
         implicit none
! input
! =====
! Number of data and space/time dimension
         integer ndata,ndim
! Coordinates
         real*8 coord(ndim,ndata)
! vector to which apply R
         real*8 x(ndata)
! Length scales
         real*8 LS(ndim)
! output
!=======
         real*8 Rx(ndata)
!
! Working variables
         integer,DIMENSION(:,:,:,:,:,:),ALLOCATABLE:: NP
         integer,DIMENSION(:,:,:,:,:,:,:),ALLOCATABLE:: IP
         real*8 coordmin(ndim),coordmax(ndim)
         integer nx(6),ng(6),NPP
         integer*8 gridindex(ndata),NB,NBM,ires,NBMM
         real*8 dist,COV,range,dis,bessk1
         external bessk1

         integer i,j,NPMAX,i1,i2,i3,i4,i5,i6,i7,ii

!
         if(ndim.gt.6) then
           write(6,*) 'Dimension larger then 6 not allowed ',ndim
         return
         endif
! Compute bounding box for grid extend
         coordmin(1:ndim)=coord(1:ndim,1)
         coordmax(1:ndim)=coord(1:ndim,1)
         do i=2,ndata
          do j=1,ndim
          coordmin(j)=amin1(coord(j,i),coordmin(j))
          coordmax(j)=amax1(coord(j,i),coordmax(j))
          enddo
         enddo
! Slightly enlarge bounding box to be sure all points remain in box even when rounding occurs
          do j=1,ndim
          range=coordmax(j)-coordmin(j)
          coordmin(j)=coordmin(j)-range*1E-6
          coordmax(j)=coordmax(j)+range*1E-6
          enddo


! Now number of grid points in each direction
         do j=1,ndim
         nx(j)=int((coordmax(j)-coordmin(j))/(3*LS(j)))+1
         enddo
         do j=ndim+1,6
         nx(j)=1
         ng(j)=1
         enddo

! now allocate array
!         write(6,*) 'dimensions',nx,coordmin,coordmax
         ALLOCATE(NP(NX(1),NX(2),NX(3),NX(4),NX(5),NX(6)))
         NP(:,:,:,:,:,:)=0

! First dummy loop, identify the maximum number of points which fall into any bin of a regular grid

         do i=1,ndata
          do j=1,ndim
           NG(j)=int((coord(j,i)-coordmin(j))/(3*LS(j)))+1
          enddo

          NP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6))=   &
            NP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6))+1
         enddo
!
         NPMAX=maxval(NP)
!
! Now we can allocate the array which indexes points that fall into the grid
         ALLOCATE(IP(NX(1),NX(2),NX(3),NX(4),NX(5),NX(6),NPMAX))
! For each grid point collect index all points which fall into bin

         NP(:,:,:,:,:,:)=0
         do i=1,ndata
          do j=1,ndim
           NG(j)=int((coord(j,i)-coordmin(j))/(3*LS(j)))+1
          enddo
          NP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6))=   &
            NP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6))+1
          NPP=NP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6))
          IP(NG(1),NG(2),NG(3),NG(4),NG(5),NG(6),NPP)= i
! For all points get index of grid bin where if falls (stored as integer order of Fortran style)
! Via loop  on dimensions
! imax*jmax*(k-1)+imax*(j-1)+i
          NB=1
          gridindex(i)=NG(1)
          do j=2,ndim
           NB=NB*NX(j-1)
           gridindex(i)=gridindex(i)+NB*(NG(j)-1)
          enddo

         enddo

! Ok , now finally calculate covariances and application
         NBMM=1
         do j=1,ndim-1
         NBMM=NBMM*NX(j)
         enddo


         do i=1,ndata
! Find grid indexes
          ires=gridindex(i)
          NBM=NBMM
          do j=ndim+1,6
          NG(j)=1
          enddo
          do j=ndim,2,-1
          NG(j)=(ires-1)/NBM+1
          ires=ires-(NG(j)-1)*NBM
          NBM=NBM/NX(j-1)
          enddo
          NG(1)=ires
! Now all boxes around this one

          Rx(i)=0
          do i1=max(1,NG(1)-1),min(NX(1),NG(1)+1)
          do i2=max(1,NG(2)-1),min(NX(2),NG(2)+1)
          do i3=max(1,NG(3)-1),min(NX(3),NG(3)+1)
          do i4=max(1,NG(4)-1),min(NX(4),NG(4)+1)
          do i5=max(1,NG(5)-1),min(NX(5),NG(5)+1)
          do i6=max(1,NG(6)-1),min(NX(6),NG(6)+1)
! Now for each point in the box calculate contribution
           do i7=1,NP(i1,i2,i3,i4,i5,i6)
            ii=IP(i1,i2,i3,i4,i5,i6,i7)
!            write(6,*) i,i1,i2,i3,i4,i5,i6,i7,ii
!            write(6,*) i,gridindex(i)
            COV=1
            dist=0
            do j=1,ndim

             dis=(coord(j,i)-coord(j,ii))/LS(j)
             dist=dist+dis*dis
            enddo
             COV=exp(-dist)
!             dist=sqrt(dist)
!             if (dist.le.0.001) then
!              COV=1
!             else
!              COV=dist*Bessk1(dist)
!             endif
!             write(6,*) COV,dist
             Rx(i)=Rx(i)+COV*x(ii)
           enddo



          enddo
          enddo
          enddo
          enddo
          enddo
          enddo




         enddo





         return
         end

      function bessi1(X)

      include'../Calc/divapre.h'

      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0, &
          0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1, &
          -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,   &
          -0.2895312D-1,0.1787654D-1,-0.420059D-2/

      IF(ABS(X).LT.3.75) THEN
         Y = X*X / (3.75*3.75)
         BESSI1 = X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         AX = ABS(X)
         Y = 3.75 / AX
         BESSI1 = (EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+         &
                  Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
         IF(X.LT.0.) BESSI1 = - BESSI1
      ENDIF

      RETURN
      END

      function bessk1(X)

      include'../Calc/divapre.h'
      EXTERNAL BESSI1

      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0, &
          -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1, &
          0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/



      IF(X.LE.0.) STOP 'ERROR X <= 0'

      IF(X.LE.2.0) THEN
         Y = X * X * 0.25
         BESSK1 = (LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+Y*(P3+ &
                  Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         Y = 2.0 / X
         BESSK1 = (EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+        &
                  Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END

      subroutine llxy(x,y)
      real*8 rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,rpi
      real*8 rcoordchange,x,y,xxx,dyyy
      integer isspheric, icoordchange
      COMMON/COORDCH/rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,RPI, &
       rcoordchange,isspheric,icoordchange
      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000.
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180.))
      x=(x-rlonmean)*xxx
      else
      x=(x-rlonmean)*dxkm
      y=(y-rlatmean)*dykm
      endif


      return
      end

      subroutine xyll(x,y)
      real*8 rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,rpi
      real*8 rcoordchange,x,y,xxx,dyyy
      integer isspheric, icoordchange
      COMMON/COORDCH/rlatmin,rlatmax,rlonmean,rlatmean,dxkm,dykm,RPI, &
       rcoordchange,isspheric,icoordchange

      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000.
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180.))
      x=x/xxx+rlonmean
      else
      x=x/dxkm+rlonmean
      y=y/dykm+rlatmean
      endif
      return
      end

      function mjd(y,m,d,s)
      implicit none
      integer d,m,y
      real s
      real*8 mjd

! Mathematicians and programmers have naturally 
! interested themselves in mathematical and computational 
! algorithms to convert between Julian day numbers and 
! Gregorian dates. The following conversion algorithm is due 
! to Henry F. Fliegel and Thomas C. Van Flandern: 
! The Julian day (jd) is computed from Gregorian day, month
! and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

! ModifiedJulianDay = 0 for 1858-11-17 CE.

      mjd = (( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +  &  
     &       ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -&
     &       ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +&
     &       d - 32075 - 2400001)*1d0 + s/(24*60*60d0)               

      end function mjd
