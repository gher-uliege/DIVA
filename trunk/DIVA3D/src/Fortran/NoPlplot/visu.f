      read(5,*)id
      
      if (id .eq. 0) then
        call datavisu
          call meshvisu
      else
        if (id .eq. 1) then
          call meshvisu
        else
          if (id .eq. 2) then
            call divavisu
          else
            close (10)
            stop
          endif
        endif
      endif

      END


C     ###############
      subroutine datavisu
C     ###############

      character*70 dataname,contname
      character*80 xname,yname
      character*100 listdata(100),title
      character*20 dev
      character*80 datatitle(100)
      real*4 x(600000),y(600000),val,xcont(50000),ycont(50000),
     +       nbcont(1000)
      integer dim,idim,asc,nbc,nbcontpt,ipos,itot,idepth,nbprof

      read(5,*) nbprof
      do i=1,nbprof
        read(5,'(A)')datatitle(i)
        read(5,'(A)')listdata(i)
      enddo
      read(5,'(A)') contname
      read(5,'(A)') xname
      read(5,'(A)') yname
      read(5,'(A)') dev

C     OPEN (UNIT=10,file=listdata)
C     idepth=0
C8    read(10,'(A)',end=9)listprof(idepth+1),lstdtfile(idepth+1)
C     idepth=idepth+1
C     goto 8
C9    continue
C     close (10)
C     write(6,*)idepth
C     do i=1,idepth
C     write(6,'(A)')listprof(i),lstdtfile(i)
C     enddo

      xcontmax = -10E6
      ycontmax = -10E6
      xcontmin = 10E6
      ycontmin = 10E6

      if (contname .NE. "0") then

        ipos = 0

        OPEN (UNIT=12,file=contname)
        read(12,*)nbc

        do 20 itot=1,nbc

          read(12,*)nbcont(itot)

          do i=1,nint(nbcont(itot))
            ipos = ipos + 1
            read(12,*)xcont(ipos),ycont(ipos)

            xcontmax=max(xcontmax,xcont(ipos))
            ycontmax=max(ycontmax,ycont(ipos))
            xcontmin=min(xcontmin,xcont(ipos))
            ycontmin=min(ycontmin,ycont(ipos))

          enddo

 20     continue

        close (12)

      endif

      xmax = -10E6
      ymax = -10E6
      xmin =  10E6
      ymin =  10E6

      do k=1,nbprof

      OPEN (UNIT=11,file=listdata(k))
      dim=0
 10   read(11,*,end=11)x(dim+1),y(dim+1)
      dim=dim+1
      goto 10
 11   continue
      close (11)

      do idim=1,dim
        xmax=max(xmax,x(idim))
        ymax=max(ymax,y(idim))
        xmin=min(xmin,x(idim))
        ymin=min(ymin,y(idim))
      enddo

      enddo

      xmax=max(xmax,xcontmax)
      ymax=max(ymax,ycontmax)
      xmin=min(xmin,xcontmin)
      ymin=min(ymin,ycontmin)

! Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display

c      call plscmap0n(16)

! Initialize plplot

      write(6,*) 'before plstart'
c      call plstart(dev,1,1)
      write(6,*) 'after plstart'

      do 100 k=1,nbprof

c      call pladv(0)

      OPEN (UNIT=11,file=listdata(k))
      dim=0
 12   read(11,*,end=13)x(dim+1),y(dim+1),val
      dim=dim+1
      goto 12
 13   continue
      close (11)

c      call plvpor(0.1,0.9,0.1,0.9)
C     call plwind(0.99*xmin,1.01*xmax,0.99*ymin,1.01*ymax)
c      call plwind(xmin,xmax,ymin,ymax)

      if (contname .NE. "0") then

c        call plcol(11)
C       call plcol(4)
        ipos = 0
  
        do itot=1,nbc
          iposinit=ipos+1
          do i=1,nint(nbcont(itot)-1)
            ipos=ipos+1
c            call pljoin(xcont(ipos),ycont(ipos),xcont(ipos+1),
c     +           ycont(ipos+1))
          enddo
          ipos=ipos+1
c            call pljoin(xcont(ipos),ycont(ipos),xcont(iposinit),
c     +           ycont(iposinit))
        enddo

      endif

c      call plcol(15)
C     call plcol(15)
c      call plpoin(dim,x,y,1)

c      call plcol(2)
C     call plcol(1)
c      call plbox('bcinst',0.,0,'bcinstv',0.,0)
c      call plcol(1)
C     call plcol(2)
c      call pllab(xname,yname,datatitle(k))
C     call plmtex('rvt',2.,-0.1,0.,listprof(k))

100   continue

c      call plend

      write(6,*) 'Sorry, visu.f was compiled in NoPlplot'
      
      END
 

C     ###############
      subroutine meshvisu
C     ###############
      character*20 dev
      character*80 mh4,mh5,tpo,meshtitle(100),xname,yname
      PARAMETER (NMAX=100000)
      real*4 NODE(NMAX,2),XMIN,XMAX,YMIN,YMAX,XST,YST,XC(4),YC(4),dummy
      integer NBNODE,NBNINT,NBMESH,MESH(NMAX,6),nbprof,indprof(100),
     +icount,istiff(100000),val,col
 
      read(5,'(A)')mh4
      read(5,'(A)')mh5
      read(5,'(A)')tpo
      read(5,*)nbprof
      if (nbprof .ne. 0) then
        do i=1,nbprof
          read(5,*)indprof(i)
          read(5,'(A)')meshtitle(i) 
        enddo
        icount=nbprof
      else
        read(5,'(A)')meshtitle(1)
        icount=1
      endif
      read(5,'(A)')xname
      read(5,'(A)')yname
      read(5,'(A)')dev
C     read(5,*)asprt

C --- READING mh4 FILE ---

      OPEN (UNIT=11,file=mh4)
           READ (11,*) NBNODE, NBNINT, NBMESH
      CLOSE (11)

C --- READING tpo FILE ---

      XMAX = -10E6
      YMAX = -10E6
      XMIN =  10E6
      YMIN =  10E6

      OPEN (UNIT=12,file=tpo)
           DO I = 1,NBNODE
           READ (12,*) K,NODE(I,1),NODE(I,2)

           XMAX = MAX (XMAX,NODE(I,1))
           XMIN = MIN (XMIN,NODE(I,1))

           YMAX = MAX (YMAX,NODE(I,2))
           YMIN = MIN (YMIN,NODE(I,2))

           ENDDO
 
           DO I=1, NBNINT
           READ(12,*)
           ENDDO

           DO I=1,NBMESH
           READ (12,*)MESH(I,1),dummy,MESH(I,2),dummy,MESH(I,3),dummy
           ENDDO
      CLOSE (12)

      XST = XMAX - XMIN
      YST = YMAX - YMIN

      IF (XST.GT.YST) THEN
        XRAP = 1.
        YRAP = YST/XST
      ELSE
        XRAP = XST/YST
        YRAP = 1.
      ENDIF

! Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display

c      call plscmap0n(15)

c      call plstart(dev,1,1)

C     call plinit()

      do 110 k=1,icount

      if (nbprof .ne. 0) then
        open (unit=13,file=mh5)
        do i=1,nbmesh
          read(13,*)(val,j=1,indprof(k)-1),istiff(i)
        enddo 
        close (13)
      endif

c      call pladv(0)

C     call plvpor(0.1, 0.9*xrap, 0.1, 0.9*yrap)
c      call plvpor(0.1, 0.9, 0.1, 0.9)

c     call plvasp(rap)

c      call plwind(xmin,xmax,ymin,ymax)
     
      DO I=1,NBMESH

        XC(1) = NODE(MESH(I,1),1)
        XC(2) = NODE(MESH(I,2),1)
        XC(3) = NODE(MESH(I,3),1)
        XC(4) = XC(1)
        YC(1) = NODE(MESH(I,1),2)
        YC(2) = NODE(MESH(I,2),2)
        YC(3) = NODE(MESH(I,3),2)
        YC(4) = YC(1)

        if ( (nbprof .ne. 0) .AND. (istiff(i) .eq. 0) ) then
c          call plcol(11)
c          call plfill(3,xc,yc)
        endif
c          call plcol(1)
c        call pljoin(xc(1),yc(1),xc(2),yc(2))
c        call pljoin(xc(2),yc(2),xc(3),yc(3))
c        call pljoin(xc(3),yc(3),xc(1),yc(1))

      enddo

c      call plcol(2)
c      call plbox('bcnt', 0.0, 0, 'bcntv', 0.0, 0)

c      call plcol(1)
c      call pllab(xname,yname,meshtitle(k))

 110  continue

c      call plend

      write(6,*) 'Sorry, visu.f was compiled in NoPlplot'
 
      END


C     ###################
      subroutine divavisu
C     ###################

      character*80 name 
      real*4 VALEX,AA(500000),clevel(512),A(200000)
      real*8 c8(1)
      integer IMAX,JMAX,KMAX,clrlv

      read(5,'(A)')name
      read(5,*)clrlv
c    mr
      write(6,*) name
      OPEN (UNIT=10,FILE=name,FORM='UNFORMATTED')
      CALL UREADC (10,C8,AA,VALEX,IPR,IMAX,JMAX,KMAX,NB)
      CLOSE (10)

      call divavisu2(A,AA,imax,jmax,kmax,valex,clrlv,clevel)

      end

C     ############################################
      subroutine divavisu2(A,AA,imax,jmax,kmax,valex,clrlv,clevel)
C     ############################################

      integer imax,jmax,kmax,clrlv,kk,levelmin,levelmax
      real*4 A(imax,jmax),AA(imax,jmax,kmax),clevel(2,clrlv)
      real*4 ZMIN(100),ZMAX(100),zzmin,zzmax,depth
      character*100 title(100)
      character*80 xname, yname
      character*20 dev
      character*6 charz1,charz2,charz3
      character*3 DEPTHDATA(100)
      integer   NCONTR
      real*4    xmin,xmax,ymin,ymax,lonmin,lonmax,latmin,latmax
      real*4    shade_min, shade_max, sh_color
      integer   i, j, sh_cmap, sh_width,nbdepth
      integer   min_color, min_width, max_color, max_width,info

      read(5,*)info
      if (info .ne. 0) then
        read(5,*)lonmin
        read(5,*)lonmax
        read(5,*)latmin
        read(5,*)latmax
      endif
      read(5,*)nbdepth
      if (nbdepth .eq. 1) then
        read (5,*)level
        read(5,'(A)')title(level)
      else 
        do k=1,nbdepth
          read(5,'(A)')title(k)
        enddo
      endif
      read(5,*)depthscale
      read(5,'(A)')xname
      read(5,'(A)')yname
      read(5,'(A)')dev

      levelmin = 1
      levelmax = kmax
      if (nbdepth .eq. 1) then
        levelmin = level
        levelmax = level
      endif

      xmin = 1.
      xmax = real(imax)
      ymin = 1.
      ymax = real(jmax)

C     do k=depthmin,depthmax
C     write(DEPTHDATA(k),'(I3)')k
C     enddo

! Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display

c      call plscmap0n(3)

! Initialize plplot

c      call plstart(dev,1,1)

      zzmin = 1E10
      zzmax = 1E-10
      do k=1,kmax
      ZMIN(k) = 1E10
      ZMAX(k) = 1E-10
      enddo

      do k=levelmin,levelmax
      do i=1,imax
      do j=1,jmax
         IF (AA(I,J,K).NE.VALEX) THEN
         ZMIN(k) = MIN (ZMIN(k),AA(I,J,K))
         ZMAX(k) = MAX (ZMAX(k),AA(I,J,K))
         ENDIF
      ENDDO
      ENDDO
      ENDDO
      do k=levelmin,levelmax
        zzmin = min (zzmin,zmin(k))
        zzmax = max (zzmax,zmax(k))
      enddo

      if (ymax .GE. xmax) then
        xrap = xmax/ymax
        yrap = 1.
      else
        xrap = 1.
        yrap = ymax/xmax
      endif 

c     xrap = 1.
c     yrap = 1.

      sh_cmap = 1
      min_color = 1
      min_width = 0
      max_color = 0
      max_width = 0



      do 1000 kk=levelmax,levelmin,-1

      do i=1,imax
        do j=1,jmax
          A(i,j)=AA(i,j,kk)
        enddo
      enddo

      if (depthscale .eq. 1.) then
        zzmin = zmin(kk)
        zzmax = zmax(kk)
      endif 

      do i = 1, clrlv
         clevel(1,i) = zzmin+(zzmax-zzmin)*(i - 1.)/real(clrlv-1)
         clevel(2,i) = clevel(1,i)
      enddo

c cc     call pladv(0)

C     call plvpor(0.1, 0.8*xrap,0.1, 0.8*yrap)
c      call plvpor(0.1, 0.8,0.1, 0.8)

C (mr)   call plwind(xmin,xmax,ymin,ymax)
c      call plwind(lonmin,lonmax,latmin,latmax)

      do 200 i = 1, clrlv
         shade_min = zzmin+(zzmax-zzmin)*real(i - 1)/real(clrlv)
         shade_max = zzmin+(zzmax-zzmin)*real(i)/real(clrlv)
         sh_color = real(i - 1) / real(clrlv - 1)
         sh_width = 2
c         call plpsty(0)

c         call plshade0(A,imax,jmax, ' ',
C (mr)     &        xmin,xmax,ymin,ymax,
c     &        lonmin,lonmax,latmin,latmax,
c     &        shade_min, shade_max,
c     &        sh_cmap, sh_color, sh_width,
c     &        min_color, min_width, max_color, max_width)

 200  continue

c      call plcol(2)
c      call plbox('bcinst',0.,0,'bcinstv',0.,0)
c      call plcol(1)
c      call pllab(xname,yname,title(kk))
C     call plmtex('rvt',7.,0.,0.,DEPTHDATA(kk)) 

C     xsc1 = 0.05+0.8*xrap
C     xsc2 = 0.1+0.8*rap
C     call plvpor(0.05+0.8*xrap,0.1+0.8*xrap, 0.1, 0.9)
c      call plvpor(0.05+0.8,0.1+0.8, 0.1, 0.9)
c      call plwind(-1.0, 1.0, zzmin,zzmax)

      do 210 i=1,clrlv
         shade_min = zzmin+(zzmax-zzmin)*real(i - 1)/real(clrlv)
         shade_max = zzmin+(zzmax-zzmin)*real(i)/real(clrlv)
         sh_color = real(i - 1) / real(clrlv - 1)
         sh_width = 2
c         call plpsty(0)

c         call plshade0(clevel,2,clrlv, ' ',
c     &        -1., 1., zzmin,zzmax,
c     &        shade_min, shade_max,
c     &        sh_cmap, sh_color, sh_width,
c     &        min_color, min_width, max_color, max_width)

 210  continue
c      call plcol(1)

      write(charz1,'(F6.1)')zzmin
      write(charz2,'(F6.1)')(zzmax+zzmin)/2.
      write(charz3,'(F6.1)')zzmax

c      call plmtex('rv',0.,0.,0.,charz1)
c      call plmtex('rv',0.,0.5,0.,charz2)
c      call plmtex('rvt',0.,1.,0.,charz3)

 1000 enddo

c      call plend

      write(6,*) 'Sorry, visu.f was compiled in NoPlplot'
 
      END

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
