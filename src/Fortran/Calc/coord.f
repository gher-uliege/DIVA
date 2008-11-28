C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  COORD (MODULE)
C     -  TOPOLOLLXY (coordinate change ll to xy for topology)
C     -  TOPOLOXYLL (coordinate change xy to ll for topology)
C     -  DATALLXY   (coordinate change ll to xy for data)
C     -  DATAXYLL   (coordinate change xy to ll for data)
C     -  XYLL       (coordinate change xy to ll)
C     -  LLXY       (coordinate change ll to xy)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             COORD MODULE                             C
C          Coordinate change ll to xy and xy to ll if requested        C 
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine coord(ipr)
      include'divapre.h'
      include'divainc.h'
C
C  INPUT OF GENERAL DATA
C     
      isspheric=0
      read(10,*) rcoordchange
      if (rcoordchange.gt.0) icoordchange=1
      if (rcoordchange.gt.1.5) then
      write(6,*) 'Pseudo-spherical'
      isspheric=1
      endif
      if (rcoordchange.lt.0) icoordchange=-1
      if(ipr.gt.0) write(6,*) ' Coordinate change', icoordchange 

      return
      end


      subroutine llxy(x,y)
      include'divapre.h'
      include'divainc.h'

C      write(6,*) 'BEF x,y',x,y
      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180))
      x=(x-rlonmean)*xxx
      else
      x=(x-rlonmean)*dxkm
      y=(y-rlatmean)*dykm
      endif
C      write(6,*) 'AFT x,y',x,y

      return
      end

      subroutine xyll(x,y)
      include'divapre.h'
      include'divainc.h'
      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180))
      x=x/xxx+rlonmean
      else
      x=x/dxkm+rlonmean
      y=y/dykm+rlatmean
      endif
      return
      end

C
C CHANGE SCALE OF ALPHA0 and ALPHA1 if COORDINATE CHANGE 
C

      subroutine alphallxy()

      include'divapre.h'
      include'divainc.h'

C      write(6,*) 'alphamr',rl0,alpha0, alpha1,dykm
      rl0=sqrt(2./alpha1)
      rl0=rl0*dykm
      if (alpha0.ne.0) alpha0=1/(rl0**4)
      alpha1=2/(rl0**2)

C      write(6,*) 'alphamr',rl0,alpha0, alpha1,dykm
      return
      end

      subroutine topollxy(tcoog,ipr)

C
C COORDINATE CHANGE TOPO LL to xy 
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2)
C
C FIND LAT MIN, LAT MAX
      if (icoordchange.lt.0) then
      write(6,*) 'Anisotropic case'
      rlonmin=tcoog(1,1)
      rlonmax=tcoog(1,1)
      rlatmin=tcoog(1,2)
      rlatmax=tcoog(1,2)
      do 1011 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
 1011   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=1
      dxkm=-rcoordchange
      
      endif
      
      
      if (icoordchange.eq.1) then
      rlonmin=360.
      rlonmax=-360.
      rlatmin=90.
      rlatmax=-90.
      do 10 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
C         write(6,*) tcoog(i,1),tcoog(i,2)
 10   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=(4*asin(1.)*6360.)/360.
      dxkm=asin(1.)*rlatmean/90.
      dxkm=6360.*cos(dxkm)
      dxkm=(4*asin(1.)*dxkm)/360.

      if(ipr.ge.3) then
         write(6,*)' nnt1:',nnt1 
         write(6,*)' rlonmin:',rlonmin 
         write(6,*)' rlatmin:',rlatmin 
         write(6,*)' rlatmax:',rlatmax 
         write(6,*)' rlatmean:',rlatmean 
         write(6,*)' dxkm:',dxkm 
         write(6,*)' dykm:',dykm 
      endif
      endif
      
      
      if (isspheric.eq.1) then
      write(6,*) 'Spherical case'
      rlonmin=tcoog(1,1)
      rlonmax=tcoog(1,1)
      rlatmin=tcoog(1,2)
      rlatmax=tcoog(1,2)
      do 1091 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
 1091   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=0
      dykm=1
      dxkm=1
      write(6,*) 'Mean longitude',rlonmean
      endif
      
      
      
      do 20 i=1,nnt1
       call llxy(tcoog(i,1),tcoog(i,2))
 20   continue
C
C OUTPUT OF DATA SET DESCRIPTION
C
      if(ipr.ge.3) then
         write(6,*)' List of X and Y pos (llxy) of topology'
         write(6,*)' --------------------------------------'
         do 100 i=1,nnt1
           write(6,*) tcoog(i,1),tcoog(i,2)
 100     continue
      endif

      call alphallxy()

      return
      end



      subroutine datallxy(tdata,ipr)
 
C
C COORDINATE CHANGE LL to XY
C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

      do 20 i=1,ndata
       call llxy(tdata(i,1),tdata(i,2))
       tdata(i,4)=tdata(i,4)/(dykm**2)
 20   continue
C
C OUTPUT OF DATA SET DESCRIPTION
C
      if(ipr.ge.3) then
       write(6,*)' List of X and Y pos (llxy), value and weight of data'
       write(6,*)' ----------------------------------------------------'
       do 100 i=1,ndata
         write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100   continue
      endif
      return
      end


      subroutine propllxy(tprop,ipr)
C
C IF icoordchange: change aspect ratio of u,v
C 

      include'divapre.h'
      include'divainc.h'

      if (icoordchange.ne.0) then
c      do i=1,nnt1
c       tprop(i,1)=tprop(i,1)*dykm/dxkm
c      enddo


      endif
      return
      end

