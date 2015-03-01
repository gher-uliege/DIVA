C test divacovar

      if(ipipe.eq.1) then
      call pingpong(0,0,0)
      open(61,file='divapipe',form='unformatted')
      rewind(61)
      endif

      C0C0C0=0

      read(61) xjjj,yjjj,corre

      read(61) (tdata(i,3),i=1,ndata)
      if (corre.gt.1) then
      write(6,*) '??',ndata,xjjj,yjjj,corre
      write(6,*) (tdata(i,3),i=1,ndata)
      endif

      if(ipipe.eq.1) then
      close(61)
      call pingpong(0,1,0)
      iillmm=iillmm+1
c      write(6,*) '???kernel',iillmm
      endif
      
      
      do 101 i=1,ndata
c         read(59,*) corre
C peut etre l'ordre est modifie (meme surement), indirection?
C
c         tdata(i,3)=corre*varbak
          tdata(i,3)=tdata(i,3)*varbak
 101   continue

C      write(6,*) 'Covariance retrieved',xjjj,yjjj,corre
      C0C0C0=corre
      return
