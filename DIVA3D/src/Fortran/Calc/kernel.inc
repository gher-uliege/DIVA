C test divacovar
      C0C0C0=0
C Uncomment line when full kernel to be tested
c      goto 1122
C      write(6,*) '???OP',xob,yob
      rewind(57)
      rewind(58)
      close(57)
      close(58)
      if(ikern.ne.1) then
      write(6,*) 'NDATA',ndata,ikern
      do i=1,ndata
      xjj=tdata(i,1)
      yjj=tdata(i,2)
c      write(6,*) 'kelos',l(lkelos+i-1)
      if (icoordchange.ne.0) call xyll(xjj,yjj)
      write(58,*) xjj,yjj
C      write(6,*) xjj,yjj
      enddo
      close(58)
      ikern=1
c      stop
      endif
C back to real coordinates
      xjj=xob
      yjj=yob
      if (icoordchange.ne.0) call xyll(xjj,yjj)
C normally only water points are analysed
      rlm=rl0
      if (icoordchange.ne.0) rlm=rlm/dykm
      forw=4*3.1415926536/rlm/rlm
C uses S/N=1
      write(6,*) 
      write(6,*) 
      write(6,*) 'Kernel calculation=============='
      write(6,*) 'Covariances with point',xjj,yjj
      write(57,*) xjj,yjj,1,forw
      close(57)
      call system("bash ../dvcovar")
      rewind(59)
      do 101 i=1,ndata
         read(59,*) xjjj,yjjj,corre
c         if(yob.lt.-4.99) write(6,*) '???',xjjj,yjjj,corre
c         if(corre.gt.1) then
c         if(corre.lt.100) then
c         write(6,*) '???CORRE???',corre,xjj,yjj,xjjj,yjjj
c         corre=1
c         endif
c         endif
         tdata(i,3)=corre*varbak
 101   continue
      close(59)
      rewind(61)
      read(61,*) xjjj,yjjj,corre
      write(6,*) 'Covariance retrieved',xjjj,yjjj,corre
      C0C0C0=corre
      close(61)
      write(6,*) 'Kernel calculation finished====='
      write(6,*) 
      return
 1122 continue