       parameter(icl=50,imax=1000000,ig=5)
C First; data index, second: class index, third: group index
       real*4 d(imax*icl)
       real*4 da(imax*icl)
       real*4 w(imax*icl)
       integer*4 ic(icl,ig),ipoint(imax,icl,ig),indy
       integer*4 indi(ig),ndata,iclm(ig),nd
       real*4 rm(icl,ig),wm(icl,ig)
       
       character*12 cn
       character*1 groupe
       
       read(5,*) MG
       if(MG.GT.IG) then
       write(6,*) 'Too many classes, decrease input classes'
       write(6,*) ' or increase IG in detrend.f and recompile'
       stop
       endif
C make a 2 pass algo
       do j=1,ig
       do i=1,icl
       ic(i,j)=0
       rm(i,j)=0
       wm(i,j)=0
       enddo
       enddo
       do k=1,MG
       iclm(k)=0
       enddo
       ndata=0
 1     continue
       read(88,*,end=99,err=99) x,y,dd,ww,(indi(k),k=1,MG)
       read(89,*,end=99,err=99) x,y,dda
       ndata=ndata+1
       do k=1,MG
       ii=indi(k)
       iclm(k)=max(iclm(k),ii)
       ic(ii,k)=ic(ii,k)+1
       ipoint(ic(ii,k),ii,k)=ndata
       enddo
       d(ndata)=dd
       w(ndata)=ww
       da(ndata)=dda
       goto 1
 99    continue
C now subtract
C first group
       do k=1,MG
       do i=1,iclm(k)
       
       do jj=1,ic(i,k)
       indy=ipoint(jj,i,k)
       rm(i,k)=rm(i,k)+w(indy)*(d(indy)-da(indy))
       wm(i,k)=wm(i,k)+w(indy)
       enddo
       rm(i,k)=rm(i,k)/wm(i,k)
       enddo
       RR=0
       do i=1,iclm(k)
       RR=RR+rm(i,k)
       enddo
       RR=RR/max(iclm(k),1)
       write(6,*) ' Mean to be subtracted', RR
       do i=1,iclm(k)
       rm(i,k)=rm(i,k)-RR
       enddo
       
      write(groupe,88) k
 88   format(I1)
      cn="trends."//groupe//".dat"
       open(file=cn,unit=97)
       do i=1,iclm(k)
       write(97,*) i,rm(i,k)
       enddo
       close(97)
       rewind(97)
C now subtract from data
       do i=1,iclm(k)
       do jj=1,ic(i,k)
       indy=ipoint(jj,i,k)
       d(indy)=d(indy)-rm(i,k)
       enddo
       enddo
C now next group
       enddo
       rewind(88)


       nd=0
 11     continue
       read(88,*,end=999,err=999) x,y,dd,ww,(indi(k),k=1,MG)
       nd=nd+1
       write(90,*) x,y,d(nd),ww,(indi(k),k=1,MG)
       goto 11
 999    continue
       stop
       end  