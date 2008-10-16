       parameter(icl=50,imax=1000000,ig=5)
C First; data index, second: class index, third: group index
       real*4 d(imax*icl)
       real*4 da(imax*icl)
       real*4 w(imax*icl)
       integer*4 ic(icl,ig),ipoint(imax,icl,ig),indy
       integer*4 indi(ig),ndata,iclm(ig),nd
       real*4 rm(icl,ig),wm(icl,ig)
       real*8 rms1,rms2,rms3
     
       
       character*12 cn
       character*1 groupe
       rms1=0
       rms2=0
       rms3=0
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
C       write(6,*) k,iclm(k)
       ic(ii,k)=ic(ii,k)+1
       ipoint(ic(ii,k),ii,k)=ndata
       enddo
       d(ndata)=dd
       w(ndata)=ww
       da(ndata)=dda
       rms1=rms1+ww*(dd-dda)*(dd-dda)
       rms3=rms3+ww
       goto 1
 99    continue
C now subtract
       write(6,*) 'Summary'
       write(6,*) 'Data',ndata
       write(6,*) 'Groups',MG
       write(6,*) 'Classes',(iclm(k),k=1,MG)
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
       rms2=rms2+ww*(d(nd)-da(nd))*(d(nd)-da(nd))
       write(90,*) x,y,d(nd),ww,(indi(k),k=1,MG)
       goto 11
 999    continue
       write(6,*) 'Misfit after detrend/misfit before',rms2/rms1
       write(42,*) rms2/rms1,sqrt(rms2/rms3),sqrt(rms1/rms3)
       stop
       end  