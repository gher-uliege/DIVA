       parameter(icl=100,imax=1000000)
       real*4 d(imax,icl)
       real*4 da(imax,icl)
       real*4 w(imax,icl)
       integer*4 ic(icl)
       real*4 rm(icl),wm(icl)
C make a 2 pass algo
       do i=1,icl
       ic(i)=0
       rm(i)=0
       wm(i)=0
       enddo
       iclm=0
 1     continue
       read(88,*,end=99,err=99) x,y,dd,ww,ii
       read(89,*,end=99,err=99) x,y,dda
       iclm=max(iclm,ii)
       ic(ii)=ic(ii)+1
       d(ic(ii),ii)=dd
       w(ic(ii),ii)=ww
       da(ic(ii),ii)=dda
       goto 1
 99    continue
C now subtract
       do i=1,iclm
       
       do jj=1,ic(i)
       
       rm(i)=rm(i)+w(jj,i)*(d(jj,i)-da(jj,i))
       wm(i)=wm(i)+w(jj,i)
       enddo
       rm(i)=rm(i)/wm(i)
       
       do jj=1,ic(i)
       d(jj,i)=d(jj,i)-rm(i)
       enddo
       enddo
       
       do i=1,iclm
       write(97,*) i,rm(i)
       enddo
       
       rewind(88)
       do i=1,icl
       ic(i)=0
       enddo


 11     continue
       read(88,*,end=999,err=999) x,y,dd,ww,ii
       ic(ii)=ic(ii)+1
       write(90,*) x,y,d(ic(ii),ii),ww,ii
       goto 11
 999    continue
       stop
       end  