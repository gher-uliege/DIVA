       parameter (nmax=30000000)
       integer*2 ic(nmax) 
       real*8 err
       
  
  
       err=0
       read(5,*) valex
       
       i=0
 101   continue
      
       read(20,*,end=200,err=200) x,y,val
      
       read(21,*,end=200,err=200) x1,y1,val1
       
       if (val.ne.valex.and.val1.ne.valex) then
       err=err+(val-val1)**2
       i=i+1
       endif
       goto 101
 200   continue
       write(22,*) err,i,sqrt(err/i)
       stop 
       end 
