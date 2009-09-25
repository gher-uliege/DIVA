       real*8 err,vardata,errb
       
  
  
       err=0
       errb=0
       trace=0
       read(5,*) valex,cvval
       read(33,*) vardata
       i=0
 101   continue
      
       read(20,*,end=200,err=200) x,y,val
      
       read(21,*,end=200,err=200) x1,y1,val1
       
       read(22,*,end=200,err=200) x2,y2,qc,aii
       
       if (abs(val-valex).gt.0.0001*abs(valex)) then
       if (abs(val1-valex).gt.0.0001*abs(valex)) then
       
       err=err+(val-val1)**2/(1-aii)**2
       errb=errb+(val-val1)**2
       trace=trace+Aii
       i=i+1
       endif
       endif
       goto 101
 200   continue
       trace=trace/i
       write(23,999) cvval,sqrt(err/i),vardata,trace,sqrt(errb/i),0.0,float(i)
!C23456
 999  format(7(E11.4))
       stop 
       end 
