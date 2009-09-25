       parameter (nmax=30000000)
       integer*2 ic(nmax) 
       real*8 errc,errt,datavar,datavart
       
  
  
  
       read(5,*) cvval
       errt=0
       datavart=0
       i=0
       j=0
 101   continue
      
       read(20,*,end=200,err=200) errc,n
       read(21,*,end=200,err=200) datavar,nd
       i=i+n
       j=j+nd
       errt=errt+errc
       datavart=datavart+datavar*nd
       goto 101
 200   continue
       write(22,76) cvval,sqrt(errt/i),datavart/j,sqrt(errt)/i,0.0,0.0,float(i)
 76   FORMAT(7(E11.4))
       stop 
       end 
