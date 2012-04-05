      

	read(5,*,end=99)  valex
 99   continue
	
	

        
 150  continue
        read(20,*,end=200)  x,y,val
        read(21,*,end=200)  x,y,val1
        diff=val+val1
        if (abs(val1-valex).lt.0.00001*abs(valex)) diff=0
        if (abs(val-valex).lt.0.00001*abs(valex)) diff=0
        write(22,*) x,y,diff
                goto 150


 200  continue
        stop
        end


