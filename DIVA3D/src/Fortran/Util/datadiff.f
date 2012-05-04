      
	read(5,*,end=99)  nbcol
	read(5,*,end=99)  valex
 99   continue
	
	if (nbcol.eq.3) then

        write(6,*) '3col'
 150  continue
        read(44,*,end=200)  x,y,val
        read(45,*,end=200)  x,y,val1
        diff=val-val1
        if (abs(val1-valex).lt.0.00001*abs(valex)) diff=0
        write(46,*) x,y,diff
                goto 150

	else

        write(6,*) '4col'
 160  continue
        read(44,*,end=200)  x,y,val,w
        read(45,*,end=200)  x,y,val1
        diff=val-val1
        if (abs(val1-valex).lt.0.00001*abs(valex)) diff=0

        write(46,*) x,y,diff,w
        
	goto 160

	endif

 200  continue
        stop
        end


