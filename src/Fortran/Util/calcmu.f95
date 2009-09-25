      xi=1
	read(5,*,end=99) RL,SNR, nbcol,xi
 99   continue
	write(6,*) 'To calculate data weights, using'
	write(6,*) 'Length scale,SNR,xi', RL, SNR,xi
	w=1
	unsk=16*Atan(1.0)
	
	if(abs(xi).gt.1.) then
	angle=log(abs(xi)+sqrt(xi*xi-1))
	if (angle.eq.0.) then
	 unsk=16*Atan(1.0)
	                 else
	 unsk=16*Atan(1.0)*sinh(angle)/angle
	endif
	endif
	
	if(abs(xi).le.1) then
	angle=acos(xi)
	if (angle.eq.0.) then
	 unsk=16*Atan(1.0)
	                 else
	 unsk=16*Atan(1.0)*sin(angle)/angle
	endif
	endif

	if (nbcol.eq.3) then

        write(6,*) 'Data, 3 columns, hence without relative weights'
 150  continue
        read(44,*,end=200)  x,y,val
!C	write(6,*) x,y,val,w
        write(20,*) x,y,val,w*unsk*SNR/(RL*RL)
!c         write(20,*) x,y,val,w*8*SNR/(RL*RL)
        goto 150

	else

        write(6,*) 'Data, 4 columns, using relative weights'
 160  continue
        read(44,*,end=200)  x,y,val,w
!C	write(6,*) x,y,val,w
        write(20,*) x,y,val,w*unsk*SNR/(RL*RL)
!c        write(20,*) x,y,val,w*8*SNR/(RL*RL)
	goto 160

	endif

 200  continue
        stop
        end


