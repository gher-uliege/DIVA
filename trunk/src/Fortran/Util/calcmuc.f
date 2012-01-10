     
	  read(5,*,end=99,err=100)  RL,SNR,xi
 99   continue
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
      write(6,*) unsk*SNR/(RL*RL)
      stop
 100  continue
      write(6,*) 16*Atan(1.0)
        stop
        end


