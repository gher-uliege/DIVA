
	read(5,*) RL,SNR, nbcol,GCVVAL,tracea,varbak
	write(6,*) RL, SNR, nbcol,GCVVAL,tracea,varbak
	w=1
      EPS=sqrt(varbak/SNR*(1-tracea))
	if (nbcol.eq.3) then

        write(6,*) '3col'
 150  continue
        read(44,*,end=200)  x,y,val
C	write(6,*) x,y,val,w
        write(76,*) x,y,EPS/sqrt(w)
        goto 150

	else

        write(6,*) '4col'
 160  continue
        read(44,*,end=200)  x,y,val,w
C	write(6,*) x,y,val,w
        write(76,*) x,y,EPS/sqrt(w)
	goto 160

	endif

 200  continue
        stop
        end


