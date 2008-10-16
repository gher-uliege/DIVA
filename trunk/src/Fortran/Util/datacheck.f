C read the n contours (maximum number ncmax)
      integer*4 idata,idatan
      xmin=1E36
      ymin=xmin
      xmax=-xmin
      ymax=xmax
      read(10,*,err=999) n
      do i=1,n
       read(10,*,err=999) ii
       j=1
 17     continue
       if(j.gt.ii) goto 101
       read(10,*,err=999) x,y
       j=j+1
       xmin=min(x,xmin)
       ymin=min(y,ymin)
       xmax=max(x,xmax)
       ymax=max(y,ymax)
       goto 17
 101    continue
      enddo
      close(10)
      write(6,*) 'Bounding box of contours:'
      write(6,*) '(',xmin,',',xmax,')x(',ymin,',',ymax,')'
      write(6,*) 'Eliminating all data points outside this box'
      
      irange=0
      read(60,*,end=844,err=844) fmin,fmax
      irange=1
      write(6,*) 'Bounding range for data'
      write(6,*) '(',fmin,',',fmax,')'
      write(6,*) 'Eliminating all data points outside this range'
 844  continue
      
      
	read(5,*,end=99) nbcol
 99   continue
c	write(6,*)  nbcol
	
	idata=0
	idatain=0
	if (nbcol.eq.3) then

        write(6,*) '3 columns in data file'
 150  continue
        read(44,*,end=200)  x,y,val
        idata=idata+1
        if(irange.eq.1) then
        if(val.lt.fmin) goto 150
        if(val.gt.fmax) goto 150
        endif
        
        
        if((x.ge.xmin).and.(x.le.xmax)
     &     .and.(y.ge.ymin).and.(y.le.ymax)) then
        write(20,*) x,y,val
        idatain=idatain+1
        endif

        goto 150

	else

        write(6,*) '4 columns in data file'
 160  continue
       read(44,*,end=200)  x,y,val,w
        idata=idata+1
        if(irange.eq.1) then
        if(val.lt.fmin) goto 160
        if(val.gt.fmax) goto 160
        endif
        
        if(((x.ge.xmin).and.(x.le.xmax))
     &     .and.((y.ge.ymin).and.(y.le.ymax))) then
            write(20,76) x,y,val,w
        idatain=idatain+1
        endif
        
	goto 160

	endif

 200  continue
  76   FORMAT(4(E19.7))
      write(6,*) 'Out of ',idata,' data we retained ',idatain
      stop 
 999  continue
      write(6,*) 'Contour file seems corrupted'
      end
      
