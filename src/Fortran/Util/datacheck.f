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
      write(61,*) xmin
      write(61,*) xmax
      write(61,*) ymin
      write(61,*) ymax
      stop
      
 999  continue
      write(6,*) 'Contour file seems corrupted'
      write(61,*) -1E36
      write(61,*) 1E36
      write(61,*) -1E36
      write(61,*) 1E36
      stop
      end
      
