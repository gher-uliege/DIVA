      real *8 x,y,v
      open (61,file='divapipe',form='unformatted')
  1   continue
      read(61,end=2) x,y,v
      write(6,*) x,y,v
      goto 1
  2   stop
      end
      