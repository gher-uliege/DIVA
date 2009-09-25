      read(10,*)
      write(11,*)
      read(10,*) RL
      write(11,*) RL
      read(10,*)
      write(11,*)
      read(10,*) IC
      write(11,*) IC
      read(10,*)
      write(11,*)
      read(10,*) IC
      write(11,*) IC
      read(10,*)
      write(11,*)
      read(10,*) IC
      IS=0
      write(11,*) IS
      read(10,*)
      write(11,*)
      read(10,*) x1
      write(11,*) x1
      read(10,*)
      write(11,*)
      read(10,*) y1
      write(11,*) y1
      read(10,*)
      write(11,*)
      read(10,*) dx
      write(11,*) dx
      read(10,*)
      write(11,*)
      read(10,*) dy
      write(11,*) dy
      read(10,*)
      write(11,*)
      read(10,*) M
      write(11,*) M
      read(10,*)
      write(11,*)
      read(10,*) N
      write(11,*) N
      read(10,*)
      write(11,*)
      read(10,*) R
      write(11,*) R
      read(10,*)
      write(11,*)
      read(10,*) SN
      write(11,*) SN
      read(10,*)
      write(11,*)
      read(10,*) vv
      ii=0
      write(11,*) ii      

            

      
      
      write(20,*) x1
      write(20,*) y1
      write(20,*) dx
      write(20,*) dy
      write(20,*) M
      write(20,*) N
!c make large enough grid
      write(30,*) 1
      write(30,*) 4
      write(30,*) x1-dx,y1-dx
      write(30,*) x1+M*dx,y1-dy
      write(30,*) x1+M*dx,y1+N*dy
      write(30,*) x1-dx,y1+N*dy
      write(30,*) 
      
      stop 
      end
      
