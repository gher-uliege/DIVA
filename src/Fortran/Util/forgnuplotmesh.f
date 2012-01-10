       parameter(nm=10000000)
       integer*4 inode(nm)
       real*4 x(nm),y(nm)
       FAC=1
       read(23,*) nd1
       read(23,*) nd2
       read(23,*) nd3
       do i=1,nd1
       read(22,*) inode(i),x(i),y(i)
       enddo
       do i=1,nd2
       read(22,*)
       enddo
       do i=1,nd3
       read(22,*) i1,i2,i3,i4,i5
       x1=x(i1)
       y1=y(i1)
       x2=x(i3)
       y2=y(i3)
       x3=x(i5)
       y3=y(i5)
       xc=(x1+x2+x3)/3
       yc=(y1+y2+y3)/3
       
       x1=xc+(x1-xc)*FAC
       y1=yc+(y1-yc)*FAC
       x2=xc+(x2-xc)*FAC
       y2=yc+(y2-yc)*FAC
       x3=xc+(x3-xc)*FAC
       y3=yc+(y3-yc)*FAC
       
       
       
       write(69,*) '#'
       write(69,*) x1,y1,0,i
       write(69,*) x2,y2,0,i
       write(69,*)
       write(69,*) x3,y3,0,i
       write(69,*) x3,y3,0,i
       write(69,*)
       write(69,*)
       enddo
       stop
       end
       
       
