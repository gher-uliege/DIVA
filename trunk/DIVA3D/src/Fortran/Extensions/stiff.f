      integer prof(100000),val(100),iprof,nelt
      character*100 mh4,mh5,res
      parameter (stiffm=1.E-6)
      one=1.0

      read(5,'(A)')mh4
      read(5,'(A)')mh5
      read(5,'(A)')res
      read(5,*)iprof

      open (unit=10,file=mh4)
      read(10,*)
      read(10,*)
      read(10,*)nelt
      close(10)
 
      open (unit=11,file=mh5)
      do i=1,nelt
        read(11,*)(val(j),j=1,iprof-1),prof(i)
      enddo
      close (11)

      open (unit=12,file=res)
      do i=1,nelt
        if (prof(i).eq.1) then
          write(12,*)one
        else
          write(12,*)stiffm
        endif
      enddo
      close (12)

      end 
      
