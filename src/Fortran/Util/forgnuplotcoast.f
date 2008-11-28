       read(66,*) NC
       do i=1,NC
        read(66,*) NP
        do j=1,NP
        read(66,*) x,y
        write(67,*) x,y
        enddo
        write(67,*)
        write(67,*)
       enddo
       stop
       end
       
