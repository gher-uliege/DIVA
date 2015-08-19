       include'../Calc/divainc.h'

       integer, dimension(:), allocatable::ksort
       integer, dimension(:,:) , allocatable::kconn
       real*8, dimension(:,:) , allocatable ::tcoog
       integer, dimension(:,:) , allocatable ::nma

      read(23,*) nnt1
       write(6,*) ' Total number of vertex nodes :',nnt1
      read(23,*) nnint
       write(6,*) ' Total number of interfaces   :',nnint
      nnt=nnt1+nnint
       write(6,*) ' Total number of nodes        :',nnt
      read(23,*) nelt
       write(6,*) ' Total number of elements     :',nelt
C
      nnel=6
      allocate(ksort(nnt))
      allocate(kconn(nelt,6))
      allocate(tcoog(nnt1,2))
      allocate(nma(nnt,2))
      do j=1,2
      do i=1,nnt
        nma(i,j)=0
      enddo
      enddo

      do 10 i=1,nnt1
#ifdef DIVABINARYFILESMESH
       read(22) ksort(i),tcoog(i,1),tcoog(i,2)
#else
       read(22,*) ksort(i),tcoog(i,1),tcoog(i,2)
#endif

 10   continue
C
C  INPUT OF ID of INTERFACE NODES
C
      do 20 i=1+nnt1,nnt
#ifdef DIVABINARYFILESMESH
       read(22) ksort(i)
#else
       read(22,*) ksort(i)
#endif

 20   continue
C
C  INPUT OF CONNECTIVITY TABLE
C
      do 30 i=1,nelt
#ifdef DIVABINARYFILESMESH
       read(22) (kconn(i,j),j=1,nnel)
#else
       read(22,*) (kconn(i,j),j=1,nnel)
C       write(6,*) (kconn(i,j),j=1,nnel)
#endif

 30   continue

C Now check wich interface nodes are only on one mesh and not two: those are the boundaries
      do i=1,nelt
       do j=1,3
       jj=-kconn(i,2*j)
C       write(6,*) '??',i,jj,j
       if(nma(jj,1).eq.0) then
       nma(jj,1)=i
       else
       nma(jj,2)=i
       endif
       enddo
      enddo
      do j=1+nnt1,nnt
      if(nma(j,2).eq.0) then
       i=nma(j,1)
C       write(6,*) 'Found a single',j,nma(j,1),kconn(i,2),kconn(i,4)
C     & ,kconn(i,6)

      if (-kconn(i,2).eq.j) then
       j1=1
       j2=2
       j3=3
      endif
      if (-kconn(i,4).eq.j) then
       j1=2
       j2=3
       j3=1
      endif
      if (-kconn(i,6).eq.j) then
       j1=3
       j2=1
       j3=2
      endif
C      write(6,*) j1,j2,j3 ,kconn(i,j1),kconn(i,j2),kconn(i,j3)
      i1= kconn(i,2*j1-1)
      i2= kconn(i,2*j2-1)
      i3= kconn(i,2*j3-1)
      xx=0.5*0.5*(tcoog(i1,1)+tcoog(i2,1))+0.5*tcoog(i3,1)
      yy=0.5*0.5*(tcoog(i1,2)+tcoog(i2,2))+0.5*tcoog(i3,2)
      write(80,*) xx,yy,0,1E-8





      endif

      enddo
      stop
      end


