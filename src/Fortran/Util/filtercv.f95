        integer, parameter :: iw=1000000
        REAL(KIND=4) ::  cv(iw),cvf(iw),w(iw)
        REAL(KIND=4) ::  w1(iw),w2(iw),w3(iw),w4(iw),w5(iw)
        
        
        read(5,*) nsamp,nlayer
        write(6,*) 'CV',nsamp,nlayer
        
        call filtercv(cv,cvf,w,nsamp,nlayer,w1,w2,w3,w4,w5)
        stop
        end
        subroutine filtercv(cv,cvf,w,ns,nl,w1,w2,w3,w4,w5)
        REAL(KIND=4) ::  cv(ns,nl)
        REAL(KIND=4) ::  cvf(ns,nl)
        REAL(KIND=4) ::  w(ns,nl)
        REAL(KIND=4) ::  w1(ns,nl)
        REAL(KIND=4) ::  w2(ns,nl)
        REAL(KIND=4) ::  w3(ns,nl)
        REAL(KIND=4) ::  w4(ns,nl)
        REAL(KIND=4) ::  w5(ns,nl)
        do j=1,nl
         do i=1,ns
         read(20,*,end=99,err=99) w1(i,j),cv(i,j),w2(i,j),w3(i,j),w4(i,j),w5(i,j),w(i,j)
         enddo
        enddo
        do kk=1,3
        do j=1,nl
         do i=1,ns
         jp=min(j+1,nl)
         jm=max(j-1,1)
         a=sqrt(w(i,jp)/(w(i,j)+w(i,jp)))
         b=sqrt(w(i,jm)/(w(i,j)+w(i,jm)))
         cvf(i,j)=cv(i,j)+0.25*(a*(cv(i,jp)-cv(i,j))-b*(cv(i,j)-cv(i,jm)))
         enddo
        enddo
        do j=1,nl
        do i=1,ns
        cv(i,j)=cvf(i,j)
        enddo
        enddo
        
        
        enddo
        
        do j=1,nl
         do i=1,ns
         write(21,60) w1(i,j),cvf(i,j),w2(i,j),w3(i,j),w4(i,j),w5(i,j),w(i,j)
         enddo
        enddo
        
        
 60     Format(7(E12.5))
        return
 99     continue
        write(6,*) 'Invalid input file'
        stop
        end
        
