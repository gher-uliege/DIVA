C
        REAL*4 V(5000000)
        REAL*4 W(5000000)
        REAL*8 W8


        character*100 in1,in2,out
        read(5,*) val, valm
C        write(6,*) val, valm
        read(5,'(A)')in2
C        write(6,*) in2
        read(5,'(A)')out
C        write(6,*) out
C READING U,V FIELDS ---------------------------------------

        open (unit=11,file=in2,form='unformatted')
        write(6,*) 'Going to read',in2
        CALL UREADC(11,W8,V,VALEXW,IPR,IMAX,JMAX,KMAX,NW)
        
        ipr=4
        nw=imax*jmax
        do i=1,nw
        w(i)=valexw
        if (v(i).ne.valexw) then
C        w(i)=val+valm*V(i)
C        w(i)=val*exp(v(i))
        w(i)=val*sqrt(amax1(0.,1.-v(i)))
        endif
        enddo
        open (unit=12,file=out,form='unformatted')
        CALL UWRITC(12,W8,W,VALEXW,IPR,IMAX,JMAX,KMAX,NW)

        STOP
        END

        INCLUDE 'ureadc.f'
        INCLUDE 'uwritc.f'
