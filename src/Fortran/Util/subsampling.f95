       integer, parameter :: nmax=30000000
       INTEGER(KIND=2) :: ic(nmax) 
  
  
  
       read(5,*) nsample,ndata,seed,ncol
       
       write(6,*) 'Subsampling',nsample,ndata,seed,ncol
       
       if(ndata.gt.nmax) then
       write(6,*) "increase nmax in subsampling.f"
       ndata=nmax
       endif
       do i=1,ndata
       ic(i)=0
       enddo
       i=0
       ii=nint(seed+ndata)
       call mysrand(ii)
 99    continue
       if(i.ge.nsample) goto 100
       
       j=nint((ndata-1)*randf())+1
       if (ic(j).eq.1) goto 99
       ic(j)=1
       i=i+1
       goto 99
 100   continue
       i=0
 101   continue
       if (ncol.eq.3) then
       read(20,*,end=200,err=200) x,y,val
       else
       read(20,*,end=200,err=200) x,y,val,w
       endif
       i=i+1
!c       write(6,*) 'ii',i,x,y,ndata,ic(i)
       if(i.gt.ndata) then
        i=ndata
        write(6,*) 'Problem with data file (last line?)'
        goto 200
        endif
!C reduced data
       if(ic(i).eq.0) then
       if (ncol.eq.3) then
       write(44,*) x,y,val
                      else
       write(44,76) x,y,val,w                      
       endif
!C reference data
                      else
       if (ncol.eq.3) then
       write(45,*) x,y,val
                      else
       write(45,76) x,y,val,w                      
       endif
       
       endif
       goto 101
 200   continue             
  76   FORMAT(4(E19.7))
       stop 
       end 

      FUNCTION RANDF()
      
      integer iseed,ia,ic,iq,ir
      COMMON /CSEED/ ISEED
      DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
      
        IH = ISEED/IQ
        IL = MOD(ISEED,IQ)
        IT = IA*IL-IR*IH
        IF(IT.GT.0) THEN
          ISEED = IT
        ELSE
          ISEED = IC+IT
        END IF
        RANDF = ISEED/FLOAT(IC)
      RETURN
      END
      subroutine mysrand(i)
      COMMON /CSEED/ ISEED
      iseed=i
      return
      end
