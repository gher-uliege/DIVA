       parameter(n=50,m=10000)
       
       real*8 X(m)
       read(55,*) IFL
       if(ifl.eq.0) then
       rewind(66)
       write(66,*) '-----------------------------------------------'
       write(66,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       write(66,*) 'Pipe does not work'
       write(66,*)  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       write(66,*) 'Make sure to kill piperw.a                     '
       write(66,*)
       write(66,*) 'If you work under cygwin:'
       write(66,*) 'Look at divacompile'
       write(66,*) 'Do not use -mno-cygwin in general compiler flag'
       write(66,*) 'But keep -mno-cygwin in compiler parameter nocyg'
       write(66,*)  'Then compile again and test again'
       write(66,*)
       write(66,*) 'Otherwise:'
       write(66,*) 'Please put  let ipipe=0 in beginning of divacalc'
       write(66,*) '------------------------------------------------'
       endif
       
       call pingpong(IFL,0,1)
!c       call system('sleep 1')
       call pingpong(IFL,1,1)
       
       if (IFL.EQ.1) then
       do i=1,n
       write(6,*) 'writer',i

       open(61,file='../divapipe',form='unformatted')
       rewind(61)
       write(61) (0.1D0*(I+J),J=1,M)
       write(61) I
       close(61)
       call pingpong(IFL,1,0)
       call pingpong(IFL,0,0)
       enddo
!c       call pingpong(IFL,1,0)
       else
       
       write(6,*) 'Reader'
       write(6,*) 'into reader loop'
       do i=1,n
       call pingpong(IFL,0,0)
       write(6,*) 'do some things'
       open(61,file='divapipe',form='unformatted')
       rewind(61)
       read(61) (x(j),J=1,M)
       read(61) II
       close(61)
       write(6,*) I,II,X(m)
       call pingpong(IFL,1,0)
       enddo
 99    continue
       endif
 111   format(1(E12.5))
       write(6,*) 'Finished'
       if(IFL.EQ.0) then
       if (nint(X(m)).eq.1005) then
       rewind(66)
       write(66,*) '------------------------------------'
       write(66,*) 'Test finished succesfully'
       write(66,*) 'You can keep let ipipe=1 in divacalc'
       write(66,*) 
       write(66,*) '------------------------------------'
       
       endif
       endif
       
       
       stop
       end  
       
       
         subroutine pingpong(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

!c          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           
!c           call system('sleep 0.001')
           read(89,100) istherea
!c           write(6,*) 'w: waiting',istherea
           
           irwtd=irwtd+1
           
           
           
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           
!c           call system('sleep 0.001')
           read(88,100) isthere
!c           write(6,*) 'r: waiting',isthere
           
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
!c          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end
          
          
         subroutine pingpongd(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           rewind(88)
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           rewind(89)
!c           call system('sleep 0.001')
           read(89,100,end=11,err=11) istherea
!c           write(6,*) 'w: waiting',istherea
           if(istherea.eq.0) goto 11
           irwtd=irwtd+1
           
           rewind(88)
           write(88,100) 0
           
           call flush(88)
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           rewind(89)
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           rewind(88)
!c           call system('sleep 0.001')
           read(88,100,end=111,err=111) isthere
!c           write(6,*) 'r: waiting',isthere
           if(isthere.eq.0) goto 111
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           rewind(89)
           write(89,100) 0
           
           call flush(89)
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end
          

          
