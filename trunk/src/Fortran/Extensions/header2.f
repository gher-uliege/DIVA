      CHARACTER*100 NAME
      integer imaxc,jmaxc,kmaxc,iprec,nbmots
      real*4 valexc

      READ (5,'(A)') NAME
      OPEN (UNIT=10,FILE=NAME,FORM='UNFORMATTED')

      do 10 kb=1,10
        read(10)
 10   continue

      read(10) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
      close(10)

      write(6,*)imaxc
      write(6,*)jmaxc
      write(6,*)kmaxc

      end
