#include "../cppdefs.h"

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  ALLODY (dynamical allocation of storage area in S or L vector)


      SUBROUTINE ALLODY(NCOMP,IR,TAB,IDEB,IPR)
C     ====================================
      include "divapre.h"
      include "divainc.h"

      CHARACTER*5 TAB
      DATA ZERO/0.0D0/
      IF(IR.EQ.0) GO TO 20
C
C  STORAGE IN S VECTOR
C
      IRE1=IRE+NCOMP
C
C  LENGTH TEST
C
      IF(IRE1.GT.NREA) THEN

#ifdef DIVADYNAMIC
       write(6,*) 'Increase the parameters NREA passed to ',
     &            'diva.a in divacalc'
       write(6,*) 'and please file a bug report.'
#else
       write(6,*) 'Increase the parameters NREA in divainc.h'
#endif

       WRITE(6,400) TAB,IRE1,NREA
       STOP 'Insufficient memory'
      END IF

      IDEB=IRE+1
      IRE=IRE1
      IF(IRE.GT.IREMAX) IREMAX=IRE
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,401) TAB,IDEB,IRE
 401  FORMAT(2X,'ARRAY ',A5,' STORED IN S(',I8,') TO S(',I8,')')
C
C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
C  unless call with ipr=99
C 

      DO I=IDEB,IRE
       S(I)=ZERO
      END DO

      RETURN
 20   CONTINUE
C
C  STORAGE IN L VECTOR
C
      IEN1=IEN+NCOMP
C
C  LENGTH TEST
C
      IF(IEN1.GT.NENT) THEN
#ifdef DIVADYNAMIC
       write(6,*) 'Increase the parameters NENT passed to ',
     &            'diva.a in divacalc'
       write(6,*) 'and please file a bug report.'
#else
       write(6,*) 'Increase the parameters NENT in divainc.h'
#endif

       WRITE(6,400) TAB,IEN1,NENT
       STOP 'Insufficient memory'
      END IF

      IDEB=IEN+1
      IEN=IEN1
      IF(IEN.GT.IENMAX) IENMAX=IEN
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,402) TAB,IDEB,IEN

 400  FORMAT(/'  ** ERROR - ALLODY - STORAGE OF ',A5,/,' REQUIRED SPACE'
     *       ,I8,/'   AVAILABLE SPACE : ',I8)
 402  FORMAT(2X,'ARRAY ',A5,' STORED IN L(',I8,') TO L(',I8,')')
C
C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
C
      
      DO I=IDEB,IEN
       L(I)=0
      END DO



      RETURN
      END


