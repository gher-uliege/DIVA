C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  ALLODY (dynamical allocation of storage area in S or L vector)


      SUBROUTINE ALLODY(NCOMP,IR,TAB,IDEB,IPR)
C     ====================================
      include'divapre.h'
      INCLUDE'divainc.h'
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
      IF(IRE1.LE.NREA) GO TO 10
C#ifdef DIVADYNAMIC
       write(6,*) 'Dynamic reallocation'
       write(6,*) 'Might cause crash if not enough memory left'
       write(6,*) 'Momentarely doubles memory needed'

C      allocate SN(IRE)
C      copy S into SN
C      deallocate S
C      allocate S(IRE1)
C      copy SN into S
C      NREA=IRE1
C      goto 10
C#endif
      WRITE(6,400) TAB,IRE1,NREA
 400  FORMAT(/'  ** ERROR - ALLODY - STORAGE OF ',A5,/,' REQUIRED SPACE'
     *       ,I10,/'   AVAILABLE SPACE : ',I10)
      STOP 'SEVERE ERROR'
 10   IDEB=IRE+1
      IRE=IRE1
      IF(IRE.GT.IREMAX) IREMAX=IRE
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,401) TAB,IDEB,IRE
 401  FORMAT(2X,'ARRAY ',A5,' STORED IN S(',I8,') TO S(',I8,')')
C
C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
C  unless call with ipr=99
C 

      DO 11 I=IDEB,IRE
 11   S(I)=ZERO
      RETURN
 20   CONTINUE
C
C  STORAGE IN L VECTOR
C
      IEN1=IEN+NCOMP
C
C  LENGTH TEST
C
      IF(IEN1.LE.NENT) GO TO 30
C#ifdef DIVADYNAMIC

C      allocate LN(IEN)
C      copy L into LN
C      deallocate L
C      allocate L(IEN1)
C      copy SN into S

C      NENT=IEN1
C      goto 30
C#endif

      WRITE(6,400) TAB,IEN1,NENT
      STOP
 30   IDEB=IEN+1
      IEN=IEN1
      IF(IEN.GT.IENMAX) IENMAX=IEN
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,402) TAB,IDEB,IEN
 402  FORMAT(2X,'ARRAY ',A5,' STORED IN L(',I8,') TO L(',I8,')')
C
C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
C
      
      DO 31 I=IDEB,IEN
 31   L(I)=0
      RETURN
      END


