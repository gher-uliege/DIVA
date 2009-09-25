C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  BCONDI (MODULE)
C     -  RDCOND (READ THE BOUNDARY CONDITIONS)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             BCONDI MODULE                            C
C             Dirichlet boundary conditions to be fixed                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine bcondi(ipr)
      include'divapre.h'
      include'divainc.h'
C
C  READ THE BOUNDARY CONDITIONS TO BE IMPLEMENTED
C
      read(10,*)ncond
      if(ipr.gt.0) write(6,*) ' Number of Boundary Conditions =',ncond
      info=1
      if(ityp.eq.2.or.ityp.eq.3) info=2
C
C ALLOCATION OF STORAGE TABLES:
C  ==> TCNDI(I)    : VALUE OF BOUNDARY CONDITION I
C  ==> KCNDI(I,*)  : NODE FOR BOUNDARY CONDITION
C                    (IF ITYP.EQ.2 OR 3, ALSO CONNECTOR AT NODE !!!)
C
      call allody(ncond,1,'tcndi',ltcndi,ipr)
      call allody(ncond*info,0,'kcndi',lkcndi,ipr)
      call rdcond(l(lkcndi),s(ltcndi),ipr)
      return
      end


      subroutine rdcond(kcndi,tcndi,ipr)
C
C  I/O BOUNDARY CONDITIONS TO BE FIXED (ONLY DIRICHLET TYPE)
C
      include'divapre.h'
      include'divainc.h'
      dimension tcndi(ncond),kcndi(ncond,info)
C
C  INPUT OF B.C. DESCRIPTION
C
      do 10 i=1,ncond
         read(30,*) tcndi(i),(kcndi(i,j),j=1,info)
 10   continue
C
C  OUTPUT OF B.C. DESCRIPTION
C
      if(ipr.ge.3) then
         write(6,*)' List of B.C. value,  nodes  and  d.o.f.           '
         write(6,*)' --------------------------------------------------'
         do 100 i=1,ncond
           write(6,*) tcndi(i),(kcndi(i,j),j=1,info)
 100     continue
      endif
      return
      end
