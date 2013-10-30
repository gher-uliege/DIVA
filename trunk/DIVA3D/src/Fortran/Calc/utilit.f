
C     -  APPEND (transform an elementary matrix in a vector)
C     -  ASSEL  (assembling of elementary array accorting to skyline)
C     -  CALLOC (compute the localization of one element in the struct.)
C     -  CALSKY (compute the KSKYH vector from the topologic data)
C     -  IMPMAT (print a matrix)
C     -  LOCPT2 (locate the (x,y) point in the structure (for ityp=2)
C     -  LOCPT3 (locate the (x,y) point in the structure (for ityp=3)
C     -  SOL    (solution of a linear system according to skyline meth.)
C     -  UWRITC (write a matrix in standard GHER format)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




      subroutine append(tkele,nddle,tvele,nspace,
     &                  isym)
      include'divapre.h'
      dimension tkele(nddle,nddle),tvele(nspace)
C
C  TRANSFORM AN ELEMENTARY MATRIX IN A VECTOR
C  IF ISYM = 1 ==> ONLY THE UPPER PART OF THE MATRIX
C
      iv=1
      if(isym.eq.0) then
         do 10 j=1,nddle
            do 20 i=1,j
               tvele(iv)=tkele(i,j)
               iv=iv+1
 20         continue
 10      continue
                    else
         do 30 j=1,nddle
            do 40 i=1,nddle
               tvele(iv)=tkele(i,j)
               iv=iv+1
 40         continue
 30      continue
      endif
      return
      end



      SUBROUTINE ASSEL(IKG,IFG,IDLE,NSYM,KLOCE,KLD,VKE,VFE,VKGS,
     *  VKGD,VKGI,VFG)
C=======================================================================
C     ASSEMBLAGE D'UNE MATRICE ET/OU D'UN VECTEUR ELEMENTAIRE
C     (MATRICE SYMETRIQUE OU NON)
C       ENTREES
C            AUTEURS: Dhatt et Touzot, 1985
C          IKG    SI IKG.EQ.1 ASSEMBLAGE DE LA MATRICE ELEMENTAIRE KE
C          IFG    SI IFG.EQ.1 ASSEMBLAGE DU VECTEUR ELEMENTAIRE FE
C          IDLE   NOMBRE DE D. L. DE L'ELEMENT
C          NSYM   0=PROBLEME SYMETRIQUE, 1=PROBLEME NON SYMETRIQUE
C          KLOCE  VECTEUR DE LOCALISATION DE L'ELEMENT
C          KLD    HAUTEURS CUMULEES DE COLONNES DE KG
C          VKE    MATRICE ELEMENTAIRE KE(PLEINE OU TRIANGLE SUPERIEUR
C                 PAR COLONNES DESCENDANTES)
C          VFE    VECTEUR ELEMENTAIRE FE
C       SORTIES
C          VKGS,VKGD,VKGI   MATRICE GLOBALE (LIGNE DE CIEL)
C                  (SYMETRIQUE OU NON)
C          VFG    VECTEUR SOLLICITATIONS GLOBAL
C=======================================================================
      include'divapre.h'
      DIMENSION KLOCE(*),KLD(*),VKE(*),VFE(*),VKGS(*),VKGD(*),
     *  VKGI(*),VFG(*)
C.......................................................................
C
C......   ASSEMBLAGE DE LA MATRICE ELEMENTAIRE
C
      IF(IKG.NE.1) GO TO 100
      IEQ0=IDLE
      IEQ1=1
C......   POUR CHAQUE COLONNE DE KE
      DO 90 JD=1,IDLE
      IF(NSYM.NE.1) IEQ0=JD
      JL=KLOCE(JD)
C      IF(JL)90,90,10
C replaced old fashioned if
      IF (JL.LE.0) goto 90
 10   I0=KLD(JL+1)
      IEQ=IEQ1
      IQ=1
C......   POUR CHAQUE LIGNE DE KE
      DO 80 ID=1,IDLE
      IL=KLOCE(ID)
      IF(NSYM.EQ.1) GO TO 30
C      IF(ID-JD) 30,20,20
C replaced old fashioned if test
      IF(ID.LT.JD) goto 30
 20   IQ=ID
C 30   IF(IL) 80,80,40
C replaced old fashioned if test
  30   IF(IL.LE.0) goto 80
 40   IJ=JL-IL
C      IF(IJ) 70,50,60
C replaced old fashioned if test
       IF(IJ.LT.0) goto 70
       IF(IJ.GT.0) goto 60
C......   TERMES DIAGONAUX DE KG
 50   VKGD(IL)=VKGD(IL)+VKE(IEQ)
      GO TO 80
C......   TERMES DU TRIANGLE SUPERIEUR DE KG
 60   I=I0-IJ
      VKGS(I)=VKGS(I)+VKE(IEQ)
      GO TO 80
C......   TERMES DU TRIANGLE INFERIEUR DE KG
 70   IF(NSYM.NE.1) GO TO 80
      I=KLD(IL+1)+IJ
      VKGI(I)=VKGI(I)+VKE(IEQ)
 80   IEQ=IEQ+IQ
 90   IEQ1=IEQ1+IEQ0
C
C......   ASSEMBLAGE DU VECTEUR ELEMENTAIRE
C
 100  IF(IFG.NE.1) GO TO 130
      DO 120 ID=1,IDLE
      IL=KLOCE(ID)
C      IF(IL) 120,120,110
C replaced old fashioned if test
      IF(IL.LE.0) goto 120
 110  VFG(IL)=VFG(IL)+VFE(ID)
 120  CONTINUE
 130  RETURN
      END



      subroutine calloc(iel,loce,nloc,kconn,klink,ipr)

C  COMPUTE THE LOCALISATION OF THE D.O.F. OF ELEMENT IEL IN THE
C  GLOBAL D.O.F. VECTOR

      include'divapre.h'
      include'divainc.h'
      dimension loce(nloc),kconn(nelt,nnel),klink(nnt)
      do 10 i=1,nloc
         loce(i)=0
 10   continue
C
C  COMPUTE LOCE FOR ELEMENTS OF TYPE 2 : 3 * 3 DOF AT VERTEX NODES
C                                        + 3 INTERFACES
C
      if(ityp.eq.2) then
         inod1=kconn(iel,1)
         inod2=kconn(iel,3)
         inod3=kconn(iel,5)
         iint1=-kconn(iel,2)
         iint2=-kconn(iel,4)
         iint3=-kconn(iel,6)
         loce(1)=klink(inod1)
         loce(2)=loce(1)+1
         loce(3)=loce(2)+1
         loce(4)=klink(inod2)
         loce(5)=loce(4)+1
         loce(6)=loce(5)+1
         loce(7)=klink(inod3)
         loce(8)=loce(7)+1
         loce(9)=loce(8)+1
         loce(10)=klink(iint1)
         loce(11)=klink(iint2)
         loce(12)=klink(iint3)
      endif
C
C  COMPUTE LOCE FOR ELEMENTS OF TYPE 3 : 4 * 3 DOF AT VERTEX NODES
C                                        + 4 INTERFACES
C
      if(ityp.eq.3) then
         inod1=kconn(iel,1)
         inod2=kconn(iel,3)
         inod3=kconn(iel,5)
         inod4=kconn(iel,7)
         iint1=-kconn(iel,2)
         iint2=-kconn(iel,4)
         iint3=-kconn(iel,6)
         iint4=-kconn(iel,8)
         loce(1)=klink(inod1)
         loce(2)=loce(1)+1
         loce(3)=loce(2)+1
         loce(4)=klink(inod2)
         loce(5)=loce(4)+1
         loce(6)=loce(5)+1
         loce(7)=klink(inod3)
         loce(8)=loce(7)+1
         loce(9)=loce(8)+1
         loce(10)=klink(inod4)
         loce(11)=loce(10)+1
         loce(12)=loce(11)+1
         loce(13)=klink(iint1)
         loce(14)=klink(iint2)
         loce(15)=klink(iint3)
         loce(16)=klink(iint4)
      endif
C
C  PRINT LOCE FOR ELEMENT IEL ...
C
      if(ipr.ge.5) then
        write(6,*)'  LOCE COMPUTED FOR ELEMENT ',IEL
        write(6,910) (loce(i),i=1,nddle)
 910    format(12(i6))
      endif
      return
      end



      subroutine calsky(kskyh,kconn,klink,kloce,ipr)
C
C  COMPUTE THE KSKYH VECTOR FROM THE TOPOLOGIC DATA
C
      include'divapre.h'
      include'divainc.h'
      dimension kskyh(nddlt+1),kconn(nelt,nnel),klink(nnt),
     &          kloce(nddle)
      do 10 iel=1,nelt
         call calloc(iel,kloce,nddle,kconn,klink,ipr)
         lhmin=999999
         do 20 id=1,nddle
            lhmin=min(lhmin,kloce(id))
 20      continue
         do 30 id=1,nddle
            iloc=kloce(id)
            lh=iloc-lhmin
            if(kskyh(iloc).lt.lh) kskyh(iloc)=lh
 30      continue
 10   continue
      if(ipr.ge.5) call prskyh(kskyh,nddlt+1)
C
C  compute the cumulated height index vector
C
      kskyh(1)=1
      inter1=kskyh(2)
      kskyh(2)=1
      do 50 i=3,nddlt+1
         inter2=kskyh(i)
         kskyh(i)=kskyh(i-1)+inter1
         inter1=inter2
 50   continue
C
C  INITIALIZATION OF NTERM (SIZE OF NON DIAG VECTOR CONTAINING THE
C  UPPER OR LOWER STIFFNESS MATRIX
C
      nterm=kskyh(nddlt+1)-1
      if(ipr.ge.5) call prskyh(kskyh,nddlt+1)
      return
      end

      SUBROUTINE IMPMAT(A,L,M,NDIM,IUCT)
C     ==================================
      include'divapre.h'
      include'../Mesh/iodv.h'
      DIMENSION A(NDIM,M)
      if (iodv.eq.0) then

      K=1
 1    KK=K+7
C      IF (KK-M) 3,3,2
      if(KK.LE.M) goto 3
 2    KK=M
 3    WRITE(IUCT,200) (J,J=K,KK)
      DO 4 I=1,L
 4    WRITE(IUCT,201) I,(A(I,J),J=K,KK)
      K=K+8
C      IF (K-M) 1,1,5
      if(K.LE.M) goto 1
      if(K.GT.M) goto 5
                     else
      DO 10 J=1,M
      DO 10 I=1,L
#ifdef DIVABINARYFILES
          WRITE(IUCT) A(I,J)
#else
          WRITE(IUCT,1661) A(I,J)
#endif


 10   continue
      endif
 1661 format(1(E22.9))

 5    RETURN
 200  FORMAT('0',8(9X,I3,3X))
 201  FORMAT(1X,I3,8(E15.5))
      END


      subroutine locpt2(x,y,tcoog,kconn,ielem,isub,ipr)
C
C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 2)
C
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      ielem=-1
      isub=-1
      iel=0
 100  iel=iel+1
      if(iel.gt.nelt) goto 20
cmr
c      write(34,*) x,y,iel 
C
C  DOES DATA ID BELONG TO ELEMENT IEL ?
C
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 100
       isub=itria
       itria=1
C
C  WHICH SUB-ELEMENT IN ELEMENT IEL ?
C
      ielem=iel
c      iF(itria.eq.0) then
c         write(6,*) '%%% ERROR  LOCPT2 IN THE LOCALIZATION %%%'
c         stop
c      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',
     &        i4,'(',i1,')')
 20   continue
      return
      end


      subroutine locpt3(x,y,tcoog,kconn,tcele,ielem,isub,ipr)
C
C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 3)
C
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2),tcele(nelt,2)
      ielem=-1
      isub=-1
      iel=0
 100  iel=iel+1
      if(iel.gt.nelt) goto 20
C
C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 1 or 2)?
C
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      x4=tcoog(kconn(iel,7),1)
      y4=tcoog(kconn(iel,7),2)
      x0=tcele(iel,1)
      y0=tcele(iel,2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 50
C
C  WHICH SUB-ELEMENT IN ELEMENT IEL (1 or 2 ) ?
C
      ielem=iel
      call istria(x,y,x1,y1,x2,y2,x0,y0,itria)
      if(itria.ge.1) then
         isub=1
                     else
         isub=2
      endif
      goto 22
 50   continue
C
C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 3 or 4)?
C
      call istria(x,y,x1,y1,x3,y3,x4,y4,itria)
      if(itria.eq.0) goto 100
C
C  WHICH SUB-ELEMENT IN ELEMENT IEL (3 or 4 ) ?
C
      ielem=iel
      call istria(x,y,x1,y1,x0,y0,x4,y4,itria)
      if(itria.ge.1) then
         isub=4
                     else
         isub=3
      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',
     &        i4,'(',i1,')')
 20   continue
      return
      end

      Subroutine UWRITC(iu,c8,c4,valexc,iprec,imaxc,jmaxc,kmaxc,nbmots)
c                ======
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c writes the field C(I,J,K)  into fortran unit iu
c writes the field in the array c4 if iprecr=4
c writes the field in the array c8 if iprecr=8
c
c The KBLANC blank lines are at the disposal of the user
c JMB 6/3/91
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      PARAMETER(KBLANC=10)
      include'divapre.h'
      real*4 c4(*)
      real*8 c8(*)
c in the calling routin you can specify the following equivalence to
c save memory space:
c      equivalence(c,c4)
c      equivalence(c,c8)
c
c skip KBLANC lines
       do 1 kb=1,KBLANC
        write(iu,ERR=99)
 1     continue
c
        write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
c
c compute the number of full records to read and the remaining words
        nl=(imaxc*jmaxc*kmaxc)/nbmots
        ir=imaxc*jmaxc*kmaxc-nbmots*nl
        ide=0
c
c if pathological case, write only four values C0 and DCI,DCJ,DCK found 
c as the two four elements of the array so that C(I,J,K) =
c C0 + I * DCI + J * DCJ + K * DCK
        if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
         nl=0
         ir=4
        endif
c
c
c single precision
        if(iprec.eq.4) then
         do 10 kl=1,nl
          write(iu,ERR=99) (c4(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 10      continue
          write(iu,ERR=99) (c4(ide+kc),kc=1,ir)
                       else
c
c double precision
        if(iprec.eq.8) then
         do 20 kl=1,nl
          write (iu,ERR=99) (c8(ide+kc),kc=1,nbmots)
          ide=ide+nbmots
 20      continue
          write (iu,ERR=99) (c8(ide+kc),kc=1,ir)
                       else
           goto 99
         endif
         endif
c
         return
 99      continue
         write(*,*) 'Data error in UWRITC, not a conform file'
         return
         end


	 subroutine uwbimg4(iu,c4,nx,ny,nz,spval,dx,dy,xori,yori)
	 implicit none


	 integer nx,ny,nz,nt,ndim,icod
	 integer i,j,k,l,iu
	 real*4 c4(nx,ny,nz)
	 real*4 spval,dx,dy,xori,yori
	 character*80 record

         nt=1
	 ndim=1 
	 icod=1

	 write(iu) record
	 write(iu) record
	 write(iu) record
	 write(iu) record

	 xori=xori+dx
         yori=yori+dy

	 write(iu) nx,ny,nz,nt,ndim,icod
	 write(iu) xori,yori,dx,dy,spval
	 write(iu)(float(k),k=1,nz)

	 do l=1,nt
	   write(iu) float(l)
	   do k=1,nz
	      write(iu) ((c4(i,j,k),i=1,nx),j=1,ny)
           enddo
	 enddo
 
	 return
	 end




	 subroutine uwbimg8(iu,c8,nx,ny,nz,spvald,dxd,dyd,xorid,yorid)
	 implicit none

	 integer nx,ny,nz,nt,ndim,icod
	 integer i,j,k,l,iu
	 real*8 c8(nx,ny,nz)
	 real*8 spvald,dxd,dyd,xorid,yorid
	 real*4 spval,dx,dy,xori,yori
	 character*80 record

         spval=spvald
	 dx=dxd
	 dy=dyd
	 xori=xorid
	 yori=yorid

	 xori=xori+dx
         yori=yori+dy

         nt=1
	 ndim=1 
	 icod=1

	 write(iu) record
	 write(iu) record
	 write(iu) record
	 write(iu) record

	 write(iu) nx,ny,nz,nt,ndim,icod
	 write(iu) xori,yori,dx,dy,spval
	 write(iu) (float(k),k=1,nz)

	 do l=1,nt
	   write(iu) float(l)
	   do k=1,nz
	      write(iu) ((sngl(c8(i,j,k)),i=1,nx),j=1,ny)
           enddo
	 enddo
 
	 return
	 end



