************************************************************************
      PROGRAM DATANAMTRANS
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbccol,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans,idum
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL,y
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght,ran2,lon,lat,var,triage
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
      CHARACTER (len=99)   :: ALLDATA
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO,VARFILEOU
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name
!
      LOGICAL                           :: exist
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) itrans
      READ(44,*) first_level
      READ(44,*) last_level
      READ(44,*) nblines
      READ(44,*) nbccol
      READ(44,*) istep
      CLOSE(44)
      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(y(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
        WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep
        WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
!
        WRITE(VARFILEOU,
     &'(a,"_noise")')TRIM(var_name)
        WRITE(ALLDATA,
     &'("../input/divadata/",a)')TRIM(VARFILEOU)
!
       OPEN(10, FILE=datafile, STATUS='OLD')
       IF(nbccol == 10) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n)
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
       CLOSE(10)
!
       i=0
       DO n = 1,nblines
        i=i+1
        idum=-54786228+i*12454583
        y(n)=VARVAL(n)+ran2(idum)*0.0000001
       ENDDO
!
      INQUIRE(FILE=ALLDATA,EXIST=exist)
      IF(exist) THEN

      OPEN(UNIT=11,FILE=ALLDATA,STATUS='OLD',POSITION='APPEND')
      ELSE

      OPEN(UNIT=11,FILE=ALLDATA,STATUS='NEW')
      ENDIF

       IF(nbccol == 10) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,TRIM(FINLINE(1,n))
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
         ENDDO
       ENDIF
       CLOSE(11)
       STOP
  999 continue
      write(*,*)'data groupment and noise: '
      write(*,*)'total number of data does not match for '
      write(*,*)TRIM(datafile)
!
      STOP
!
!-----Done--------------------------------------------------------------
!
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0?#<
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-8,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END




C  (C) Copr. 1986-92 Numerical Recipes Software 0?#<
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-8,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
