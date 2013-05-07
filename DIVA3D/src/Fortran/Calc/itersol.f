


       subroutine itersol(skmatx,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
C
C
C      include'divapre.h'
C      REAL*8 VKGS(*),VKGD(*),VFG(*)
      implicit none
      REAL*8 skmatx(*),VKGD(*),VFG(*)
      Integer*4 KLD(*),neq,ifac,isol
#ifdef DIVAITERATIVE
      integer, save:: isfirsttime=1
C      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: skmatx
      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: bidon
      integer, DIMENSION(:) , SAVE,   ALLOCATABLE :: maxa

      REAL*8, DIMENSION(:),    ALLOCATABLE :: AO
      integer, DIMENSION(:) ,   ALLOCATABLE :: JAO,IAO
      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: A
      integer, DIMENSION(:) , SAVE,   ALLOCATABLE :: JA,IA,INDU

      REAL*8, DIMENSION(:), SAVE,   ALLOCATABLE :: ALU,WU,WL,SOL,VV
      integer, DIMENSION(:), SAVE, ALLOCATABLE :: JR,JWU,JWL,JU,IWORK
      integer, DIMENSION(:), SAVE, ALLOCATABLE :: JLU

      integer ntotal ,ierr,iwk
      real*8 residue,erress

      integer i,j,k,k0,k00,kmax,nthreads,ntotv
      integer nthreadsasked
      common/forsolver/   nthreadsasked
      integer nsk
      integer omp_get_max_threads
      external omp_get_max_threads
      real*8 d    ,total,abstol,reltol,tol,eps
      integer kk,nbest,nitermax

      integer omp_get_thread_num
      external OMP_GET_THREAD_NUM
      integer omp_get_num_procs
      external omp_get_num_procs
      integer imod ,lfil ,im

      im=50
      ntotv=neq
      write(6,*) 'Into new iterative solver'
      if (isfirsttime.eq.1) then
      nsk=neq+kld(neq+1)-1
C      write(6,*) 'total number of elements',nsk,neq
C      ALLOCATE(skmatx(nsk))
      ALLOCATE(bidon(neq))
      ALLOCATE(maxa(neq))


      maxa(1)=1
      do i=1,NEQ
      bidon(i)=VKGD(i)
      enddo


      do i=2,NEQ

      maxa(i)=maxa(i-1)+KLD(i+1)-KLD(i)+1
c        do kk=1,KLD(i+1)-KLD(i)
c          skmatx(maxa(i-1)+kk)=VKGS(KLD(i)+kk-1)
c        enddo
c      skmatx(maxa(i))=VKGD(i)
      enddo
C Check alternative way without allocating big matrix but only array so that reordering can be done
C
      do i=NEQ,1,-1
          skmatx(maxa(i))=bidon(i)
      do kk=KLD(i+1)-KLD(i),1,-1
          skmatx(maxa(i-1)+kk)=skmatx(KLD(i)+kk-1)
      enddo
      enddo
      DEALLOCATE(bidon)

      ntotal=0
      do i=1,maxa(neq)
        if(skmatx(i).ne.0) ntotal=ntotal+1
      enddo

      write(6,*) 'non Zeros',ntotal,maxa(neq),ntotal/maxa(neq)*100
C Based on number of nonzeros, make some allocation for arraus
C     2*ntotal-neq
      imod=2
C From symmetric skyline to symmetric compressed row
      allocate(AO(ntotal))
      allocate(JAO(ntotal))
      allocate(IAO(neq+1))

      call sskssr ( neq, imod, skmatx, maxa, ao, jao, iao, ntotal,ierr)
      write(6,*) 'to CSRS',ierr

      allocate(A(2*ntotal-neq+1))
      allocate(JA(2*ntotal-neq+1))
      
      allocate(IA(neq+1))
      allocate(indu(neq+1))

C From symmetric CRS to general CRS
      call ssrcsr(neq,ao,jao,iao,2*ntotal-neq,a,ja,ia,indu,ierr)
      write(6,*) 'to CSRSS',ierr
      deallocate(AO)
      deallocate(JAO)
      deallocate(IAO)

C Check ordering or sort anyway (Maybe take test out)
C      allocate(iwork(4*ntotal-2*neq+3))
C      call csort ( neq, a, ja, ia, iwork, .true. )
C      deallocate(IWORK)

      lfil=15+nint(0.1*sqrt(neq*1.0))
C      lfil=30
      iwk=2*ntotal-neq+(2*lfil)*neq+1
      allocate(ALU(iwk))
      allocate(JLU(iwk))

      allocate(WU(neq+1))
      allocate(WL(neq+1))
      allocate(JU(neq+1))
      allocate(JR(neq+1))
      allocate(JWU(neq+1))
      allocate(JWL(neq+1))

      allocate(SOL(neq))

      allocate(vv(neq*(im+1)))

      isfirsttime=0
      endif
C Finished creation of matrices

C If factorisation: now make preconditioning with large ilut
      tol=0.00001


      if(IFAC.EQ.1) then
      call ilut(neq,a,ja,ia,lfil,tol,alu,jlu,ju,iwk,wu,wl,jr,
     &    jwl, jwu, ierr )
      write(6,*) 'ilut',ierr
C2345
      endif

      if(ISOL.EQ.1) then
C IF solution, now iterate with PGMRES
C abstol=0.0001*sqrt(dot_product(VFG(1:NEQ),VFG(1:NEQ)))
      eps=0.000001
      nitermax=nint(sqrt(neq*1.0))+1
      do i=1,NEQ
C      write(6,*) skmatx(maxa(i))
      sol(i)=VFG(i)/(1+skmatx(maxa(i)) )
C      sol(i)=0
      enddo
      erress= sqrt(dot_product(VFG(1:NEQ),VFG(1:NEQ)))

       call pgmres ( neq, im, VFG, sol, vv, eps, nitermax, 6,
     &    a, ja, ia, alu, jlu, ju, ierr,residue )


       write(6,*) 'pmgres',ierr,
     & residue/erress,erress
       do i=1,NEQ
       VFG(i)=sol(i)
       enddo

      endif
#endif
      return
      end