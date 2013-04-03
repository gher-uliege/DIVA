

      subroutine newsol(VKGS,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
C      include'divapre.h'
      REAL*8 VKGS(*),VKGD(*),VFG(*)
      Integer*4 KLD(*)
#ifdef DIVAPARALLEL
      integer, save:: isfirsttime=1
      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: skmatx
      integer, DIMENSION(:) , SAVE,   ALLOCATABLE :: maxa
      integer i,id,ie,ie0,ie0old,ihl,ih2,ihesitate,
     &      iloop,iquit,ir,is,ithread,iwait,j,jd,
     &       jh,jmax,jr,k,k0,k00,kmax,nthreads,ntotv
      integer omp_get_max_threads,omp_get_thread_num
      ntotv=neq
      if (isfirsttime.eq.1) then
      nsk=neq+kld(neq+1)-1
      write(6,*) 'total number of elements',nsk,neq
      ALLOCATE(skmatx(nsk))
      ALLOCATE(maxa(neq))

      ipoint=1
      maxa(1)=1
      skmatx(1)=VKGD(1)
      do i=2,NEQ

      maxa(i)=maxa(i-1)+KLD(i+1)-KLD(i)+1
        do kk=1,KLD(i+1)-KLD(i)
          skmatx(maxa(i-1)+kk)=VKGS(KLD(i+1)-kk)
          skmatx(maxa(i-1)+kk)=VKGS(KLD(i)+kk-1)
C          skmatx(maxa(i)-kk)=VKGS(KLD(i)+kk-1)
C         skmatx(maxa(i-1)+(KLD(i+1)-KLD(i)-kk) )=VKGS(KLD(i)+kk-1)
        enddo


      skmatx(maxa(i))=VKGD(i)
      enddo
      isfirsttime=0
      WRITE(6,*) 'last element',maxa(neq)
      endif
C skmatx(jd)  -> VKGD(j)
C skmatx(jr)  -> VKGD(j-1)
C skmatx(jr+1) -> VKGS(KLD(j))
C skmatx(mata(z)) -> VKGD(z)
C skmatx(u) -> ??? need to find column
C maybe better given index ii of skmatx(u) provide jj and ii bof, pas sur que contigue:
C So maube better create the matrix in full form ... not an option since double memory need ?
C tried for now so if efficient maybe try to fill on the matrix directly at the source and just put dummy for the rest ?
C
C

      if (ifac.eq.1) then
C Factorize
      call omp_set_num_threads(6)
! Get (actual) number of threads
      nthreads=omp_get_max_threads()
      write(6,*) 'NTHREADS',nthreads
! Initializations
      jmax=l
      kmax=l
C$OMP PARALLEL DEFAULT (NONE)
C$OMP& PRIVATE (d,i,id,ie,ie0,ie0old,ih1,ih2,ihesitate,
C$OMP& iloop,iquit,ir,is,ithread,iwait,j,jd,jh,
C$OMP& jr,k,k0,k00)
C$OMP& SHARED (VFG,jmax,kmax,maxa,nthreads,ntotv,ifac,
C$OMP& skmatx)
! Factorize skmatx and reduce fmatx
       ithread=omp_get_thread_num()
       iloop=0
       iquit=0
       do while (iquit.eq.0)
          iloop=iloop+1
          j=(iloop-1)*nthreads+ithread+2
           if (j.gt.ntotv) then
              iquit=1
              exit
           endif
! Characteristic positions in skyline
          jr=maxa(j-1)
          jd=maxa(j)
          jh=jd-jr
          is=j-jh+2
! Start of core code
          ie0=0

!$OMP flush (kmax)
           do while (kmax.lt.jd)
! Initializations
            ihesitate=0
            ie0old=ie0
!$OMP flush (jmax,kmax)
! Judge if hesitation is necessary
             if (kmax.lt.jr) then
               ihesitate=1
               ie0=jmax
             endif
             if (jh.eq.2) then
! Reduce diagonal term
               iwait=1
               do while (iwait.eq.1)
!$OMP flush (kmax)
                if (kmax.ge.jr-1) then
                   d=skmatx(jr+1)
                   skmatx(jr+1)=d/skmatx(jr)
                   skmatx(jd)=skmatx(jd)-d*skmatx(jr+1)
                   iwait=0
                endif
               enddo
! Reduce right hand side (VFG)
C               VFG(j)=VFG(j)-skmatx(jr+1)*VFG(j-1)
               ihesitate=0
             elseif (jh.gt.2) then
! Reduce all equations except diagonal
                ie=jd-1+(ie0-j+1)*ihesitate
                k00=jh-j-1+ie0old
                if (k00.lt.0) k00=0
                k0=0
                do k=max0(jr+2,jd-j+ie0old+1),ie
                   ir=maxa(is+k0+k00-1)
                   id=maxa(is+k0+k00)
                   ih1=min0(id-ir-1,1+k0+k00)
                   if (ih1.gt.0) then
                    ih2=min0(id-ir-j+(j-1-k0-k00)
     &                 *ihesitate,
     &                  2-j+k0+k00+(j-1-k0-k00)
     &                  *ihesitate)
                    if (ih2.lt.1) ih2=1
                    skmatx(k)=skmatx(k)
     &     -dot_product(skmatx(k-ih1:k-ih2),skmatx (id-ih1: id-ih2))
                   endif
                   k0=k0+1
                enddo
                if (ihesitate.eq.0) then
! Reduce diagonal term
                  ir=jr+1
                  ie=jd-1
                  k=j-jd
                  do i=ir,ie
                    id=maxa(k+i)
                    d=skmatx(i)
                    skmatx(i)=d/skmatx(id)
                    skmatx(jd)=skmatx(jd)-d*skmatx(i)
                  enddo
! Reduce right hand side (VFG)
C                  VFG(j)=VFG(j)
C     &  -dot_product(skmatx(jr+1:jr+jh-1),VFG(is-1:is+jh-3))
                 endif
               endif
               if (ihesitate.eq.0) then
!$OMP critical
                 if (j.gt.jmax) then
                  jmax=j
                  kmax=jd
                 endif
!$OMP end critical
               endif
!$OMP flush (jmax,kmax)
            enddo
          enddo





!$OMP END PARALLEL


C Take out parts working on right hand side in all cases
      write(6,*) 'factorizing'
      endif


      if(isol.eq.1) then
C
      write(6,*) 'Reduction'
      
      
            call omp_set_num_threads(4)
! Get (actual) number of threads
      nthreads=omp_get_max_threads()
      write(6,*) 'NTHREADS',nthreads
! Initializations
      jmax=l
      kmax=l
C$OMP PARALLEL DEFAULT (NONE)
C$OMP& PRIVATE (d,i,id,ie,ie0,ie0old,ih1,ih2,ihesitate,
C$OMP& iloop,iquit,ir,is,ithread,iwait,j,jd,jh,
C$OMP& jr,k,k0,k00)
C$OMP& SHARED (VFG,jmax,kmax,maxa,nthreads,ntotv,ifac,
C$OMP& skmatx)
! Factorize skmatx and reduce fmatx
       ithread=omp_get_thread_num()
       iloop=0
       iquit=0
       do while (iquit.eq.0)
          iloop=iloop+1
          j=(iloop-1)*nthreads+ithread+2
           if (j.gt.ntotv) then
              iquit=1
              exit
           endif
! Characteristic positions in skyline
          jr=maxa(j-1)
          jd=maxa(j)
          jh=jd-jr
          is=j-jh+2
! Start of core code
          ie0=0

!$OMP flush (kmax)
           do while (kmax.lt.jd)
! Initializations
            ihesitate=0
            ie0old=ie0
!$OMP flush (jmax,kmax)
! Judge if hesitation is necessary
             if (kmax.lt.jr) then
               ihesitate=1
               ie0=jmax
             endif
             if (jh.eq.2) then
! Reduce diagonal term
               iwait=1

!$OMP flush (kmax)

! Reduce right hand side (VFG)
               VFG(j)=VFG(j)-skmatx(jr+1)*VFG(j-1)
               ihesitate=0
             elseif (jh.gt.2) then
! Reduce all equations except diagonal
                ie=jd-1+(ie0-j+1)*ihesitate
                k00=jh-j-1+ie0old
                if (k00.lt.0) k00=0
                k0=0

                if (ihesitate.eq.0) then
! Reduce diagonal term
                  ir=jr+1
                  ie=jd-1
                  k=j-jd

! Reduce right hand side (VFG)
                  VFG(j)=VFG(j)
     &  -dot_product(skmatx(jr+1:jr+jh-1),VFG(is-1:is+jh-3))
                 endif
               endif
               if (ihesitate.eq.0) then
!$OMP critical
                 if (j.gt.jmax) then
                  jmax=j
                  kmax=jd
                 endif
!$OMP end critical
               endif
!$OMP flush (jmax,kmax)
            enddo
          enddo





!$OMP END PARALLEL









      write(6,*) 'Backward'
      
      do i=1,ntotv
        id=maxa(i)
        VFG(i)=VFG(i)/skmatx(id)
      enddo
! Back substitution
      j=ntotv
      jd=maxa(j)
  100 d=VFG(j)
      j=j-1

      if (j.le.0) return
      jr=maxa(j)
      if (jd-jr.gt.1) then
       is=j-jd+jr+2
       k=jr-is+1
       VFG(is:j)=VFG(is:j)-skmatx(is+k:j+k)*d
      endif
      jd=jr
      goto 100

      endif

#endif
      return
      end