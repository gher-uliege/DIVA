

C      subroutine newsol(VKGS,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
       subroutine newsol(skmatx,VKGD,VFG,KLD,NEQ,IFAC,ISOL)
C
C Interface with a parallel skyline solver code adapted from
C
C Nielsen, C. V., Zhang, W., Alves, L., Bay, N., & Martins, P. F. (2012).
C   Modeling of Thermo-Electro-Mechanical Manufacturing Processes with
C   Applications in Metal Forming and Resistance Welding. Springer.
C
C      include'divapre.h'
C      REAL*8 VKGS(*),VKGD(*),VFG(*)
      implicit none
      REAL*8 skmatx(*),VKGD(*),VFG(*)
      Integer*4 KLD(*)      
      integer neq,ifac,isol,nsk
#ifdef DIVAPARALLEL
      integer, save:: isfirsttime=1
C      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: skmatx
      REAL*8, DIMENSION(:) , SAVE,   ALLOCATABLE :: bidon
      integer, DIMENSION(:) , SAVE,   ALLOCATABLE :: maxa
      integer i,id,ie,ie0,ie0old,ih1,ih2,ihesitate,
     &      iloop,iquit,ir,is,ithread,iwait,j,jd,
     &       jh,jmax,jr,k,k0,k00,kmax,nthreads,ntotv
      integer nthreadsasked
      logical ntt
      common/forsolver/   nthreadsasked
      
      integer omp_get_max_threads
      external omp_get_max_threads
      real*8 d    ,total
      integer kk,nbest

      integer omp_get_thread_num
      external OMP_GET_THREAD_NUM
      integer omp_get_num_procs
      integer omp_get_num_threads
c      integer omp_set_dynamic
c      external omp_set_dynamic
      external omp_get_num_threads
      external omp_get_num_procs
      ntotv=neq
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
C Check
c      total=0
c      total2=0
c      total3=0
c      do ii=1,maxa(neq)
c        total=total+abs(skmatx(ii)-VKGS(ii))
c        total2=total2+abs(VKGS(ii))
c        total3=total3+abs(skmatx(ii))
c      enddo
c      write(6,*) 'Test',total,total2,total3
      total=0
      do i=1,maxa(neq)
        if(skmatx(i).eq.0) total=total+1
      enddo
      write(6,*) 'Zeros',total,maxa(neq),total/maxa(neq)*100

      isfirsttime=0
c      WRITE(6,*) 'last element',maxa(neq)
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
C guess on number of threads to be usefull

      nthreads=omp_get_num_procs()
C      write(6,*) 'NTHREADS',nthreads
      nbest=4*neq/160000+2
C      nbest=1
      if(nthreadsasked.gt.0) nbest=nthreadsasked
      if(nbest.gt.nthreads) nbest=nthreads
      call omp_set_num_threads(nbest)
! Get (actual) number of threads

      write(6,*) 'Dynamic threading'
      call omp_set_dynamic(ntt)
      nthreads=omp_get_max_threads()
      write(6,*) 'NTHREADS',nthreads
! Initializations
      jmax=1
      kmax=1
C$OMP PARALLEL DEFAULT (NONE)
C$OMP& PRIVATE (d,i,id,ie,ie0,ie0old,ih1,ih2,ihesitate,
C$OMP& iloop,iquit,ir,is,ithread,iwait,j,jd,jh,
C$OMP& jr,k,k0,k00)
C$OMP& SHARED (VFG,jmax,kmax,maxa,nthreads,ntotv,
C$OMP& skmatx)
! Factorize skmatx and reduce fmatx
C       write(6,*) 'Next omp call'
        write(6,*) 'Thrads in use', omp_get_num_threads()
       ithread=omp_get_thread_num()
C       write(6,*) 'In Parallel section thread',ithread
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

C$OMP FLUSH (kmax)
           do while (kmax.lt.jd)
! Initializations
            ihesitate=0
            ie0old=ie0
! JMB2013 why flush in jmax here
C$OMP FLUSH (kmax)
! Judge if hesitation is necessary
             if (kmax.lt.jr) then
               ihesitate=1
!               write(6,*) 'Hesitation'
C$OMP FLUSH (jmax)
               ie0=jmax
             endif
             if (jh.eq.2) then
! Reduce diagonal term
               iwait=1
               do while (iwait.eq.1)
C$OMP FLUSH (kmax)
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
C$OMP CRITICAL
C$OMP FLUSH (jmax)
C JMB TRY FLUSH jmax ??? ALSO CHANGE !OME into COPM ?
                 if (j.gt.jmax) then
                  jmax=j
                  kmax=jd
                 endif
C$OMP END CRITICAL
               endif
C$OMP FLUSH (jmax,kmax)
             write(6,*) ithread,jmax,kmax,ihesitate,jr,is,jh,j,id,ih1
            enddo
          enddo





C$OMP END PARALLEL


C Take out parts working on right hand side in all cases
C      write(6,*) 'Factorized'
      endif


      if(isol.eq.1) then
C
C      write(6,*) 'Reduction'


            call omp_set_num_threads(1)
! Get (actual) number of threads
      nthreads=omp_get_max_threads()
C      write(6,*) 'NTHREADS',nthreads
! Initializations
      jmax=1
      kmax=1
C$OMP PARALLEL DEFAULT (NONE)
C$OMP& PRIVATE (d,i,id,ie,ie0,ie0old,ih1,ih2,ihesitate,
C$OMP& iloop,iquit,ir,is,ithread,iwait,j,jd,jh,
C$OMP& jr,k,k0,k00)
C$OMP& SHARED (VFG,jmax,kmax,maxa,nthreads,ntotv,
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
C               iwait=1

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









C      write(6,*) 'Backward'
      
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