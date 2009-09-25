      subroutine N2brunt(valex,imax,jmax,maxt,Z,ict,alpha,beta,T,S,N2,rmsp,rmsn)
      implicit none
!
! in : z, T, S fields
! out: N2 = db/dz field, where b is the buoyency
!      rmsn = rms of all the negative N2 in the field
!      rmsp =                positive
!
      integer  :: imax,jmax,maxt
      real, dimension(imax,jmax,maxt), intent(in) :: S,T
      real, dimension(imax,jmax)                  :: alpha,beta
      integer ,dimension(imax,jmax,maxt), intent(in) :: ict
      real, dimension(imax,jmax,maxt-1), intent(out) :: N2
      real, dimension(maxt), intent(in) :: Z
      real, intent(out) :: rmsp,rmsn

      real, dimension(imax,jmax,maxt) :: B
      real :: DZ,G,FACA,FACB
!
      real :: valex
      integer :: i,j,k, nv,np,nn,istat

! inplace bouyancy in place
!
      G = 9.81
      do j=1,jmax
      do i=1,imax
        IF (ict(i,j,1) == 1) then
           B(i,j,1) = G*alpha(i,j)*(T(i,j,2)-T(i,j,1))
           B(i,j,2) = G*beta(i,j) *(S(i,j,2)-S(i,j,1))
        else
          B(i,j,1:2) = valex
        endif
      enddo
      enddo
!
      nn=0
      np=0
      nv=0
      rmsn=0.
      rmsp=0.

      k=1
       do j=1,jmax
        do i=1,imax
           if (ict(i,j,k) == 1 ) then
              N2(i,j,k)=(B(i,j,1)-B(i,j,2))/(Z(k+1)-Z(k))
              if (N2(i,j,k).lt.0.) then
                rmsn=rmsn+(N2(i,j,k)*N2(i,j,k))
                nn=nn+1
              else
                rmsp=rmsp+(N2(i,j,k)*N2(i,j,k))
                np=np+1
              end if
           else
              N2(i,j,k)=valex
              nv=nv+1
           end if
        end do
       end do
      !=========

      if (nn.gt.0) then
        rmsn=sqrt(rmsn/nn)
      else
        rmsn=0.0
      end if
      if (np.gt.0) then
        rmsp=sqrt(rmsp/np)
      else
        rmsp=0.0
      end if
!
!xx      write(*,*)'  rmsp = ',rmsp
!xx      write(*,*)'  rmsn = ',rmsn

      end subroutine
