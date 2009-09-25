
      
      subroutine uur(iu,U,im,jm,valex,valrepl)
      real*4 U(im,jm)
      real*8 c8
      real*4 valex
      valex=0
      write(6,*) 'Going to read constraint file, size ', im,jm 
      call ureadc(iu,c8,U,valex,ipr,imax,jmax,kmax,lw)
      write(6,*) 'Finished: read constraint file, size ', imax,jmax
      if(im.ne.imax) write(6,*) 'Problem in constraint file',iu
      if(jm.ne.jmax) write(6,*) 'Problem in constraint file',iu
      
!C implement it with ureadc and check for dimensions be coherent with info file
!c      do i=1,im
!C      read(iu,*) (U(i,j),j=1,jm)
!C      enddo
      
      do i=1,im
       do j=1,jm
!c       if(abs(U(i,j)).gt.999.) U(i,j)=0
       if(U(i,j).eq.valex) U(i,j)=valrepl
  

       enddo
       enddo
      return
      end
