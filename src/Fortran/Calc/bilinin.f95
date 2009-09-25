       subroutine bilinin(UF,VF,x0f,dxxf,dyxf,y0f,dxyf,dyyf &
                           ,imaxf,jmaxf,                    &
                          UT,VT,xt,yt)
!c                 ========
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C Interpolates from a regular full field into xt,yt
!C regular full field
!C Input geometry:
!C   x= x0f + I dxxf + J dyxf
!C   y= y0f + I dxyf + J dyyf
!C
!C
!C
!C Interpolated field Tt computed from field Tf
!C
!C
!C JMB 15/5/93
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      integer imaxf,jmaxf,imaxt,jmaxt,i,j,ii,jj
      real*4 UF(imaxf,jmaxf)
      real*4 VF(imaxf,jmaxf)
      real*4 UT,VT
      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf
      real*4 ri,rj,det,xt,yt,xi,yi,xj,yj
      real*4 xbt,ybt
!c     write(6,*) 'Interpolating',x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf
!c     write(6,*) ' to',x0t,dxxt,dyxt,y0t,dxyt,dyyt,imaxt,jmaxt
!c
      det=dxxf*dyyf-dxyf*dyxf
      XI=dyyf/det
      YI=-dyxf/det
      XJ=-dxyf/det
      YJ=dxxf/det

       Ri = XI * ( xt - x0f ) + YI * ( yt - y0f )
       Rj = XJ * ( xt - x0f ) + YJ * ( yt - y0f )
       ii = Ri
       jj = Rj
       Ri = Ri - ii
       Rj = Rj - jj

       if( ii.lt.1 ) then
        if(jj.lt.1 ) jj=1
        if(jj.ge.jmaxf) jj=jmaxf
        UT=UF(1,jj)
        VT=VF(1,jj)
                     else
          if(ii.ge.imaxf) then
             if(jj.lt.1 ) jj=1
             if(jj.ge.jmaxf) jj=jmaxf
             UT=UF(imaxf,jj)
             VT=VF(imaxf,jj)
                          else
             if(jj.lt.1 ) then          
                 UT=UF(ii,1)
                 VT=VF(ii,1)
                          else
                 if(jj.ge.jmaxf) then          
                  UT=UF(ii,jmaxf)
                  VT=VF(ii,jmaxf)
                                  else
!C Interpolate...
                  UT= rj* ( ri * UF(ii+1,jj+1)                &
                                + ( 1 - ri )*UF(ii,jj+1) )    &
                 + (1 -rj) * ( ri * UF(ii+1,jj)               &
                                + ( 1 - ri )*UF(ii,jj) )
                  VT= rj* ( ri * VF(ii+1,jj+1)                &
                                + ( 1 - ri )*VF(ii,jj+1) )    &
                 + (1 -rj) * ( ri * VF(ii+1,jj)               &
                                + ( 1 - ri )*VF(ii,jj) )

                      endif
             endif
          endif
        endif
 99     continue
        return
        end

        
        
               subroutine bilininl(UF,x0f,dxxf,dyxf,y0f,dxyf,dyyf     &
                           ,imaxf,jmaxf,                              &
                          UT,xt,yt)
!c                 ========
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C Interpolates from a regular full field into xt,yt
!C regular full field
!C Input geometry:
!C   x= x0f + I dxxf + J dyxf
!C   y= y0f + I dxyf + J dyyf
!C
!C
!C
!C Interpolated field Tt computed from field Tf
!C
!C
!C JMB 15/5/93
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      integer imaxf,jmaxf,imaxt,jmaxt,i,j,ii,jj
      real*4 UF(imaxf,jmaxf)

      real*4 UT,VT
      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf
      real*4 ri,rj,det,xt,yt,xi,yi,xj,yj
      real*4 xbt,ybt
!c     write(6,*) 'Interpolating',x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf
!c     write(6,*) ' to',x0t,dxxt,dyxt,y0t,dxyt,dyyt,imaxt,jmaxt
!c
      det=dxxf*dyyf-dxyf*dyxf
      XI=dyyf/det
      YI=-dyxf/det
      XJ=-dxyf/det
      YJ=dxxf/det

       Ri = XI * ( xt - x0f ) + YI * ( yt - y0f )
       Rj = XJ * ( xt - x0f ) + YJ * ( yt - y0f )
       ii = Ri
       jj = Rj
       Ri = Ri - ii
       Rj = Rj - jj

       if( ii.lt.1 ) then
        if(jj.lt.1 ) jj=1
        if(jj.ge.jmaxf) jj=jmaxf
        UT=UF(1,jj)

                     else
          if(ii.ge.imaxf) then
             if(jj.lt.1 ) jj=1
             if(jj.ge.jmaxf) jj=jmaxf
             UT=UF(imaxf,jj)

                          else
             if(jj.lt.1 ) then          
                 UT=UF(ii,1)

                          else
                 if(jj.ge.jmaxf) then          
                  UT=UF(ii,jmaxf)

                                  else
!C Interpolate...
                  UT= rj* ( ri * UF(ii+1,jj+1)                   &
                                + ( 1 - ri )*UF(ii,jj+1) )       &
                 + (1 -rj) * ( ri * UF(ii+1,jj)                  &
                                + ( 1 - ri )*UF(ii,jj) )


                      endif
             endif
          endif
        endif
 99     continue
        return
        end
