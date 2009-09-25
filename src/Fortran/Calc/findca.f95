      subroutine findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)
!C============================================================================
      real*8 x,y,tlcx,tlcy,tmix,tmiy
      integer ncax,ncay,ikn,jkn
!c      include'divapre.h'
!c      include'divainc.h'
!c      dimension kntc(ncax,ncay,*)

!C IN WHICH REGION IS THE DATA?
!cmr      ikn=((x-tmix-mod(x,tlcx))/tlcx)+1
!cmr      jkn=((y-tmiy-mod(y,tlcy))/tlcy)+1
      ikn=((x-tmix-mod(x-tmix,tlcx))/tlcx)+1
      jkn=((y-tmiy-mod(y-tmiy,tlcy))/tlcy)+1
!C JMB test
      ikn=((x-tmix)/tlcx)+1
      jkn=((y-tmiy)/tlcy)+1
!cmr added
      ikn=max(1,min(ikn,ncax))
      jkn=max(1,min(jkn,ncay))
!c      write(6,*) '????',ikn,jkn
!C============================================================================
      return
      end
