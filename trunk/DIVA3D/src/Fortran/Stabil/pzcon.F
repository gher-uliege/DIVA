      subroutine pzcon(cnvrsn,grav,dynz,prsdb,depth)
c
c     title:
c     *****
c
c       pzcon  -- convert pressure in decibars to depth in meters
c                 (or visa versa)
c
c     system:
c     ******
c
c       pacodf hydrographic data library
c
c     purpose:
c     *******
c
c       to calculate depth in meters (mantyla,saunders)
c       from pressure in decibars (or pressure from depth).
c
c       ref: journ. physical ocean.,vol 11 no. 4, april, 1981
c            (saunders)
c            private correspondence 1982-1983
c            (mantyla)
c
c     method:
c     ******
c
c       a standard ocean (salinity=35.0,t=0.0) is used plus a dynamic
c       height correction to account for deviations from the standard
c       ocean. pressure to depth conversion is effected as:
c
c       z = p/(b*c) + dynz/b
c
c       where:
c
c          p    = insitu pressure (decibars)
c          b    = insitu gravity as a function of latitude and pressure
c                 (decameters/sec/sec)
c          c    = insitu mean density rho(35.0,0.0,p)
c                 (grams/centimeter**3)
c          dynz = dynamic height in dynamic meters
c          z    = depth in meters
c
c     parameters:
c     **********
c
c       cnvrsn  -> conversion to be performed:
c                  0 = pressure to depth
c                  1 = depth to pressure
c       grav    -> acceleration of gravity meters/sec/sec
c                  at station latitude, pressure =0.0db
c       dynz    -> dynamic height in dynamic meters
c       prsdb   -> pressure in decibars (cnvrsn=0)
c               <- pressure in decibars (cnvrsn=1)
c       depth   <- depth in meters (cnvrsn=0)
c               -> depth in meters (cnvrsn=1)
c
        integer cnvrsn
        real grav,prsdb,dynz,depth
c
c     variables:
c     *********
c
        real*4 a,b,c
        real*8 dd,dg,dh,da,db,dc
c
c     external functions:
c     ******** *********
c
        integer isnan
c
c     code:
c     ****
c
c     select
c       (convert pressure to depth):
          if(cnvrsn.ne.0) go to 10
          a = 2.2e-6*prsdb
c             /* pressure correction to gravity */
          b = 0.1*(grav+0.5*a)
c             /* insitu gravity decameters/sec/sec */
c
          c = 1.0285+a
c             /* insitu mean density rho(35.0,0.0,p) */
c
          depth = prsdb/(b*c)
c             /* pressure to depth conversion in standard ocean */
c
c         select
c           (dynamic height):
c
        	if(dynz.eq.9.9e37) then
		isnan = 1
	        else
		isnan = 0
	        endif
              if(isnan.eq.0)
     1        depth = depth + dynz/b
c           (otherwise):
c         end select
          go to 999
c       (convert depth to pressure):
   10     continue
          dd = dble(depth)
          dg = dble(grav*0.1)
          dh = dble(dynz)
          da = 2.42d-13*dd
          db = dd*(1.13135d-7+2.2e-6*dg)-1.0
          dc = dg*dd
c         select
c           (dynamic height):


        	if(dynz.eq.9.9e37) then
		isnan = 1
	        else
		isnan = 0
	        endif
              if(isnan.ne.0) go to 20
              db = db - 2.2d-6*dh
              dc = dc-dh
c           (otherwise):
   20         continue
c         end select
c
          dc = 1.0285d0*dc
c
          prsdb  = 0.0
          if(da.ne.0.0)
     1    prsdb  = sngl((-db-dsqrt(db*db-4.0d0*da*dc))/(da+da))
c     end select
c
c     /* return */
c
  999 continue
c
      return
c
c     end pzcon
c
      end
