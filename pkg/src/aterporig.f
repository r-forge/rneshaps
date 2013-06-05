      subroutine aterporig(x0,y0,z0,n0,xg,yg,zg,nx,ny,
     x     ixf,iyf,fxf,fyf,
     x     z11f,z12f,z21f,z22f,nacon)

c     LLNL-CODE-637312

c     subroutine to interpolate z value at arbitrary x,y locations
c     from z values on regular rectangular grid, as returned by
c     the splus function interp()

c        interpolate to:
c     x0,y0 locations at which interpolated values are desired
c     z0    place to put interpolated values
c     n0    number or x0,y0 pairs
c
c        interpolate from:
c     xg,yg   x and y coordinates of grid to intepolate from
c     nx,ny   number of x and y values
c     zg      one-dimensional array of length nx*ny containing
c             z values for each x,y pair. zg is actually an
c             nx by ny matrix unrolled into a long 1-d array.

c     if x0,y0 is not between any pair of grid point, how do I return NA or NaN or something that will be
c     distinguishable from a number

c     size arrays big enough for practical use
c     assume that the splus function calling this routine will test
c     assumes that x0 and y0 are sorted smallest to largest

c     4/25/04 supply a very large negative number (nacon) when input
c     grid matrix has NA in R. Then return the same (nacon) whenever
c     any adjacent grid location is nacon. This handles the case when
c     the path we are interpolating onto goes outside the range of available data

c     6/19/10 change grid size xg, yg from 400 to 500 and zg to 250000

      double precision x0(500),y0(500),z0(500)
      double precision fxf(500),fyf(500)
c      dimension ixf(500),iyf(500)
      integer ixf(500),iyf(500)
      double precision z11f(500),z12f(500),z21f(500),z22f(500)
c      integer ixf,iyf
      double precision xg(500),yg(500),zg(250000)
      double precision nacon

      do 100 i0=1,n0
         x=x0(i0)
         y=y0(i0)
         do 200 ix=1,nx-1
            if (x .ge. xg(ix) .and. x .lt. xg(ix+1)) then
               do 300 iy=1,ny-1
                     if (y .ge. yg(iy) .and. y .lt. yg(iy+1)) then
                     fx=(x-xg(ix))/(xg(ix+1)-xg(ix))
                     fy=(y-yg(iy))/(yg(iy+1)-yg(iy))

                     z11=zg((iy-1)*nx+ix)
                     z12=zg(iy*nx+ix)
                     z21=zg((iy-1)*nx+ix+1)
                     z22=zg(iy*nx+ix+1)
                     if (z11 .le. nacon .or.
     x                    z12 .le. nacon .or.
     x                    z21 .le. nacon .or.
     x                    z22 .le. nacon) then
                        z0(i0)=nacon
                     else
                        z0(i0)=(1.-fy)*(1.-fx)*z11 +
     x                       (1.-fy)*fx*z12 +
     x                       fy*(1.-fx)*z21 +
     x                       fy*fx*z22
                     endif
                     ixf(i0)=ix
                     iyf(i0)=iy
                     fxf(i0)=fx
                     fyf(i0)=fy
                     z11f(i0)=z11
                     z12f(i0)=z12
                     z21f(i0)=z21
                     z22f(i0)=z22
                     goto 100
                     endif
 300           continue
               endif
 200      continue
 100   continue
       return
       end
