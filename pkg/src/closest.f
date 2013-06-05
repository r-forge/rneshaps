      subroutine closest(x0,y0,z0,n0,xg,yg,zg,nx,ny,ixf,iyf,fxf,fyf,
     .     z11f,z12f,z21f,z22f,nacon)

c     LLNL-CODE-637312

c     subroutine to get z value at arbitrary x,y locations
c     from z values at closest location on regular rectangular grid, as returned by
c     the splus function interp()

c     x0,y0 locations at which interpolated values are desired
c     z0    place to put interpolated values
c     n0    number or x0,y0 pairs
c     xg,yg   x and y coordinates of grid to intepolate from
c     nx,ny   number of x and y values
c     zg      one-dimensional array of length nx*ny containing
c             z values for each x,y pair. zg is actually an
c             nx by ny matrix unrolled into a long 1-d array.

c     size arrays big enough for practical use
c     assume that the splus function calling this routine will test
c     assumes that x0 and y0 are sorted smallest to largest

c     4/25/04 supply a very large negative number (nacon) when input
c     grid matrix has NA in R. Then return the same (nacon) whenever
c     any adjacent grid location is nacon. This handles the case when
c     the path we are interpolating onto goes outside the range of available data

c     10/25/12 increase dimensions to 800, 640000

      double precision x0(800),y0(800),z0(800)
      double precision fxf(800),fyf(800)
      integer ixf(800),iyf(800)
      double precision z11f(800),z12f(800),z21f(800),z22f(800)
      double precision xg(800),yg(800),zg(640000)
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
                        if ( (fx .le. 0.5) .and. (fy .le. 0.5) ) then
                           z0(i0) = z11
                       elseif (( fx .gt. 0.5) .and. (fy .le. 0.5) ) then
                           z0(i0) = z12
                       elseif ( (fx .le. 0.5) .and. (fy .gt. 0.5) ) then
                           z0(i0) = z21 
                       elseif ( (fx .gt. 0.5) .and. (fy .gt. 0.5) ) then
                           z0(i0) = z22
                        endif
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
