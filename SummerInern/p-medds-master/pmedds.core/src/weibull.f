      subroutine  weibull(ncases,PklMat,times,shape,scale,inorm)

      implicit none

      integer ncases, inorm
      real*8 PklMat(ncases,ncases),times(ncases)
      real*8 shape,scale
      real*8 dweibull,rweibull, psum
      integer k,l,m

       do k=1,ncases
         do l=1,ncases
            if(times(k) >= times(l)) then
               PklMat(k,l) = dweibull(times(k)-times(l),shape,scale)
            else
               PklMat(k,l)=0.0d0
            endif
         enddo
      enddo

! Normalize the matrix if inorm=0
      if (inorm .ne. 0) return

      do k=1,ncases
         do l=1,ncases
            psum=0.0d0
            do m=1,ncases 
               psum=psum + PklMat(k,m)
            enddo
            psum=psum-PklMat(k,k)
            if (psum == 0.0d0) Then
               PklMat(k,l) = 0.0d0
            else
               PklMat(k,l) = PklMat(k,l)/psum
            endif

         enddo ! end of loop on l  
      enddo ! End of loop on k

      return
      end subroutine weibull


! Calculate a weibull density f(x) given x and the shape and scale parameters

      function dweibull(x,shape,scale)

      implicit none
      real*8 dweibull
      real*8 x,shape,scale

      dweibull = (shape/scale)*(x/scale)**(shape-1.0d0) * 
     $     exp(-(x/scale)**shape)
      return

      end function dweibull


c$$$      FUNCTION rweibull(shape, SCALE) RESULT(ans)
c$$$      DOUBLE PRECISION SHAPE,scale,temp,ans
c$$$      IF (shape <= 0.0d0) THEN
c$$$
c$$$        WRITE(*,*) "Shape PARAMETER must be positive"
c$$$      END IF
c$$$      IF (scale <= 0.0d0) THEN
c$$$
c$$$        WRITE(*,*) "Scale PARAMETER must be positive"
c$$$      END IF
c$$$      CALL RANDOM_NUMBER(temp)
c$$$      ans= SCALE * (-log(temp))**(1.0 / SHAPE)
c$$$      END FUNCTION rweibull

!
