      subroutine binom(ireal,ncases,nwrtf,PklMat,Rl, iseed, iverborse)

      implicit none

      integer ncases, ireal, nwrtf,iseed, iverborse
      real*8 Rl(ncases,ireal)
      real*8 PklMat(ncases,ncases)
      integer i,j,k,l
      real*8 pkl, bnldev
      logical verborse

      Rl = 0.0d0

      if (iverborse .eq. 0) then
         verborse=.false.
      else
         verborse=.true.
      endif



      do i=1,ireal
         if (mod(i,nwrtf) == 0 .and. verborse) 
     $        call intpr('Iteration ',-1,i,1)
         do j=1,ncases
            do k=1,ncases
               pkl = PklMat(k,j)
               Rl(j,i) = Rl(j,i) + bnldev(pkl,1,iseed)
            enddo
         enddo
      enddo

      return
      end subroutine binom


      FUNCTION bnldev(pp,n,idum)  
      INTEGER idum,n  
      REAL*8 bnldev,pp,PI  
CU    USES gammln,ran2  
      PARAMETER (PI=3.141592654d0)  
      INTEGER j,nold  
      REAL*8 am,em,en,g,oldg,p,pc,pclog,plog,pold,sq,t,y,gammln,ran2  
      SAVE nold,pold,pc,plog,pclog,en,oldg  
      DATA nold /-1/, pold /-1./  

      if(pp.le.0.5)then  
        p=pp  
      else  
        p=1.-pp  
      endif  
      am=n*p  
      if (n.lt.25)then  
        bnldev=0.  
        do 11 j=1,n  
          if(ran2(idum).lt.p)bnldev=bnldev+1.  
11      continue  
      else if (am.lt.1.) then  
        g=exp(-am)  
        t=1.  
        do 12 j=0,n  
          t=t*ran2(idum)  
          if (t.lt.g) goto 1  
12      continue  
        j=n  
1       bnldev=j  
      else  
        if (n.ne.nold) then  
          en=n  
          oldg=gammln(en+1.)  
          nold=n  
        endif  
        if (p.ne.pold) then  
          pc=1.-p  
          plog=log(p)  
          pclog=log(pc)  
          pold=p  
        endif  
        sq=sqrt(2.*am*pc)  
2       y=tan(PI*ran2(idum))  
        em=sq*y+am  
        if (em.lt.0..or.em.ge.en+1.) goto 2  
        em=int(em)  
        t=1.2*sq*(1.+y**2)*exp(oldg-gammln(em+1.)-gammln(en-em+1.)+em*  
     *plog+(en-em)*pclog)  
        if (ran2(idum).gt.t) goto 2  
        bnldev=em  
      endif  
      if (p.ne.pp) bnldev=n-bnldev  
      return  
      END  

      FUNCTION ran2(idum)  
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV  
      REAL*8 ran2,AM,EPS,RNMX  
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,  
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)  
      INTEGER j,k,iv(NTAB),iy  
      SAVE iv,iy  
      DATA iv /NTAB*0/, iy /0/  
      if (idum.le.0.or.iy.eq.0) then  
        idum=max(-idum,1)  
        do 11 j=NTAB+8,1,-1  
          k=idum/IQ  
          idum=IA*(idum-k*IQ)-IR*k  
          if (idum.lt.0) idum=idum+IM  
          if (j.le.NTAB) iv(j)=idum  
11      continue  
        iy=iv(1)  
      endif  
      k=idum/IQ  
      idum=IA*(idum-k*IQ)-IR*k  
      if (idum.lt.0) idum=idum+IM  
      j=1+iy/NDIV  
      iy=iv(j)  
      iv(j)=idum  
      ran2=min(AM*iy,RNMX)  
      return  
      END  

      FUNCTION gammln(xx)  
      REAL*8 gammln,xx  
      INTEGER j  
      REAL*8 ser,stp,tmp,x,y,cof(6)  
      SAVE cof,stp  
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,  
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  
     *-.5395239384953d-5,2.5066282746310005d0/  
      x=xx  
      y=x  
      tmp=x+5.5d0  
      tmp=(x+0.5d0)*log(tmp)-tmp  
      ser=1.000000000190015d0  
      do 11 j=1,6  
        y=y+1.d0  
        ser=ser+cof(j)/y  
11    continue  
      gammln=tmp+log(stp*ser/x)  
      return  
      END  
 
