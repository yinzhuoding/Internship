      subroutine mcmc(simdat,gamasim,shdat,schooldat,nweeks,nweeksFit,
     $     nparam,dt,nstep,nsamps,logbase,pmax,pmin,ilog,
     $     step,imask,pval,iupdate,ithin,iseed,
     $     solbest,rbest,tab,iverborse,accept_rate,wweight)

      use ranvars
      implicit real*8(a-h,o-z)
      
      real*8 simdat(nweeks),gamasim(nweeks)
      real*8 shdat(nweeks),schooldat(nweeks)
      real*8 solbest(nweeks),wweight(nweeks)
      real*8 rvec(nweeks),rbest(nweeks)
      integer logbase,nstep,nweeks,nweeksFit,iupdate,ithin
      integer nparam,Nproxy,ilog(nparam),imask(nparam),iseed
      integer iaccept_vec(nparam),iverborse
      integer iopt(nparam),nopt
      real*8 dt
      real*8 pmax(nparam),pmin(nparam),pval(nparam)
      real*8 step(nparam),curpars(nparam),savepar(nparam)
      real*8 parupdt(nparam),copypar(nparam)
      real*8 parbest(nparam)
      real*8 savestep(nparam)
      real*8 solinit(nweeks),solnew(nweeks)
      logical plog(nparam),accept,verborse
      real*8 tab(nsamps/ithin,(nparam+1))
      real*8 ran1, scale, range_min, range_max, myaccept
      integer iadapt, ionep


! Initialize random number generator. If running multiple 'mcmc' calls during an
! R console session, iv and iy do not get properly reset. Here we manually reset.
      iseed = -iseed
      iv    = 0
      iy    = 0

! For an adaptive size MCMC - decide if we need to update step size ever 1% of steps
      scale = 2.0d0
      ionep = ceiling(nsamps * 0.01)
      range_min = 0.20d0
      range_max = 0.40d0
      iadapt = 0

! convert 0 and 1 to true and false

      do i=1,nparam
         if (ilog(i) .eq. 0) Then
            plog(i) = .false.
         else
            plog(i) = .true.
         endif
      enddo

! build an auxilaary vector with the indices of the parameters we are optimizing. 
! nopt is the number of paramters we are optimizing

      nopt = 0
      iopt = 0
      do i=1,nparam
         if (imask(i) > 0) Then
            nopt=nopt+1
            iopt(nopt) = i
         endif
      enddo


      if (iverborse .eq. 0) then
         verborse=.false.
      else
         verborse=.true.
      endif

! now we will have to split the code into the two cases: update one parameter at a time
! or update all at once
! The parameter that determines which procedure we use is called 'iupdate' and it is
! given to the subroutine from the R code
! iupdate < 0 - one parameter at a time
! iupdate > 0 - all at once 


      if (iupdate < 0) Then 

         icount = 0
         
         curpars = pval
         solinit = 0.0d0

! solinit will hold the initial model solution
         CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,nweeks,
     $        nweeksFit,curpars,nparam,curLLK,solinit,rvec,wweight)
            

! we will keep track of the best LLK value for each chain and return these
! trajectories to the R code in the matrix solbest
         curMIN = curLLK

! Number of MCMC steps is number of samples times number of parameters

         iaccept = 0
         iaccept_vec = 0
         

         do i =1,nsamps

            savepar = curpars

            do k = 1,nopt
                  
               isampno = (i-1)*nparam + k
               j = iopt(k)
               savepar(j) = curpars(j)
! Propose a solution 
               parnew=fnProposeParamUpdatesSingle(curpars(j),pmin(j)
     $              ,pmax(j),step(j),logbase,plog(j),iseed)

! update the relevant parameter
! calculate new solution 
               curpars(j) = parnew
               solnew = 0.0d0
               CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,nweeks,
     $            nweeksFit,curpars,nparam,fnewLLK,solnew,rvec,wweight)

               diff_like = fnewLLK - curLLK

               accept = .false.
               if (diff_like .lt. 0.0d0) Then
                  accept = .true.
               else
                  rnd = ran1(iseed)
                  if (exp(-diff_like) .gt.rnd) Then
                     accept = .true.
                  else
                     accept = .false.
                  endif
                     
               endif


               if (accept) Then
                  iaccept = iaccept + 1
                  iaccept_vec(j) = iaccept_vec(j)+1
                  savepar(j) = curpars(j)
                  curLLK = fnewLLK
                  if (curLLK .le. curMin) then !keep track of the best profile we have
                     solbest=solnew
                     rbest=rvec
                  endif
               else
                  curpars(j) = savepar(j)
               endif

 101           continue
                  
            enddo               ! end of loop on nparam

!update change information only every ithin iterations
            if (mod(i,ithin) .eq. 0) Then
               icount = icount+1
               tab(icount,1:nparam)   = curpars
               tab(icount,(nparam+1)) = curLLK

               if (verborse .and. mod(i,ithin*100) .eq. 0) Then
                   call dblepr('curLLK',-1,curLLK,1)
                endif
            endif

         enddo                  ! end of loop on nsamps
            
         do j = 1,nopt
            accept_rate = accept_rate + dble(iaccept)/dble(nMcMc)
         enddo
         accept_rate = accept_rate * 100.0d0 / dble(nparam)
         if (verborse) Then
            call dblepr('Final value of LLK',-1,curLLK,1)
            call intpr('iaccept',-1,iaccept,1)
            call dblepr('acceptence %',-1,dble(iaccept)/dble(nMcMc)
     $           *100.0d0,1)
            call dblepr('acceptence %',-1,dble(iaccept_vec)/dble(nsamps)
     $           *100.0d0,nparam)
         endif


         else  ! The case of all parameters updated at once-this is the default
           
           savestep = step
           
           icount = 0

           curpars = pval
           solinit = 0.0d0

! solinit will hold the initial model solution

           CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,nweeks,
     $          nweeksFit,curpars,nparam,curLLK,solinit,rvec,wweight)

           curMin = curLLK
           solbest=solinit

           if (verborse) call dblepr("initial LLK",-1,curMin,1)

           iaccept = 0
           
           do i =1,nsamps
 
              savepar = curpars
              copypar = curpars
              
!     Propose a new value for each parameter we are optimizing 
! half the steps will be small and half will be the given size
              rnd = ran1(iseed)
              if (rnd. le. 0.5) Then
                 step = savestep
              else
                 step = savestep/10.0d0
              endif

              parupdt = copypar

              call fnProposeParamUpdates(nparam,copypar,
     $             pmin,pmax,step,logbase,
     $             plog,parupdt,iseed,nopt,iopt)
               
! update all the parameters and calculate a new solution and LLK

              curpars = parupdt

              solnew = 0.0d0

              CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,nweeks,
     $           nweeksFit,curpars,nparam,fnewLLK,solnew,rvec,wweight)

              diff_like = fnewLLK - curLLK

              accept = .false.
              if (diff_like .lt. 0.0d0) Then
                 accept = .true.
              else
                 rnd = ran1(iseed)
                 if (exp(-diff_like) .gt.rnd) Then
                    accept = .true.
                 else
                    accept = .false.
                 endif                 
              endif
               
! if step is accepted 

              if (accept) Then
                 iaccept = iaccept + 1
                 iadapt  = iadapt + 1
                 
                 savepar = curpars

                 curLLK = fnewLLK
                 if (curLLK .le. curMin) then !keep track of the best profile we have
                    solbest=solnew
                    rbest=rvec
                    parbest = curpars
                  endif
! if step is rejected - restore saved values of parameters
               else
                  
                  curpars = savepar

               endif
               
!update change information only every ithin iterations 
               
               if (mod(i,ithin) .eq. 0) Then
                  icount = icount + 1
                  tab(icount,1:nparam)   = curpars
                  tab(icount,(nparam+1)) = curLLK
                  
               endif

               if (mod(i,ionep) .eq. 0) Then

                  call dblepr('curLLK',-1,curLLK,1)

                  myaccept = dble(iadapt)/dble(ionep)

                  if (myaccept > range_max )
     $              step(iopt(1:nopt))= step(iopt(1:nopt))*scale
                  if (myaccept < range_min )
     $              step(iopt(1:nopt))= step(iopt(1:nopt))/scale
                  iadapt= 0        !reset acceptance number
               endif

            enddo               ! end of loop on nsamps for one chain
            accept_rate = dble(iaccept)/dble(nsamps) *100.0d0
            if (verborse ) Then
               call dblepr('acceptence %',-1,accept_rate,1)
            endif

           pval = curpars
            
      endif

!  record the best value of the parameters

      pval = parbest

      return
      end subroutine mcmc

c##############################################################################
      subroutine prop(y,gamay,shdat,schooldat,dt,nstep,nweeks,
     $           nweeksFit,param,nparam,chi2,x,rvec,wweight)

C Order of parameters is: R0min, deltaR, aparam, pC, seed, Tg, N, Time0,alpha,delta,ts and dur    

	
	implicit real*8(a-h,o-z)

	real*8 dt, param(nparam)
! Param holds all the parameters

	integer nweeks,nweeksFit,nstep,nparam
	real*8 y(nweeks),gamay(nweeks),shdat(nweeks),schooldat(nweeks)
        real*8 rvec(nweeks),x(nweeks),wweight(nweeks)
! Y has in it the incidence data, x is generated by the SIR model
	real*8 x0(4)
	real*8 dsdt(nweeks*nstep)
	real*8 chi2, calcFit
	external CalcFit

	R0min = param(1)
	deltaR = param(2)
        aparam = param(3)
	pC = param(4)
	seed = param(5)
	Tg = param(6)
	fNeff = param(7) 
	t0 = param(8)
        alpha = param(9)
	delta = param(10)
	ts    = param(11) 
	dur   = param(12) 
        
! Initial values for S(0), I(0), R(0) and -dS(0)/dt
! For both populations

	x0(2) =1.          !I(0) 
	
        S0 = fNeff - x0(2)

	x0(1) = S0          !S(0)
	x0(3) = 0.0d0       !R(0) 
	x0(4) = 0.0d0       !-ds/dt 
	
! Here we must call the RK4 routine and calculate the model prediction for incidence rate
        dsdt = 0.0d0
	call RK4(dt,nweeks,nstep,x0,Param,nparam,shdat,schooldat,
     $       dsdt,rvec)
           
! Now need to integrate over one week periods

	call weekly(nweeks,nstep,dsdt,x,pC,seed)

! Nowe we can calculate the -LLK

	chi2 = calcFit(y,gamay,x,nweeks,nweeksFit,wweight)

	return
	end subroutine prop

c----------------------------------------------------------------

      function SR_to_unit(y,ymin,ymax,logbase,logflag)

      implicit real*8 (a-h,o-z)
      real*8 y, ymin,ymax,rtn,SR_to_Unit
      integer logbase
      logical logflag

      if (logflag) Then
         if (logbase .eq. 10) Then
            rtn = (log10(y) - log10(ymin)) /
     $            (log10(ymax)-log10(ymin))
         else
            rtn = (log(y) - log(ymin)) /
     $            (log(ymax)-log(ymin))
         endif
      else
         rtn = (y - ymin)/(ymax - ymin)
      endif

      SR_to_unit = rtn
 
      return
      end function SR_to_unit

c----------------------------------------------------------------

      function SR_from_unit(x,ymin,ymax,logbase,logflag)
      
      implicit real*8(a-h,o-z)
      
      real*8 x,ymin,ymax,rtn,SR_from_unit
      integer logbase
      logical logflag


      if (logflag) Then
         if (logbase .eq. 10) Then
            rtn = ymin * 
     $           10.0**(x*(log10(ymax)-log10(ymin)))
         else
            rtn = ymin * exp(x*(log(ymax)-log(ymin)))
         endif

      else
         rtn = ymin + (ymax-ymin)*x
      endif

      SR_from_unit = rtn

      return
      end function SR_from_unit

c----------------------------------------------------------------

      subroutine fnProposeParamUpdates(nparam,curval,valmin,
     $     valmax,step,logbase,logflag,parupdt,iseed,
     $     nopt,iopt)

      implicit real*8(a-h,o-z)
      integer nparam,nopt,iopt(nparam)
      real*8 curval(nparam),valmin(nparam),valmax(nparam)
      real*8 parupdt(nparam),step(nparam)
      real*8 x, rv,rtn
      real*8 ran1
      integer logbase, iseed
      logical logflag(nopt)
      external SR_to_unit, SR_from_unit
      integer i, j

      

       do j = 1, nopt

          i = iopt(j)

          rv = ran1(iseed)

          rv = (rv - 0.50d0)*step(i)


! convert to a zero - one scale

          x = SR_to_unit(curval(i),valmin(i),valmax(i),
     $         logbase,logflag(i))

          x = x + rv

! periodic boundary conditions 

          if (x .lt. 0.0d0) x = 1.0d0 + x
          if (x .gt. 1.0d0) x = x - 1.0d0

! bring value back to original scale
      
         rtn = SR_from_unit(x,valmin(i),valmax(i),
     $        logbase,logflag(i))

         parupdt(i) = rtn
         
      enddo

      return
      end subroutine fnProposeParamUpdates

!----------------------------------------------------------------

      function fnProposeParamUpdatesSingle(curval,valmin,valmax,
     $     step,logbase,logflag,iseed)

      implicit real*8(a-h,o-z)
      real*8 curval,valmin,valmax,step,temp(3)
      integer logbase,iseed
      logical logflag
      real*8 ran1
      external SR_to_unit
 
      rv = ran1(iseed)

      rv = (rv - 0.50d0)*step

      x = curval 

      x = SR_to_unit(curval,valmin,valmax,logbase,logflag)

      x = x + rv

      if (x .lt. 0.0d0) x = 1.0d0 + x
      if (x .gt. 1.0d0) x = x - 1.0d0
      
      rtn = SR_from_unit(x,valmin,valmax,logbase,logflag)

      fnProposeParamUpdatesSingle = rtn

      return
      end function fnProposeParamUpdatesSingle

!---------------------------------------------------------------------------------

! NR machine independent random number generator

      FUNCTION ran1(idum)

      
      USE ranvars
      implicit none
!      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      INTEGER idum,IA,IM,IQ,IR,NDIV
      REAL*8 ran1,AM,EPS,RNMX  
!      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
!     *     NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *     NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
!      INTEGER j,k,iv(NTAB),iy
      INTEGER j,k
!      SAVE iv,iy
!      DATA iv /NTAB*0/, iy /0/

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
      ran1=min(AM*iy,RNMX)  
      return  
      END  
