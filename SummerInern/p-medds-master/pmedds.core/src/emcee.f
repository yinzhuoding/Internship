      subroutine emcee(simdat,gamasim,shdat,schooldat,nweeks,nweeksFit,
     $     nparam,dt,nstep,nsamps,pmax,pmin,pval,imask,ithin,iseed,
     $     solbest,rbest,tab,iverborse,accept_rate,wweight,nwalk)

! The EMCEE algorithm - all walkers are advanced at once.  See the D. Foreman-Mackey et al 2013a manuscript
      use ranvars
      implicit none
      
      real*8 simdat(nweeks),gamasim(nweeks)
      real*8 shdat(nweeks),schooldat(nweeks)
      real*8 solbest(nweeks),wweight(nweeks)
      real*8 rvec(nweeks),rbest(nweeks)
      integer nstep,nweeks,nweeksFit,nsamps,nparam,ithin,nwalk
      integer iseed,imask(nparam)
      integer iaccept,iverborse
      real*8 dt,accept_rate
      real*8 pmax(nparam),pmin(nparam),pval(nparam,nwalk)
      real*8 curpars(nparam,nwalk),savepar(nparam,nwalk)

      real*8 parbest(nparam)

      real*8 curLLK(nwalk),fnewLLK,curMin,diff_like
      real*8 solinit(nweeks),solnew(nweeks)
      logical accept,verborse, lrange,lcheckpar
      real*8 tab(nsamps/ithin,(nparam+1),nwalk)
      real*8 parnew(nparam)
      real*8 ran1
      real*8 apar,gz_norm,z, fQ,rnd, tmp(3)
      integer iwalk, jwalk
      integer i,j,k,icount,iBest
      integer nopt,iopt(nparam)
  
! set the parameter 'a'. Must be greater than 1
! the Foreman-Mackey et al paper suggests to use a=2
! This plays hte role of step size.
! It seems that for us a value of 1.5 does a better job.
! larger values of play the role (but not exactly) of a larger step size
! gz = 1/sqrt(z) between 1/sqrt(a) and (a). Otherwise zero.
! to normalize we divide by gz_norm

      apar = 1.5

      gz_norm = 2.0d0*(sqrt(apar) - sqrt(1.0d0/apar))
      
! iwalk is the number of 'walkers' we have 

! Initialize random number generator. If running multiple 'mcmc' calls during an
! R console session, iv and iy do not get properly reset. Here we manually reset.
      iseed = -iseed
      iv    = 0
      iy    = 0

      if (iverborse .eq. 0) then
         verborse=.false.
      else
         verborse=.true.
      endif

      nopt = 0
      iopt = 0
      do i=1,nparam
         if (imask(i) > 0) Then
            nopt=nopt+1
            iopt(nopt) = i
         endif
      enddo


      curpars = pval
      solinit = 0.0d0
      curMin = 1.d6
! solinit will hold the initial model solution

      do iwalk=1,nwalk

         CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,
     $        nweeks,nweeksFit,curpars(1:nparam,iwalk),nparam,
     $        curLLK(iwalk),solinit,rvec,wweight)

         if(curLLK(iwalk) < curMin) Then
            curMin = curLLK(iwalk)
            parBest = curpars(1:nparam,iwalk)
            solbest = solinit
            rbest=rvec
            iBest = iwalk
         endif
      enddo
      
      call dblepr('Initial LLK',-1,curLLK,nwalk)
      call dblepr('ini par',-1,parBest(iopt),nopt)

! Number of MCMC steps is number of samples times number of parameters

      iaccept = 0
         
      icount = 0
      
      do i =1,nsamps

! update the relevant parameter
! calculate new solution 
            solnew = 0.0d0

            do iwalk = 1,nwalk

               savepar(1:nparam,iwalk)=curpars(1:nparam,iwalk)
! choose another walker randomly 
 101           continue
               rnd = ran1(iseed)
               jwalk = 1+int(rnd*(nwalk-1+1))
               if (jwalk .eq. iwalk) go to 101
! Now need to choose 'Z' 
! msut repeat the selection if any of the parameters is outside pmin/pmax range

 102           continue

               rnd = ran1(iseed)
               z = rnd *gz_norm/2.0d0+ 1.0d0/sqrt(apar)
               z = z * z

               parnew = curpars(1:nparam,iwalk)
               Do j=1,nopt
                  parnew(iopt(j)) = curpars(iopt(j),jwalk) +
     $                 Z * (curpars(iopt(j),iwalk) -
     $                 curpars(iopt(j),jwalk))
               enddo

! check that we did not go out of the min/max range, if we did sample again

               lrange = lcheckPar(nopt,pmin(iopt),pmax(iopt)
     $              ,parnew(iopt))

               if (.not.lrange) go to 102
               
!calculate new profile and new likelihood 

               CALL PROP(simdat,gamasim,shdat,schooldat,dt,nstep,
     $              nweeks,nweeksFit,parnew,nparam,
     $              fnewLLK,solnew,rvec,wweight)

               diff_like = fnewLLK-curLLK(iwalk)

! Let's calculate the natural log of fQ since fQ itself might be very big

               !fQ = z**(nopt-1) * exp(-diff_like)
               fQ = dble(nopt-1)*log(z) - diff_like

               rnd = ran1(iseed)

               if (log(rnd) .le. fQ) Then

                  accept = .true. 
                  iaccept = iaccept + 1
               
                  savepar(1:nparam,iwalk) = parnew
                  curpars(1:nparam,iwalk) = parnew
                  curLLK(iwalk) = fnewLLK
                  if (curLLK(iwalk) .le. curMin) then !keep track of the best profile we have
                     curMin = curLLK(iwalk)
                     parbest = parnew
                     solbest=solnew
                     rbest=rvec
                     iBest = iwalk
                  endif

               else
                   accept = .false.
                  curpars(1:nparam,iwalk) = savepar(1:nparam,iwalk)
               endif
               
            enddo               ! End of loop on walkers

!     update chain information only every ithin iterations
            if (mod(i,ithin) .eq. 0) Then
               icount = icount+1
               tab(icount,1:nparam,1:nwalk) = curpars
               tab(icount,(nparam+1),1:nwalk) = curLLK

               call dblepr('curLLK',-1,curLLK,nwalk)
               
             endif

         enddo                  ! end of loop on nsamps
                     
         accept_rate = dble(iaccept)/dble(nsamps*nwalk) *100.0d0
         if (verborse ) Then
            call dblepr('acceptence %',-1,accept_rate,1)
         endif

!  record the current set of parameters

      pval = curpars


      return
      end subroutine emcee

c##############################################################################
 
      function lcheckPar(nopt,pmin,pmax,pnew)

      implicit none 
      logical lcheckPar 
      integer nopt,i
      real*8 pmin(nopt),pmax(nopt),pnew(nopt)


      lcheckPar = .true.

      do i=1,nopt
         if (pnew(i) > pmax(i) .or. pnew(i) < pmin(i)) Then
            lcheckPar = .false.
            return
         endif

      enddo

         
      return
      end function lcheckPar
