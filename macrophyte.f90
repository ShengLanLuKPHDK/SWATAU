      subroutine macrophyte

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream macrophyte growth
!!    

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nh4sed(:)    |g N/m2       |ammonia in river bed sediment porus water
!!    no3sed(:)    |g N/m2       |nitrate in river bed sediment porus water
!!    srpsed(:)    |g P/m2       |solute reactive phosphuros in bed sediment porus water
!!    hru_ra(:)    |MJ/m^2        |solar radiation for the day in HRU
!!    inum1        |none          |reach number
!!    radmax       |              |maximum observed solar radiation
!!    hru_ra(:)    |              |solar radiation of the 
!!    sdti         |m^3/s         |average flow on day in reach
!!    rnum1        |none          |fraction of overland flow
!!    rttime       |hr            |reach travel time
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    tmpav(:)     |deg C         |average air temperature on current day in HRU
!!    death(:)     |g/m2         |macrophyte death rate  
!!    grmaxm(:)    |g/m2         |macrophyte growth rate at 20 C
!!    SefSma(:)    |g/m2         |macrophyte self-shading 
!!    k_n_m        |g N/m2       |michaelis-menton half-saturation constant
!!                                |for nitrogen
!!    k_p_m        |g P/m2       |michaelis-menton half saturation constant
!!                                |for phosphorus
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name                     |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ma_biomass(:)    |kg DW/m2        |macrophyte biomass 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    thgrma      |none          |temperature adjustment factor for macrophyte
!!                               |growth rate
!!    R           |              |solar radiation normalized to 0-1   
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphoru
!!    jrch        |none          |reach number
!!    wtmp        |deg C         |temperature of water in reach
!!    grma        |g/m2/day     |combined macrophyte growth rates


!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch
      real :: wtmp, Insed, fnn, fpp, R, S, d, thgrma = 1
      real :: grma, grate,ch_change, B, factor, vchannel,part
!      real:: SefSma = , death = , 
!      real:: k_n = , k_p = 

      jrch = 0
      jrch = inum1
      grma = 0.
!==========================equations from INCA-P add N limitations==========================
         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         wtmp = 0.
         wtmp = 5.0 + 0.75 * tmpav(jrch)
         if (wtmp <= 0.) wtmp = 0.1

         !! calculate macrophyte growth limitation factors for nitrogen
         !! and phosphorus 
         INsed = nh4sed(jrch) + no3sed(jrch)
         
         fnn = 0.
         fpp = 0.
         fnn = INsed / (INsed + k_n_m(jrch))
         fpp = srpsed(jrch) / (srpsed(jrch) + k_p_m(jrch))
       
       !!radiation factor
         R = hru_ra(hru1(jrch))/radmax      !correct so varies between 0-1
       !!selfshading
         S = SefSma(jrch)/(SefSma(jrch) + ma_biomass(jrch))
       !! combinded growth rate
         grate = 0.
         grate = Theta(grmaxm(jrch),thgrma,wtmp)   
         grma = grate * R * S * Min(fnn, fpp)     
       !! death rate and impact by flow
!         d = death(jrch) * sdti
         d = death(jrch) * vel_chan(jrch)
         if (minDeath(jrch) > 0.) d = max(d, minDeath(jrch))
       !! update macrophyte biomass
!         grow_biomass(jrch) = (grma - d)* ma_biomass(jrch)
         grow_biomass(jrch) = grma* ma_biomass(jrch) 
!        if (jrch==1) print*, iida, grate, grma, d,grow_biomass(jrch),fpp
!         ma_biomass(jrch) = ma_biomass(jrch) + grow_biomass(jrch)
         ma_biomass(jrch) = ma_biomass(jrch) + (grma - d)* ma_biomass(jrch)
         ma_biomass(jrch) = max(10. , ma_biomass(jrch))

!!===========section for impact of macrophyte biomass on manning's N========
 !Equation from Green et al. (2005)
 !B is blockage factor
         if(max_biomass(jrch) > 0.) B = ma_biomass(jrch)/max_biomass(jrch)
         B = max(0.,B)
         B = min(1.,B)
 !Vplant is the flow velocity in the plnats
  !       vchannel = max(0.1, vel_chan(jrch))
         if(vplant(jrch) > vel_chan(jrch)) vplant(jrch) = vel_chan(jrch)
         if(vplant(jrch) <= 0.) vplant(jrch) = vel_chan(jrch)
         part = rh_rch(jrch)**(2/3)*Sqrt(ch_s(2,jrch))
         ch_n(2,jrch) = part/(B*vplant(jrch) + (1-B)*part/ch_n_initial(jrch))
  !       factor = vchannel/(B*vplant(jrch) + (1-B)*vchannel)
  !       factor = max(0.,factor)
  !       factor = min(30.,factor)
  !       ch_n(2,jrch) = factor * ch_n_initial(jrch)
  !       ch_n(2,jrch) = max(0.01,ch_n(2,jrch))
!     if (jrch==1 .and. curyr > nyskip ) then
!     write(80000,1000) vel_chan(jrch),rchdep,sdti, ch_n(2,jrch), ma_biomass(jrch), B,part
!     endif
!1000 format (9f14.7) 
      return
      end