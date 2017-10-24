      subroutine bedsediment

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates P and N in river bed sediment
!!    

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units          |definition
!!    tppsed(:)    |mg P/kg        |total perticulate phosphurus in river bed sediment
!!    detnsed(:)   |mg N/kg        |detritese N in river bed sediment
!!    nh4sed(:)    |g N/m2        |ammonia in river bed sediment porus water
!!    no3sed(:)    |g N/m2        |nitrate in river bed sediment porus water
!!    srpsed(:)    |g P/m2        |solute reactive phosphuros in bed sediment porus water
!!    ch_bed_bd(:) |mg/m3          |bulk density of bed sediment
!!    ch_l2(:)     |km             |length of main channel
!!    ch_w(2,:)    |m              |average width of main channel
!!    sedst(:)     |metric tons    |amount of sediment stored in reach
!!    rchdep       |m              |depth of flow on day
!!    difpo4(:)    |m2/day         |mol PO4 diffusion constant
!!    difno3(:)    |m2/day         |mol NO3 diffusion constant
!!    difnh4(:)    |m2/day         |mol nh4 diffusion constant
!!    difo2(:)     |m2/day         |mol o2 diffusion constant
!!    pfr(:)       |g P/ g DW     |fraction of p in macrophyte
!!    nfr(:)       |g N/ g DW     |fraction of n in macrophyte
!!    Kd           |dm^3/kg        |sediment sorption of P from water 
!! ma_biomass(jrch)|g C/m2        |macrophyte biomass 
!!grow_biomass(jrch)  |g C/m2        |increase of macrophyte biomass during time step 


!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tppsed(:)    |mg P/kg        |total perticulate phosphurus in river bed sediment
!!    detnsed(:)   |mg N/kg        |detritese N in river bed sediment
!!    nh4sed(:)    |g N/m2        |ammonia in river bed sediment porus water
!!    no3sed(:)    |g N/m2        |nitrate in river bed sediment porus water
!!    srpsed(:)    |g P/m2        |solute reactive phosphuros in bed sediment porus water
!!    depp(:)      |g P/m2        |deposited PP with sediment
!!    depn(:)      |g N/m2        |deposited detritese N with sediment
!!    resp(:)      |g P/m2        |resuspended pp with sediment
!!    resn(:)      |g N/m2        |resuspended detritese N with sediment

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units           |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    por         |none            |porosity of bed sediment
!!    sdepth      |m               |sediment layer depth
!!    tppsedpor   |g/m2           |total perticulate phosphurus in river bed sediment
!!    detnsedpor  |g/m2           |detritese N in river bed sediment
!!    deposed     |g/m2           |deposited sediment
!!    resused     |g/m2           |resuspended sediment
!!    nh4sedcon   |mg N/l          |ammonia concentration in river bed sediment porus water
!!    no3sedcon   |mg N/l          |nitrate concentrationin river bed sediment porus water
!!    srpsedcon   |mg P/l          |solute concentration reactive phosphuros in bed sediment porus water
!!    diffnh4     |g N/m2         |ammonia diffusion from sediment to water column
!!    diffno3     |g N/m2         |nitrate diffusion from sediment to water column
!!    diffsolp    |g P/m2         |solute reactive phosphuros diffusion from sediment to water column
!!    minN        |g N/m2         |amount of detritese N mineralized to NH4
!!    nitrifiN    |g N/m2         |amount of NH4 nitrified to NO3
!!    denitriN    |g N/m2         |amount of NO3 defitrified



      use parm

      integer :: jrch
!      real:: difpo4 = , difno3 = , difnh4 = 
!      real:: minrate = , nirate = , denitrate = 
      real:: themin = 1, theni = 1, thede = 1
      real:: diffsolp, diffnh4, diffno3
      real:: tppsedpor, detnsedpor, deposed,resused
      real:: nh4sedcon, no3sedcon, srpsedcon
      real:: orgn, orgp
      real:: sdepth, por !, Kd = 0.1*200
      real:: minN, nitrifiN, denitriN
!      real:: pfr = , nfr = 
      real:: puptake,no3uptake, nuptake

      jrch = 0
      jrch = inum1

!! convert sediment nutrient to per m3
      sdepth = 0.1 !! assume a sediment layer depth
!! mg/kg to g/m2
      tppsedpor = tppsed(jrch) * ch_bed_bd(jrch) * sdepth/1e3
      detnsedpor = detnsed(jrch) * ch_bed_bd(jrch) * sdepth/1e3

!=================================movement with sediment==========================
!! sediment resuspention and deposition
!! metric tons to kg/m2
     depsed = rchdy(57,jrch)/ (ch_l2(jrch) * ch_w(2,jrch)*1e3) 
     resused = rchdy(61,jrch) /(ch_l2(jrch) * ch_w(2,jrch)*1e3) 
!     print*, iyr, iida, jrch,rchdy(57,jrch), rchdy(61,jrch)
!! PP and orgN in deposition: content in SS * deposition
!! calculate PP and organic N content in suspended sediment in water column
     orgn = 0.
     orgp = 0.
     if (sedrch > 0.) then
     orgn = organicn(jrch)*rtwtr /sedrch     !mg N/kg sediment (mg N/l * (10^3 l)/ (10^3 kg))
     orgp = organicp(jrch)*rtwtr  /sedrch      !mg P/kg sediment
     endif
     depp(jrch) = orgp * depsed *1e-3               !g P/m2 (mg/kg * kg/m2 / 1e3 -> g/m2)
     depn(jrch) = orgn * depsed *1e-3               !g N/m2
!! PP and orgN in resuspension: content in bed sediment * resuspention
     resp(jrch) = tppsed(jrch) * resused *1e-3      !g P/m2 (mg/kg * kg/m2 / 1e3 -> g/m2)
     resn(jrch) = detnsed(jrch) * resused *1e-3     !g N/m2

!if (jrch==1) print*, depp(jrch),depn(jrch),resp(jrch),resn(jrch)
!======================sediment N processes========================================
!! how to correlate temerature in sediment?
!! PClake use water temperature for sediment
         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         wtmp = 0.
         wtmp = 5.0 + 0.75 * tmpav(jrch)
         if (wtmp <= 0.) wtmp = 0.1
!! mineralisation similar to PC lake
   minN = Theta(minrate(jrch),themin,wtmp)* detnsedpor         !g N/m2
!! Nitrification
   nitrifiN =Theta(nitrate(jrch),theni,wtmp) * nh4sed(jrch)    !shoule be corrected for oxygen?
!! Denitrification
   denitriN = Theta(denirate(jrch),thede,wtmp) * no3sed(jrch)  !should include carbon? or just a lost rate?




!=======================diffusion==================================================
!! prosity
    por = 1 - ch_bed_bd(jrch)/2.65
!! convert solute nutrients into concentrations
    nh4sedcon = nh4sed(jrch) / sdepth / por !* 1e3         !g/m2 to mg/l
    no3sedcon = no3sed(jrch) / sdepth / por !* 1e3
    srpsedcon = srpsed(jrch) / sdepth / por !* 1e3
!! calculate the amount of diffusion
    diffsolp = 0.
    diffno3 = 0.
    diffnh4 = 0.
    diffnh4 = difnh4(jrch) * por * (nh4sedcon - ammonian(jrch)) !should correct for temperature, distance and other stuff?
    diffno3 = difno3(jrch) * por * (no3sedcon - nitraten(jrch)) !mg/l
    diffsolp = difpo4(jrch) * por * (srpsedcon - disolvp(jrch))


!=====================macrophyte uptake===========================================
       porwaterdepth = 0.2 * sdepth
      !! compute amount of uptake
        puptake = pfr(jrch) * grow_biomass(jrch) /porwaterdepth * por
        if (puptake < 0.) puptake = 0.
      !!uptake from NH4 and NO3 pools (preference for NH4)
        nuptake = nfr(jrch) * grow_biomass(jrch) /porwaterdepth * por
        if (nuptake <0.) nuptake = 0.
!=====================sediment P processes========================================
!! first compute consumptuion of solp, then compare with equibillium content
!! if higher than equibillim then adsorbed to PP, ellers more realses from PP 
   srpsed(jrch) = srpsed(jrch) - puptake - diffsolp
   
!! equaibilium concentraton of solp in pore water
!! equations from INCA-P PP = solp * kd/porwaterdepth * sediment mass * 1e-3
   sedmass = ch_bed_bd(jrch) * sdepth  
  
   srpsedeny = tppsedpor / Kd(jrch) * sedmass * porwaterdepth / 1e-3 !kg/m2
   srpchange = srpsedny - srpsed(jrch)           !positive then more srp released, negetive then more srp sorbed  
   tppsedpor =  tppsedpor + depp(jrch) - resp(jrch) - srpchange   
   srpsed(jrch) = srpsedeny 

!=====================update N in sediment=================================
   no3uptake = 0.
   nh4uptake = 0.
   detnsedpor = detnsedpor + depn(jrch) - resn(jrch) - minN
!   nh4sed(jrch) = nh4sed(jrch) + minN - nitrifiN - nuptake - diffnh4
   nh4sed(jrch) = nh4sed(jrch) + minN - nitrifiN - diffnh4
!N uptake strong preference for ammonium
   if (nuptake > nh4sed(jrch)) then
     nh4uptake = nh4sed(jrch) - 0.1
     no3uptake = nuptake - nh4uptake + 0.1
     nh4sed(jrch) = 0.1
   else
     nh4uptake = nuptake
     nh4sed(jrch) = nh4sed(jrch) - nh4uptake 
   endif

   no3sed(jrch) = no3sed(jrch) + nitrifiN - denitriN - no3uptake - diffno3
   
!===================update N P in water===================================
  ammonian(jrch) = ammonian(jrch) + diffnh4/rchdep
  nitraten(jrch) = nitraten(jrch) + diffno3/rchdep
  disolvp(jrch) = disolvp(jrch) + diffsolp/rchdep
  if (depn(jrch)/rchdep > organicn(jrch)) depn(jrch) = organicn(jrch)*rchdep
  organicn(jrch) = organicn(jrch) - depn(jrch)/rchdep + resn(jrch)/rchdep
  if (depp(jrch)/rchdep > organicp(jrch)) depp(jrch) = organicp(jrch)*rchdep
  organicp(jrch) = organicp(jrch) - depp(jrch)/rchdep + resp(jrch)/rchdep

      return
      end
