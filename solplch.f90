      subroutine solplch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost in runoff,tile flow 
!!    and percolation out of the soil profil
!!    

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conv_wt(:,:)  |none         |factor which converts kg/kg soil to kg/ha
!!    curyr         |none         |current year of simulation
!!    hru_dafr(:)   |none         |fraction of watershed area located in HRU
!!    ihru          |none         |HRU number
!!    nyskip        |none         |number of years to skip output summarization
!!                                |and printing
!!    phoskd        |none         |Phosphorus soil partitioning coefficient
!!                                |Ratio of phosphorus attached to sediment to
!!                                |phosphorus dissolved in soil water
!!    pperco        |none         |phosphorus percolation coefficient (0-1)
!!                                |0:concentration of soluble P in surface
!!                                |  runoff is zero
!!                                |1:percolate has same concentration of soluble
!!                                |  P as surface runoff
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_nly(:)    |none         |number of layers in soil profile
!!    sol_prk(:,:)  |mm H2O       |percolation from soil layer on current day
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    sol_solpcon(:,:) |mg/l      |amount of phosohorus in solution concentration
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    wshd_plch     |kg P/ha      |average annual amount of phosphorus leached
!!                                |into second soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    wshd_plch     |kg P/ha       |average annual amount of phosphorus leached
!!                                 |into second soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    vap         |kg P/ha       |amount of P leached from soil layer
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, jj, m, n
      real :: xx, vap, percplyr, vap1
      real :: sro, vv, vsolp, co, dg
      real :: a, b, c, theta
      real ::  P_ads_max
      real :: maxiron(mlyr)

      j = 0
      j = ihru
      vap = 0.
      dep = 0.
      maxiron = MAXVAL(sol_ec, DIM=2)
      do jj = 1, sol_nly(j)
        !! add solp leached from layer above
        sol_solp(jj,j) = sol_solp(jj,j) + vap
	    if (sol_solp(jj,j) < 1.e-6) sol_solp(jj,j) = 0.
!! Langmuir equation to partition sol_p in soild and liquid phase
!!equations updated from Nelson NO and Parsons JE. 2006 
!!Modification and validation of GLEAMS for prediction of phosphorus leaching in waste-amended soils.
!!Trans. ASABE 49(5):1395-1407
            !!calculate relative soil moisture
            dg = 0.
            theta = 0.
            dg = sol_z(jj,j) - dep !soil layer depth
            dep = sol_z(jj,j)
            theta = (sol_st(jj,j)/dg)/sol_por(jj,j) !relative soil moisture     
                  
!! set default iron extratble stuff
            if (sol_ec(jj,j) < 1e-4 ) then
                if (maxiron(jj) < 1e-4) maxiron(jj) = 80.
                sol_ec(jj,j) = maxiron(jj)                
                if (jj ==4) then
                   sol_ec(jj,j) = sol_ec(jj-1,j)
                 endif
            endif
!            if (sol_ec(jj,j) < 1e-4) print*, sol_ec(jj,j)

            P_ads_max = Qmax_beta * sol_ec(jj,j)*31 !use dummy swat input sol_ec represents Alox + Feox
            a = 0.
            b = 0.
            c = 0.
            a = k_langmuir*theta
            b = P_ads_max * k_langmuir * sol_bd(jj,j) + theta - sol_solp(jj,j) * k_langmuir/dg
            c = -sol_solp(jj,j)/dg

            sol_solpcon(jj,j) = (-b + sqrt(b**2 - 4.0*a*c))/(2.0*a) !(mg/l)       
!! Langmuir equation to partition sol_p in soild and liquid phase

!! compute soluble P lost in surface runoff        
        if (jj == 1) then
         surqsolp(j) = sol_solpcon(jj,j) * surfq(j) 
         surqsolp(j) = Min(surqsolp(j), sol_solp(1,j))
         surqsolp(j) = Max(surqsolp(j), 0.)
         sol_solp(jj,j) = sol_solp(jj,j) - surqsolp(j)
!         if(surqsolp(j) > 0.) print*, sol_solpcon(jj,j), surqsolp(j), sol_solp(1,j)     
        endif 

!! compute soluble P leaching  
        vap = sol_solpcon(jj,j) * sol_prk(jj,j)  !(kg/ha)                                                                
        vap = Min(vap, .2 * sol_solp(jj,j))
        sol_solp(jj,j) = sol_solp(jj,j) - vap
        solp_leach(jj,j) = vap
    enddo
    

!! route sol_p in tile drains 
     vap = 0.    
      if (tileq(j)>0.) then
          do jj = 1, ldrain(j)
               vap = sol_solpcon(jj,j) * tileq(j) * tile_fr(jj,j) !concentration(mg/l) * tilelfow (mm) * 1e-2-> kg/ha
               vap = min(vap, 0.5*sol_solp(jj,j))
               tileminp(j) = tileminp(j) + vap                     !Add up then substract from the mineral P pool
               sol_solp(jj,j) = sol_solp(jj,j) - vap
               tilep_ly(jj,j) = vap        
          enddo               
      endif

      if (curyr > nyskip) then
!        wshd_plch = wshd_plch + percp(j) * hru_dafr(j)
         wshd_plch = wshd_plch + solp_leach(sol_nly(j),j) * hru_dafr(j)
      end if

        
      return
      end
