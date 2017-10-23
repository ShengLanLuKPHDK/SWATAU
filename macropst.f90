      subroutine macro_pst(pestcon,k)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes solute P that leach to the tile drains
!!    Calculated with the same equation for surface erosion
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
       use parm

       integer :: j, nly
 !      real :: xx,fastilep

       j = ihru

       macropst(k,j) = pestcon* macroq(j)
       macropst(k,j) = min(macropst(k,j), sol_pst(k,j,1))
       sol_pst(k,j,1) = sol_pst(k,j,1) - macropst(k,j)
       

       nly = wet_nly(j)
       if(ddrain(j)>0.) then
          macrotilepst(k,j)  = macropst(k,j)
       else
          macrotilepst(k,j) = 0.
          sol_pst(k,j,nly) = sol_pst(k,j,nly) + macropst(k,j)
       endif

       return
       end