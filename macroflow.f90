      subroutine macroflow
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes preferencial flow by macropores
!!    macropore flow is a portion of the sufficient precipitation
!!    Macropore flow occurs when 
!!          1. the macropore layers are at field capactiy, or
!!          2. when sepday > (sol_fc(1,j)-sol_st(1,j)) and sol_st(1,j) > fr*sol_fc(1,j)
!!    writen by S. Lu 08-03-12

!!    A boundary soil layer is created at the depth of the macropore(dep_wet)



!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc-wp)
!!    sol_nly(:)  |none          |numer of layers in soil profile
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |any given day (less wilting point water)
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    fr          |none          |fraction of field capacity when macropore flow occure
!!    dep_wet     |mm            |depth where soil need to saturate before macropore starts

!!    theta       |none          |relative soil moisture
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    macroq(:)   |mm H2O        |preferencial flow by macropores
!!    macrotile(:)|mm H2O        |preferencial flow goes to tiles
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    alfa        |none          |fraction of incoming water for macropore flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dry         |none          |pointer if the soil is too dry for macro flow
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layer)
!!    nly         |none          |layer where macropore ends
!!    infil       |mm H2O        |infiltrationto the micropires
!!    macro       |mm H2O        |soil water when macropore occur
!!    xx          |mm H2O        |calculation for soil layer depth
!!    alfa1       |none          |alfa for each layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ly, nly,l,dry
      real ::  infil, sol_ma, ftile, filtot,sep
      real:: xx,dg,theta,minalfa,exwater
      real,dimension(5)::alfa1

      j = ihru       
 !!   determin the soil layer that the macropore ends
 !     nly=1
 !     do while(sol_z(nly,j)<dep_wet)                       
 !           nly=nly+1
 !     enddo
      nly = wet_nly(ihru)
 !     do ly = 1, sol_nly(j)
 !       if(sol_z(ly,j)>=dep_wet) then
 !           nly = ly
 !           exit
 !       endif
 !     enddo
      macro = 0.
      infil = 0.
      filtot = 0.
      dry = 0
      sep = sepday
      xx = 0.
      do ly = 1,nly
        dg = 0.
        theta = 0.
        dg = sol_z(ly,j) - xx !soil layer depth
        xx = sol_z(ly,j)
        theta = (sol_st(ly,j)/dg)/sol_por(ly,j) !relative soil moisture
        sol_theta(ly,j) = theta
!!calculate alfa from relative soil moisture
 !       alfa1(ly) = exp(sol_theta(ly,j)) -1 !alfa calculated from soil moisture
        alfa1(ly) = sqrt(sol_theta(ly,j)) 
        alfa1(ly) = min(alfa1(ly),1.)
     ! alfa1(ly) = (theta) !**2
!!find the minimum alfa through out layers where macropore exist
         minalfa = alfa1(1)
        if (minalfa > alfa1(ly)) minalfa = alfa1(ly)
!!macropore flow happens only when soil not frozen
        if(sol_tmp(ly,j)>0.) then
!!find water need to fill wet soil layers to field capacity
            if(sol_st(ly,j) < sol_fc(ly,j))then
                infil = sol_fc(ly,j) - sol_st(ly,j)                                           
            else
                infil = 0.                               
            endif
!!find soil water threshold for each layer
            sol_ma = fr*sol_ul(ly,j)
   !         sol_ma = fr*sol_fc(ly,j)
!!when layer is not at field capacity but soil water is higher than threshold 
!!and there's enough water to fill all the layers where macropore exist to field capacity
!!then there is also macropore flow
            if (sep>infil.and.sol_st(ly,j)>sol_ma) then 
!!calculate total water need to fill the layers to field capacity                                     
                 filtot = filtot + infil 
                 sep = sepday - infil !!sepage to next layer, only for condition not in calculation
             else
                 dry = dry+1 !!dry is condtion, if one layer does not fulfill condition, no macropore flow                                     
             endif                                      
        else
            dry = dry+1 !! soil forzen, no macro flow
        endif       
      enddo 
      
!!the soil layers where macropore ends are not filled to field capacity in this modual
!!but the amount of water to fill them field capacity is reserved, and does not go to macroflow
      exwater = 0.
      if(dry == 0) then
        macroq(j) = minalfa*sepday
        exwater = sepday - filtot
        macroq(j) = min(macroq(j),exwater)
        sepday = sepday - macroq(j)    
      endif
      
      if (ddrain(j) > 0.) then
        macrotile(j) = macroq(j)
      else
        macrotile(j) = 0.
        sol_st(nly,j) = sol_st(nly,j) + macroq(j)
      endif
write(1111,*) (alfa1(ly),ly=1,2), (sol_theta(ly,j),ly=1,2),sol_fc(ly,j)
!print*, iyr, iida, j, macroq(j)
      return
      end