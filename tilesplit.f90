      subroutine tileqsplit
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the tile drain flow from each soil layer 
!!    between shallow water table and the tile drain pipe

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wat_tbl(:)    |mm           |water table based on depth from soil surface
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    ddrain(:)     |mm           |depth of drain tube from the soil surface
!!    ihru          |none         |HRU number
!!    sol_nly(:)    |none         |total number of soil layers
!!    ldrain(:)     |none         |soil layer of the tile drain pipe

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tile_fr(:,:)  |none       |fraction of tile drain flow from each soil layer

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    jj          |none          |counter
!!    n           |none          |counter
!!	  lwat_tbl	  |none			 |soil layer number of the shallow water table
!!	  dr_dif      |mm 		     |difference bewteen shallow water table and tile drain pipe
!!    dep         |mm 		     |layer thickness for the contribuing layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use parm
      integer :: j, jj, lwat_tbl,n
      real :: dr_dif, dep

      j = 0
      j = ihru
      lwat_tbl = 0
      n = 0
      dr_dif = 0.
      dep = 0.
      tile_fr = 0.

 !! determin the soil layer of the shallow water table
      if (wat_tbl(j) > sol_z(sol_nly(j),j)) then
        lwat_tbl = sol_nly(j)
      else
        do jj = 1, sol_nly(j)
          if (sol_z(jj,j) > wat_tbl(j)) then
           lwat_tbl = jj 
           exit
          endif
        end do
      endif  

!! determin the tile drain flow contribution from each layer
      if (tileq(j) > 0.) then
       dr_dif = ddrain(j) - wat_tbl(j)
       n = ldrain(j) - lwat_tbl 
       if (n <= 0) then  ! water table are not higher than tile drain 
         tile_fr(ldrain(j),j) = 1.0
       else 
         do jj = ldrain(j),lwat_tbl,-1              ! water table just above tile drain
           if (jj == ldrain(j) ) then
             dep = ddrain(j) -sol_z(ldrain(j)-1,j)  !tileflow fraction in the tile drain layer (tile_fr(ly) in soil layer ldrain(j))
           else if (jj == lwat_tbl) then 
             dep = sol_z(lwat_tbl,j) - wat_tbl(j)    !tile flow fraction in the layer of the shallow water table(tile_fr(1) in soil layer lwat_tbl)
           else 
             dep = sol_z(jj,j) -  sol_z(jj-1,j) 
           endif 
           tile_fr(jj,j) = dep/dr_dif 
         enddo
       endif  
!       write (9999,3000), iyr,iida,j,tileqslow(j),(
!     &tile_fr(ly,j), ly = 1,ldrain(j))
      endif      
!! determin the tile drain flow contribution from each layer
!3000  format(i0,1x,i0,1x,i0,1x,5e14.7)

end