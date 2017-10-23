      subroutine pestdrain(pstcon,ly,k)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of pesticide lost in tile flow 
!!    
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)   |none          |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru         |none          |HRU number
!!    npmx         |none          |number of different pesticides used in
!!                                |the simulation
!!    npno(:)      |none          |array of unique pesticides used in watershed
!!    pst_wsol(:)  |mg/L (ppm)    |solubility of chemical in water
!!    sol_bd(:,:)  |Mg/m**3       |bulk density of the soil
!!    sol_kp(:,:,:)|(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                 |    or m^3/ton|ratio of the concentration in the solid
!!                                |phase to the concentration in solution
!!    sol_nly(:)   |none          |number of layers in soil profile
!!    sol_por(:,:) |none          |total porosity of soil layer expressed as
!!                                |a fraction of the total volume
!!    sol_prk(:,:) |mm H2O        |percolation from soil layer on current day
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer
!!    sol_wpmm(:,:)|mm H20        |water content of soil at -1.5 MPa (wilting
!!                                |point)
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    tileqslow(:) |mm            |amount of matrix tile drain flow 
!!    wat_tbl(:)   |mm            |shallow water table in soil
!!    ldrain(:)    |none          |soil layr of the tile drain pipe
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    tilepst (:)   |kg  /ha       |amount of pesticide in tile drain for the day

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    wat_tbl_1   |none          |layer number of the shallow water table
!!    tile_fr     |none          |contribution of each soil layr to the tile drain flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      integer :: j, kk,ly
      real:: yy

      j = 0
      j = ihru
      
      if(ifast==1) tilepst(k,j) = tilepst(k,j) + macrotilepst(k,j) 
      yy = 0. 
      if (tileq(j) > 0.) yy =  pstcon*tile_fr(ly,j)*tileq(j)   
      if ( yy > sol_pst(k,j,ly)) yy=  sol_pst(k,j,ly) 
      sol_pst(k,j,ly) = sol_pst(k,j,ly) - yy      
      tilepst(k,j) = tilepst(k,j) + yy   
      

     end