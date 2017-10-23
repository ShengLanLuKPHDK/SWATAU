      subroutine soilPout
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine covert soil phosphorus in each soil layer and sums up for whole soil profile
!!    and percolation out of the soil profil
!!    

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conv_wt(:,:)  |none         |factor which converts kg/kg soil to kg/ha
!!    ihru          |none         |HRU number
!!    sol_actp(:,:) |kg P/ha      |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_stap(:,:) |kg P/ha      |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    sol_orgp(:,:) |kg P/ha      |amount of phosphorus stored in the organic
!!                                |P pool in soil layer
!!    sol_fop(:,:)  |kg P/ha      |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_minp(:,:) |kg P/ha      |total amount of phosphorus stored in the
!!                                |mineral phosphorus pool
!!    sol_torgp(:,:)|kg P/ha      |total amount of phosphorus in the soil layer
!!                                |stored in the organic phosphorus pool
!!    sum_minp(:,:) |kg P/ha      |total amount of mineral phosphorus in the
!!                                |soil profile
!!    sum_torgp(:,:)|kg P/ha      |total amount of organic phosphorus in the 
!!                                |soil profile
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ji          |none          |counter soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
     use parm
     integer:: j,j1
     real, dimension(mlyr,mhru) :: sol_minp, sol_torgp
     real, dimension(mhru) ::  sum_torgp, sum_minp
      
      j = 0
      ji = 0      
      j = ihru

      sum_minp(j) = 0.
      sum_torgp(j) = 0.
      do j1 = 1, sol_nly(j)
        sol_minp(j1,j) = (sol_solp(j1,j) +sol_actp(j1,j) + sol_stap(j1,j))/conv_wt(j1,j) * 1000000.
        sol_torgp(j1,j) = (sol_orgp(j1,j) + sol_fop(j1,j))/ conv_wt(j1,j) * 1000000.
        sum_torgp(j) = sum_torgp(j) + sol_orgp(j1,j) + sol_fop(j1,j) 
        sum_minp(j) = sum_minp(j) + sol_solp(j1,j) + sol_actp(j1,j) + sol_stap(j1,j)
      end do
      
      write(200,1000) cpnm(idplt(j)), j, iyr, iida,hru_km(j), (sol_minp(m,j), m = 1, sol_nly(j) ), (sol_torgp(m,j),m = 1, sol_nly(j))
      write(201,1001) cpnm(idplt(j)), j, iyr, iida,hru_km(j), fertp, fertsolp, fertorgp,pplnt(j),plantp(j),yieldp, sedorgp(j), sedminpa(j) + sedminps(j), minpgw(j),tileminp(j),surqsolp(j),solp_leach(sol_nly(j),j),sum_minp(j), sum_torgp(j)
 !     yieldp = 0.
 1000 format(a4,1x,i4,1x,i4,1x,i4,1x,11e14.7) 
 1001 format(a4,1x,i4,1x,i4,1x,i4,1x,15e14.7)
      return
      end     