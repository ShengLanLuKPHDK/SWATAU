      subroutine macrosed
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes sediment that leach to the tile drains
!!    Concept from Javis et al. 1999 particle leaching model
!!    with adaption from Larsson et. al 2007   
!!    Detached sediment is calculated with adapted USLE equation with a sediment pool Ms
!!    generated in the top soil layer 
!!    Then the detached sediment leach to the tiles recuded due to 'filtering'
!!    The sediment available sediment pool Ms is calculated dynamicly
!!          -reduce when sediment is leaching
!!          -replenish with a kr(1-Ms/Mmax)
!!    writen by S. Lu 17-04-12
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    msmax(j)    |g/g           |maximum available sediment pool for leaching
!!                               |in the top soil
!!    ihru        |none          |HRU number
!!    ms(j)       |g/g           |available sediment pool for leaching in the top soil
!!                               |layer at field capacity (fc-wp)
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    usle_k(:)   |none          |USLE equation soil erodibility (K) factor
!!    crop(:)     |none          |land cover & management factor
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    ddrain(:)   |mm            |depth to the tile drain
!!    kr          |none          |replenishment rate coefficient
!!    filt        |m-1           |parameter for filtering sed to tile
!!    beta        |none          |fraction of macropore flow to fast tile
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    macrosed(:)   |ton/ha        |preferencial flow by macropores
!!    tilesed(:)    |ton/ha        |preferencial flow goes to tiles
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    macro       |none          |
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layer)
!!    infil       |mm H2O        |infiltrationto the micropires
!!    sol_ma      |mm H2O        |soil water when macropore occur
!!    xx          |mm H2O        |water deficiency in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm
      integer :: j
      real :: detach, replanish,f,masedc,d, ei

      j = ihru

      detach = 0.
      replanish = 0.
      f = 0.

!! if soil tillaged then available pool recover to the maximum pool
      if (idtill > 0) then
        ms(j) = msmax(j)
!        idtill = 0
      endif
      
!! detached sediment is calculated using USLE euqation 
!! but slope,coarse fragment,support practice factor are not included
!! adapted with available sediment pool ms
      masedc = 0.
      d = 0.
      if (macroq(j)>0.) then !!only occours when macroflow occours
!        ei = 29*(1-0.72*exp(-0.05*infilpcp))
!        d = usle_k(j)* ei * inflpcp * ms(j)
        d = usle_k(j)*usle_ei*usle_cfac(j)*ms(j)
        masedc = d/(inflpcp+sol_st(1,j))
!        write(1111,100) iyr,iida,j,macroq(j),usle_k(j),usle_ei,ms(j),crop(j),mased(j)
!100  format(i0,1x,i0,1x,i0,6f14.7)
 !      print *, iida,j,usle_ei
!        mased(j) = max(0.,mased(j))
        masedc = max(0.,masedc)
!        print *, iida,j,usle_ei,crop(j),mased(j)
!        masedc = mased(j)/macroq(j)*1e5 !convert to concentration mg/l  
      else
        !mased(j) = 0.
        masedc = 0. 
      endif 
!      print*, mased(j)
!!due to filtering, sediment concentration is reduced along the depth
      if(macrotile(j)>0.) then
 !       f = exp(filt*ddrain(j)/1000) - 1
 !       f = min(f,1.)
        sink = filt *masedc !* macroq(j)
        tilesed(j) = (masedc - sink) * macrotile(j)
!        tilesed(j) = f*masedc*macrotile(j)
!        if (mased(j)>0.) print*,j,macrotile(j),tilesed(j),mased(j)
!!the sink term f is linear portion to the concentration
!!and increase to the travel distance  
      ! d =   (exp(ddrain(j)/dep_wet)-2)  
      ! f = filt*masedc*macrotile(j)
      ! f = min(mased(j),f)
      ! tilesed(j) = mased(j) - f        
      else
        tilesed(j) = 0.
      endif
    
 !  if(mased(j)>0.) print *, iida,j,mased(j), f,tilesed(j),d, masedc,macrotile(j)!usle_ei,
   ! if(preceff>0.)print*,iida, ms(39),usle_k(39),usle_ei,mased(39),tilesed(39)  
   ! if(mased(j)>0.)  print *,iiday, j,usle_ei,tilesed(j),mased(j) !
      tilesed(j) = tilesed(j)* hru_ha(j) !! from ton/ha to ton
!      if(tilesed(j)>0.) print*, j,tilesed(j)*1000
!!leached sediment are mainly fine sediment, calculated from clay
!! tile sediment are added to sediment yield
      sedyld(j) = sedyld(j) + tilesed(j)
      clayld(j) = clayld(j) + tilesed(j)
!!      print*, macrotile(j)
!! update the available pool after calculation
      detach = d/(sol_avbd(j) * sol_z(1,j))
!      replanish = kr * (1 - ms(j)/msmax(j))
      replanish = kr * (1 - ms(j)/msmax(j))/(sol_avbd(j) * sol_z(1,j))
      ms(j) = ms(j) - detach + replanish
      ms(j) = min(ms(j),msmax(j))
      ms(j) = max(0.,ms(j))
    !  if (mased(j) == NAN) then

!    if(macroq(j)>0.) then 
!    write(6667,100) iyr,iida,j,tilesed(j), masedc, d, ei,usle_ei, inflpcp, ms(j),sink,f
!      write(1111,100) iyr,j,ms(j),detach,replanish, msmax(j), tilesed(j)
!    endif
! 100  format(i0,1x,i0,1x,i0,9f14.7)
!      mased(j) = masedc



      return
      end