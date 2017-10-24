      subroutine readyr

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the input data for the recyear command
     
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number
!!     nbyr         |none          |number of years simulated
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     bactlpyr(:,:)|# bact/day    |average daily loading of less persistent
!!                                 |bacteria for year
!!     bactpyr(:,:) |# bact/day    |average daily loading of persistent bacteria
!!                                 |for year
!!     cbodyr(:,:)  |kg/day        |average daily loading of CBOD in year
!!     chlayr(:,:)  |kg/day        |average daily loading of chlorophyll-a in 
!!                                 |year
!!     cmtl1yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#1 for year
!!     cmtl2yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#2 for year
!!     cmtl3yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#3 for year
!!     disoxyr(:,:) |kg/day        |average daily loading of dissolved O2 in 
!!                                 |year
!!     floyr(:,:)   |m**3/d        |average daily water loading for year
!!     minpyr(:,:)  |kg P/day      |average daily mineral P loading for year
!!     nh3yr(:,:)   |kg N/day      |average daily NH3-N loading for year
!!     no2yr(:,:)   |kg N/day      |average daily NO2-N loading for year
!!     no3yr(:,:)   |kg N/day      |average daily NO3-N loading for year
!!     orgnyr(:,:)  |kg N/day      |average daily organic N loading for year
!!     orgpyr(:,:)  |kg P/day      |average daily organic P loading for year
!!     sedyr(:,:)   |metric tons/d |average daily sediment loading for year
!!     solpstyr(:,:)|mg pst/day    |average daily soluble pesticide loading
!!                                 |for year
!!     srbpstyr(:,:)|mg pst/day    |average daily sorbed pesticide loading
!!                                 |for year
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     ii           |none          |counter
!!     iya          |none          |counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: ii, iya, eof, ia1
      integer :: n, startyr

!!    initialize variables
      eof = 0

      do ii = 1, 6 
        read (108,5000) titldum
      end do

!!    Read until the year is the beginning year of simulation
      iya = 1
        do
        read (108,*,iostat=eof) ia1, floyr(i,iya),                      
     &                       sedyr(i,iya),                              
     &                       orgnyr(i,iya),                             
     &                       orgpyr(i,iya),                             
     &                       no3yr(i,iya),                              
     &                       nh3yr(i,iya),                              
     &                       no2yr(i,iya),                              
     &                       minpyr(i,iya),                             
     &                       cbodyr(i,iya),                             
     &                       disoxyr(i,iya),                            
     &                       chlayr(i,iya),                             
     &                       solpstyr(i,iya),                           
     &                       srbpstyr(i,iya),                           
     &                       bactpyr(i,iya),                            
     &                       bactlpyr(i,iya),                           
     &                       cmtl1yr(i,iya),                            
     &                       cmtl2yr(i,iya),                            
     &                       cmtl3yr(i,iya)
            if (ia1 == iyr) exit
	      if (eof < 0) exit
!! S.Lu for when the point source starting year is later than simulation year
            if (ia1 > iyr) then
              n = ia1 -iyr      !find the starting year
              startyr = iya + n !find the starting year
          !!assign value to the starting year
               floyr(i,startyr) = floyr(i,iya)
               sedyr(i,startyr) = sedyr(i,iya)
               orgnyr(i,startyr) = orgnyr(i,iya)                        
               orgpyr(i,startyr) = orgpyr(i,iya)                        
               no3yr(i,startyr) = no3yr(i,iya)                          
               nh3yr(i,startyr) = nh3yr(i,iya)                          
               no2yr(i,startyr) = no2yr(i,iya)                          
               minpyr(i,startyr) = minpyr(i,iya)                        
               cbodyr(i,startyr) = cbodyr(i,iya)                        
               disoxyr(i,startyr) = disoxyr(i,iya)                      
               chlayr(i,startyr) = chlayr(i,iya)                       
               solpstyr(i,startyr) = solpstyr(i,iya)                    
               srbpstyr(i,startyr) = srbpstyr(i,iya)                   
               bactpyr(i,startyr) = bactpyr(i,iya)                     
               bactlpyr(i,startyr) = bactlpyr(i,iya)                    
               cmtl1yr(i,startyr) = cmtl1yr(i,iya)                     
               cmtl2yr(i,startyr) = cmtl2yr(i,iya)                      
               cmtl3yr(i,startyr) = cmtl3yr(i,iya)
          !!set values before starting year to 0
               do iya = 1 , n
                 floyr(i,iya) = 0.
                 sedyr(i,iya) = 0.
                 orgnyr(i,iya) = 0.                             
                 orgpyr(i,iya) = 0.                             
                 no3yr(i,iya) = 0.                              
                 nh3yr(i,iya) = 0.                              
                 no2yr(i,iya) = 0.                              
                 minpyr(i,iya) = 0.                             
                 cbodyr(i,iya) = 0.                             
                 disoxyr(i,iya) = 0.                            
                 chlayr(i,iya) = 0.                          
                 solpstyr(i,iya) = 0.                        
                 srbpstyr(i,iya) = 0.                        
                 bactpyr(i,iya) = 0.                         
                 bactlpyr(i,iya) = 0.                       
                 cmtl1yr(i,iya) = 0.                         
                 cmtl2yr(i,iya) = 0.                         
                 cmtl3yr(i,iya) = 0.                
               enddo
               exit
            endif
!! S.Lu for when the point source starting year is later than simulation year
        end do

!      do iya = 2, nbyr+2  !2 extra for forecast scenarios
      do iya = startyr+1, nbyr + 2
        read (108,*,iostat=eof) ia1, floyr(i,iya),                      
     &                       sedyr(i,iya),                              
     &                       orgnyr(i,iya),                             
     &                       orgpyr(i,iya),                             
     &                       no3yr(i,iya),                              
     &                       nh3yr(i,iya),                              
     &                       no2yr(i,iya),                              
     &                       minpyr(i,iya),                             
     &                       cbodyr(i,iya),                             
     &                       disoxyr(i,iya),                            
     &                       chlayr(i,iya),                             
     &                       solpstyr(i,iya),                           
     &                       srbpstyr(i,iya),                           
     &                       bactpyr(i,iya),                            
     &                       bactlpyr(i,iya),                           
     &                       cmtl1yr(i,iya),                            
     &                       cmtl2yr(i,iya),                            
     &                       cmtl3yr(i,iya)
        if (eof < 0) exit
      end do

      close (108)

      return
 5000 format (a80)
      end