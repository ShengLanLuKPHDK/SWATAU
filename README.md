# New modules for SWAT 

## Macropore and pesticide transport to tile drains

**New module** : Macropore and drain discharge and pesticide transport

**Original author** : Shenglan Lu

**Github link** : [https://github.com/ShengLanLuKPHDK/SWATAU](https://github.com/ShengLanLuKPHDK/SWATAU)

**Branch name** : feature/drainPST

**SWAT code base version** : 664

### New source files
| Filename(s) | Key content/functionality of subroutine(s) | comments |
|:--- |:--- | --- |
| macroflow.f90 | computes preferential flow from macropores |   |
| macropst.f90 | computes soluble pesticide that leach to the tile drains via macropore |   |
| macrosed.f90 | computes sediment transport to the tile drains via macropore |   |
| macrosed_pst.f90 | computes sediment bound pesticide that leach to the tile drains via macropore |   |
| pestdrain.f90 | computes soluble pesticide lost from tile drain |   |
| tilesplit.f90 | computes fraction of the tile drain flow from each soil layer |  |

### Changes to existing source files
| Filename(s) | Key content/functionality added to existing subroutine(s) | comments |
|:--- |:--- | --- |
| allocate_parms.f | allocate new parameters |   |
| layersplit.f | determine the soil layer number of the wet layer |   |
| modparm.f | add new parameters |   |
| percmain.f | call macroflow and tilesplit module |   |
| pestlch.f | calculate soluble pesticide concentration in soil, call macropst and pestdrain module. |   |
| pesty.f | call macrosed_pst module |   |
| readbsn.f | read new input parameters, including calibration parameters and flags to turn on pestdrain and macropore | overwrite original swat bacterial parameters |
| sim_initday.f | reset variables daily |   |
| soil_phys.f | compute maximum detachable sediment pool for macropore sediment |   |
| subbasin.f | call macrosed module |   |
| sumv.f | add macropore sediment to sediment yield, summarize pesticide from tile drains |   |
| varinit.f | initialize variables |   |
| virtual.f | route tile drain pesticide to rivers |   |
| zero0.f | initialize variables |   |

### New model parameters

| Parameter name(s) | Replacing which existing parameter in which file\* incl. on/off switches | comments |
|:--- |:--- | --- |
| macro_fr | WDPQ  in basin.bsn | fraction of saturated soil water content, condisiton for macropre onset (ranging from 0-1) |
| dep_wet | WGPQ  in basin.bsn | depth of wet layers (range depend on input soil depth) |
| kr | WDLPQ  in basin.bsn | macropore sediment replenishment rate coefficient (not calibrated, last value 4) |
| filt | WGLPQ  in basin.bsn | macropore sediment filtering when reaching tile drains (ranging 0-1) |
| k_theta | WDPS in basin.bsn | Michaelis-Menton half-saturation constant for the macropore flow fraction (default value 0.5, ranging 0-1) |
| percot | wgps in basin.bsn | soluble pesticide concentration in macropore and tile drain flow (similar concept with percop for surface runoff) (ranging from 0-1)  |
| ifast | new line at the end of bansin.bsn | switch for macropore module, 0 off 1 on |
| ipest | new line at the end of bansin.bsn | switch for soluble pesticide leaching module, 0 off 1 on |

\*for the purpose of SWATCUP calibration 

## Soluble reactive phosphorus (SRP) transport from tile drains
**New module** : Soluble phosphorus transport from tile drains

**Original author** : Shenglan Lu

**Github link** : [https://github.com/ShengLanLuKPHDK/SWATAU](https://github.com/ShengLanLuKPHDK/SWATAU)

**Branch name** : feature/drainP

**SWAT code base version** : 664

### New source files
| Filename(s) | Key content/functionality of subroutine(s) | comments |
|:--- |:--- | --- |
| soilPout.f90 | sum soil P for whole soil profile and print soil P budget |   |
| soilplch.f90 | compute soluble P leaching, transport in surface runoff, tile drains and lost from soil profile | use soil salinity input (sol_ec in .chm) for the phosphorus bounding material (Alox + Feox) |
| tilqsplit.f90 | computes fraction of the tile drain flow from each soil layer |   |

### Changes to existing source files
| Filename(s) | Key content/functionality added to existing subroutine(s) | comments |
|:--- |:--- | --- |
| allocate_parms.f | allocate new parameters |   |
| header.f | add header tile P in daily hru level output |   |
| hruday.f90 | add tilep output at hru level, call soilPout module |   |
| hrumon.f | route tile p at monthly hru level | overwrite crackflow output |
| modparm.f | add new parameters |   |
| percmain.f | call tilesplit module |   |
| readbsn.f | read in new input parameters for calibration and flag for drainP | overwrite swat bacterial parameter |
| readfile.f | create soil P content (soilP.out) and soil P budget output file(soilPbudget.out) | only daily output |
| readyr.f | correct point source input when point source input starting year is later than simulation starting year | otherwise the point source is not loaded |
| sim_initday.f | initialize subbasin level tile p |   |
| subaa.f | sum soluble p from tile drains and ground water |   |
| subbasin.f | call solplch module |   |
| submon.f | sum soluble p from tile drains and ground water |   |
| subyr.f | sum soluble p from tile drains and ground water |   |
| sumv.f | sum soluble p from surface runoff, tile drains and ground water |   |
| varinit.f | initialize variables |   |
| virtual.f | route soluble p from tile drains to the stream |   |
| zero0.f | initialize variable |   |

### New model parameters
| Parameter name(s) | Replacing which existing parameter in which file\* incl. on/off switches | comments |
|:--- |:--- | --- |
| k_langmuir | WDPQ in basin.bsn | Langmuir adsorption constant (l mg-1) (range 0.5-2.4) |
| Qmax_beta | WGPQ in basin.bsn | Maximum adsorption fraction (range 0.06-0.23) |
| itilep | new line at the end of the basin.bsn file | switch for drainP module, 0 off 1 on |

\*for the purpose of SWATCUP calibration


## Macrophyte growth in the streams
*Notice*: Benthic sediment nutirent dynamics and interaction with water colum not tested

**New module** : Macrophytes in SWAT rivers

**Original author** : Shenglan Lu

**Github link** : [https://github.com/ShengLanLuKPHDK/SWATAU](https://github.com/ShengLanLuKPHDK/SWATAU)

**Branch name** : feature/macrophyte

**SWAT code base version** : 664

### New source files
| Filename(s) | Key content/functionality of subroutine(s) | comments |
|:--- |:--- | --- |
| bedsediment.f90 | computes P and N in river benthic sediment |   |
| macrophyte.f90 | computes in-stream macrophyte growth | also change mannings N |

### Changes to existing source files
| Filename(s) | Key content/functionality added to existing subroutine(s) | comments |
|:--- |:--- | --- |
| allocate_parms.f | allocate new parameters |   |
| modparm.f | add new parameters |   |
| rchinit.f | initialize variable for sediment resuspension (rchdy(61, jrch)) |   |
| readbsn.f | flag for macrophyte growth and maximum solar radiation |   |
| readpnd.f | basin level input for benthic sediment parameters and macrophyte growth |   |
| readrte.f | initialize sediment P and P values | Not complete |
| route.f | reset sediment resuspension, call macrophyte and bedsediment module |   |
| rtday.f | assign hydraulic radius for each channel |   |
| rthsed.f | initialize variable for sediment resuspension (rchdy(61, jrch)) |   |
| rtout.f | write macrophyte biomass and sediment resuspension in the output | overwrite reacted pesticide in swat |
| rtsed.f | assign sediment resuspension |   |
| rtsed_bagnold.f | assign sediment resuspension |   |
| rtsed_kodatie.f | assign sediment resuspension |   |
| rtsed_Molinas_Wu.f | assign sediment resuspension |   |
| rtsed_yandsand.f | assign sediment resuspension |   |
| sim_initday.f | reset variables |   |
| zero.f | initialize variables |   |

### New model parameters
| Parameter name(s) | Replacing which existing parameter in which file* incl. on/off switches | comments |
|:--- |:--- | --- |
| k_n_m | PND_PSA in .pnd file | Michaelis-Menton half-saturation constant for nitrogen for macrophyte growth (mg N g-2) (range 0.01 -0.03) |
| k_p_m | PND_PVOL in .pnd file | Michaelis-Menton half-saturation constant for phosphor for macrophyte growth (mg P g-2) (range 0.006 – 0.03) |
| SefSma | PND_ESA in .pnd file | Self-shading for macrophyte growth (g DW m-2) (range 10-30) |
| death | PND_EVOL in .pnd file | Death rate due to flow (range 0.008-0.015) |
| grmax | PND_VOL in .pnd file | Macrophyte growth rate at 20 °C (range 0.4-0.7) |
| minDeath | PND_ORGN in .pnd file | Minimum death rate due to respiration (range 0.01-0.04) |
| max_biomass | PND_ORGP in .pnd file | Maximum macrophyte biomass within a stream (g DW m -2) (range 220-300) |
| difpo4 | PND_SED in .pnd file | Never calibrated or initialized. Mol soluble reactive phosphorus diffusion constant (m-2day-1) |
| difno3 | PND_NSED in .pnd file | Never calibrated or initialized. Mol NO3 diffusion constant (m-2day-1) |
| diffnh4 | PND_K in .pnd file | Never calibrated or initialized. Mol NH4 diffusion constant (m-2day-1) |
| minrate | IFLOD1 in .pnd file | Never calibrated or initialized. Nitrogen mineralization rate |
| nitrate | IFLOD2 in .pnd file | Never calibrated or initialized. Nitrogen nitrification rate |
| pfr | PSETLP1 in .pnd file | Never calibrated or initialized. Fraction of phosphorus in macrophyte |
| nfr | PSETLP2 in .pnd file | Never calibrated or initialized. Fraction of nitrogen in macrophyte |
| nh4sed | NSETLP1 in .pnd file | Never calibrated or initialized. Initial amount of ammonia in river bed sediment porus water |
| no3sed | NSETLP2 in .pnd file | Never calibrated or initialized. Initial amount of nitrate in river bed sediment porus water |
| srpsed | CHLAP in .pnd file | Never calibrated or initialized. Initial amount of solute reactive phosphuros in bed sediment porus water |
| kd | SECCIP in .pnd file | Never calibrated or initialized. Ability of sediment absorbing phosphorus from surrounding substance |
| ma_biomass | PND_NO3 in .pnd file | Never calibrated or initialized. Initial macrophyte biomass |
| imacrophyte | new line at the end of basin.bsn | switch for macrophyte module, 0 off 1 on |
| radmax | new line at the end of basin.bsn | maximum solar radiation, input parameter. Difficult to determine during SWAT simulation, because swat does not read all weather input data before simulation. |

\*for the purpose of SWATCUP calibration