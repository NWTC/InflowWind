2014.08.31  started
2014.11.26  updated

Stuff working on right now:
-- Driver code: setup the datatype for IfW_InitInput
-- Driver code: setup the storage arrays for the data from IfW.  This be setup beforehand -- may cause out of bounds during IfW exectution, but good test of that.
                  -- Everything but the number of timesteps for the DEFAULT DT and NumTimeSteps cases.  Set that up after initialization.
                  -- Dimensions set up beforehand. Index 0 for start.
-- Driver code: setup the FFT.  May need to create some additional types for storing the data.

List of items to tackle:
==============================

IfW control level
-- change all the windtype numbers to the new numbers (figure out exactly what this will be first, then update everything)
   -- still need to split FFWind.  Others done.
-- input file parsing / sanity checking of the sections
   -- filedata parameter setting routine --> copy data needed to the submodules.  Init others to zero.
-- summary output file.  Start this early, then add to it in the submodules.
-- revamp submodule arrangement (renaming/splitting etc)
   -- change the calling structure in IfW.f90, then make the changes to the submodules.  This will be somewhat iterative.
-- placeholder for the userwind.f90 submodule.  Put directions within that module and an error message in the module with information on how
   to impliment it.
-- mean wind direction for all modules
   -- in the CalcOutput, translate from XYZ to the X'Y'Z' coordinates (wind direction coordinates), pass in to submodule,
      translate the U'V'W' windspeed (wind coordinates) into the global coordinates.
   -- document how this is done someplace/somehow
   -- for HHWind (UniformWind), give a warning if the global winddirection and wind direction in the file are different (init routine someplace,
      using first timestep, differrent warning if non-zero global wind direction and direction changes from zero within file within the file. 
      Continue with sim anyhow)



Submodules
-- Uniform wind (also steady wind)
   -- summary file writing
   -- new interpolator
-- FFWind -> split to TurbSimFF and BladedStyleFF.  Change the WindNumbers for this.
   -- summary file writing
   -- new interpolator
-- HAWCWind -> Major revamp with the scaling.
   -- summary file writing
   -- new interpolator
   -- Scaling parameters --> must output info on this to the summary file.
-- BladedWind -> includes native Bladed and TurbSim bladed files.
   -- summary file writing
   -- new interpolator
-- CTWind -> major revamp of how this is implimented (talk to Bonnie when we get there)
   -- new interpolator (not sure if this applies here or not)
   -- summary file writing



Driver
-- Update driver to work with updated code
-- input file for the driver  -- keep the other method with some enhancement
-- list of points to pass in at each timestep (grid) (3D cartesian grid for summary file output -- see feature 1)
--



Additional items
-- CertTests and examples for each type of file.
-- change all documentation to refer to uniform wind instead of hub-height
-- change documentation so that it refers to the "reference-height" (RefHt) instead of "hub-height"
-- TestRoutines directory --> need to update all of that or get rid of it if it is replaced by the driver



Features needed
1) pass in 3D cartesian grid, output the results to a summary file
2) pass into init a value corresponding to the number of points we wish to calculate.  We will initialize the array for the points then and
   set the parameter for it.  This still needs some thinking as to what the best way to accomplish it is.
   -- do we pad that array slightly in case there are overruns?
   -- do we need to add the array for the requested positions to it, or is that handled automatically?
   -- can we add some logic or a second unallocated array to pass in for future points handling?  This might make it easier later when we
      need to add in a lidar unit for some of the timesteps but not others.  Make some simple documentation concerning this when we get to it.



Features to consider
a) add OutSwitch like in HydroDyn.  Is this something we want, or should this just be a driver level thing?



Questions
-- How are Bladed wind files (not the bladed-style ones) handled?  How do we need to modify things for that to work?  Right now there is no
   provision in the input file for hte Bladed files, just the bladed-style TurbSim generated ones.
-- Timeshift when periodic files are used.  We timeshift to T=0 (I think).  This is an issue when we want to compare a periodic to non-periodic
   file.  Maybe include a flag in the InitInputType and some driver handling for this?  Later add this to the input file?


Coding philosophy
-- all the submodules will be called based on the windtype associated with them.  All this happens on the module level.  The submodules don't
   need to know what their number is at all.
-- for InflowWind_Init and other routines, do all checking based on the number -- this number comes from the input file and must match the
   value in the InflowWind registry file (how do we verify that they are the same?).  For each of these routines, put a case structure in and
   call / do the appropriate stuff based on that (the idea is that this will make it easier to strip out a submodule or add one later, or
   reorder their numbering).  For some of the routines, we may want group things into subroutines so as to simplify the calling structures
   and make things more readable.
