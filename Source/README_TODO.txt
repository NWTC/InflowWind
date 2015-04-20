2014.08.31  started
2015.03.03  updated

Stuff working on right now:
-- Driver output file writing -- something quick for testing.  The outlist will be handled exclusively by the FAST glue code.
-- IfW summary file writing.


List of items to tackle:
==============================

IfW control level
-- summary output file.
-- for HHWind (UniformWind), give a warning if the global winddirection and wind direction in the file are different (init routine someplace,
   using first timestep, different warning if non-zero global wind direction and direction changes from zero within file.
   Continue with sim anyhow)
X- setup the FFT.  May need to create some additional types for storing the data.



Documentation
-- what the input file parameters description (see the FAST 8 README in Bonnie's branch)



Submodules
-- Uniform wind (also steady wind)
   -- summary file writing
-- TSFFWind
   -- summary file writing
   -- new interpolator
-- BladedFFWind
   -- summary file writing
   -- new interpolator
-- HAWCWind -> Major revamp with the scaling.
   -- summary file writing
   -- new interpolator
   -- Scaling parameters --> must output info on this to the summary file.
-- CTWind -> major revamp of how this is implimented (talk to Bonnie when we get there)
   -- new interpolator (not sure if this applies here or not)
   -- summary file writing


Additional items
-- CertTests and examples for each type of file.
-- change documentation so that it refers to the "reference-height" (RefHt) instead of "hub-height"
-- TestRoutines directory --> need to update all of that or get rid of it if it is replaced by the driver



Features needed
Lidar module integration -- Bonnie has something in the other branch that this will be based on.  Will do that after other things are done.


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
