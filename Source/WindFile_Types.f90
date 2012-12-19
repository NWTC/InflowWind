MODULE   WindFile_Types
!FIXME: I'm not sure what to do with these. This might be the wrong place to be keeping this information.
!        -->   Can't make them parameters.
!        -->   I think these are used locally only, not by any external code. Can't really tell though since they used
!        -->   to be PUBLIC,PARAMETER.
!
!        --> Make these parameters within the module, but not shared to the outside world. Put in the subroutines?
!  
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of InflowWind.
!
!    InflowWind is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!    
!    You should have received a copy of the GNU General Public License along with InflowWind.  
!    If not, see <http://www.gnu.org/licenses/>.
!    
!**********************************************************************************************************************************


   ! The parameters here are not considered private, but are not accessable unless the module is called.
   IMPLICIT NONE


   INTEGER,PARAMETER          :: DEFAULT_Wind = -1    ! Undetermined wind type; calls internal routine to guess what type it is
   INTEGER,PARAMETER          :: Undef_Wind   =  0    ! This is the code for an undefined WindFileType
   INTEGER,PARAMETER          :: HH_Wind      =  1    ! Hub-Height wind file
   INTEGER,PARAMETER          :: FF_Wind      =  2    ! Binary full-field wind file
   INTEGER,PARAMETER          :: UD_Wind      =  3    ! User-defined wind
   INTEGER,PARAMETER          :: FD_Wind      =  4    ! 4-dimensional wind (LES)
   INTEGER,PARAMETER          :: CTP_Wind     =  5    ! Coherent turbulence wind field (superimpose KH billow on background wind)
   INTEGER,PARAMETER          :: HAWC_Wind    =  6    ! Binary full-field wind file in HAWC format

END MODULE   WindFile_Types
