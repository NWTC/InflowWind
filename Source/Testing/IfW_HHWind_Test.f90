PROGRAM HHWind_Test
!-----------------------------------------------------------------------------------------------------------------------------------
!  This program is for testing the wind modules during development.
!
!
!  v1.00.00a-adp  -- HHWind tested
!
!
!
!
!
!
!
!-----------------------------------------------------------------------------------------------------------------------------------


   USE NWTC_Library
   USE IfW_HHWind_Types
   USE IfW_HHWind


   IMPLICIT NONE

   TYPE( ProgDesc ), PARAMETER                        :: ProgInfo = ProgDesc("Wind_Test","v1.00.00a-adp","6-Feb-2013")



      ! Error handling
   CHARACTER(4096)                                    :: ErrMsg
   INTEGER(IntKi)                                     :: ErrStat


      ! The types used by HHWind
   TYPE(IfW_HHWind_InitInputType)                     :: HH_InitData
   TYPE(IfW_HHWind_InputType)                         :: HH_InData
   TYPE(IfW_HHWind_ParameterType)                     :: HH_ParamData
   TYPE(IfW_HHWind_ContinuousStateType)               :: HH_ContStates
   TYPE(IfW_HHWind_DiscreteStateType)                 :: HH_DiscStates
   TYPE(IfW_HHWind_ConstraintStateType)               :: HH_ConstrStates
   TYPE(IfW_HHWind_OtherStateType)                    :: HH_OtherStates
   TYPE(IfW_HHWind_OutputType)                        :: HH_OutData

   REAL(DbKi)                                         :: HH_Interval


      ! Local variables
   CHARACTER(1024)                                    :: WindFileName
   REAL(DbKi)                                         :: Time
   REAL(ReKi)                                         :: WindPosition(2,3)
   REAL(ReKi)                                         :: WindVelocity(2,3)





   !-=- Setup some things  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL NWTC_Init
   CALL DispNVD( ProgInfo )

   Time = 2.0
   HH_Interval = 0.01

      ! setup the file info
   HH_InitData%WindFile       = "../../Samples/Steady.wnd"           ! HHWind file
   HH_InitData%WindFile       = "../../Samples/SampleCase/Sample1.hh"
   HH_InitData%ReferenceHeight = 80.                        ! meters
   HH_InitData%Width           = 100.                       ! meters

   WindPosition(1,1) = 0.0                                  ! longitudinal front/back of tower
   WindPosition(1,2) = 0.0                                  ! lateral position left/right of tower
   WindPosition(1,3) = HH_InitData%ReferenceHeight          ! Height above ground

   WindPosition(2,1) = 10.0
   WindPosition(2,2) = 10.0
   WindPosition(2,3) = HH_InitData%ReferenceHeight

   ErrMsg   = ""
   ErrStat  = ErrID_None





   !-=- Initialize the module  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL WrScr(NewLine//" Initializing HHWind"//NewLine)

   CALL IfW_HHWind_Init(HH_InitData,   HH_InData,     HH_ParamData,                       &
                        HH_ContStates, HH_DiscStates, HH_ConstrStates,  HH_OtherStates,   &
                        HH_OutData,    HH_Interval,                                       &
                        ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrStat  = ErrID_None
   ErrMsg   = ""

   CALL AllocAry( HH_InData%Position, 2, 3, "Input position data 2x3 array", ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrStat  = ErrID_None
   ErrMsg   = ""

      ! Copy the WindPosition over to the InData%Position array
   HH_InData%Position   = WindPosition

   !-=- Simple call to get windspeed at just the hub -=-=-=-=-=-=-=-

   CALL WrScr(" Calculating wind velocity:")

   CALL  IfW_HHWind_CalcOutput(  Time,    HH_InData,     HH_ParamData,                          &
                           HH_ContStates, HH_DiscStates, HH_ConstrStates,     HH_OtherStates,   &
                           HH_OutData,    ErrStat,       ErrMsg)

      ! copy the Velocity data over for this timestep
   WindVelocity=HH_OutData%Velocity

   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrMsg   = ""
   ErrStat  = ErrID_None


   !-=- Write out some info about what we just did -=-=-=-=-=-=-=-=-

   CALL WrScr("   Time: "//TRIM(Num2LStr(Time)))
   CALL WrScr("          (x, y, z)          (U, V, W)")

   CALL WrScr("          ("//TRIM(Num2LStr(WindPosition(1,1)))//", "//TRIM(Num2LStr(WindPosition(1,2)))//", " &
                           //TRIM(Num2LStr(WindPosition(1,3)))//")        (" &
                           //TRIM(Num2LStr(WindVelocity(1,1)))//", "//TRIM(Num2LStr(WindVelocity(1,2)))//", " &
                           //TRIM(Num2LStr(WindVelocity(1,3)))//")")

   CALL WrScr("          ("//TRIM(Num2LStr(WindPosition(2,1)))//", "//TRIM(Num2LStr(WindPosition(2,2)))//", " &
                           //TRIM(Num2LStr(WindPosition(2,3)))//")        (" &
                           //TRIM(Num2LStr(WindVelocity(2,1)))//", "//TRIM(Num2LStr(WindVelocity(2,2)))//", " &
                           //TRIM(Num2LStr(WindVelocity(2,3)))//")")

   !-=- Close everything -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL IfW_HHWind_End( HH_InData,     HH_ParamData,                                      &
                        HH_ContStates, HH_DiscStates, HH_ConstrStates,  HH_OtherStates,   &
                        HH_OutData,                                                       &
                        ErrStat,       ErrMsg )

   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrMsg   = ""
   ErrStat  = ErrID_None


END PROGRAM


! TODO:
!  -- Fix the error handling so it is correct
!  -- Rename routines to match framework
!  -- Convert the Init routine to the framework
!  -- Convert the Calc routine to the framework -- unfunction it
!  -- Convert the End  routine to the framework
!  -- Figure out how to store the info we need (types)
