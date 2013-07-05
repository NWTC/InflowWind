PROGRAM HHWind_Test
!-----------------------------------------------------------------------------------------------------------------------------------
!  This program is for testing the wind modules during development.
!
!
!  v1.00.00a-adp  -- HHWind tested
!
!
!  The purpose of this code is simply to test the IfW_HHWind module. It does not do much useful beyond initialize the module,
!  calculate some values, and end the module.
!
!
!
!
!-----------------------------------------------------------------------------------------------------------------------------------


   USE NWTC_Library
   USE IfW_HHWind_Types
   USE IfW_HHWind


   IMPLICIT NONE

   TYPE( ProgDesc ), PARAMETER                        :: ProgInfo = ProgDesc("Wind_Test","v1.00.00a-adp","28-Feb-2013")



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
   REAL(DbKi)                                         :: Time
   REAL(ReKi)                                         :: WindPosition(3,3)
   REAL(ReKi)                                         :: WindVelocity(3,3)


      ! Temporary variables
   CHARACTER(1024)                                    :: TmpChar
   INTEGER(IntKi)                                     :: TmpInt




   !-=- Setup some things  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL NWTC_Init
   CALL DispNVD( ProgInfo )

   Time = 2.0
   HH_Interval = 0.01

      ! setup the file info
   HH_InitData%WindFileName   = "../../Samples/Steady.wnd"           ! HHWind file
   HH_InitData%WindFileName   = "../../Samples/SampleCase/Sample1.hh"
   HH_InitData%ReferenceHeight = 80.                        ! meters
   HH_InitData%Width           = 100.                       ! meters

   WindPosition(1,1) = 0.0                                  ! longitudinal front/back of tower
   WindPosition(2,1) = 0.0                                  ! lateral position left/right of tower
   WindPosition(3,1) = HH_InitData%ReferenceHeight          ! Height above ground

   WindPosition(1,2) = 30.0
   WindPosition(2,2) = 30.0
   WindPosition(3,2) = HH_InitData%ReferenceHeight

   WindPosition(1,3) = 0.0
   WindPosition(2,3) = 0.0
   WindPosition(3,3) = HH_InitData%ReferenceHeight+30

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

   CALL AllocAry( HH_InData%Position, 3, 2, "Input position data 3x2 array", ErrStat, ErrMsg )
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

   CALL WrScr(NewLine//NewLine// &
         '   Time           x           y           z               U               V               W'//NewLine// &
         ' -------       -------     -------     -------         ---------       ---------       ---------')
   DO TmpInt=1,3
      write (TmpChar, "( f8.3,'"//"  "//"', 3(f12.2),'"//"  "//"', 3(f16.4))")            &
                  Time,                                                                   &
                  WindPosition(1,TmpInt),WindPosition(2,TmpInt),WindPosition(3,TmpInt),   &
                  WindVelocity(1,TmpInt),WindVelocity(2,TmpInt),WindVelocity(3,TmpInt)
      CALL WrScr( TRIM(TmpChar))
   ENDDO

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
