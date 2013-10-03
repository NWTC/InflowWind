PROGRAM FFWind_Test
!-----------------------------------------------------------------------------------------------------------------------------------
!  This program is for testing the wind modules during development.
!
!
!  v1.00.00a-adp  -- FFWind tested
!
!
!  The purpose of this code is simply to test the IfW_FFWind module. It does not do much useful beyond initialize the module,
!  calculate some values, and end the module.
!
!
!
!
!-----------------------------------------------------------------------------------------------------------------------------------


   USE NWTC_Library
   USE IfW_FFWind_Types
   USE IfW_FFWind


   IMPLICIT NONE

   TYPE( ProgDesc ), PARAMETER                        :: ProgInfo = ProgDesc("FFWind_Test","v1.00.00a-adp","18-Sep-2013")



      ! Error handling
   CHARACTER(4096)                                    :: ErrMsg
   INTEGER(IntKi)                                     :: ErrStat


      ! The types used by FFWind
   TYPE(IfW_FFWind_InitInputType)                     :: FF_InitData
   TYPE(IfW_FFWind_InputType)                         :: FF_InData
   TYPE(IfW_FFWind_ParameterType)                     :: FF_ParamData
   TYPE(IfW_FFWind_ContinuousStateType)               :: FF_ContStates
   TYPE(IfW_FFWind_DiscreteStateType)                 :: FF_DiscStates
   TYPE(IfW_FFWind_ConstraintStateType)               :: FF_ConstrStates
   TYPE(IfW_FFWind_OtherStateType)                    :: FF_OtherStates
   TYPE(IfW_FFWind_OutputType)                        :: FF_OutData
   TYPE(IfW_FFWind_InitOutputType)                    :: FF_InitOutData

   REAL(DbKi)                                         :: FF_Interval


      ! Local variables
   REAL(DbKi)                                         :: Time
   REAL(ReKi),ALLOCATABLE                             :: WindPosition(:,:)
   REAL(ReKi),ALLOCATABLE                             :: WindVelocity(:,:)


      ! Temporary variables
   CHARACTER(1024)                                    :: TmpChar
   INTEGER(IntKi)                                     :: TmpInt
   CHARACTER(1024)                                    :: TmpErrMsg
   INTEGER(IntKi)                                     :: TmpErrStat
   INTEGER(IntKi)                                     :: NumPoints




   !-=- Setup some things  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL NWTC_Init
   CALL DispNVD( ProgInfo )

   Time = 2.0
   FF_Interval = 0.01

      ! setup the file info
   FF_InitData%WindFileName   = "../../SampleCase/Sample1.bts"
   FF_InitData%ReferenceHeight = 80.                        ! meters
   FF_InitData%Width           = 100.                       ! meters


      ! Allocate the WindPosition array
   CALL AllocAry( WindPosition, 3, 3, "WindPosition data", ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
      ErrStat = 0
      ErrMsg   = ''
   ENDIF


   WindPosition(1,1) = 0.0                                  ! longitudinal front/back of tower
   WindPosition(2,1) = 0.0                                  ! lateral position left/right of tower
   WindPosition(3,1) = FF_InitData%ReferenceHeight          ! Height above ground

   WindPosition(1,2) = 30.0
   WindPosition(2,2) = 30.0
   WindPosition(3,2) = FF_InitData%ReferenceHeight

   WindPosition(1,3) = 0.0
   WindPosition(2,3) = 0.0
   WindPosition(3,3) = FF_InitData%ReferenceHeight+30

   ErrMsg   = ""
   ErrStat  = ErrID_None





   !-=- Initialize the module  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL WrScr(NewLine//" Initializing FFWind"//NewLine)

   CALL IfW_FFWind_Init(FF_InitData,   FF_InData,     FF_ParamData,                       &
                        FF_ContStates, FF_DiscStates, FF_ConstrStates,  FF_OtherStates,   &
                        FF_OutData,    FF_Interval,   FF_InitOutData,                     &
                        ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrStat  = ErrID_None
   ErrMsg   = ""

   DEALLOCATE( FF_InData%Position, STAT = TmpErrStat)

   CALL AllocAry( FF_InData%Position, 3, SIZE( WindPosition,2 ), "Input position data 3xN", ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
      ErrStat  = ErrID_None
      ErrMsg   = ''
   ENDIF


      ! Copy the WindPosition over to the InData%Position array
   FF_InData%Position   = WindPosition

   !-=- Simple call to get windspeed at just the hub -=-=-=-=-=-=-=-

   CALL WrScr(" Calculating wind velocity:")

   CALL  IfW_FFWind_CalcOutput(  Time,    FF_InData,     FF_ParamData,                          &
                           FF_ContStates, FF_DiscStates, FF_ConstrStates,     FF_OtherStates,   &
                           FF_OutData,    ErrStat,       ErrMsg)

      ! Allocate the WindVelocity array
   CALL AllocAry( WindVelocity, 3, SIZE(FF_OutData%Velocity,2), "WindVelocity data", ErrStat, ErrMsg )
   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
      ErrStat = 0
      ErrMsg   = ''
   ENDIF

      ! copy the Velocity data over for this timestep
   WindVelocity=FF_OutData%Velocity

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
   DO TmpInt=1,SIZE(WindVelocity,2)
      write (TmpChar, "( f8.3,'"//"  "//"', 3(f12.2),'"//"  "//"', 3(f16.4))")            &
                  Time,                                                                   &
                  WindPosition(1,TmpInt),WindPosition(2,TmpInt),WindPosition(3,TmpInt),   &
                  WindVelocity(1,TmpInt),WindVelocity(2,TmpInt),WindVelocity(3,TmpInt)
      CALL WrScr( TRIM(TmpChar))
   ENDDO

   !-=- Close everything -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL IfW_FFWind_End( FF_InData,     FF_ParamData,                                      &
                        FF_ContStates, FF_DiscStates, FF_ConstrStates,  FF_OtherStates,   &
                        FF_OutData,                                                       &
                        ErrStat,       ErrMsg )

   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrMsg   = ""
   ErrStat  = ErrID_None



END PROGRAM
