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
   USE HHWind



   IMPLICIT NONE

   TYPE( ProgDesc ), PARAMETER                        :: ProgInfo = ProgDesc("Wind_Test","v1.00.00a-adp","6-Feb-2013")



      ! Error handling
   CHARACTER(2048)                                    :: ErrMsg
   INTEGER(IntKi)                                     :: ErrStat


      ! Local variables
   CHARACTER(1024)                                    :: WindFileName
   INTEGER(IntKi)                                     :: UnWind         !FIXME: this should be removed when fully converted to the modular framework
   REAL(DbKi)                                         :: Time
   REAL(ReKi)                                         :: WindPosition(3)
   REAL(ReKi)                                         :: WindVelocity(3)


!FIXME: remove this one
   TYPE(HH_Info)                                      :: HHInitInfo



   !-=- Setup some things  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL NWTC_Init
   CALL DispNVD( ProgInfo )

      ! setup the file info
   WindFileName      = "../../Samples/Steady.wnd"           ! HHWind file
   HHInitInfo%ReferenceHeight = 80.                         ! meters
   HHInitInfo%Width           = 100.                        ! meters

   WindPosition(1)   = 0.0                                  ! longitudinal front/back of tower
   WindPosition(2)   = 0.0                                  ! lateral position left/right of tower
   WindPosition(3)   = HHInitInfo%ReferenceHeight           ! Height above ground

      ! find a unit number to use, then check the errors
   CALL GetNewUnit(UnWind,ErrStat,ErrMsg)

   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrMsg   = ""
   ErrStat  = ErrID_None



   !-=- Initialize the module  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL WrScr(NewLine//" Initializing HHWind"//NewLine)

   CALL HH_Init( UnWind, WindFileName, HHInitInfo, ErrStat, ErrMsg )

   IF ( ErrStat >= ErrID_Severe ) THEN
      CALL ProgAbort(ErrMsg)
   ELSEIF ( ErrStat /= ErrID_None ) THEN
      CALL ProgWarn(ErrMsg)
   ENDIF
   ErrMsg   = ""
   ErrStat  = ErrID_None


   !-=- Simple call to get windspeed at just the hub -=-=-=-=-=-=-=-

   CALL WrScr(" Calculating wind velocity:")

   WindVelocity = HH_GetWindSpeed( Time, WindPosition, ErrStat, ErrMsg )

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

   CALL WrScr("          ("//TRIM(Num2LStr(WindPosition(1)))//", "//TRIM(Num2LStr(WindPosition(2)))//", " &
                           //TRIM(Num2LStr(WindPosition(3)))//")        (" &
                           //TRIM(Num2LStr(WindVelocity(1)))//", "//TRIM(Num2LStr(WindVelocity(2)))//", " &
                           //TRIM(Num2LStr(WindVelocity(3)))//")")

   !-=- Close everything -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

   CALL HH_Terminate( ErrStat, ErrMsg )

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
