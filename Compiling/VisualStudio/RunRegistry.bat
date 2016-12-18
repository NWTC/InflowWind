@ECHO OFF

set lines=======================================================================
echo %lines%
IF "%1"=="" (
ECHO.
ECHO   The calling syntax for this script is
ECHO             RunRegistry ModuleName
ECHO.
GOTO Done
)


REM ----------------------------------------------------------------------------
REM ------------------------- LOCAL PATHS --------------------------------------
REM ----------------------------------------------------------------------------
REM -- USERS MAY EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL MACHINES. -
REM -- NOTE: do not use quotation marks around the path names!!!! --------------
REM ----------------------------------------------------------------------------
REM ----------------------------------------------------------------------------

SET Registry=..\..\bin\Registry_win32.exe
SET Source_Loc=..\..\Source

SET NWTC_Lib_Loc=%Source_Loc%\NWTC_Library\source

SET IfW_Loc=%Source_Loc%
SET IfW_Reg_Loc=%Source_Loc%\Registry
SET IfW_Reg_OutLoc=%Source_Loc%



IF /I "%2"=="dev" CALL ..\Set_FAST_paths.bat

SET ModuleName=%1

GOTO %ModuleName%

REM ----------------------------------------------------------------------------
REM ---------------- RUN THE REGISTRY TO AUTO-GENERATE FILES -------------------
REM ----------------------------------------------------------------------------

:InflowWind
:Lidar
SET CURR_LOC=%IfW_Loc%
%REGISTRY% "%IfW_Reg_Loc%\%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%" -O "%CURR_LOC%"
GOTO checkError



:IfW_TSFFWind
:IfW_HAWCWind
:IfW_BladedFFWind
:IfW_UserWind
:IfW_UniformWind
:IfW_4Dext
SET CURR_LOC=%IfW_Loc%
%REGISTRY% "%IfW_Reg_Loc%\%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%" -noextrap  -O "%CURR_LOC%"
GOTO checkError


:checkError
ECHO.
IF %ERRORLEVEL% NEQ 0 (
ECHO Error running FAST Registry for %ModuleName%.
) ELSE (
ECHO Registry for %ModuleName% completed.
)

:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET REGISTRY=

SET NWTC_Lib_Loc=
SET Source_Loc=

SET ModuleName=
SET CURR_LOC=
:Done
echo %lines%
set lines=