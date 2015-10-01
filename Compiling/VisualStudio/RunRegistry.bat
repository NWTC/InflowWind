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

SET Registry=C:\Users\aplatt\Documents\software_development\windsvn\FAST\branches\FAST_Registry\bin\Registry_Win32.exe

SET NWTC_Lib_Loc=C:\Users\aplatt\Documents\software_development\windsvn\NWTC_Library\trunk\source


SET IfW_Reg_Loc=C:\Users\aplatt\Documents\software_development\windsvn\InflowWind\branches\modularization2\Source\Registry
SET IfW_Reg_OutLoc=C:\Users\aplatt\Documents\software_development\windsvn\InflowWind\branches\modularization2\Source


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
:IfW_UniformWind
:IfW_UserWind
SET CURR_LOC=%IfW_Loc%
%REGISTRY% "%IfW_Reg_Loc%\%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%" -noextrap  -O "%CURR_LOC%"
GOTO checkError


:checkError
ECHO.
IF %ERRORLEVEL% NEQ 0 (
ECHO Error running FAST Registry for %ModuleName%.
) ELSE (
ECHO Registry for %ModuleName% completed.
REM COPY /Y "%ModuleName%_Types.f90"   "%CURR_LOC%"
rem IF /I "%ModuleName%"=="MAP" COPY /Y "%ModuleName%_Types.h" "%CURR_LOC%"
)




:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET REGISTRY=

SET NWTC_Lib_Loc=
SET IfW_Loc=

SET ModuleName=
SET CURR_LOC=
:Done
echo %lines%
set lines=