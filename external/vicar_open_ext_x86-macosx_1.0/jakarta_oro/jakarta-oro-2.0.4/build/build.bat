@echo off

REM $Id: build.bat,v 1.2 2001/07/25 18:54:44 dfs Exp $
REM --------------------------------------------
REM Default == jar
REM "lib"            target builds the library
REM "examples"       target builds the example programs
REM "tools"          target builds the tools
REM "clean"          target removes generated files
REM "jar"            target builds core + jar
REM "javadocs"       target builds the javadoc
REM "package"        target builds core + jar + javadoc + distribution
REM --------------------------------------------
set TARGET=%1%


REM --------------------------------------------
REM No need to edit anything past here
REM --------------------------------------------
set BUILDFILE=build.xml
if "%TARGET%" == "" goto setdist
goto final

:setdist
set TARGET=jar
goto final

:final

if "%JAVA_HOME%" == "" goto javahomeerror

if exist %JAVA_HOME%\lib\tools.jar set CLASSPATH=%CLASSPATH%;%JAVA_HOME%\lib\tools.jar
set CP=%CLASSPATH%;ant-1.3.jar;parser.jar;jaxp.jar;junit-3.7.jar

echo Classpath: %CP%
echo JAVA_HOME: %JAVA_HOME%

%JAVA_HOME%\bin\java.exe -classpath "%CP%" org.apache.tools.ant.Main -buildfile %BUILDFILE% %TARGET%

goto end


REM -----------ERROR-------------
:javahomeerror
echo "ERROR: JAVA_HOME not found in your environment."
echo "Please, set the JAVA_HOME variable in your environment to match the"
echo "location of the Java Virtual Machine you want to use."

:end
