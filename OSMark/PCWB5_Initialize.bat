rem delete the old MSOffice work files:
del c:\worldbench\msoffice\*.* /q

rem extract the MSOffice work files:
c:\worldbench\cabarc -o X c:\worldbench\msoffice.inp.cab c:\worldbench\msoffice\

rem extract the Outlook files:
md C:\pcwb5_msoffice_use
c:\worldbench\cabarc -o X c:\worldbench\msoffice.use.cab *.pst *.dat C:\pcwb5_msoffice_use\

rem copy the files to user directory:
xcopy C:\pcwb5_msoffice_use\*.* "%USERPROFILE%\Local Settings\Application Data\Microsoft\Outlook" /c/h/e/r/s/k/y
xcopy C:\pcwb5_msoffice_use\*.* "%USERPROFILE%\Outlook" /c/h/e/r/s/k/y

rem delete the temp directory:
del C:\pcwb5_msoffice_use /q

