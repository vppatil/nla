
executable = condorRun.bat
transfer_input_files = unzip.exe,bzip2.dll,zip.exe,zip32z64.dll,convertGdb_condor.py,NHDH_SC_931V210.zip
arguments = 


universe   = vanilla
output = phase1.out
error = phase1.err
log = phase1.log
requirements = (TARGET.Arch == "X86_64") && ((TARGET.OpSys == "WINNT61")) && (Machine =!= "hanson-i5.ad.wisc.edu")


should_transfer_files = YES
+AccountingGroup = "Blah"
+WantFlocking = true
when_to_transfer_output = ON_EXIT
notification = never
queue
