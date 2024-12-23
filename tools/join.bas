DECLARE SUB JoinFile(pathname AS STRING, inputfile AS STRING, outputfile AS STRING)

CONST FILEPATH = "C:\Usr\Projects\Dark Castle FB018\"

KILL    FILEPATH + "tools\all.bas"
JoinFile FILEPATH, "dcastle.bas","tools\all.bas"
JoinFile FILEPATH, "menlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "menlib.bas", "tools\all.bas"
JoinFile FILEPATH, "gamlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "gamlib.bas", "tools\all.bas"
JoinFile FILEPATH, "maplib.bi",  "tools\all.bas"
JoinFile FILEPATH, "maplib.bas", "tools\all.bas"
JoinFile FILEPATH, "isolib.bi",  "tools\all.bas"
JoinFile FILEPATH, "isolib.bas", "tools\all.bas"
JoinFile FILEPATH, "texlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "texlib.bas", "tools\all.bas"
JoinFile FILEPATH, "pcxlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "pcxlib.bas", "tools\all.bas"
JoinFile FILEPATH, "conlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "conlib.bas", "tools\all.bas"
JoinFile FILEPATH, "autlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "autlib.bas", "tools\all.bas"
JoinFile FILEPATH, "matlib.bi",  "tools\all.bas"
JoinFile FILEPATH, "matlib.bas", "tools\all.bas"

SUB JoinFile(pathname AS STRING, inputfile AS STRING, outputfile AS STRING)

  'Variables
  DIM AS STRING file1, file2
  DIM AS INTEGER handle1, handle2
  DIM AS STRING fline
  
  file1 = pathname + inputfile
  file2 = pathname + outputfile
  
  handle1 = FREEFILE: OPEN file1 FOR INPUT AS #handle1
  handle2 = FREEFILE: OPEN file2 FOR APPEND AS #handle2
  
  PRINT# #handle2, ""
  PRINT# #handle2, "'*** Begin of " + inputfile + " ***"
  PRINT# #handle2, ""
  
  DO
    LINE INPUT #handle1, fline
    PRINT# #handle2, fline
  LOOP WHILE EOF(handle1) <> -1

  PRINT# #handle2, ""
  PRINT# #handle2, "'*** End of " + inputfile + " ***"
  PRINT# #handle2, ""

  CLOSE #handle1
  CLOSE #handle2

END SUB