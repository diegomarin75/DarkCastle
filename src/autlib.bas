#INCLUDE "autlib.bi"
#INCLUDE "conlib.bi"

'Common variables
DIM SHARED exitflag AS INTEGER = 0
REDIM SHARED automsub(MAXSUBS) AS AutomRefresh

'Function hook
SUB AutomSubHook(index AS INTEGER, subptr AS SUB, period AS DOUBLE)
  automsub(index).subptr = subptr
  automsub(index).period = period
  automsub(index).msecs  = 0
  automsub(index).used   = 1
END SUB

'Function clear
SUB AutomSubClear(index AS INTEGER)
  automsub(index).subptr = 0
  automsub(index).period = 0
  automsub(index).msecs  = 0
  automsub(index).used   = 0
END SUB

'Function clear all
SUB AutomSubClearAll
  DIM AS INTEGER i
  FOR i=0 TO MAXSUBS-1
    AutomSubClear(i)
  NEXT i
END SUB

'Set exit flag
SUB AutomSubExit()
  exitflag = 1
END SUB

'Automatic function refresh
SUB AutomSubExecute(waitmode AS INTEGER)
  
  'Variables
  DIM AS INTEGER i      'Counter
  DIM AS INTEGER finish 'Finish loop
  DIM AS STRING keyb    'Keyboard key
  
  'Refresh loop
  finish = 0
  DO
  
    'Execute automatic routines
    FOR i=0 TO MAXSUBS-1
      IF automsub(i).used = 1 THEN
        IF TIMER() - automsub(i).msecs > automsub(i).period THEN
          (automsub(i).subptr)()
          automsub(i).msecs = TIMER()
          IF exitflag = 1 THEN EXIT FOR
        ENDIF
      ENDIF
    NEXT i
    
    'Exit if exit flag is set
    IF exitflag = 1 THEN
      exitflag = 0
      EXIT DO
    ENDIF
    
    'Exit if not in wait mode
    IF waitmode = 0 THEN EXIT DO
    
    'Wait interval
    keyb = INKEY$
    IF keyb = "" THEN
      SLEEP(INTERVAL)
    ELSE
      SetKeyb(keyb)
      finish = 1
    ENDIF
  
  LOOP WHILE finish = 0
  
END SUB