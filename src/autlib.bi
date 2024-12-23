
CONST MAXSUBS  = 10  'Maximum number of functions in automatic refresh table
CONST INTERVAL = 50  'Internal waiting interval (msecs)

'Automatic refresh table
TYPE automrefresh
  subptr AS SUB
  period AS DOUBLE
  msecs AS DOUBLE
  used AS INTEGER
END TYPE

'Functions
DECLARE SUB AutomSubHook(index AS INTEGER, subptr AS SUB, period AS DOUBLE)
DECLARE SUB AutomSubClear(index AS INTEGER)
DECLARE SUB AutomSubClearAll
DECLARE SUB AutomSubExit()
DECLARE SUB AutomSubExecute(waitmode AS INTEGER)
