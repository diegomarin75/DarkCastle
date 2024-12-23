'Dark Castle v1.0

'Compiling options:
'"<$fbc>" "<$file>" gamlib.bas autlib.bas conlib.bas isolib.bas maplib.bas pcxlib.bas texlib.bas matlib.bas menlib.bas

#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "maplib.bi"
#INCLUDE "texlib.bi"
#INCLUDE "autlib.bi"
#INCLUDE "menlib.bi"
#INCLUDE "gamlib.bi"
#INCLUDE "vbcompat.bi"
#INCLUDE "fbgfx.bi"

'Constants
CONST MENUIMAGE = "silentmoon640x480.pcx"
CONST MAXENTRY  = 10
  
'Variables
DIM AS INTEGER mcode
DIM AS INTEGER scan
DIM AS INTEGER finish

'Entry point maps
REDIM SHARED AS STRING * 15 epmap(MAXENTRY)

'Functions
DECLARE SUB PrintIntro
DECLARE SUB PrintCredits
DECLARE SUB ScanEntryPoints
DECLARE FUNCTION ConfirmExit AS INTEGER

  'System initialization
  InitFonts()
  InitPictograms()
  InitPcxFonts()
  KillTexMemory()
  LoadObjectList()
  LoadEntityTypes()
  ActionTable()
  _LinkLoadFile()
  MenuInitNodes()
  ScanEntryPoints()
  
  'Screen mode
  SCREEN 18,16,3,FB.GFX_FULLSCREEN
  SETMOUSE ,,0
  SLEEP 500
  
  'Intro screen
  PrintIntro()

  'System loop
  finish = 0
  mcode = MENU_NEW_GAME
  scan = 1
  DO
    
    'System menu
    mcode = MenuCall(mcode,MENUIMAGE)
    
    'Switch on menu code
    SELECT CASE mcode
    
      'Map editor
      CASE MENU_GAME_EDITOR: 
        MapEditor()
        ScanEntryPoints()
      
      'Credits
      CASE MENU_CREDITS: 
        PrintCredits()
    
      'Escape
      CASE -1: 
        finish = ConfirmExit()
        mcode = MENU_EXIT
      
      'Exit
      CASE MENU_EXIT: 
        finish = ConfirmExit()
        
      'Run maps
      CASE ELSE
        IF mcode >= 1000 THEN MapExecute(epmap(mcode-1000))
    
    END SELECT 
  
  LOOP WHILE finish = 0
  
'Credits
SUB PrintIntro
  
  'Print intro screen
  SCREENSET 1,0
  SetPCXFont(2)
  PCXFSetColor(MENU_COLOR)
  PCXFCentered(1)
  MenuBackground(MENUIMAGE)
  PCXFPrintStrVS(XCol(1),XRow(06),"This game was developed as an attempt of creating  ")
  PCXFPrintStrVS(XCol(1),XRow(07),"a fragment of an internal timeless universe.       ")
  PCXFPrintStrVS(XCol(1),XRow(09),"I hope that creating that reality this way it      ")
  PCXFPrintStrVS(XCol(1),XRow(10),"generates a replica of itself somewhere else, in   ")
  PCXFPrintStrVS(XCol(1),XRow(11),"another consciousness level.                       ")
  PCXFPrintStrVS(XCol(1),XRow(13),"This universe might be real in another dimension...")
  
  'Fade in and wait
  MenuFadeScreen(1,0,30,1)
  GetKeyb(1)
  
  'Fade out and wait
  MenuFadeScreen(1,0,30,0)
  SLEEP 500

END SUB

'Credits
SUB PrintCredits
  
  'Print credits
  SCREENSET 1, 0
  PCXFSetColor(MENU_COLOR)
  PCXFCentered(1)
  MenuBackground(MENUIMAGE)
  SetPCXFont(1)
  PCXFPrintStrVS(XCol(0),XRow(01),"Credits")
  SetPCXFont(2)
  PCXFPrintStrVS(XCol(1),XRow(06),"This game has been made entirely with  ")
  PCXFPrintStrVS(XCol(1),XRow(07),"FreeBASIC. It was started in April 2007")
  PCXFPrintStrVS(XCol(1),XRow(08),"but the idea came two years earlier.   ")
  PCXFPrintStrVS(XCol(1),XRow(10),"Texture resources used come from Doom, ")
  PCXFPrintStrVS(XCol(1),XRow(11),"Doom II, Heretic and Hexen. Background ")
  PCXFPrintStrVS(XCol(1),XRow(12),"music was mainly taken from Bluemars   ")
  PCXFPrintStrVS(XCol(1),XRow(13),"and Drone Zone internet radio stations.")
  SCREENCOPY 1,0
  SCREENSET 0,0
  GetKeyb(1)

END SUB

'Confirm exit
FUNCTION ConfirmExit AS INTEGER
  
  'Variables
  DIM AS INTEGER code
  
  'Print credits
  SCREENSET 1, 0
  MenuBackground(MENUIMAGE)
  PCXFSetColor(MENU_COLOR)
  SetPCXFont(1)
  PCXFCentered(1)
  PCXFPrintStrVS(XCol(0),XRow(03),"Exit game?")
  SetPCXFont(2)
  PCXFCentered(0)
  PCXFPrintStrVS(XCol(17),XRow(8),"- Press (y) to exit     ")
  PCXFPrintStrVS(XCol(17),XRow(9),"- Press (n) to continue ")
  SCREENCOPY 1,0
  SCREENSET 0,0
  
  'Return result
  code = GetKeyb(1)
  IF code = ASC("y") OR code = ASC("Y") THEN
    ConfirmExit = 1
  ELSE
    ConfirmExit = 0
  ENDIF

END FUNCTION

'Scan entry maps
SUB ScanEntryPoints
  
  'Variables
  DIM AS INTEGER i
  DIM AS STRING filename
  DIM AS INTEGER pipefile
  DIM AS roomattr rattr
  DIM AS INTEGER numepm = 0
  REDIM AS STRING * 23 epnam(MAXENTRY)
  
  'Find entry maps
  i = 0
  numepm = 0
  pipefile = FREEFILE
  OPEN PIPE "dir maps\*.map /b /on" FOR INPUT AS #pipefile 
  WHILE NOT EOF(pipefile) AND i < MAXENTRY
    LINE INPUT #pipefile, filename
    IF filename <> "" THEN
      MapReadAttr(filename,rattr)
      IF rattr.epmap = 1 THEN
        epmap(i) = filename
        epnam(i) = rattr.mname
        i = i + 1
      ENDIF
    ENDIF
  WEND
  CLOSE #pipefile
  numepm = i
  
  'Disable new game if no entry maps found
  IF numepm = 0 THEN
    MenuItemActive(MENU_NEW_GAME,0)

  'Set entry maps in menu
  ELSE
    MenuItemActive(MENU_NEW_GAME,1)
    FOR i=0 TO numepm-1
      IF i < numepm-1 THEN
        SetNodeItem(NODE_NEW_GAME,i,epnam(i),1000+i,NODE_NULL,0)
      ELSE
        SetNodeItem(NODE_NEW_GAME,i,epnam(i),1000+i,NODE_NULL,1)
      ENDIF
    NEXT i
  ENDIF
  
END SUB