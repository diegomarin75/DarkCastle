
'*** Begin of dcastle.bas ***

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

'*** End of dcastle.bas ***


'*** Begin of menlib.bi ***

'Constants
CONST MENUFONTAX   = 30 'Menu font size
CONST MENUFONTAY   = 47 'Menu font size
CONST MAXNODE      = 10 'Maximum menu nodes
CONST MAXITEM      = 10 'Maximum menu nodes

'Menu node codes
CONST NODE_NULL          = -1
CONST NODE_MAIN          =  0
CONST NODE_NEW_GAME      =  1
CONST NODE_LOAD_GAME     =  2
CONST NODE_CONFIGURATION =  3

'Menu item codes (can't be zero)
CONST MENU_NEW_GAME      = 1
CONST MENU_SAVE_GAME     = 2
CONST MENU_LOAD_GAME     = 3
CONST MENU_CONFIGURATION = 4
CONST MENU_GAME_EDITOR   = 5
CONST MENU_CREDITS       = 6
CONST MENU_EXIT          = 7
CONST MENU_GRAPHICS      = 8
CONST MENU_SOUND         = 9

'Color menu
CONST MENU_COLOR = RGB(0,0,180)
CONST MENU_DISAB = RGB(0,0,050)

'Menu item
TYPE mitem
  mname AS STRING
  code AS INTEGER
  subnode AS INTEGER
  lastitem AS INTEGER
  enable AS INTEGER
END TYPE

'Functions
DECLARE FUNCTION MenuCall(scode AS INTEGER,bfile AS STRING) AS INTEGER
DECLARE SUB MenuInitNodes
DECLARE SUB MenuPrintNode(node AS INTEGER)
DECLARE SUB MenuItemMark(node AS INTEGER, item AS INTEGER)
DECLARE SUB MenuFadeScreen(img AS INTEGER, disp AS INTEGER, times AS INTEGER,inout AS INTEGER)
DECLARE SUB SetNodeItem(node AS INTEGER, item AS INTEGER, mname AS STRING, code AS INTEGER, subnode AS INTEGER, lastitem AS INTEGER)
DECLARE SUB SetNodeTitle(node AS INTEGER, title AS STRING)
DECLARE SUB MenuItemActive(mcode AS INTEGER, enable AS INTEGER)
DECLARE SUB MenuBackground(bfile AS STRING)
DECLARE FUNCTION MenuSearch(commd AS STRING,value AS INTEGER, BYREF node AS INTEGER, BYREF optio AS INTEGER) AS INTEGER

'*** End of menlib.bi ***


'*** Begin of menlib.bas ***

#INCLUDE "menlib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "pcxlib.bi"

'Menu definition
REDIM SHARED AS STRING ntitle(MAXNODE)
REDIM SHARED AS mitem menu(MAXNODE,MAXITEM)
REDIM SHARED AS INTEGER menuscrn(AX0*AY0)
DIM SHARED AS STRING background

'Initialize menu entries
SUB MenuInitNodes
  
  'Set node macro
  #MACRO _SetNode(node,item,mname0,code0,subnode0,lastitem0)
    menu(node,item).mname    = mname0
    menu(node,item).code     = code0
    menu(node,item).subnode  = subnode0
    menu(node,item).lastitem = lastitem0
    menu(node,item).enable   = 1
  #ENDMACRO

  'Set menu nodes
  _SetNode(NODE_MAIN         ,0,"New game"        ,MENU_NEW_GAME     ,NODE_NEW_GAME     ,0) 
  _SetNode(NODE_MAIN         ,1,"Save game"       ,MENU_SAVE_GAME    ,NODE_NULL         ,0) 
  _SetNode(NODE_MAIN         ,2,"Load game"       ,MENU_LOAD_GAME    ,NODE_NULL         ,0) 
  _SetNode(NODE_MAIN         ,3,"Configuration"   ,MENU_CONFIGURATION,NODE_CONFIGURATION,0) 
  _SetNode(NODE_MAIN         ,4,"Game editor"     ,MENU_GAME_EDITOR  ,NODE_NULL         ,0) 
  _SetNode(NODE_MAIN         ,5,"Credits"         ,MENU_CREDITS      ,NODE_NULL         ,0) 
  _SetNode(NODE_MAIN         ,6,"Exit"            ,MENU_EXIT         ,NODE_NULL         ,1) 
  _SetNode(NODE_CONFIGURATION,0,"Graphics"        ,MENU_GRAPHICS     ,NODE_NULL         ,0) 
  _SetNode(NODE_CONFIGURATION,1,"Sound"           ,MENU_SOUND        ,NODE_NULL         ,1)  
  
  'Node titles
  ntitle(NODE_MAIN)          = "Dark Castle v1.0"
  ntitle(NODE_NEW_GAME)      = "Choose entry point"
  ntitle(NODE_CONFIGURATION) = "Configuration"
  
END SUB

'Set menu node
SUB SetNodeItem(node AS INTEGER, item AS INTEGER, mname AS STRING, _
                code AS INTEGER, subnode AS INTEGER, lastitem AS INTEGER )
  menu(node,item).mname    = mname 
  menu(node,item).code     = code 
  menu(node,item).subnode  = subnode
  menu(node,item).lastitem = lastitem
  menu(node,item).enable   = 1
END SUB

'Set menu node title
SUB SetNodeTitle(node AS INTEGER, title AS STRING)
  ntitle(node) = title
END SUB

'Enable / Disable menu items
SUB MenuItemActive(mcode AS INTEGER, enable AS INTEGER)
  
  'Variables
  DIM AS INTEGER node, item
  
  'Search menu for item code
  IF MenuSearch("ITEM_CODE",mcode,node,item) = 1 THEN
    menu(node,item).enable = enable
  ENDIF

END SUB

'Set menu background
SUB MenuBackground(bfile AS STRING)

  'Read background image
  IF background <> bfile THEN
    CLS 0
    SetBackground(bfile)
    GET (0,0)-(AX0-1,AY0-1), menuscrn
    background = bfile
  ENDIF
    
  'Dump background image
  PUT (0,0), menuscrn, PSET

END SUB

'Call menu 
FUNCTION MenuCall(scode AS INTEGER,bfile AS STRING) AS INTEGER

  'Variables
  DIM AS INTEGER i,finish
  DIM AS INTEGER code,lastitem
  DIM AS INTEGER optio,node
  DIM AS INTEGER roptio,rnode
  DIM AS INTEGER aoptio,anode
  DIM AS INTEGER keyb
    
  'Get starting node and option
  MenuSearch("ITEM_CODE",scode,node,optio)
  
  'Menu loop
  finish = 0
  aoptio = -1
  anode = -1
  DO
      
    'Search last item on node
    FOR i=0 TO MAXITEM-1
      IF menu(node,i).lastitem = 1 THEN
        lastitem = i
        EXIT FOR
      ENDIF
    NEXT i
    
    'Redraw screen
    IF finish <> 1 AND ( aoptio <> optio OR anode <> node ) THEN
      SCREENSET 1, 0
      MenuBackground(bfile)
      MenuItemMark(node,optio)
      MenuPrintNode(node)
      SCREENCOPY 1,0
      SCREENSET 0,0
    ENDIF
      
    'Save previous option & node
    aoptio = optio
    anode = node
        
    'Read key
    keyb = GetKeyb(1)
        
    'Switch on key
    SELECT CASE keyb
      
      CASE 27 'ESC -> exit
        IF MenuSearch("SUBNODE",node,rnode,roptio) = 1 THEN
          node = rnode
          optio = roptio
        ELSE
          MenuCall = -1
          finish = 1
        ENDIF
            
      CASE 13 'ENTER -> Select item
        IF menu(node,optio).enable = 1 THEN
          IF menu(node,optio).subnode <> -1 THEN
            node = menu(node,optio).subnode
            optio = 0
          ELSE
            MenuCall = menu(node,optio).code
            finish = 1
          ENDIF
        ENDIF
            
      CASE -72 'Up -> Move item    
        IF optio > 0 THEN
          optio = optio - 1
        ELSE
          optio = lastitem
        ENDIF
        
      CASE -80 'Down -> Move item    
        IF optio < lastitem THEN
          optio = optio + 1
        ELSE
          optio = 0
        ENDIF
      
    END SELECT
        
  LOOP WHILE finish <> 1

END FUNCTION

'Search item function
FUNCTION MenuSearch(commd AS STRING,value AS INTEGER, BYREF node AS INTEGER, _
                    BYREF optio AS INTEGER) AS INTEGER

  'Variables
  DIM AS INTEGER i,j
  
  'Get starting node and option
  node = -1
  optio = -1
  FOR i=0 TO MAXNODE-1
    FOR j=0 TO MAXITEM-1
      IF ( commd = "ITEM_CODE" AND value = menu(i,j).code    ) _ 
      OR ( commd = "SUBNODE"   AND value = menu(i,j).subnode ) THEN
        IF menu(i,j).mname <> "" AND menu(i,j).code <> 0 THEN
          node = i
          optio = j
          EXIT FOR
        ENDIF
      ENDIF
      IF menu(i,j).lastitem = 1 THEN EXIT FOR
    NEXT j
    IF node <> -1 AND optio <> -1 THEN EXIT FOR
  NEXT i
  
  'Return code
  IF node <> -1 AND optio <> -1 THEN
    MenuSearch = 1
  ELSE
    MenuSearch = 0
  ENDIF

END FUNCTION
  
' Print menu node contents
SUB MenuPrintNode(node AS INTEGER)

  'Variables
  DIM AS INTEGER i
    
  'Font and attributes
  SetPCXFont(1)
  PCXFCentered(1)
  
  'Print Node title
  PCXFSetColor(MENU_COLOR)
  PCXFPrintStrVS(XCol(0),XRow(1),ntitle(node))
    
  'Print node contents
  FOR i=0 To MAXITEM-1
    SELECT CASE menu(node,i).enable
      CASE 0: PCXFSetColor(MENU_DISAB)
      CASE 1: PCXFSetColor(MENU_COLOR)
    END SELECT
    PCXFPrintStrVS(XCol(0),XRow(i+3),menu(node,i).mname)
    IF menu(node,i).lastitem = 1 THEN EXIT FOR
  NEXT i
    
END SUB

'Mark menu item
SUB MenuItemMark(node AS INTEGER, item AS INTEGER)
  
  'Set font and sttributes
  SetPCXFont(1)
  PCXFCentered(1)
  PCXFSetColor(MENU_COLOR)
  
  'Print item marks
  PCXFPrintStrVS(XCol(0),XRow(item+3),">>" + menu(node,item).mname + "<<")

END SUB

'Menu fade efect on screen 
SUB MenuFadeScreen(img AS INTEGER, disp AS INTEGER, times AS INTEGER, inout AS INTEGER)

  'Variables
  DIM AS INTEGER i, page, alpha
  DIM AS DOUBLE factor, smooth
  REDIM AS INTEGER scrn(AX0*AY0)
  
  'Initialization
  SCREENSET img, disp
  GET (0,0)-(AX0-1,AY0-1), scrn
    
  'Fade loop
  SELECT CASE inout
    
    'Fade in
    CASE 1:
      smooth = 0.1
      SCREENSET disp, disp
      CLS 0
      SCREENSET img, disp
      FOR i=0 TO times-1
        factor = (255 * LOG(1+i*smooth)) / LOG(1+(times-1)*smooth)
        CLS 0
        IF i > 0 THEN PUT (0,0), scrn, ALPHA, INT(factor)
        SCREENSYNC
        SCREENCOPY img, disp
        SLEEP 50
      NEXT i
  
    'Fade out
    CASE 0:
      smooth = 0.3
      SCREENSET img, disp
      FOR i=times-1 TO 0 STEP -1
        factor = (255 * LOG(1+i*smooth)) / LOG(1+(times-1)*smooth)
        CLS 0
        IF i > 0 THEN PUT (0,0), scrn, ALPHA, INT(factor)
        SCREENSYNC
        SCREENCOPY img, disp
        SLEEP 50
      NEXT i
  
  END SELECT 
  
 'Restore image planes
  SCREENSET img, disp

END SUB

'*** End of menlib.bas ***


'*** Begin of gamlib.bi ***

'Window sizes
CONST WINDOWPX = 141
CONST WINDOWPY = 11
CONST WINBORDR = 14  
CONST WINPIXEL = (AX0 - WINBORDR - WINDOWPX + 1) * (AY0 - WINBORDR - WINDOWPY + 1)
CONST WINACTLS = 100
CONST WINACTLB = 316

'Text colors
CONST COL_TEXT = RGB(255,255,255)
CONST COL_DSEL = RGB(210,0,0)
CONST COL_NSEL = RGB(80,80,80)
CONST COL_BACK = RGB(0,0,150)
CONST COL_TXRE = RGB(150,150,150)
CONST COL_DELE = RGB(255,0,0)

'Menu bars
CONST MENMAIN1 = " F1:Help  F2:File>  F3:Mode  F4:Textures  F5:Objects  F6:E.Types  F12:Exit      "
CONST MENMAIN2 = " F2:New  F3:Open  F4:Save                                                       "
CONST MENUTXB1 = " F2:Folder  F3:Category  F4:Import  ESC:Exit                                    "
CONST MENUTXB2 = " F3:Change Category  ENTER:Select  ESC:Exit                                     "
CONST MENUTXB3 = " F2:Scale  W:Wall  G:Ground  O:Object  E:Entity  ESC:Exit                       "
CONST MENUOPEN = " ENTER:Select  Ctrl-Del:Delete  Pag.Up/Down:Scale  ESC:Exit                     "
CONST MENUOBJ1 = " F2:Rename  F3:Delete  F4:Sort  ESC:Exit                                        "
CONST MENUOBJ2 = " ENTER:Select  ESC:Exit                                                         "
CONST MENUETY1 = " ESC:Exit                                                                       "
CONST MENUETY2 = " ENTER:Select  ESC:Exit                                                         "
CONST MENULINK = " ENTER:Set link point location  ESC:Exit                                        "
CONST MENUCHAN = " File was modified. Loose changes to this file (Y/N)?                           "
CONST MENUDFIL = " Delete this file (Y/N)?                                                        "
CONST MENUCONT = " Press any key to continue...                                                   "
CONST MENUWARN = " Save file before setting linking points!                                       "
CONST SPACEBAR = "                                                                                "

'Other
CONST HELPAGES = 3

'Edition modes
CONST ED_MODES = 8
CONST EDIT_MAP = 1
CONST EDIT_CEL = 2
CONST EDIT_ATR = 3
CONST EDIT_OVL = 4
CONST EDIT_LIG = 5
CONST EDIT_LNK = 6
CONST EDIT_EVT = 7
CONST EDIT_NAV = 8

'Cursor modes
CONST CURSOR_OVLIS = 1
CONST CURSOR_OVTEX = 2
CONST CURSOR_EVCEL = 1
CONST CURSOR_EVLIS = 2
CONST CURSOR_EVACT = 3

'Menu bar modes
CONST MBAR = 1
CONST EBAR = 2
CONST WBAR = 3
CONST SBAR = 4
CONST IBAR = 5

'Initial map name
CONST DEFAULTMAP = "newfile.map"

'Subroutine declarations
DECLARE FUNCTION TextureBrowser(texdir AS INTEGER) AS STRING
DECLARE FUNCTION ObjectBrowser(browser AS INTEGER) AS STRING
DECLARE FUNCTION ETypeBrowser(browser AS INTEGER) AS STRING
DECLARE FUNCTION Min(a AS INTEGER, b AS INTEGER) AS INTEGER
DECLARE FUNCTION Max(a AS INTEGER, b AS INTEGER) AS INTEGER
DECLARE FUNCTION FileSave(rattr AS roomattr, cmap() AS mapcell, light() AS maplight, ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio, deffile AS STRING, BYREF cancel AS INTEGER ) AS STRING
DECLARE FUNCTION FileLoad(rattr AS roomattr, cmap() AS mapcell, light() AS maplight, ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio, defile AS STRING, gotomap AS STRING, BYREF cancel AS INTEGER ) AS STRING
DECLARE FUNCTION ChooseLinkPoint(BYREF linkmap AS STRING, BYREF linkcx AS INTEGER, BYREF linkcy AS INTEGER) AS INTEGER
DECLARE FUNCTION InfoCheckbox(value AS INTEGER) AS STRING
DECLARE SUB PrintDispMode
DECLARE SUB HelpScreen(page AS INTEGER)
DECLARE SUB DrawTextureBrowser(TexDir AS INTEGER, Texture AS STRING,cname AS STRING,x AS INTEGER, y AS INTEGER, mode AS INTEGER)
DECLARE SUB DrawTextureInfoCell(TexDir AS INTEGER, Texture AS STRING, x AS INTEGER, y AS INTEGER)
DECLARE SUB DrawTextureInfoAttr(TexDir AS INTEGER, Texture AS STRING, seltexture AS INTEGER, ofx AS SHORT, ofy AS SHORT, cont AS SHORT, pass AS SHORT, x AS INTEGER, y AS INTEGER)
DECLARE SUB DrawTextureInfoOvl(ovtex AS mapovtex, x AS INTEGER, y AS INTEGER)
DECLARE SUB DrawObjectBrowser(Object AS objdef, x AS INTEGER, y AS INTEGER, mode AS INTEGER)
DECLARE SUB PrintMenuBar(mode AS INTEGER, MenuText AS STRING, cur AS INTEGER)
DECLARE SUB PrintCatMenu(cat AS INTEGER, txd AS INTEGER, numtex AS INTEGER, mode AS INTEGER)
DECLARE SUB PrintSingleCategory(x AS INTEGER, y AS INTEGER, cat AS INTEGER,catsel AS INTEGER, txd AS INTEGER, numtex AS INTEGER, mode AS INTEGER)
DECLARE SUB PrintInfoBox(mode AS INTEGER, x AS INTEGER, y AS INTEGER, mapname AS STRING, rattr AS roomattr, cmap() AS mapcell, light() AS maplight, ovtex() as mapovtex, event() AS mapevent, actio() AS mapactio, lightsel AS INTEGER, seltexture AS INTEGER, selovtex AS INTEGER, selovrows AS INTEGER, selevent AS INTEGER, selevrows AS INTEGER, selactio AS INTEGER, selacrows AS INTEGER,cursmode1 AS INTEGER,cursmode2 AS INTEGER,changes AS INTEGER,actwindow AS INTEGER, actwlines AS INTEGER, actwtop AS INTEGER, scrn AS INTEGER)
DECLARE SUB DrawETypeBrowser(etype AS etypedef, etypestate() AS etypestdef, stindex AS INTEGER)
DECLARE SUB DrawETypeStateFrame(etypestate AS etypestdef, x AS INTEGER, y AS INTEGER, frame AS INTEGER, heading AS STRING)
DECLARE SUB DrawOccupiedMemory(page AS INTEGER)
DECLARE SUB MapRefreshDaemon
DECLARE SUB MapWindow(commd AS STRING, parm AS INTEGER)
DECLARE SUB MapEditor

'*** End of gamlib.bi ***


'*** Begin of gamlib.bas ***

#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "maplib.bi"
#INCLUDE "texlib.bi"
#INCLUDE "autlib.bi"
#INCLUDE "menlib.bi"
#INCLUDE "gamlib.bi"
#INCLUDE "vbcompat.bi"

'Room variables
DIM SHARED AS roomattr rattr                      'Room attributes
REDIM SHARED AS mapcell cmap(MAXCELLX, MAXCELLY)  'Ground cell definition
REDIM SHARED AS mapcell cpmap(MAXCELLX, MAXCELLY) 'Ground cell definition (clipboard)
REDIM SHARED AS maplight light(MAXLIGHT)          'Light sources
REDIM SHARED AS mapovtex ovtex(MAXOVTEX)          'Overlay textures
REDIM SHARED AS mapovtex cpotx(MAXOVTEX)          'Overlay textures (clipboard)
REDIM SHARED AS mapevent event(MAXEVENT)          'Map events
REDIM SHARED AS mapevent cpevt(MAXEVENT)          'Map events (clipboard)
REDIM SHARED AS mapactio actio(MAXACTIO)          'Map actions
REDIM SHARED AS mapactio cpact(MAXACTIO)          'Map actions (clipboard)
DIM SHARED AS UINTEGER PTR ipt                    'Texture buffer
DIM SHARED AS mapactio iactio                     'Input action

'Object variables
EXTERN AS INTEGER numobj
EXTERN AS objdef obj()
EXTERN AS INTEGER numetype
EXTERN AS etypedef etype()
EXTERN AS etypestdef etypestate()

'Arrays
REDIM SHARED AS INTEGER mapscr1(WINPIXEL)
REDIM SHARED AS INTEGER mapscr2(WINPIXEL)

'Force infobox refresh flag
DIM SHARED AS INTEGER forceinfo = 0

'Map editor
SUB MapEditor

  'Variables
  DIM AS INTEGER finish, Refresh, code, ecode, found
  DIM AS INTEGER i, j, x, y, ax, ay, arx, ary, cpax, cpay
  DIM AS INTEGER mode, edit, aedit, lightsel, entidx, entypenr
  DIM AS INTEGER rfcount, changes, cancel, fpsmode
  DIM AS INTEGER Counter, PrvCount, checklight, seltexture
  DIM AS INTEGER selovtex, selovrows, cpovrows
  DIM AS INTEGER selevent, selevrows, cpevrows
  DIM AS INTEGER selactio, selacrows, cpacrows
  DIM AS INTEGER cursmode1, cursmode2, ovflag, ovgroup
  DIM AS INTEGER actwindow, prvwindow, actwlines, actwtop
  DIM AS INTEGER helpscr, helppag, menubar, linkcx, linkcy, linkway
  DIM AS INTEGER drawshplanes, lmapupdate, fileupdate
  DIM AS STRING  ename, filename, seltex, selobj, seletype
  DIM AS STRING  linkmap, inpstr, actstr, actstr0, perror
  DIM AS DOUBLE  StartTime, FrameCount0, FrameCount1
  DIM AS DOUBLE  PrvFrame, FrameSec 
  DIM AS SINGLE  lightfactor
  
  'Background screen
  SCREENSET 2, 0
  SetBackground("backgrnd.pcx")
  SCREENCOPY 2,1
  SCREENSET 1,0
  
  'Map editor initialization
  WINDOWTITLE "DARK CASTLE Map Editor v1.0"
  SetFont(2)
  SetPictogramFont(1)
  FileName = DEFAULTMAP
  MapInit(rattr,cmap(),light(),ovtex(),event(),actio())
  SetDispMode(LIGHT_MODE,MODE_LDYN)
  AutomSubClearAll()
  AutomSubHook(0,@MapRefreshDaemon,0.12)
  EntRefreshKill
  entidx = EntRefreshEnqueue(16, 16, 0, etype(0))
  EntRefreshSetMapPointer(@rattr,@cmap(0,0))
  MapWindow("SET_WINDOW",1)
  _MapImgInit()
  
  'Map drawing loop
  refresh = 2
  finish = 0
  edit = EDIT_CEL
  aedit = 0
  mode = 5
  fpsmode = 0
  changes = 0
  helpscr = 0
  helppag = 1
  x = 0: y = 0: 
  arx = 0: ary = 0
  rfcount = 0
  lightsel = 99
  seltexture = 0
  selovtex = 0
  selovrows = 0
  cpovrows = 0
  selevent = 0
  selevrows = 0
  cpevrows = 0
  selactio = 0
  selacrows = 0
  cpacrows = 0
  cursmode1 = CURSOR_OVLIS
  cursmode2 = CURSOR_EVCEL
  menubar = 1
  entypenr = 0
  drawshplanes = 0
  lmapupdate = 1
  actwindow = 1
  prvwindow = 1
  actwlines = 9
  actwtop = 0
  forceinfo = 1
  DO
    
    'Edition mode change
    IF edit <> aedit OR prvwindow <> actwindow THEN
      
      'Init secondary Z-Buffer
      ISOZBufferLayer(2)
      ISOZBufferClear
      
      'Set window map window size
      IF edit = EDIT_EVT THEN
        SELECT CASE actwindow
          CASE 1: MapWindow("SET_WINDOW",2)
          CASE 2: MapWindow("SET_WINDOW",3)
        END SELECT
        SCREENCOPY 2,1
        refresh = 2
      ELSEIF aedit = EDIT_EVT THEN  
        MapWindow("SET_WINDOW",1)
        SCREENCOPY 2,1
        refresh = 2
      ENDIF
    
    ENDIF
  
    'Refresh map
    IF Refresh = 2 THEN
      MapWindow("SET_VIEW_CLEAR",0)
      RoomMap rattr, cmap(), light(), ovtex(), lmapupdate
      MapWindow("INIT_VIEW",0)
      MapWindow("STORE_WINDOW",1)
    END IF
    
    'Refresh cursor
    IF Refresh = 1 OR refresh = 2 THEN
      MapWindow("RESTORE_WINDOW",1)
      MapWindow("SET_VIEW",0)
      IF edit = EDIT_CEL OR edit = EDIT_ATR _
      OR edit = EDIT_LNK OR edit = EDIT_EVT THEN
        FOR i = Min(x,x+arx) TO Max(x,x+arx)
        FOR j = Min(y,y+ary) TO Max(y,y+ary)
          RoomCursor i, j, rattr, cmap(), light()
        NEXT j
        NEXT i
      ELSEIF edit = EDIT_LIG THEN
        LightCursor lightsel, light(), rattr
      ENDIF
      IF Edit = EDIT_LNK THEN RoomLinkPoints filename, rattr, cmap()
      IF Edit = EDIT_EVT THEN RoomEvents rattr, cmap()
      IF Edit = EDIT_CEL OR Edit = EDIT_ATR _
      OR edit = EDIT_LNK OR edit = EDIT_EVT THEN
        PrintStrBKg FCol(98), FRow(50),FORMAT(x,"00")+","+FORMAT(y,"00"), COL_TEXT,1,1
      ENDIF
      IF helpscr = 1 THEN HelpScreen helppag
      VIEW SCREEN
    END IF
    
    'Refresh info
    IF Refresh = 1 OR refresh = 2 OR refresh = 3 THEN
      PrintInfoBox edit, x, y, filename, rattr, cmap(), light(), ovtex(), event(), actio(), _
      lightsel, seltexture, selovtex, selovrows, selevent, selevrows, selactio, selacrows, _
      cursmode1, cursmode2, changes, actwindow, actwlines, actwtop, 1
      SELECT CASE Edit
        CASE EDIT_MAP: ename = "Map"
        CASE EDIT_CEL: ename = "Cells"
        CASE EDIT_ATR: ename = "Attrib."
        CASE EDIT_OVL: ename = "Overlay"
        CASE EDIT_LIG: ename = "Lights"
        CASE EDIT_LNK: ename = "Link.P."
        CASE EDIT_EVT: ename = "Events"
        CASE EDIT_NAV: ename = "Navig."
      END SELECT
      FOR i=1 TO ED_MODES
        IF edit = i THEN
          PrintPictogram PCol(52)+2,PRow(i)-5,PIC_MODE_MAP+i-1,COL_TEXT
          LINE (PCol(52)+1,PRow(i)-6)-STEP(12,16),RGB(255,0,0),B
        ELSE
          PrintPictogram PCol(52)+2,PRow(i)-5,PIC_MODE_MAP+i-1,COL_NSEL
          LINE (PCol(52)+1,PRow(i)-6)-STEP(12,16),RGB(50,50,50),B
        ENDIF
      NEXT i
      PrintStrBkg FCol(25), FRow(0), STRING(80," "), COL_TEXT, 1, 2
      PrintStrBkg FCol(25), FRow(0), "Mode:" + ename, COL_TEXT, 1, 2
      PrintDispMode
      IF menubar = 1 THEN 
        PrintMenuBar MBAR,MENMAIN1, 1
      ELSEIF menubar = 2 THEN 
        PrintMenuBar MBAR,MENMAIN2, 1
      ENDIF
      DrawOccupiedMemory 1
    END IF
    
    'Print frames per second
    IF fpsmode = 1 THEN
      Counter = (Timer - StartTime) MOD 2
      IF Counter = 0 THEN
        FrameCount0 = FrameCount0 + 1: FrameSec = (FrameCount1 + PrvFrame) / 2
      ELSE
        FrameCount1 = FrameCount1 + 1: FrameSec = (FrameCount0 + PrvFrame) / 2
      ENDIF
      IF PrvCount <> Counter THEN
        IF Counter = 0 THEN: PrvFrame = FrameCount0: FrameCount0 = 0: ENDIF
        IF Counter = 1 THEN: PrvFrame = FrameCount1: FrameCount1 = 0: ENDIF
        PrvCount = Counter
      ENDIF
      PrintStrBkg FCol(98)-5, FRow(1)+5, "FPS:" + Format(FrameSec,"00"), COL_TEXT, 1, 1
    ENDIF
  
    'Get map image for refresh daemon
    IF Refresh = 1 OR refresh = 2 THEN
      MapWindow("STORE_WINDOW",2)
    END IF
    
    'Object refresh map mode   
    ObjRefreshMode 1
  
    'Display entities only in navigation mode
    IF edit = EDIT_NAV THEN 
      SetDispMode(ENTS_MODE,MODE_ON)
    ELSE
      SetDispMode(ENTS_MODE,MODE_OFF)
    ENDIF
    
    'Update screen
    IF refresh > 0 THEN
      refresh=0
      IF GetDispMode(OBJS_MODE) <> MODE_ON THEN SCREENCOPY 1,0
    ENDIF
  
    'Get keyboard code & refresh objects
    code = GetKeyb(0)
    IF fpsmode = 0 AND code = 0 THEN 
      AutomSubExecute(1)
    ELSEIF fpsmode = 1 THEN
      AutomSubExecute(0)
    ENDIF
    
    'Save previous edition mode and active window mode
    aedit = edit
    prvwindow = actwindow
    
    'Generic commands for all edition modes
    SELECT CASE code
      
      CASE 6 'Ctrl-F -> Toogle FPS mode
        IF fpsmode = 1 THEN
          fpsmode = 0
        ELSE
          fpsmode = 1
        ENDIF
        StartTime = Timer
        FrameCount0 = 0
        FrameCount1 = 0
        PrvFrame = 0
        PrvCount = -1
        refresh = 2
      
      CASE 27 'ESC -> Menu exit
        IF menubar = 2 THEN
          menubar = 1
          refresh = 1
        ENDIF
      
      CASE -59 'F1 -> Help on / off
        IF menubar = 1 THEN
          IF helpscr = 0 THEN
          	helpscr = 1
          ELSE
            helpscr = 0
          END IF
          refresh = 2
        ENDIF
  
      CASE -94 'Ctrl-F1 -> Next help screen
        IF menubar = 1 THEN 
          IF helpscr = 1 THEN
        	  helppag = helppag + 1
            IF helppag > HELPAGES THEN helppag = 1
            refresh = 2
          ENDIF
        ENDIF
  
      CASE -60 'F2 -> Enter file menu / New file
        IF menubar = 1 THEN
          menubar = 2
          refresh = 1
        ELSEIF menubar = 2 THEN
          fileupdate = 0
          SCREENSET 0,0
          IF changes = 1 THEN
            PrintMenuBar MBAR,MENUCHAN,0
            code = GetKeyb(1)
            IF code = ASC("Y") OR code = ASC("y") OR code = 13 THEN
              changes = 0
              refresh = 2
              fileupdate = 1
            ELSE
              refresh = 3
            ENDIF
          ELSE
            refresh = 2
            fileupdate = 1
          ENDIF
          SCREENSET 1,0
          IF fileupdate = 1 THEN
            MapInit rattr,cmap(),light(),ovtex(),event(),actio()
            EntRefreshKill
            entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize, _
            cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(0))
            lmapupdate = 1
          ENDIF
        ENDIF
  
      CASE -61 'F3 -> Change edit mode / Load file
        IF menubar = 1 THEN
          edit = edit + 1
          IF edit > ED_MODES THEN
          	Edit = 1
          END IF
          refresh = 1
        ELSEIF menubar = 2 THEN
          fileupdate = 0
          SCREENSET 0,0
          IF changes = 1 THEN
            PrintMenuBar MBAR,MENUCHAN,0
            code = GetKeyb(1)
            IF code = ASC("Y") OR code = ASC("y") OR code = 13 THEN
              changes = 0
              refresh = 2
              fileupdate = 1
            ELSE
              refresh = 3
            ENDIF
          ELSE
            refresh = 2
            fileupdate = 1
          ENDIF
          IF fileupdate = 1 THEN
            filename = FileLoad(rattr,cmap(),light(),ovtex(),event(),actio(),filename,"",cancel)
            lmapupdate = IIF(rattr.lmapStore = 1, 0, 1)
            EntRefreshKill
            entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize, _
            cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(0))
          ENDIF
          IF GetDispMode(LIGHT_MODE) = MODE_LDYN AND lmapupdate = 0 THEN SetDispMode(LIGHT_MODE, MODE_LMAP)
          SCREENSET 1,0
        ENDIF
     
      CASE -62 'F4 -> Texture browser / Save file
        IF menubar = 1 THEN
          SCREENSET 0,0
          seltex = TextureBrowser(0) 
          SCREENSET 1,0
          refresh = 1
        ELSEIF menubar = 2 THEN
          SCREENSET 0,0
          filename = FileSave(rattr,cmap(),light(),ovtex(),event(),actio(),filename,cancel)
          IF cancel = 0 THEN: _LinkSyncFile(): _MapImgDelete(filename): changes = 0: ENDIF
          refresh = 3
          SCREENSET 1,0
        ENDIF
     
      CASE -63 'F5 -> Object browser
        IF menubar = 1 THEN
          SCREENSET 0,0
          selobj = ObjectBrowser(1) 
          SCREENSET 1,0
          refresh = 2
        ENDIF
      
      CASE -64 'F6 -> Entity type browser
        IF menubar = 1 THEN
          SCREENSET 0,0
          seletype = ETypeBrowser(1) 
          SCREENSET 1,0
          refresh = 2
        ENDIF
      
      CASE 8 'Return -> Calculate light map
        IF lmapupdate = 1 THEN
          SCREENSET 0,0
          PrintMenuBar MBAR,SPACEBAR, 0
          ISOLMapUnifyAllPlanes
          ISOLMapCalculate
          ISOLMapDifuse(rattr.lmapdifuse)
          SCREENSET 1,0
          lmapupdate = 0
          SetDispMode(LIGHT_MODE,MODE_LMAP)
          refresh = 2
        ENDIF
     
      CASE 20 'Ctrl-T -> Change texture mode
        SetDispMode(TEXT_MODE,MODE_CHANG)
        refresh = 2
  
      CASE 15 'Ctrl-O -> Change object mode
        SetDispMode(OBJS_MODE,MODE_CHANG)
        refresh = 2
      
      CASE 12 'Ctrl-L -> Change light mode
        SetDispMode(LIGHT_MODE,MODE_CHANG)
        IF lmapupdate = 1 AND GetDispMode(LIGHT_MODE) = MODE_LMAP THEN
          SetDispMode(LIGHT_MODE,MODE_LDYN)
        ENDIF
        refresh = 2
      
      CASE 49 '1 -> Change to mode 1: map 
        edit = EDIT_MAP
        refresh = 1
  
      CASE 50 '2 -> Change to mode 2: heights & textures
        edit = EDIT_CEL
        refresh = 1
  
      CASE 51 '3 -> Change to mode 3: Texture attributes
        edit = EDIT_ATR
        refresh = 1
  
      CASE 52 '4 -> Change to mode 4: Superimposed textured
        edit = EDIT_OVL
        refresh = 1
  
      CASE 53 '5 -> Change to mode 5: lights
        edit = EDIT_LIG
        refresh = 1
  
      CASE 54 '6 -> Change to mode 6: Link points
        edit = EDIT_LNK
        arx = 0
        ary = 0
        refresh = 1
  
      CASE 55 '7 -> Change to mode 7: Events
        edit = EDIT_EVT
        refresh = 1
  
      CASE 56 '8 -> Change to mode 8: navigation
        edit = EDIT_NAV
        refresh = 1
  
      CASE -134,-107 'F12,Close window -> exit
        IF ( code = -134 AND menubar = 1 ) _
        OR ( code = -107 ) THEN
          IF changes = 1 THEN
            SCREENSET 0,0
            PrintMenuBar MBAR,MENUCHAN,0
            SCREENSET 1,0
            code = GetKeyb(1)
            IF code = ASC("Y") OR code = ASC("y") OR code = 13 THEN
              finish = 1
            ELSE
              refresh = 3
            ENDIF
          ELSE
            finish = 1
          ENDIF
        ENDIF
      
    END SELECT
    
    'Mode 1 commands: Map attributes
    IF edit = EDIT_MAP THEN
      
      SELECT CASE code
      
        CASE -75 'Cursor left -> x++
          rattr.px = rattr.px - 4
          changes = 1
          refresh = 2	
          lmapupdate = 1
      
        CASE -77 'Cursor right -> x--
          rattr.px = rattr.px + 4
          changes = 1
          refresh = 2	
          lmapupdate = 1
      
        CASE -72 'Cursor up -> y --
          rattr.py = rattr.py + 4
          changes = 1
          refresh = 2	
          lmapupdate = 1
      
        CASE -80 'Cursor down -> y ++
          rattr.py = rattr.py - 4
          changes = 1
          refresh = 2	
          lmapupdate = 1
      
        CASE -115 'Ctrl-Left -> AreaX++
          IF rattr.ax + 1 < MAXCELLX THEN
            rattr.ax = rattr.ax + 1
            changes = 1
            Refresh = 2
            lmapupdate = 1
          ENDIF
        
        CASE -116 'Ctrl-Right -> AreaX--
          IF rattr.ax > 5 THEN
            rattr.ax = rattr.ax - 1
            changes = 1
            Refresh = 2
            lmapupdate = 1
          ENDIF
  
        CASE -145 'Ctrl-Down -> AreaY++
          IF rattr.ay + 1 < MAXCELLY THEN
            rattr.ay = rattr.ay + 1
            changes = 1
            Refresh = 2
            lmapupdate = 1
          ENDIF
        
        CASE -141 'Ctrl-Up -> AreaY--
          IF rattr.ay > 5 THEN
            rattr.ay = rattr.ay - 1
            changes = 1
            Refresh = 2
            lmapupdate = 1
          ENDIF
  
      CASE -73 'Page Up -> Increase cellsize
        IF rattr.cellsize < 16 THEN
          'rattr.cellsize = rattr.cellsize * 2
          rattr.cellsize = rattr.cellsize + 1
          changes = 1
          refresh = 2
          lmapupdate = 1
        ENDIF
  
      CASE -81 'Page Down -> Decrease cellsize
        IF rattr.cellsize > 2 THEN
          'rattr.cellsize = rattr.cellsize / 2
          rattr.cellsize = rattr.cellsize - 1
          changes = 1
          refresh = 2
          lmapupdate = 1
        ENDIF
      
      CASE 32 'SPACE -> Toogle map entry point
        IF rattr.epmap = 1 THEN
          rattr.epmap = 0
        ELSE
          rattr.epmap = 1
        ENDIF
        changes = 1
        refresh = 1
      
      CASE ASC("d") 'd -> Set map description
        SCREENSET 0,0
        inpstr = GetStringBCol(0,9,LEN(rattr.mname),COL_TEXT,RGB(0,0,130),rattr.mname,ecode)
        IF ecode = 13 THEN
          rattr.mname = inpstr
          changes = 1
        ENDIF
        refresh = 1
        SCREENSET 1,0
        
    END SELECT
    
    'Mode 2 commands: Heights, textures, objects
    ELSEIF edit = EDIT_CEL THEN
      
      SELECT CASE code
    
        CASE 3 'Ctrl-C -> Copy area
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cpmap(i - Min(x,x+arx),j - Min(y,y+ary)) = cmap(i,j)
          NEXT j
          NEXT i
          cpax = Max(x,x+arx) - Min(x,x+arx) + 1
          cpay = Max(y,y+ary) - Min(y,y+ary) + 1
      
        CASE 22 'Ctrl-V -> Paste area
          IF cpax > 0 AND cpay > 0 THEN
            FOR i = Min(x,x+arx) TO Min(x,x+arx) + cpax - 1
            FOR j = Min(y,y+ary) TO Min(y,y+ary) + cpay - 1
              cmap(i,j) = cpmap(i - Min(x,x+arx),j - Min(y,y+ary))
            NEXT j
            NEXT i
            changes = 1
            refresh = 2
            lmapupdate = 1
          ENDIF
      
        CASE ASC("f"),ASC("F"),ASC("m"),ASC("M"),ASC("c"),ASC("C") 'Set wall textures
          SCREENSET 0,0
          seltex = TextureBrowser(1)
          SCREENSET 1,0
          IF seltex <> "ESC" THEN
            SELECT CASE code
              CASE ASC("f"): 
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texfl = seltex
                NEXT j
                NEXT i
              CASE ASC("F")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texfr = seltex
                NEXT j
                NEXT i
              CASE ASC("c")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texcl = seltex
                NEXT j
                NEXT i
              CASE ASC("C")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texcr = seltex
                NEXT j
                NEXT i
              CASE ASC("m")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texml = seltex
                NEXT j
                NEXT i
              CASE ASC("M")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texmr = seltex
                NEXT j
                NEXT i
            END SELECT
            changes = 1
            lmapupdate = 1
          ENDIF
          refresh = 2
      
        CASE ASC("b"),ASC("B") 'Set ground texture
          SCREENSET 0,0
          seltex = TextureBrowser(2)
          SCREENSET 1,0
          IF seltex <> "ESC" THEN
            SELECT CASE code
              CASE ASC("b")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texfb = seltex
                NEXT j
                NEXT i
              CASE ASC("B")
                FOR i = Min(x,x+arx) TO Max(x,x+arx)
                FOR j = Min(y,y+ary) TO Max(y,y+ary)
                  cmap(i,j).texcb = seltex
                NEXT j
                NEXT i
            END SELECT
            changes = 1
            lmapupdate = 1
          ENDIF
          refresh = 2
  
      CASE ASC("o") 'Set Object
          SCREENSET 0,0
          selobj = ObjectBrowser(0)
          SCREENSET 1,0
          IF selobj <> "ESC" THEN
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              cmap(i,j).object = selobj
            NEXT j
            NEXT i
            changes = 1
            lmapupdate = 1
          ENDIF
          refresh = 2
  
        CASE 26 'Ctrl-z -> Floor zero
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).floor = 0
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE 24 'Ctrl-x -> Ceiling zero
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).ceiling = 0
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE -118 'Ctrl-Pag- -> Height zero
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).height = 0
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE 97 'a -> Change cell floor ++
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).floor = cmap(i,j).floor + 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE 122 'z -> Change cell floor --
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).floor = cmap(i,j).floor - 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE 115 's -> Change cell ceiling ++
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).ceiling = cmap(i,j).ceiling + 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
        
        CASE 120 'x -> Change cell ceiling --
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).ceiling = cmap(i,j).ceiling - 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE -73 'Page up -> Change cell height ++
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).height = cmap(i,j).height + 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE -81 'Page down -> Change cell height --
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).height = cmap(i, j).height - 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
    
        CASE 105 'i -> Change object height +1
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).objheight = cmap(i,j).objheight + 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE 107 'k -> Change object height -1
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).objheight = cmap(i,j).objheight - 1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE 9 'Ctrl-i -> Change object height +0.1
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).objheight = cmap(i,j).objheight + 0.1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE 11 'Ctrl-k-> Change object height -0.1
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cmap(i,j).objheight = cmap(i,j).objheight - 0.1
          NEXT j
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE -115 'Ctrl-Left -> AreaX++
          arx = arx + 1
          IF x + arx > rattr.ax - 1 THEN arx = arx - 1
          refresh = 1
        
        CASE -116 'Ctrl-Right -> AreaX--
          arx = arx - 1
          IF x + arx < 0 THEN arx = arx + 1
          refresh = 1
  
        CASE -145 'Ctrl-Down -> AreaY++
          ary = ary + 1
          IF y + ary > rattr.ay - 1 THEN ary = ary - 1
          refresh = 1
        
        CASE -141 'Ctrl-Up -> AreaY--
          ary = ary - 1
          IF y + ary < 0 THEN ary = ary + 1
          refresh = 1
  
        CASE 27 'ESC -> Exit from area mode
          arx = 0: ary = 0
          refresh = 1
      
        CASE -75 'Cursor left -> x++
          IF ( arx >= 0 AND x + arx + 1 < rattr.ax ) _
          OR ( arx <  0 AND x       + 1 < rattr.ax ) THEN
           x = x + 1
           Refresh = 1
          END IF
      
        CASE -77 'Cursor right -> x--
  	      IF ( arx >= 0 AND x       - 1 >= 0 ) _
  	      OR ( arx <  0 AND x + arx - 1 >= 0 ) THEN
            x = x - 1
            refresh = 1
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF ( ary >= 0 AND y       - 1 >= 0 ) _
          OR ( ary <  0 AND y + ary - 1 >= 0 ) THEN
            y = y - 1
            refresh = 1
          END IF
      
        CASE -80 'Cursor down -> y ++
          IF ( ary >= 0 AND y + ary + 1 < rattr.ay ) _
          OR ( ary <  0 AND y       + 1 < rattr.ay ) THEN
            y = y + 1
            refresh = 1
          END IF
      
      END SELECT 
      
    'Mode 3 commands: Cell attributes
    ELSEIF edit = EDIT_ATR THEN
      
      SELECT CASE code
    
        CASE 3 'Ctrl-C -> Copy area
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            cpmap(i - Min(x,x+arx),j - Min(y,y+ary)) = cmap(i,j)
          NEXT j
          NEXT i
          cpax = Max(x,x+arx) - Min(x,x+arx) + 1
          cpay = Max(y,y+ary) - Min(y,y+ary) + 1
      
        CASE 22 'Ctrl-V -> Paste area
          IF cpax > 0 AND cpay > 0 THEN
            FOR i = Min(x,x+arx) TO Min(x,x+arx) + cpax - 1
            FOR j = Min(y,y+ary) TO Min(y,y+ary) + cpay - 1
              cmap(i,j) = cpmap(i - Min(x,x+arx),j - Min(y,y+ary))
            NEXT j
            NEXT i
            changes = 1
            refresh = 2
            lmapupdate = 1
          ENDIF
      
        CASE ASC("b") 'Selext texture: floor base
          seltexture = 1
          refresh = 1
  
        CASE ASC("B") 'Selext texture: ceiling base
          seltexture = 2
          refresh = 1
  
        CASE ASC("c") 'Selext texture: ceiling left
          seltexture = 3
          refresh = 1
  
        CASE ASC("C") 'Selext texture: ceiling right
          seltexture = 4
          refresh = 1
  
        CASE ASC("m") 'Selext texture: middle left
          seltexture = 5
          refresh = 1
  
        CASE ASC("M") 'Selext texture: middle right
          seltexture = 6
          refresh = 1
  
        CASE ASC("f") 'Selext texture: floor left
          seltexture = 7
          refresh = 1
  
        CASE ASC("F") 'Selext texture: floor right
          seltexture = 8
          refresh = 1
  
        CASE ASC("n") 'Toogle continuous flag
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            SELECT CASE seltexture
              CASE 1: IF cmap(i,j).confb = 1 THEN cmap(i,j).confb = 0: ELSE: cmap(i,j).confb = 1: ENDIF 
              CASE 2: IF cmap(i,j).concb = 1 THEN cmap(i,j).concb = 0: ELSE: cmap(i,j).concb = 1: ENDIF 
              CASE 3: IF cmap(i,j).concl = 1 THEN cmap(i,j).concl = 0: ELSE: cmap(i,j).concl = 1: ENDIF 
              CASE 4: IF cmap(i,j).concr = 1 THEN cmap(i,j).concr = 0: ELSE: cmap(i,j).concr = 1: ENDIF 
              CASE 5: IF cmap(i,j).conml = 1 THEN cmap(i,j).conml = 0: ELSE: cmap(i,j).conml = 1: ENDIF 
              CASE 6: IF cmap(i,j).conmr = 1 THEN cmap(i,j).conmr = 0: ELSE: cmap(i,j).conmr = 1: ENDIF 
              CASE 7: IF cmap(i,j).confl = 1 THEN cmap(i,j).confl = 0: ELSE: cmap(i,j).confl = 1: ENDIF 
              CASE 8: IF cmap(i,j).confr = 1 THEN cmap(i,j).confr = 0: ELSE: cmap(i,j).confr = 1: ENDIF 
            END SELECT
          NEXT j
          NEXT i
          IF seltexture <> 0 THEN changes = 1
          refresh = 2
          lmapupdate = 1
   
        CASE ASC("p") 'Toogle pass through flag
          FOR i = Min(x,x+arx) TO Max(x,x+arx)
          FOR j = Min(y,y+ary) TO Max(y,y+ary)
            SELECT CASE seltexture
              CASE 5: IF cmap(i,j).pasml = 1 THEN cmap(i,j).pasml = 0: ELSE: cmap(i,j).pasml = 1: ENDIF 
              CASE 6: IF cmap(i,j).pasmr = 1 THEN cmap(i,j).pasmr = 0: ELSE: cmap(i,j).pasmr = 1: ENDIF 
            END SELECT
          NEXT j
          NEXT i
          IF seltexture  = 5 OR seltexture = 6 THEN changes = 1
          refresh = 2
          lmapupdate = 1
      
        CASE -115 'Ctrl-Left -> AreaX++
          arx = arx + 1
          IF x + arx > rattr.ax - 1 THEN arx = arx - 1
          refresh = 1
        
        CASE -116 'Ctrl-Right -> AreaX--
          arx = arx - 1
          IF x + arx < 0 THEN arx = arx + 1
          refresh = 1
  
        CASE -145 'Ctrl-Down -> AreaY++
          ary = ary + 1
          IF y + ary > rattr.ay - 1 THEN ary = ary - 1
          refresh = 1
        
        CASE -141 'Ctrl-Up -> AreaY--
          ary = ary - 1
          IF y + ary < 0 THEN ary = ary + 1
          refresh = 1
  
        CASE 27 'ESC -> Exit from area mode / select texture: none
          IF seltexture = 0 THEN
            arx = 0: ary = 0
            refresh = 1
          ELSE
            seltexture = 0
            refresh = 1
          ENDIF
      
        CASE -75 'Cursor left -> x++
          IF seltexture = 0 THEN
            IF ( arx >= 0 AND x + arx + 1 < rattr.ax ) _
            OR ( arx <  0 AND x       + 1 < rattr.ax ) THEN
             x = x + 1
             Refresh = 1
            END IF
          ELSE
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              SELECT CASE seltexture
                CASE 3: cmap(i,j).ofclx = cmap(i,j).ofclx - 1
                CASE 4: cmap(i,j).ofcrx = cmap(i,j).ofcrx - 1
                CASE 5: cmap(i,j).ofmlx = cmap(i,j).ofmlx - 1
                CASE 6: cmap(i,j).ofmrx = cmap(i,j).ofmrx - 1
                CASE 7: cmap(i,j).offlx = cmap(i,j).offlx - 1
                CASE 8: cmap(i,j).offrx = cmap(i,j).offrx - 1
              END SELECT
            NEXT j
            NEXT i
            IF seltexture >= 3 THEN: changes = 1: lmapupdate = 1: ENDIF
            refresh = 2
          ENDIF
      
        CASE -77 'Cursor right -> x--
          IF seltexture = 0 THEN
            IF ( arx >= 0 AND x       - 1 >= 0 ) _
  	        OR ( arx <  0 AND x + arx - 1 >= 0 ) THEN
              x = x - 1
              refresh = 1
            ENDIF
          ELSE
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              SELECT CASE seltexture
                CASE 3: cmap(i,j).ofclx = cmap(i,j).ofclx + 1
                CASE 4: cmap(i,j).ofcrx = cmap(i,j).ofcrx + 1
                CASE 5: cmap(i,j).ofmlx = cmap(i,j).ofmlx + 1
                CASE 6: cmap(i,j).ofmrx = cmap(i,j).ofmrx + 1
                CASE 7: cmap(i,j).offlx = cmap(i,j).offlx + 1
                CASE 8: cmap(i,j).offrx = cmap(i,j).offrx + 1
              END SELECT
            NEXT j
            NEXT i
            IF seltexture >= 3 THEN: changes = 1: lmapupdate = 1: ENDIF
            refresh = 2
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF seltexture = 0 THEN
            IF ( ary >= 0 AND y       - 1 >= 0 ) _
            OR ( ary <  0 AND y + ary - 1 >= 0 ) THEN
              y = y - 1
              refresh = 1
            END IF
          ELSE
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              SELECT CASE seltexture
                CASE 3: cmap(i,j).ofcly = cmap(i,j).ofcly + 1
                CASE 4: cmap(i,j).ofcry = cmap(i,j).ofcry + 1
                CASE 5: cmap(i,j).ofmly = cmap(i,j).ofmly + 1
                CASE 6: cmap(i,j).ofmry = cmap(i,j).ofmry + 1
                CASE 7: cmap(i,j).offly = cmap(i,j).offly + 1
                CASE 8: cmap(i,j).offry = cmap(i,j).offry + 1
              END SELECT
            NEXT j
            NEXT i
            IF seltexture >= 3 THEN: changes = 1: lmapupdate = 1: ENDIF
            refresh = 2
          ENDIF
      
        CASE -80 'Cursor down -> y ++
          IF seltexture = 0 THEN
            IF ( ary >= 0 AND y + ary + 1 < rattr.ay ) _
            OR ( ary <  0 AND y       + 1 < rattr.ay ) THEN
              y = y + 1
              refresh = 1
            END IF
          ELSE
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              SELECT CASE seltexture
                CASE 3: cmap(i,j).ofcly = cmap(i,j).ofcly - 1
                CASE 4: cmap(i,j).ofcry = cmap(i,j).ofcry - 1
                CASE 5: cmap(i,j).ofmly = cmap(i,j).ofmly - 1
                CASE 6: cmap(i,j).ofmry = cmap(i,j).ofmry - 1
                CASE 7: cmap(i,j).offly = cmap(i,j).offly - 1
                CASE 8: cmap(i,j).offry = cmap(i,j).offry - 1
              END SELECT
            NEXT j
            NEXT i
            IF seltexture >= 3 THEN: changes = 1: lmapupdate = 1: ENDIF
            refresh = 2
          ENDIF
      
      END SELECT 
      
    'Mode 4 commands: Overlay textures
    ELSEIF edit = EDIT_OVL THEN
      
      SELECT CASE code
    
        CASE 3 'Ctrl-C -> Copy selected textures
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            cpotx(i - Min(selovtex,selovtex+selovrows)) = ovtex(i)
          NEXT i
          cpovrows = Max(selovtex,selovtex+selovrows) - Min(selovtex,selovtex+selovrows) + 1
      
        CASE 22 'Ctrl-V -> Paste selected textures
          IF cpovrows > 0 THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Min(selovtex,selovtex+selovrows) + cpovrows - 1
              ovtex(i) = cpotx(i - Min(selovtex,selovtex+selovrows))
            NEXT i
            changes = 1
            refresh = 2
            lmapupdate = 1
          ENDIF
  
        CASE 9 'TAB -> Change cursor mode
          IF cursmode1 = CURSOR_OVLIS THEN
            cursmode1 = CURSOR_OVTEX
          ELSE
            cursmode1 = CURSOR_OVLIS
          ENDIF
          refresh = 1
        
        CASE 32 'SPACE -> Change used flag
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            IF ovtex(i).used = 0 THEN
              ovtex(i).used = 1
            ELSE
              ovtex(i).used = 0
            ENDIF
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE 13 'ENTER -> Change active flag
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            IF ovtex(i).used = 1 THEN
              IF ovtex(i).active = 0 THEN
                ovtex(i).active = 1
              ELSE
                ovtex(i).active = 0
              ENDIF
              changes = 1
              refresh = 2
              lmapupdate = 1
            ENDIF
          NEXT i
  
        CASE 27 'ESC -> Clear selection
          selovrows = 0
          refresh = 1
          
        CASE -83 'DEL -> Delete overlay texture
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            ovtex(i).x       = 0 
            ovtex(i).y       = 0 
            ovtex(i).z       = 0 
            ovtex(i).dx      = 0 
            ovtex(i).dy      = 0 
            ovtex(i).dz      = 0 
            ovtex(i).texdir  = 1
            ovtex(i).texname = ""
            ovtex(i).ax      = 0 
            ovtex(i).ay      = 0 
            ovtex(i).plane   = ISOXPLANE
            ovtex(i).group   = 0 
            ovtex(i).used    = 0 
            ovtex(i).active  = 0 
          NEXT i
          changes = 1
          refresh = 2
          lmapupdate = 1
  
        CASE ASC("r"), ASC("R") 'rR -> Increase / Decrease group
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            IF ovtex(i).used = 1 THEN
              SELECT CASE code
                CASE ASC("r"): ovtex(i).group = ovtex(i).group + 1
                CASE ASC("R"): ovtex(i).group = ovtex(i).group - 1
              END SELECT
              changes = 1
              refresh = 2
              lmapupdate = 1
            ENDIF
          NEXT i
        
        CASE 61,33,34,250,36,37,38,47,40,41 'Shift-0123456789 -> Select light source
          SELECT CASE code
            CASE 061: ovgroup = 0
            CASE 033: ovgroup = 1
            CASE 034: ovgroup = 2
            CASE 250: ovgroup = 3
            CASE 036: ovgroup = 4
            CASE 037: ovgroup = 5
            CASE 038: ovgroup = 6
            CASE 047: ovgroup = 7
            CASE 040: ovgroup = 8
            CASE 041: ovgroup = 9
          END SELECT
          ovflag = -1
          FOR i = 0 TO MAXOVTEX - 1
            IF ovtex(i).group = ovgroup AND ovtex(i).used = 1 THEN
              IF ovflag = -1 THEN
                IF ovtex(i).active = 1 THEN
                  ovtex(i).active = 0
                ELSE
                  ovtex(i).active = 1
                ENDIF
                ovflag = ovtex(i).active
              ELSE
                ovtex(i).active = ovflag
              ENDIF
              changes = 1
              refresh = 2
              lmapupdate = 1
            ENDIF
          NEXT i
  
        CASE ASC("g"),ASC("w") 'Set ground / wall texture
          SCREENSET 0,0
          SELECT CASE code
            CASE ASC("w"): seltex = TextureBrowser(1)
            CASE ASC("g"): seltex = TextureBrowser(2)
          END SELECT
          IF seltex <> "ESC" THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).texname = seltex
                SELECT CASE code
                  CASE ASC("w"): ovtex(i).texdir = 1
                  CASE ASC("g"): ovtex(i).texdir = 2
                END SELECT
                IF ovtex(i).ax = 0 OR ovtex(i).ay = 0 THEN
                  ReadTexture ovtex(i).texdir, ovtex(i).texname, _
                  ovtex(i).ax, ovtex(i).ay, ipt
                ENDIF
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
          SCREENSET 1,0
  
        CASE ASC("p") 'p -> Change texture plane
          FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
            IF ovtex(i).used = 1 THEN
              IF ovtex(i).plane = ISOXPLANE THEN
                ovtex(i).plane = ISOYPLANE
              ELSEIF ovtex(i).plane = ISOYPLANE THEN
                ovtex(i).plane = ISOZPLANE
              ELSEIF ovtex(i).plane = ISOZPLANE THEN
                ovtex(i).plane = ISOFPLANE
              ELSEIF ovtex(i).plane = ISOFPLANE THEN
                ovtex(i).plane = ISOXPLANE
              ENDIF
              changes = 1
              refresh = 2
              lmapupdate = 1
            ENDIF
          NEXT i
          
        CASE -115 'Ctrl-Left -> Fine x++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dx = ovtex(i).dx + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
        
        CASE -116 'Ctrl-Right -> Fine x--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dx = ovtex(i).dx - 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
  
        CASE -145 'Ctrl-Down -> Fine y++ / Selected rows++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dy = ovtex(i).dy + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ELSEIF cursmode1 = CURSOR_OVLIS THEN
            selovrows = selovrows + 1
            IF selovtex + selovrows > MAXOVTEX - 1 THEN selovrows = selovrows - 1
            refresh = 1
          ENDIF
        
        CASE -141 'Ctrl-Up -> Fine y-- / Selected rows--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dy = ovtex(i).dy - 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ELSEIF cursmode1 = CURSOR_OVLIS THEN
            selovrows = selovrows - 1
            IF selovrows < 0 THEN selovrows = 0
            refresh = 1
          ENDIF
  
        CASE -132 'Ctrl-Pag- -> Fine z++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dz = ovtex(i).dz + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
        
        CASE -118 'Ctrl-Pag+ -> Fine z--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).dz = ovtex(i).dz - 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
  
        CASE -77 'Cursor left -> x--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
               ovtex(i).x = ovtex(i).x - 1
                IF ovtex(i).x < 0 THEN
                 ovtex(i).x = 0
               ENDIF
               changes = 1
               refresh = 2
               lmapupdate = 1
             ENDIF
          NEXT i
        ENDIF
      
        CASE -75 'Cursor right -> x++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).x = ovtex(i).x + 1
                IF ovtex(i).x > rattr.ax THEN
                  ovtex(i).x = rattr.ax
                ENDIF
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
      
        CASE -80 'Cursor down -> y ++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).y = ovtex(i).y + 1
                IF ovtex(i).y > rattr.ay THEN
                  ovtex(i).y = rattr.ay
                ENDIF
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ELSEIF cursmode1 = CURSOR_OVLIS THEN
            IF ( selovrows >= 0 AND selovtex + selovrows + 1 < MAXOVTEX ) _
            OR ( selovrows <  0 AND selovtex + 1 < MAXOVTEX ) THEN
              selovtex = selovtex + 1
             Refresh = 1
            END IF
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).y = ovtex(i).y - 1
                IF ovtex(i).y < 0 THEN
                  ovtex(i).y = 0
                ENDIF
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ELSEIF cursmode1 = CURSOR_OVLIS THEN
        	  IF ( selovrows >= 0 AND selovtex - 1 >= 0 ) _ 
  	        OR ( selovrows <  0 AND selovtex + selovrows - 1 >= 0 ) THEN
              selovtex = selovtex - 1
              refresh = 1
            ENDIF
          ENDIF
      
        CASE -81 'Pag++ -> z --
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).z = ovtex(i).z - 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
      
        CASE -73 'Pag-- -> z ++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).z = ovtex(i).z + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
      
        CASE -157 'Alt-Right -> Texture size Ax++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).ax = ovtex(i).ax + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
  
        CASE -155 'Alt-Left -> Texture size Ax--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).ax = ovtex(i).ax - 1
                IF ovtex(i).ax < 1 THEN ovtex(i).ax = 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
          
        CASE -152 'Alt-Up -> Texture size Ay++
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).ay = ovtex(i).ay + 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
  
        CASE -160 'Alt-Down -> Texture size Ay--
          IF cursmode1 = CURSOR_OVTEX THEN
            FOR i = Min(selovtex,selovtex+selovrows) TO Max(selovtex,selovtex+selovrows)
              IF ovtex(i).used = 1 THEN
                ovtex(i).ay = ovtex(i).ay - 1
                IF ovtex(i).ay < 1 THEN ovtex(i).ay = 1
                changes = 1
                refresh = 2
                lmapupdate = 1
              ENDIF
            NEXT i
          ENDIF
          
      END SELECT 
      
    'Mode 5 commands: Lights
    ELSEIF edit = EDIT_LIG THEN
      
      'Clear check light flag
      checklight = 0
      
      SELECT CASE code
  
        CASE ASC("u") '-> Draw shadow planes
          SCREENSET 0,0
          PrintMenuBar SBAR,MENUCONT, 0
          MapWindow("SET_VIEW",0)
          ISOLMapUnifyAllPlanes
          ISOLMapDrawPlanes(1)
          GetKeyb(1)
          refresh = 2
          SCREENSET 1,0
          MapWindow("INIT_VIEW",0)
        
        CASE ASC("q"), ASC("w"), 17, 23 '-> Change light source luminosity
          SELECT CASE code
            CASE ASC("q"): lightfactor = - 0.2
            CASE ASC("w"): lightfactor = + 0.2
            CASE 17      : lightfactor = - 0.05
            CASE 23      : lightfactor = + 0.05
          END SELECT
          IF lightsel = 99 THEN
            rattr.lightambl = rattr.lightambl + lightfactor
            IF rattr.lightambl < 0  THEN rattr.lightambl = 0
          ELSE
            light(lightsel).lum  = light(lightsel).lum  + lightfactor
            IF light(lightsel).lum  < 0  THEN light(lightsel).lum  = 0
          ENDIF
          changes = 1
          refresh = 2
          lmapupdate = 1
          checklight = 1
        
        CASE ASC("f"), ASC("v") '-> Change aprox factor
          SELECT CASE code
            CASE ASC("f"): rattr.lightaprox = rattr.lightaprox + 1
            CASE ASC("v"): rattr.lightaprox = rattr.lightaprox - 1
          END SELECT
          IF rattr.lightaprox < 0 THEN rattr.lightaprox = 0
          changes = 1
          refresh = 2
          checklight = 1
        
        CASE ASC("g") '-> Change difuse factor
          IF     rattr.lmapdifuse = 0 THEN
            rattr.lmapdifuse = 4
          ELSEIF rattr.lmapdifuse = 4 THEN
            rattr.lmapdifuse = 5
          ELSEIF rattr.lmapdifuse = 5 THEN
            rattr.lmapdifuse = 9
          ELSEIF rattr.lmapdifuse = 9 THEN
            rattr.lmapdifuse = 0
          ENDIF
          changes = 1
          refresh = 2
          lmapupdate = 1
          checklight = 1
        
        CASE ASC("a"), ASC("z"), 1, 26 '-> Change light source red component
          SELECT CASE code
            CASE ASC("a"): lightfactor = + 10
            CASE ASC("z"): lightfactor = - 10
            CASE 1       : lightfactor = + 1
            CASE 26      : lightfactor = - 1
          END SELECT
          IF lightsel = 99 THEN
            rattr.lightambr = rattr.lightambr + lightfactor
            IF rattr.lightambr < 0   THEN rattr.lightambr = 0
            IF rattr.lightambr > 255 THEN rattr.lightambr = 255
          ELSE
            light(lightsel).r  = light(lightsel).r  + lightfactor
            IF light(lightsel).r < 0   THEN light(lightsel).r  = 0
            IF light(lightsel).r > 255 THEN light(lightsel).r  = 255
          ENDIF
          changes = 1
          refresh = 2
          checklight = 1
          lmapupdate = 1
        
        CASE ASC("s"), ASC("x"), 19, 24 '-> Change light source green component
          SELECT CASE code
            CASE ASC("s"): lightfactor = + 10
            CASE ASC("x"): lightfactor = - 10
            CASE 19      : lightfactor = + 1
            CASE 24      : lightfactor = - 1
          END SELECT
          IF lightsel = 99 THEN
            rattr.lightambg = rattr.lightambg + lightfactor
            IF rattr.lightambg < 0   THEN rattr.lightambg = 0
            IF rattr.lightambg > 255 THEN rattr.lightambg = 255
          ELSE
            light(lightsel).g  = light(lightsel).g  + lightfactor
            IF light(lightsel).g  < 0   THEN light(lightsel).g  = 0
            IF light(lightsel).g  > 255 THEN light(lightsel).g  = 255
          ENDIF
          changes = 1
          refresh = 2
          checklight = 1
          lmapupdate = 1
        
        CASE ASC("d"), ASC("c"), 4, 3 '-> Change light source blue component
          SELECT CASE code
            CASE ASC("d"): lightfactor = + 10
            CASE ASC("c"): lightfactor = - 10
            CASE 4       : lightfactor = + 1
            CASE 3       : lightfactor = - 1
          END SELECT
          IF lightsel = 99 THEN
            rattr.lightambb = rattr.lightambb + lightfactor
            IF rattr.lightambb < 0   THEN rattr.lightambb = 0
            IF rattr.lightambb > 255 THEN rattr.lightambb = 255
          ELSE
            light(lightsel).b  = light(lightsel).b  + lightfactor
            IF light(lightsel).b < 0   THEN light(lightsel).b  = 0
            IF light(lightsel).b > 255 THEN light(lightsel).b  = 255
          ENDIF
          changes = 1
          refresh = 2
          checklight = 1
          lmapupdate = 1
        
        CASE ASC("r") '-> Reset light source
          IF lightsel = 99 THEN
            rattr.lightambl = 0.75
            rattr.lightambr = 100
            rattr.lightambg = 100
            rattr.lightambb = 100
          ELSE
            light(lightsel).lum = 1
            light(lightsel).r   = 100
            light(lightsel).g   = 100
            light(lightsel).b   = 100
          ENDIF
          changes = 1
          refresh = 2
          checklight = 1
          lmapupdate = 1
        
        CASE 33,34,250,36,37,38,47,40,41,61 'Shift-1234567890 -> Select light source
          SELECT CASE code
            CASE 033: lightsel = 0
            CASE 034: lightsel = 1
            CASE 250: lightsel = 2
            CASE 036: lightsel = 3
            CASE 037: lightsel = 4
            CASE 038: lightsel = 5
            CASE 047: lightsel = 6
            CASE 040: lightsel = 7
            CASE 041: lightsel = 8
            CASE 061: lightsel = 99
          END SELECT
          refresh = 1
      
        CASE 32 'SPACE -> Light On / Off
          IF lightsel = 99 THEN
            IF rattr.lmapstore = 1 THEN
              rattr.lmapstore = 0
            ELSE
              rattr.lmapstore = 1
            ENDIF
            refresh = 2 
            changes = 1
          ELSE
            IF light(lightsel).used = 0 THEN
              light(lightsel).used = 1
            ELSE
              light(lightsel).used = 0
            ENDIF
            refresh = 2 
            changes = 1
            lmapupdate = 1
          ENDIF
        
        CASE -83 'DEL -> Clear light source
          IF lightsel <> 99 THEN
            light(lightsel).used = 0
            light(lightsel).x = 0
            light(lightsel).y = 0
            light(lightsel).x = 0
            light(lightsel).dx = 0
            light(lightsel).dy = 0
            light(lightsel).dx = 0
            light(lightsel).lum = 1
            light(lightsel).r = 100
            light(lightsel).g = 100
            light(lightsel).b = 100
            changes = 1
            refresh = 2
            lmapupdate = 1
          ENDIF
        
        CASE -147 'CTRL-DEL -> Clear all light sources
          IF lightsel <> 99 THEN
            FOR i = 0 TO MAXLIGHT - 1
              light(i).used = 0
              light(i).x = 0
              light(i).y = 0
              light(i).x = 0
              light(i).dx = 0
              light(i).dy = 0
              light(i).dx = 0
              light(i).lum = 1
              light(i).r = 100
              light(i).g = 100
              light(i).b = 100
            NEXT i
            changes = 1
            refresh = 2
            lmapupdate = 1
          ENDIF
      
        CASE -118 'Ctrl-Pag- -> Height zero / light dz --
          IF lightsel <> 99 THEN
            light(lightsel).dz = light(lightsel).dz - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -132 'Ctrl-Pag+ -> Height zero / light dz ++
          IF lightsel <> 99 THEN
            light(lightsel).dz = light(lightsel).dz + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -73 'Page up -> Change cell height ++
          IF lightsel <> 99 THEN
            light(lightsel).z = light(lightsel).z + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -81 'Page down -> Change cell height --
          IF lightsel <> 99 THEN
            light(lightsel).z = light(lightsel).z - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
    
        CASE -115 'Ctrl-Left -> AreaX++
          IF lightsel <> 99 THEN
            light(lightsel).dx = light(lightsel).dx + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
        
        CASE -116 'Ctrl-Right -> AreaX--
          IF lightsel <> 99 THEN
            light(lightsel).dx = light(lightsel).dx - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
  
        CASE -145 'Ctrl-Down -> AreaY++
          IF lightsel <> 99 THEN
            light(lightsel).dy = light(lightsel).dy + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
        
        CASE -141 'Ctrl-Up -> AreaY--
          IF lightsel <> 99 THEN
            light(lightsel).dy = light(lightsel).dy - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
  
        CASE -75 'Cursor left -> x++
          IF lightsel <> 99 THEN
            light(lightsel).x = light(lightsel).x + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -77 'Cursor right -> x--
          IF lightsel <> 99 THEN
            light(lightsel).x = light(lightsel).x - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF lightsel <> 99 THEN
            light(lightsel).y = light(lightsel).y - 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
        CASE -80 'Cursor down -> y ++
          IF lightsel <> 99 THEN
            light(lightsel).y = light(lightsel).y + 1
            changes = 1
            refresh = 2
            checklight = 1
            lmapupdate = 1
          ENDIF
      
      END SELECT
  
      'Do not rewrite map if lights are turned off
      IF checklight = 1 AND refresh = 2 AND lightsel <> 99 THEN
        IF light(lightsel).used = 0 _
        OR GetDispMode(LIGHT_MODE) = MODE_OFF THEN
          refresh = 1
        ENDIF
        checklight = 0
      ENDIF
      
    'Mode 6 commands: Link points
    ELSEIF edit = EDIT_LNK THEN
      
      SELECT CASE code
    
        CASE ASC("d") 'Set default entry point
          rattr.depx = x
          rattr.depy = y
          changes = 1
          refresh = 1
  
        CASE ASC("l") 'Set 2-way link
          IF filename <> DEFAULTMAP THEN
            SCREENSET 0,0
            _LinkSearchPoint(filename,x,y,linkmap,linkcx,linkcy,linkway,found)
            IF found <> 1 THEN: linkmap = "": linkcx = 0: linkcy = 0: ENDIF
            IF ChooseLinkPoint(linkmap,linkcx,linkcy) = 1 THEN
              IF found = 1 THEN _LinkDeletePoint(filename,x,y)
              _LinkStorePoint(filename,x,y,linkmap,linkcx,linkcy,0)
              changes = 1
            ENDIF
            refresh = 2
            SCREENSET 1,0
          ELSE
            PrintMenuBar WBAR,MENUWARN, 0
            SLEEP
            refresh = 1
          ENDIF
        
        CASE ASC("x") 'Set exit point
          IF filename <> DEFAULTMAP THEN
            SCREENSET 0,0
            _LinkSearchPoint(filename,x,y,linkmap,linkcx,linkcy,linkway,found)
            IF found <> 1 THEN: linkmap = "": linkcx = 0: linkcy = 0: ENDIF
            IF ChooseLinkPoint(linkmap,linkcx,linkcy) = 1 THEN
              IF found = 1 THEN _LinkDeletePoint(filename,x,y)
              _LinkStorePoint(filename,x,y,linkmap,linkcx,linkcy,+1)
              changes = 1
            ENDIF
            refresh = 2
            SCREENSET 1,0
          ELSE
            PrintMenuBar WBAR,MENUWARN, 0
            SLEEP
            refresh = 1
          ENDIF
        
        CASE ASC("n") 'Set entry point
          IF filename <> DEFAULTMAP THEN
            SCREENSET 0,0
            _LinkSearchPoint(filename,x,y,linkmap,linkcx,linkcy,linkway,found)
            IF found <> 1 THEN: linkmap = "": linkcx = 0: linkcy = 0: ENDIF
            IF ChooseLinkPoint(linkmap,linkcx,linkcy) = 1 THEN
              IF found = 1 THEN _LinkDeletePoint(filename,x,y)
              _LinkStorePoint(filename,x,y,linkmap,linkcx,linkcy,-1)
              changes = 1
            ENDIF
            refresh = 2
            SCREENSET 1,0
          ELSE
            PrintMenuBar WBAR,MENUWARN, 0
            SLEEP
            refresh = 1
          ENDIF
        
        CASE 13 'Edit link point destination cell
          IF filename <> DEFAULTMAP THEN
            SCREENSET 0,0
            _LinkSearchPoint(filename,x,y,linkmap,linkcx,linkcy,linkway,found)
            IF found = 1 THEN
              IF ChooseLinkPoint(linkmap,linkcx,linkcy) = 1 THEN
                _LinkDeletePoint(filename,x,y)
                _LinkStorePoint(filename,x,y,linkmap,linkcx,linkcy,linkway)
                changes = 1
              ENDIF
            ENDIF
            refresh = 2
            SCREENSET 1,0
          ENDIF
        
        CASE -83 'DEL -> Clear link point
          IF filename <> DEFAULTMAP THEN
            _LinkSearchPoint(filename,x,y,linkmap,linkcx,linkcy,linkway,found)
            IF found = 1 THEN
              _LinkDeletePoint(filename,x,y)
              changes = 1
              refresh = 1
            ENDIF
          ENDIF
          
        CASE -75 'Cursor left -> x++
          IF x + 1 < rattr.ax THEN
           x = x + 1
           Refresh = 1
          END IF
      
        CASE -77 'Cursor right -> x--
  	      IF x - 1 >= 0 THEN
            x = x - 1
            refresh = 1
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF y - 1 >= 0 THEN
            y = y - 1
            refresh = 1
          END IF
      
        CASE -80 'Cursor down -> y ++
          IF y + 1 < rattr.ay THEN
            y = y + 1
            refresh = 1
          END IF
      
      END SELECT 
      
    'Mode 7 commands: Events
    ELSEIF edit = EDIT_EVT THEN
      
      SELECT CASE code
    
        CASE 3 'Ctrl-C -> Copy selected events / actions
          IF cursmode2 = CURSOR_EVCEL THEN
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              cpmap(i - Min(x,x+arx),j - Min(y,y+ary)) = cmap(i,j)
            NEXT j
            NEXT i
            cpax = Max(x,x+arx) - Min(x,x+arx) + 1
            cpay = Max(y,y+ary) - Min(y,y+ary) + 1
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            FOR i = Min(selevent,selevent+selevrows) TO Max(selevent,selevent+selevrows)
              cpevt(i - Min(selevent,selevent+selevrows)) = event(i)
            NEXT i
            cpevrows = Max(selevent,selevent+selevrows) - Min(selevent,selevent+selevrows) + 1
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            FOR i = Min(selactio,selactio+selacrows) TO Max(selactio,selactio+selacrows)
              cpact(i - Min(selactio,selactio+selacrows)) = actio(i)
            NEXT i
            cpacrows = Max(selactio,selactio+selacrows) - Min(selactio,selactio+selacrows) + 1
            selacrows = 0
            IF selactio < actwtop THEN selactio = actwtop
            refresh = 1
          ENDIF
      
        CASE 22 'Ctrl-V -> Paste selected events / actions
          IF cursmode2 = CURSOR_EVCEL AND cpax > 0 AND cpay > 0 THEN
            FOR i = Min(x,x+arx) TO Min(x,x+arx) + cpax - 1
            FOR j = Min(y,y+ary) TO Min(y,y+ary) + cpay - 1
              cmap(i,j) = cpmap(i - Min(x,x+arx),j - Min(y,y+ary))
            NEXT j
            NEXT i
            changes = 1
            refresh = 2
            lmapupdate = 1
          ELSEIF cursmode2 = CURSOR_EVLIS AND cpevrows > 0 THEN
            FOR i = Min(selevent,selevent+selevrows) TO Min(selevent,selevent+selevrows) + cpevrows - 1
              event(i) = cpevt(i - Min(selevent,selevent+selevrows))
            NEXT i
            changes = 1
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVACT AND cpacrows > 0 THEN
            FOR i = Min(selactio,selactio+selacrows) TO Min(selactio,selactio+selacrows) + cpacrows - 1
              actio(i) = cpact(i - Min(selactio,selactio+selacrows))
            NEXT i
            changes = 1
            refresh = 1
          ENDIF
  
        CASE 9 'TAB -> Change cursor mode
          IF cursmode2 = CURSOR_EVCEL THEN
            cursmode2 = CURSOR_EVLIS
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            cursmode2 = CURSOR_EVACT
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            cursmode2 = CURSOR_EVCEL
          ENDIF
          refresh = 1
        
        CASE -82 'Insert -> Insert new event / action
          IF cursmode2 = CURSOR_EVLIS THEN
            IF rattr.numevt < MAXEVENT THEN
              rattr.numevt = rattr.numevt + 1
              IF rattr.numevt = 1 THEN
                selevent = 0
                event(selevent).id = 1
              ELSE
                FOR i=rattr.numevt-1 TO selevent+1 STEP -1
                  event(i) = event(i-1)
                NEXT i
                selevent = selevent + 1
                event(selevent).id = 0
                FOR i=0 TO rattr.numevt-1
                  IF event(i).id > event(selevent).id THEN
                    event(selevent).id = event(i).id
                  ENDIF
                NEXT i
                event(selevent).id = event(selevent).id + 1
                IF event(selevent).id = 0 THEN event(selevent).id = 1
              ENDIF
              event(selevent).evname = "(NoDef)"
              event(selevent).evtype = EVT_SWITCH
              event(selevent).value = 0
              refresh = 1
              changes = 1
            ENDIF
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            IF rattr.numact < MAXACTIO THEN
              rattr.numact = rattr.numact + 1
              IF rattr.numact = 1 THEN
                selactio = 0
                actio(selactio).oper = ACT_BLANKLINE
              ELSE
                FOR i=rattr.numact-1 TO selactio STEP -1
                  actio(i) = actio(i-1)
                NEXT i
                actio(selactio).oper = ACT_BLANKLINE
              ENDIF
              refresh = 1
              changes = 1
            ENDIF
          ENDIF
        
        CASE -83 'Delete -> Delete events / actions
          IF cursmode2 = CURSOR_EVCEL THEN
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              cmap(i,j).eventid = 0
            NEXT j
            NEXT i
            changes = 1
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            IF rattr.numevt > 0 THEN
              FOR i=selevent TO rattr.numevt
                event(i) = event(i+1)
              NEXT i
              rattr.numevt = rattr.numevt - 1
              IF selevent > rattr.numevt-1 THEN selevent = rattr.numevt-1
              IF selevent < 0 THEN selevent = 0
              refresh = 1
              changes = 1
            ENDIF
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            IF rattr.numact > 0 THEN
              FOR i=selactio TO rattr.numact
                actio(i) = actio(i+1)
              NEXT i
              rattr.numact = rattr.numact - 1
              IF selactio > rattr.numact-1 THEN selactio = rattr.numact-1
              IF selactio < 0 THEN selactio = 0
              refresh = 1
              changes = 1
            ENDIF
          ENDIF
        
        CASE ASC("e") 'e -> Link event to cell
          IF cursmode2 = CURSOR_EVCEL THEN
            FOR i = Min(x,x+arx) TO Max(x,x+arx)
            FOR j = Min(y,y+ary) TO Max(y,y+ary)
              cmap(i,j).eventid = event(selevent).id
            NEXT j
            NEXT i
            changes = 1
            refresh = 1
          ENDIF
        
        CASE ASC("w") 'w -> Change action window size
          IF actwindow = 1 THEN
            actwindow = 2
            actwlines = 33
          ELSE
            actwindow = 1
            actwlines = 9
          ENDIF
          selacrows = 0
          IF selactio > actwtop + actwlines-2 THEN 
            selactio = actwtop + actwlines-2
          ENDIF
          refresh = 2
        
        CASE ASC("i") 'i -> Edit event id
          IF cursmode2 = CURSOR_EVLIS THEN
            SCREENSET 0,0
            inpstr = GetStringBCol(00,17+selevent,3,COL_TEXT,RGB(0,0,130),FORMAT(event(selevent).id,"00"),ecode)
            IF inpstr <> "" AND VALINT(inpstr) > 0 THEN
              event(selevent).id = VALINT(inpstr)
              refresh = 1
              changes = 1
            ENDIF
            SCREENSET 1,0
          ENDIF
        
        CASE ASC("a") 'a -> Edit event alias
          IF cursmode2 = CURSOR_EVLIS THEN
            SCREENSET 0,0
            inpstr = GetStringBCol(03,17+selevent,1+LEN(event(selevent).evname),COL_TEXT,RGB(0,0,130),event(selevent).evname,ecode)
            IF inpstr <> "" THEN
              event(selevent).evname = inpstr
              refresh = 1
              changes = 1
            ENDIF
            SCREENSET 1,0
          ENDIF
        
        CASE ASC("t") 't -> Edit event type
          IF cursmode2 = CURSOR_EVLIS THEN
            IF event(selevent).evtype = EVT_SWITCH THEN
              event(selevent).evtype = EVT_TRIGGER
            ELSEIF event(selevent).evtype = EVT_TRIGGER THEN
              event(selevent).evtype = EVT_SWITCH
            ENDIF
            refresh = 1
            changes = 1
          ENDIF
        
        CASE ASC("v") 'v -> Edit event value
          IF cursmode2 = CURSOR_EVLIS THEN
            SCREENSET 0,0
            inpstr = GetStringBCol(19,17+selevent,3,COL_TEXT,RGB(0,0,130),FORMAT(event(selevent).value,"00"),ecode)
            IF inpstr <> "" AND VALINT(inpstr) > 0 THEN
              event(selevent).value = VALINT(inpstr)
              refresh = 1
              changes = 1
            ENDIF
            SCREENSET 1,0
          ENDIF
        
        CASE 13 'ENTER -> Edit action
          IF cursmode2 = CURSOR_EVACT THEN
            SCREENSET 0,0
            ActionPrint(actio(selactio),actstr)
            GetStringExitMode(GSEXIT_UPDOWN)
            inpstr = GetStringBCol(24,03+selactio-actwtop,80,COL_TEXT,RGB(0,0,130),actstr,ecode)
            GetStringExitMode(GSEXIT_NORMAL)
            IF ecode <> 27 THEN
              IF ActionParse(iactio,inpstr,perror) = 1 THEN
                ActionPrint(iactio,actstr0)
                IF actstr0 <> actstr THEN 
                  actio(selactio) = iactio
                  changes = 1
                ENDIF
                SELECT CASE ecode
                  CASE  13: SetKeyb(CHR$(255)+CHR$(080)): SetKeyb(CHR$(013)+CHR$(000)) 'ENTER
                  CASE -72: SetKeyb(CHR$(255)+CHR$(072)): SetKeyb(CHR$(013)+CHR$(000)) 'Up
                  CASE -80: SetKeyb(CHR$(255)+CHR$(080)): SetKeyb(CHR$(013)+CHR$(000)) 'Down
                END SELECT
                refresh = 1
              ELSE
                PrintMenuBar EBAR,"Parse error: " + perror,0
                refresh = 1
                GetKeyb(1)
              ENDIF
            ENDIF
            SCREENSET 1,0
          ENDIF
        
        CASE -115 'Ctrl-Left -> AreaX++
          IF cursmode2 = CURSOR_EVCEL THEN
            arx = arx + 1
            IF x + arx > rattr.ax - 1 THEN arx = arx - 1
            refresh = 1
          ENDIF
        
        CASE -116 'Ctrl-Right -> AreaX--
          IF cursmode2 = CURSOR_EVCEL THEN
            arx = arx - 1
            IF x + arx < 0 THEN arx = arx + 1
            refresh = 1
          ENDIF
  
        CASE -145 'Ctrl-Down -> AreaY++
          IF cursmode2 = CURSOR_EVCEL THEN
            ary = ary + 1
            IF y + ary > rattr.ay - 1 THEN ary = ary - 1
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            selevrows = selevrows + 1
            IF selevent + selevrows > rattr.numevt - 1 THEN selevrows = selevrows - 1
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            IF selactio + selacrows + 1 <= rattr.numact - 1 _
            AND selactio - actwtop + selacrows + 1 <= actwlines - 1 THEN 
              selacrows = selacrows + 1
            ELSEIF selactio + selacrows + 1 <= rattr.numact - 1 THEN
              selacrows = selacrows + 1
              actwtop = actwtop + 1
            ENDIF
            refresh = 1
          ENDIF
        
        CASE -141 'Ctrl-Up -> AreaY--
          IF cursmode2 = CURSOR_EVCEL THEN
            ary = ary - 1
            IF y + ary < 0 THEN ary = ary + 1
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            selevrows = selevrows - 1
            IF selevrows < 0 THEN selevrows = 0
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            selacrows = selacrows - 1
            IF selacrows < 0 THEN selacrows = 0
            refresh = 1
          ENDIF
  
        CASE 27 'ESC -> Exit from area mode
          IF cursmode2 = CURSOR_EVCEL THEN
            arx = 0: ary = 0
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            selevrows = 0
            refresh = 1
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            selacrows = 0
            refresh = 1
          ENDIF
      
        CASE -75 'Cursor left -> x++
          IF cursmode2 = CURSOR_EVCEL THEN
            IF ( arx >= 0 AND x + arx + 1 < rattr.ax ) _
            OR ( arx <  0 AND x       + 1 < rattr.ax ) THEN
             x = x + 1
             Refresh = 1
            END IF
          ENDIF
      
        CASE -77 'Cursor right -> x--
          IF cursmode2 = CURSOR_EVCEL THEN
    	      IF ( arx >= 0 AND x       - 1 >= 0 ) _
  	        OR ( arx <  0 AND x + arx - 1 >= 0 ) THEN
              x = x - 1
              refresh = 1
            ENDIF
          ENDIF
      
        CASE -72 'Cursor up -> y --
          IF cursmode2 = CURSOR_EVCEL THEN
            IF ( ary >= 0 AND y       - 1 >= 0 ) _
            OR ( ary <  0 AND y + ary - 1 >= 0 ) THEN
              y = y - 1
              refresh = 1
            END IF
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
        	  IF ( selevrows >= 0 AND selevent - 1 >= 0 ) _ 
  	        OR ( selevrows <  0 AND selevent + selevrows - 1 >= 0 ) THEN
              selevent = selevent - 1
              refresh = 1
            ENDIF
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            IF selactio > actwtop THEN
              selactio = selactio - 1
              refresh = 1
            ELSE
              IF actwtop > 0 THEN
                actwtop = actwtop - 1
                selactio = selactio - 1
                refresh = 1
              ENDIF
            ENDIF
          ENDIF
      
        CASE -80 'Cursor down -> y ++
          IF cursmode2 = CURSOR_EVCEL THEN
            IF ( ary >= 0 AND y + ary + 1 < rattr.ay ) _
            OR ( ary <  0 AND y       + 1 < rattr.ay ) THEN
              y = y + 1
              refresh = 1
            END IF
          ELSEIF cursmode2 = CURSOR_EVLIS THEN
            IF ( selevrows >= 0 AND selevent + selevrows + 1 < rattr.numevt ) _
            OR ( selevrows <  0 AND selevent + 1 < rattr.numevt ) THEN
              selevent = selevent + 1
             Refresh = 1
            END IF
          ELSEIF cursmode2 = CURSOR_EVACT THEN
            IF selactio - actwtop + selacrows < actwlines-1 _
            AND selactio + selacrows < rattr.numact-1 THEN
              selactio = selactio + 1
              refresh = 1
            ELSEIF selactio + selacrows < rattr.numact-1 THEN
              actwtop = actwtop + 1
              selactio = selactio + 1
              refresh = 1
            END IF
          ENDIF
      
      END SELECT 
      
    'Mode 7 commands: Navigation
    ELSEIF edit = EDIT_NAV THEN
      
      'Switch on keyboard key
      SELECT CASE code
  
        CASE -75 'Cursor left -> x++
          EntRefreshCommand(entidx,ENCMOVL,2)
      
        CASE -77 'Cursor right -> x--
          EntRefreshCommand(entidx,ENCMOVR,2)
      
        CASE -72 'Cursor up -> y --
          EntRefreshCommand(entidx,ENCMOVU,2)
      
        CASE -80 'Cursor down -> y ++
          EntRefreshCommand(entidx,ENCMOVD,2)
        
        CASE -73 'Page Up -> Change entity --
          entypenr = entypenr - 1
          IF entypenr < 0 THEN entypenr = numetype - 1
          EntRefreshKill
          entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize,_
          cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(entypenr))
          
        CASE -81 'Page Down -> Change entity ++
          entypenr = entypenr + 1
          IF entypenr > numetype - 1 THEN entypenr = 0
          EntRefreshKill
          entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize,_
          cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(entypenr))
  
      END SELECT 
      
    ENDIF
  
    'Force screenrefresh in FPS mode
    IF fpsmode = 1 THEN refresh = 2
    
    'Change light mode if changes are made
    IF lmapupdate = 1 AND GetDispMode(LIGHT_MODE) = MODE_LMAP THEN 
      SetDispMode(LIGHT_MODE,MODE_LDYN)
    ENDIF
    
  LOOP WHILE finish = 0  
  
END SUB

'Run map
SUB MapExecute(mapname AS STRING)

  'Variables
  DIM AS INTEGER finish, Refresh, code, ecode, found
  DIM AS INTEGER i, j, x, y, ax, ay
  DIM AS INTEGER mode, entidx, entypenr
  DIM AS INTEGER rfcount, cancel
  DIM AS INTEGER Counter, PrvCount
  DIM AS INTEGER ovflag, ovgroup
  DIM AS INTEGER menubar, linkcx, linkcy, linkway
  DIM AS INTEGER fileupdate
  DIM AS STRING  ename, filename, seltex, selobj, seletype
  DIM AS STRING  linkmap, inpstr, actstr, actstr0, perror
  DIM AS SINGLE  lightfactor
  
  'Background screen
  'SCREENSET 2, 0
  'SetBackground("backgrnd.pcx")
  'SCREENCOPY 2,1
  'SCREENSET 1,0
  SCREENSET 1,0
  CLS 0
  
  'Map editor initialization
  WINDOWTITLE "DARK CASTLE v1.0"
  SetFont(2)
  MapRead(mapname, rattr,cmap(),light(),ovtex(),event(),actio())
  SetDispMode(TEXT_MODE,MODE_ON)
  SetDispMode(OBJS_MODE,MODE_ON)
  SetDispMode(ENTS_MODE,MODE_ON)
  IF rattr.lmapstore = 1 THEN
    SetDispMode(LIGHT_MODE,MODE_LMAP)
  ELSE
    SetDispMode(LIGHT_MODE,MODE_LDYN)
  ENDIF
  AutomSubClearAll()
  AutomSubHook(0,@MapRefreshDaemon,0.12)
  EntRefreshKill
  entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize, _
  cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(0))
  EntRefreshSetMapPointer(@rattr,@cmap(0,0))
  MapWindow("SET_WINDOW",1)
  
  'Map drawing loop
  refresh = 2
  finish = 0
  mode = 5
  x = 0: y = 0
  entypenr = 0
  DO
    
    'Refresh map
    IF Refresh = 2 THEN
      MapWindow("SET_VIEW_CLEAR",0)
      RoomMap rattr, cmap(), light(), ovtex(), 0
      MapWindow("INIT_VIEW",0)
      MapWindow("STORE_WINDOW",2)
    END IF
    
    'Clear refresh flag
    refresh=0
    SetDispMode(ENTS_MODE,MODE_ON)
  
    'Get keyboard code & refresh objects
    code = GetKeyb(0)
    IF code = 0 THEN AutomSubExecute(1)
    
    'Generic commands for all edition modes
    SELECT CASE code
      
      CASE -61 'F3 -> Change edit mode / Load file
        fileupdate = 0
        SCREENSET 0,0
        refresh = 2
        filename = FileLoad(rattr,cmap(),light(),ovtex(),event(),actio(),filename,"",cancel)
        IF rattr.lmapstore = 1 THEN
          SetDispMode(LIGHT_MODE, MODE_LMAP)
        ELSE
          SetDispMode(LIGHT_MODE, MODE_LDYN)
        ENDIF
        EntRefreshKill
        entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize, _
        cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(0))
        SCREENSET 1,0
     
      CASE -134,-107 'F12,Close window -> exit
        IF code = -134 OR code = -107 THEN
          finish = 1
        ENDIF
      
      CASE -75 'Cursor left -> x++
        EntRefreshCommand(entidx,ENCMOVL,2)
    
      CASE -77 'Cursor right -> x--
        EntRefreshCommand(entidx,ENCMOVR,2)
      
      CASE -72 'Cursor up -> y --
        EntRefreshCommand(entidx,ENCMOVU,2)
      
      CASE -80 'Cursor down -> y ++
        EntRefreshCommand(entidx,ENCMOVD,2)
        
      CASE -73 'Page Up -> Change entity --
        entypenr = entypenr - 1
        IF entypenr < 0 THEN entypenr = numetype - 1
        EntRefreshKill
        entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize,_
        cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(entypenr))
          
      CASE -81 'Page Down -> Change entity ++
        entypenr = entypenr + 1
        IF entypenr > numetype - 1 THEN entypenr = 0
        EntRefreshKill
        entidx = EntRefreshEnqueue(rattr.depx*rattr.cellsize,rattr.depy*rattr.cellsize,_
        cmap(rattr.depx,rattr.depy).floor*rattr.cellsize,etype(entypenr))
  
    END SELECT 
      
  LOOP WHILE finish = 0  
  
END SUB

'Refresh daemon
SUB MapRefreshDaemon
  
  'Init secondary z-buffer
  ISOZBufferLayer 2
  ISOZBufferClear
  ISOLMapUseFlag(0)
  
  'Rewrite map
  MapWindow("RESTORE_WINDOW",2)

  'Set clipping margin
  MapWindow("SET_VIEW",0)
  
  'Animated object refresh
  ObjRefreshMode(1)
  ObjRefreshDraw
  
  'Entity refresh
  EntRefreshDraw

  'Restore clipping
  VIEW SCREEN
  
  'Copy screen
  SCREENCOPY 1,0
  
END SUB

'Map window management
SUB MapWindow(commd AS STRING, parm AS INTEGER)

  'Variables
  STATIC AS INTEGER px1,py1,px2,py2

  'Switch on command
  SELECT CASE commd
  
    'Set window
    CASE "SET_WINDOW"
      IF parm = 1 THEN
        px1 = WINDOWPX
        py1 = WINDOWPY
        px2 = AX0 - WINBORDR
        py2 = AY0 - WINBORDR
      ELSEIF parm = 2 THEN
        px1 = WINDOWPX
        py1 = WINDOWPY + WINACTLS
        px2 = AX0 - WINBORDR
        py2 = AY0 - WINBORDR
      ELSEIF parm = 3 THEN
        px1 = WINDOWPX
        py1 = WINDOWPY + WINACTLB
        px2 = AX0 - WINBORDR
        py2 = AY0 - WINBORDR
      ENDIF
    
    'Set view
    CASE "SET_VIEW"
      VIEW SCREEN (px1,py1)-(px2,py2)

    'Set view
    CASE "SET_VIEW_CLEAR"
      VIEW SCREEN (px1,py1)-(px2,py2),0

    'Init view
    CASE "INIT_VIEW"
      VIEW SCREEN
    
    'Store window
    CASE "STORE_WINDOW"
      IF parm = 1 THEN
        GET (px1,py1)-(px2,py2), mapscr1
      ELSEIF parm = 2 THEN
        GET (px1,py1)-(px2,py2), mapscr2
      ENDIF
  
    'Restore window
    CASE "RESTORE_WINDOW"
      IF parm = 1 THEN
        PUT (px1,py1), mapscr1, PSET
      ELSEIF parm = 2 THEN
        PUT (px1,py1), mapscr2, PSET
      ENDIF

  END SELECT

END SUB  

'Checkbox print
FUNCTION InfoCheckbox(value AS INTEGER) AS STRING
  SELECT CASE value
    CASE 0: InfoCheckbox = "[_]"
    CASE 1: InfoCheckbox = "[X]"
  END SELECT
END FUNCTION

'Print info box
SUB PrintInfoBox(mode AS INTEGER, x AS INTEGER, y AS INTEGER, mapname AS STRING, _
                 rattr AS roomattr, cmap() AS mapcell, light() AS maplight, _
                 ovtex() as mapovtex, event() AS mapevent, actio() AS mapactio, _
                 lightsel AS INTEGER, seltexture AS INTEGER, selovtex AS INTEGER, _
                 selovrows AS INTEGER, selevent AS INTEGER, selevrows AS INTEGER, _
                 selactio AS INTEGER, selacrows AS INTEGER, cursmode1 AS INTEGER, _
                 cursmode2 AS INTEGER, changes AS INTEGER, actwindow AS INTEGER, _
                 actwlines AS INTEGER, actwtop AS INTEGER, scrn AS INTEGER)

  'Screen buffer
  REDIM AS INTEGER scrbuf(AX0*AY0)

  'Previous edition mode
  STATIC AS INTEGER prevmode = -1  
  STATIC AS INTEGER prevcur1 = -1
  STATIC AS INTEGER prevcur2 = -1

  'Variables
  DIM AS INTEGER i,j,k,row,index
  DIM AS UINTEGER col,colactiv
  DIM AS STRING * 13 seltxstr 
  DIM AS STRING ovname
  DIM AS STRING ovgrp
  DIM AS STRING ovactiv
  DIM AS STRING evtstr
  DIM AS STRING actstr
  DIM AS INTEGER separ,finish,found
  DIM AS STRING map1,map2
  DIM AS INTEGER x1,y1,x2,y2,way
  DIM AS INTEGER value
   
  'Initialize infobox if edition mode has changed
  IF prevmode <> mode OR forceinfo = 1 _
  OR cursmode1 <> prevcur1 OR cursmode2 <> prevcur2 _
  OR mode = EDIT_LNK  OR mode = EDIT_EVT THEN
    
    'Clear force refresh flag
    forceinfo = 0
    
    'Restore background
    SCREENSET 2,0
    GET (0,0)-(WINDOWPX-1,AY0-1), scrbuf
    SCREENSET scrn,0
    PUT (0,0), scrbuf, PSET
    IF mode = EDIT_EVT THEN
      SCREENSET 2,0
      SELECT CASE actwindow
        CASE 1: GET (WINDOWPX,0)-(AX0-1,WINDOWPY+WINACTLS-1), scrbuf
        CASE 2: GET (WINDOWPX,0)-(AX0-1,WINDOWPY+WINACTLB-1), scrbuf
      END SELECT
      SCREENSET scrn,0
      PUT (WINDOWPX,0), scrbuf, PSET
    ENDIF
    
    'Init screen
    PrintStr FCol(00), FRow(00), "DARK CASTLE Map editor", COL_TEXT
    IF mode = EDIT_MAP THEN
      PrintStr FCol(00), FRow(03), "Map center:", COL_TEXT
      PrintStr FCol(00), FRow(04), "Map size..:", COL_TEXT
      PrintStr FCol(00), FRow(05), "Cell size.:", COL_TEXT
      PrintStr FCol(00), FRow(06), "Entry map  ", COL_TEXT
      PrintStr FCol(00), FRow(08), "Map name..: ", COL_TEXT
    ELSEIF mode = EDIT_CEL THEN
      PrintStr FCol(00), FRow(03), "Floor:", COL_TEXT
      PrintStr FCol(00), FRow(04), "Ceil :", COL_TEXT
      PrintStr FCol(11), FRow(03), "Height:", COL_TEXT
      PrintStr FCol(11), FRow(04), "ObjHgt:", COL_TEXT
      PrintStr FCol(00), FRow(06), "Floor base", COL_TEXT
      PrintStr FCol(00), FRow(18), "Ceil.left", COL_TEXT
      PrintStr FCol(00), FRow(30), "Mid.Left", COL_TEXT
      PrintStr FCol(00), FRow(42), "Floor left", COL_TEXT
      PrintStr FCol(12), FRow(06), "Ceil.base", COL_TEXT
      PrintStr FCol(12), FRow(18), "Ceil.right", COL_TEXT
      PrintStr FCol(12), FRow(30), "Mid.right", COL_TEXT
      PrintStr FCol(12), FRow(42), "Floor right", COL_TEXT
    ELSEIF mode = EDIT_ATR THEN
      PrintStr FCol(00), FRow(03), "Texture:", COL_TEXT
      PrintStr FCol(00), FRow(06), "Floor base", COL_TEXT
      PrintStr FCol(00), FRow(18), "Ceil.left", COL_TEXT
      PrintStr FCol(00), FRow(30), "Mid.Left", COL_TEXT
      PrintStr FCol(00), FRow(42), "Floor left", COL_TEXT
      PrintStr FCol(12), FRow(06), "Ceil.base", COL_TEXT
      PrintStr FCol(12), FRow(18), "Ceil.right", COL_TEXT
      PrintStr FCol(12), FRow(30), "Mid.right", COL_TEXT
      PrintStr FCol(12), FRow(42), "Floor right", COL_TEXT
    ELSEIF mode = EDIT_OVL THEN
      PrintStr FCol(0), FRow(13), "Nr Gr A Texture        ", COL_TEXT
      PrintStr FCol(0), FRow(14), "-- -- - ---------------", COL_TEXT
    ELSEIF mode = EDIT_LIG THEN
      PrintStr FCol(00), FRow(03), "Map.Store:      DIF:", COL_TEXT
      PrintStr FCol(00), FRow(04), "Amb.Light:      APX:", COL_TEXT
      PrintStr FCol(00), FRow(05), "RGB Color:          ", COL_TEXT
    ELSEIF mode = EDIT_LNK THEN
      PrintStr FCol(00), FRow(03), "Link points:", COL_TEXT
    ELSEIF mode = EDIT_EVT THEN
      PrintStr FCol(09), FRow(03), "Cell", COL_TEXT
      PrintStr FCol(09), FRow(05), "XPos.:", COL_TEXT
      PrintStr FCol(09), FRow(06), "YPos.:", COL_TEXT
      PrintStr FCol(00), FRow(13), "Events", COL_TEXT
      PrintStr FCol(00), FRow(15), "Id Alias   Type    Val", COL_TEXT
      PrintStr FCol(00), FRow(16), "-- ------- ------- ---", COL_TEXT
      LINE (FCol(00)-2,FRow(03)-4)-(FCol(023),FRow(012)),RGB(80,80,80),B
      LINE (FCol(00)-2,FRow(13)-4)-(FCol(023),FRow(053)),RGB(80,80,80),B
      LINE (FCol(24)-2,FRow(03)-4)-(FCol(104),FRow(03+actwlines)),RGB(80,80,80),B
    ENDIF
 
  ENDIF

  'File name
  PrintStrBkg FCol(0), FRow(1),STRING(23," "),COL_TEXT,1,2
  IF changes = 0 THEN
    PrintStrBkg FCol(0), FRow(1), "File:" + mapname, COL_TEXT, scrn, 2
  ELSE
    PrintStrBkg FCol(0), FRow(1), "File:[*]" + mapname, COL_TEXT, scrn, 2
  ENDIF
  
  'Print information box for mode 1 (map)
  IF mode = EDIT_MAP THEN
    PrintStrBkg FCol(12), FRow(3),FORMAT(rattr.px,"000")+"x"+FORMAT(rattr.py,"000"), COL_TEXT,scrn,2
    PrintStrBkg FCol(12), FRow(4),FORMAT(rattr.ax,"000")+"x"+FORMAT(rattr.ay,"000"), COL_TEXT,scrn,2
    PrintStrBkg FCol(12), FRow(5),FORMAT(rattr.cellsize,"00"), COL_TEXT,1,2
    PrintStrBkg FCol(12), FRow(6),InfoCheckbox(rattr.epmap), COL_TEXT,1,2
    PrintStrBkg FCol(00), FRow(9),STRING(LEN(rattr.mname)," "),COL_TEXT,1,2
    IF rattr.mname = "" THEN
      PrintStrBkg FCol(00), FRow(9),"(no description)",COL_TEXT,1,2
    ELSE
      PrintStrBkg FCol(00), FRow(9),rattr.mname, COL_TEXT,1,2
    ENDIF

  'Print information box for mode 3 (map cells)
  ELSEIF mode = EDIT_CEL THEN
    TexLoaderConfig 0,""
    DrawTextureInfoCell 2, cmap(x,y).texfb,0,7
    DrawTextureInfoCell 1, cmap(x,y).texcl,0,19
    DrawTextureInfoCell 1, cmap(x,y).texml,0,31
    DrawTextureInfoCell 1, cmap(x,y).texfl,0,43
    DrawTextureInfoCell 2, cmap(x,y).texcb,12,7
    DrawTextureInfoCell 1, cmap(x,y).texcr,12,19
    DrawTextureInfoCell 1, cmap(x,y).texmr,12,31
    DrawTextureInfoCell 1, cmap(x,y).texfr,12,43
    PrintStrBkg FCol(06), FRow(3),"   "   , COL_TEXT,scrn,2
    PrintStrBkg FCol(06), FRow(4),"   "   , COL_TEXT,scrn,2
    PrintStrBkg FCol(18), FRow(3),"     " , COL_TEXT,scrn,2
    PrintStrBkg FCol(18), FRow(4),"     " , COL_TEXT,scrn,2
    PrintStrBKg FCol(06), FRow(3),STR$(cmap(x,y).floor), COL_TEXT,scrn,2   
    PrintStrBkg FCol(06), FRow(4),STR$(cmap(x,y).ceiling), COL_TEXT,scrn,2
    PrintStrBkg FCol(18), FRow(3),STR$(cmap(x,y).height), COL_TEXT,scrn,2
    PrintStrBkg FCol(18), FRow(4),FORMAT(cmap(x,y).objheight,"00.0"), COL_TEXT,scrn,2

  'Print information box for mode 4 (texture attributes)
  ELSEIF mode = EDIT_ATR THEN
    SELECT CASE seltexture
      CASE 0: seltxstr = "(none)"
      CASE 1: seltxstr = "Floor base"
      CASE 2: seltxstr = "Ceiling base"
      CASE 3: seltxstr = "Ceiling left"
      CASE 4: seltxstr = "Ceiling right"
      CASE 5: seltxstr = "Middle left"
      CASE 6: seltxstr = "Middle right"
      CASE 7: seltxstr = "Floor left"
      CASE 8: seltxstr = "Floor right"
    END SELECT
    TexLoaderConfig 0,""
    DrawTextureInfoAttr 2, cmap(x,y).texfb, seltexture, -9999, -9999, cmap(x,y).confb, -1, 0, 7                        
    DrawTextureInfoAttr 1, cmap(x,y).texcl, seltexture, cmap(x,y).ofclx, cmap(x,y).ofcly, cmap(x,y).concl, -1, 0, 19  
    DrawTextureInfoAttr 1, cmap(x,y).texml, seltexture, cmap(x,y).ofmlx, cmap(x,y).ofmly, cmap(x,y).conml, cmap(x,y).pasml, 0, 31    
    DrawTextureInfoAttr 1, cmap(x,y).texfl, seltexture, cmap(x,y).offlx, cmap(x,y).offly, cmap(x,y).confl, -1, 0, 43    
    DrawTextureInfoAttr 2, cmap(x,y).texcb, seltexture, -9999, -9999, cmap(x,y).concb, -1, 12, 7                     
    DrawTextureInfoAttr 1, cmap(x,y).texcr, seltexture, cmap(x,y).ofcrx, cmap(x,y).ofcry, cmap(x,y).concr, -1, 12,19 
    DrawTextureInfoAttr 1, cmap(x,y).texmr, seltexture, cmap(x,y).ofmrx, cmap(x,y).ofmry, cmap(x,y).conmr, cmap(x,y).pasmr, 12,31  
    DrawTextureInfoAttr 1, cmap(x,y).texfr, seltexture, cmap(x,y).offrx, cmap(x,y).offry, cmap(x,y).confr, -1, 12,43   
    PrintStrBKg FCol(8), FRow(3),"             ", COL_TEXT,scrn,2   
    PrintStrBKg FCol(8), FRow(3),seltxstr, COL_TEXT,scrn,2   
  
  'Print information box for mode 5 (overlay textures)
  ELSEIF mode = EDIT_OVL THEN
    TexLoaderConfig 0,""
    DrawTextureInfoOvl ovtex(selovtex),0,3
    FOR i=0 TO MAXOVTEX - 1
      SELECT CASE ovtex(i).used
        CASE 0: ovgrp   = "--"         
                ovname  = "(Not used)     "    
                ovactiv = " "    
                IF i >= selovtex AND i <= selovtex + selovrows THEN 
                  col = RGB(255,255,255)
                ELSE 
                  col = RGB(130,130,130)
                ENDIF
        CASE 1: ovgrp  = FORMAT(ovtex(i).group,"00") 
                ovname = ovtex(i).texname
                ovactiv = CHR$(127)    
                IF ovname = "" THEN ovname = "(Empty)        "    
                IF i >= selovtex AND i <= selovtex + selovrows THEN 
                  col = RGB(255,50,50)
                ELSE 
                  col = RGB(170,50,50)
                ENDIF
      END SELECT
      SELECT CASE ovtex(i).active
        CASE 0: colactiv = RGB(200,50,50)
        CASE 1: colactiv = RGB(50,255,50)
      END SELECT
      PrintStrBkg FCol(0), FRow(15+i), FORMAT(i,"00"), col,scrn,2
      PrintStrBkg FCol(3), FRow(15+i), ovgrp, col,scrn,2
      PrintStrBkg FCol(6), FRow(15+i), ovactiv, colactiv,scrn,2
      PrintStrBkg FCol(8), FRow(15+i), ovname, col,scrn,2
    NEXT i
    SELECT CASE cursmode1
      CASE CURSOR_OVTEX: 
        LINE (FCol(0)-2,FRow(03)-4)-(FCol(23),FCol(18))  ,RGB(150,150,150),B
        PrintStrBkg FCol(25), FRow(50), "Cursor mode: Texture", COL_TEXT,scrn,1
      CASE CURSOR_OVLIS: 
        LINE (FCol(0)-2,FRow(13)-4)-(FCol(23),FCol(79)+3),RGB(150,150,150),B
        PrintStrBkg FCol(25), FRow(50), "Cursor mode: List", COL_TEXT,scrn,1
    END SELECT

  'Print information box for mode 6 (lights)
  ELSEIF mode = EDIT_LIG THEN
    IF lightsel = 99 THEN
      col = RGB(255,50,50)
    ELSE
      col = RGB(170,50,50)
    ENDIF
    PrintStrBkg FCol(10), FRow(3), InfoCheckbox(rattr.lmapstore), col,scrn,2
    PrintStrBkg FCol(20), FRow(3),FORMAT(rattr.lmapdifuse,"00"), col,scrn,2
    PrintStrBkg FCol(10), FRow(4),FORMAT(rattr.lightambl,"0.00"), col,scrn,2
    PrintStrBkg FCol(20), FRow(4),FORMAT(rattr.lightaprox,"00"), col,scrn,2
    PrintStrBkg FCol(10), FRow(5),FORMAT(rattr.lightambr,"000") + "," + FORMAT(rattr.lightambg,"000") + "," + FORMAT(rattr.lightambb,"000"), col,scrn,2
    FOR i=0 TO MAXLIGHT-1
      IF i = lightsel AND light(i).used = 1 THEN 
        col = RGB(255,50,50)
      ELSEIF i = lightsel AND light(i).used = 0 THEN 
        col = RGB(255,255,255)
      ELSEIF i <> lightsel AND light(i).used = 1 THEN 
        col = RGB(170,50,50)
      ELSEIF i <> lightsel AND light(i).used = 0 THEN 
        col = RGB(130,130,130)
      ENDIF
      PrintStrBkg FCol(0), FRow(7+i*5+0),"Active" + InfoCheckbox(light(i).used) + "  Lum:" + FORMAT(light(i).lum,"0.00"), col,scrn,2
      PrintStrBkg FCol(0), FRow(7+i*5+1),"X:" + FORMAT(light(i).x,"00") + "(" + FORMAT(light(i).dx,"+00") + ") ", col,scrn,2
      PrintStrBkg FCol(0), FRow(7+i*5+2),"Y:" + FORMAT(light(i).y,"00") + "(" + FORMAT(light(i).dy,"+00") + ") ", col,scrn,2
      PrintStrBkg FCol(0), FRow(7+i*5+3),"Z:" + FORMAT(light(i).z,"00") + "(" + FORMAT(light(i).dz,"+00") + ") ", col,scrn,2
      PrintStrBkg FCol(11), FRow(7+i*5+1),"R:" + FORMAT(light(i).r,"000"), col,scrn,2
      PrintStrBkg FCol(11), FRow(7+i*5+2),"G:" + FORMAT(light(i).g,"000"), col,scrn,2
      PrintStrBkg FCol(11), FRow(7+i*5+3),"B:" + FORMAT(light(i).b,"000"), col,scrn,2
      LINE (FCol(16)+1,FRow(7+i*5+1))-STEP(39,5),RGB(180,0,0),B
      LINE (FCol(16)+1,FRow(7+i*5+2))-STEP(39,5),RGB(0,180,0),B
      LINE (FCol(16)+1,FRow(7+i*5+3))-STEP(39,5),RGB(0,0,180),B
      LINE (FCol(16)+2,FRow(7+i*5+1)+1)-STEP(37,3),RGB(0,0,0),BF
      LINE (FCol(16)+2,FRow(7+i*5+2)+1)-STEP(37,3),RGB(0,0,0),BF
      LINE (FCol(16)+2,FRow(7+i*5+3)+1)-STEP(37,3),RGB(0,0,0),BF
      LINE (FCol(16)+2,FRow(7+i*5+1)+1)-STEP(37*light(i).r/255,3),RGB(150,0,0),BF
      LINE (FCol(16)+2,FRow(7+i*5+2)+1)-STEP(37*light(i).g/255,3),RGB(0,150,0),BF
      LINE (FCol(16)+2,FRow(7+i*5+3)+1)-STEP(37*light(i).b/255,3),RGB(0,0,150),BF
    NEXT i
  
  'Print information box for mode 6 (Link points)
  ELSEIF mode = EDIT_LNK THEN
    row = 5
    IF rattr.depx = x AND rattr.depy = y THEN
      col = RGB(255,255,255)
    ELSE
      col = RGB(160,160,160)
    ENDIF
    LINE (FCol(0),FRow(row+0))-STEP(5,5),RGB(255,255,0),BF
    PrintStrBkg FCol(01), FRow(row+0), "[" + FORMAT(rattr.depx,"0") + "," + FORMAT(rattr.depy,"0") + "] Default entry  ", col, scrn, 2
    row = row + 2
    LINE (FCol(0),FRow(row-1))-STEP(WINDOWPX-7,0),RGB(180,180,180),BF
    _LinkListStart(mapname)
    _LinkListNext(map1,x1,y1,map2,x2,y2,way,finish)
    WHILE finish = 0
      IF x1 = x AND y1 = y THEN
        col = RGB(255,255,255)
      ELSE
        col = RGB(160,160,160)
      ENDIF
      IF way = 1 THEN 'One way exit
        LINE (FCol(0),FRow(row+0))-STEP(5,5),RGB(0,0,255),BF
        PrintStrBkg FCol(01), FRow(row+0), "[" + FORMAT(x1,"0") + "," + FORMAT(y1,"0") + "] Exit point  ", col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+1), "Map.:" + map2, col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+2), "Cell:(" + FORMAT(x2,"0") + "," + FORMAT(y2,"0") + ")", col, scrn, 2
        row = row + 4
        separ = 1
      ELSEIF way = -1 THEN 'One way entry
        LINE (FCol(0),FRow(row+0))-STEP(5,5),RGB(255,0,0),BF
        PrintStrBkg FCol(01), FRow(row+0), "[" + FORMAT(x1,"0") + "," + FORMAT(y1,"0") + "] Entry point  ", col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+1), "Map.:" + map2, col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+2), "Cell:(" + FORMAT(x2,"0") + "," + FORMAT(y2,"0") + ")", col, scrn, 2
        row = row + 4
        separ = 1
      ELSEIF way = 0 THEN 'Two way link
        LINE (FCol(0),FRow(row+0))-STEP(5,5),RGB(0,255,0),BF
        PrintStrBkg FCol(01), FRow(row+0), "[" + FORMAT(x1,"0") + "," + FORMAT(y1,"0") + "] 2-way link  ", col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+1), "Map.:" + map2, col, scrn, 2
        PrintStrBkg FCol(00), FRow(row+2), "Cell:(" + FORMAT(x2,"0") + "," + FORMAT(y2,"0") + ")", col, scrn, 2
        row = row + 4
        separ = 1
      ENDIF
      IF separ = 1 THEN LINE (FCol(0),FRow(row-1))-STEP(WINDOWPX-7,0),RGB(180,180,180),BF
      _LinkListNext(map1,x1,y1,map2,x2,y2,way,finish)
    WEND
  
  'Print information box for mode 7 (events)
  ELSEIF mode = EDIT_EVT THEN
    SELECT CASE cursmode2
      CASE CURSOR_EVCEL: 
        LINE (FCol(00)-2,FRow(03)-4)-(FCol(023),FRow(012)),RGB(255,0,0),B
        PrintStrBkg FCol(25), FRow(50), "Cursor mode: Cells", COL_TEXT,scrn,1
      CASE CURSOR_EVLIS: 
        LINE (FCol(00)-2,FRow(13)-4)-(FCol(023),FRow(053)),RGB(255,0,0),B
        PrintStrBkg FCol(25), FRow(50), "Cursor mode: Events", COL_TEXT,scrn,1
      CASE CURSOR_EVACT: 
        LINE (FCol(24)-2,FRow(03)-4)-(FCol(104),FRow(03+actwlines)),RGB(255,0,0),B
        PrintStrBkg FCol(25), FRow(50), "Cursor mode: Actions", COL_TEXT,scrn,1
    END SELECT
    CellSample(25,AY0-93,rattr,cmap(x,y))
    PrintStrBkg FCol(15), FRow(05), STR$(x), COL_TEXT, scrn, 1
    PrintStrBkg FCol(15), FRow(06), STR$(y), COL_TEXT, scrn, 1
    IF cmap(x,y).eventid <> 0 THEN
      found = 0
      FOR i=0 TO MAXEVENT-1
        IF event(i).id = cmap(x,y).eventid THEN
          index = i
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        PrintStrBkg FCol(09), FRow(08), "ID...:" + STR$(event(index).id), COL_TEXT, scrn, 1
        PrintStrBkg FCol(09), FRow(09), "Alias:" + event(index).evname, COL_TEXT, scrn, 1
        PrintStrBkg FCol(09), FRow(10), "Type.:" + event(index).evtype, COL_TEXT, scrn, 1
      ELSE
        PrintStrBkg FCol(09), FRow(08), "             ", COL_TEXT, scrn, 1
        PrintStrBkg FCol(09), FRow(09), " (wrong id)  ", COL_TEXT, scrn, 1
        PrintStrBkg FCol(09), FRow(10), "             ", COL_TEXT, scrn, 1
      ENDIF
    ELSE
      PrintStrBkg FCol(09), FRow(08), "             ", COL_TEXT, scrn, 1
      PrintStrBkg FCol(09), FRow(09), " (no events) ", COL_TEXT, scrn, 1
      PrintStrBkg FCol(09), FRow(10), "             ", COL_TEXT, scrn, 1
    ENDIF
    IF rattr.numevt <> 0 THEN
      FOR i=0 TO rattr.numevt-1
        EventPrint(event(i),evtstr)
        IF found = 1 AND index = i AND cursmode2 = CURSOR_EVCEL THEN
          PrintStrBCol FCol(00), FRow(17+i), evtstr, COL_TEXT, RGB(130,0,0)
        ELSEIF cursmode2 = CURSOR_EVCEL AND i = selevent AND selevrows = 0 THEN 
          PrintStrBCol FCol(00), FRow(17+i), evtstr, COL_TEXT, RGB(0,130,0)
        ELSEIF cursmode2 = CURSOR_EVLIS AND i = selevent AND selevrows = 0 THEN 
          PrintStrBCol FCol(00), FRow(17+i), evtstr, COL_TEXT, RGB(0,130,0)
        ELSEIF cursmode2 = CURSOR_EVLIS AND i >= selevent AND i <= selevent + selevrows THEN 
          PrintStrBCol FCol(00), FRow(17+i), evtstr, COL_TEXT, RGB(130,130,0)
        ELSE
          PrintStrBkg FCol(00), FRow(17+i), evtstr, COL_TEXT,scrn,1
        ENDIF
      NEXT i
    ELSE
      PrintStrBkg FCol(00), FRow(18), "  (press insert to  ", COL_TEXT, scrn, 1
      PrintStrBkg FCol(00), FRow(19), "  create new events)", COL_TEXT, scrn, 1
    ENDIF
    IF rattr.numact <> 0 THEN
      FOR i=0 TO actwlines-1
        ActionPrint(actio(i+actwtop),actstr)
        IF found = 1 AND actio(i+actwtop).oper = ACT_EVENT _
        AND VALINT(actio(i+actwtop).parm(0)) = event(index).id _
        AND cursmode2 = CURSOR_EVCEL THEN
          PrintStrBCol FCol(24), FRow(03+i), actstr, COL_TEXT, RGB(130,0,0)
        ELSEIF cursmode2 = CURSOR_EVACT AND i+actwtop = selactio AND selacrows = 0 THEN 
          PrintStrBCol FCol(24), FRow(03+i), actstr, COL_TEXT, RGB(0,130,0)
        ELSEIF cursmode2 = CURSOR_EVACT AND i+actwtop >= selactio AND i+actwtop <= selactio + selacrows THEN 
          PrintStrBCol FCol(24), FRow(03+i), actstr, COL_TEXT, RGB(130,130,0)
        ELSE
          PrintStrBkg FCol(24), FRow(03+i), actstr, COL_TEXT,scrn,1
        ENDIF
        IF actwtop+i >= rattr.numact-1 THEN EXIT FOR
      NEXT i
    ELSE
      PrintStrBkg FCol(24), FRow(04), "  (empty script,press ", COL_TEXT, scrn, 1
      PrintStrBkg FCol(24), FRow(05), "    insert to start)  ", COL_TEXT, scrn, 1
    ENDIF
  ENDIF

  'Save previous edition mode
  prevmode = mode  
  prevcur1 = cursmode1
  prevcur2 = cursmode2

END SUB

'Draw single texture (for info box and mode cells)
SUB DrawTextureInfoCell(TexDir AS INTEGER, Texture AS STRING, x AS INTEGER, y AS INTEGER)
 
  'Variables
  DIM AS INTEGER ax,ay
  DIM AS UINTEGER PTR ipt
  DIM AS STRING texname

  'Clear box
  LINE (FCol(x),FRow(y)+3)-STEP(63,63),0,BF
  PrintStrBkg FCol(x),FRow(y+8), "          ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x),FRow(y+9), "          ",COL_TEXT, 1, 2
  
  'Information for mode 3 (cells)
  IF Texture <> "" THEN
    texname = MID$(Texture,1,LEN(Texture)-4)
    ReadTexture TexDir, Texture, ax, ay, ipt
    ImgDisplayClip FCol(x), FRow(y)+3, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
    PrintStrBkg FCol(x),FRow(y+8), texname,COL_TEXT, 1, 2
    PrintStrBkg FCol(x),FRow(y+9), STR$(ax) + "x" + STR$(ay) + "   ",COL_TEXT, 1, 2
  ELSE
    LINE (FCol(x),FRow(y)+3)-STEP(63,63),RGB(0,0,40),BF
  ENDIF

END SUB

'Draw single texture (for info box and mode attributes)
SUB DrawTextureInfoAttr(TexDir AS INTEGER, Texture AS STRING, seltexture AS INTEGER, ofx AS SHORT, ofy AS SHORT, cont AS SHORT, pass AS SHORT, x AS INTEGER, y AS INTEGER)
 
  'Variables
  DIM AS INTEGER ax,ay
  DIM AS UINTEGER PTR ipt
  DIM AS STRING * 4 contflg
  DIM AS STRING * 4 passflg
  DIM AS STRING * 11 offset

  'Clear box
  LINE (FCol(x),FRow(y)+3)-STEP(63,63),0,BF
  PrintStrBkg FCol(x),FRow(y+8), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x),FRow(y+9), "           ",COL_TEXT, 1, 2
  
  'Information for mode 4 (attributes)
  IF Texture <> "" THEN
    SELECT CASE cont
      CASE +0: contflg = "____"
      CASE +1: contflg = "cont"
      CASE -1: contflg = "    "
    END SELECT
    SELECT CASE pass
      CASE +0: passflg = "____"
      CASE +1: passflg = "pass"
      CASE -1: passflg = "    "
    END SELECT
    IF ofx <> -9999 AND ofy <> -9999 THEN
      offset = "off:" + FORMAT(ofx,"0") + "," + FORMAT(ofy,"0")
    ELSE
      offset = "           "
    ENDIF
    ReadTexture TexDir, Texture, ax, ay, ipt
    ImgDisplayClip FCol(x), FRow(y)+3, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
    PrintStrBkg FCol(x),FRow(y+8), contflg + " " + passflg,COL_TEXT, 1, 2
    PrintStrBkg FCol(x),FRow(y+9), offset,COL_TEXT, 1, 2
  ELSE
    LINE (FCol(x),FRow(y)+3)-STEP(63,63),RGB(0,0,40),BF
  ENDIF

END SUB

'Draw single texture (for info box and mode overlay)
SUB DrawTextureInfoOvl(ovtex AS mapovtex, x AS INTEGER, y AS INTEGER)
 
  'Variables
  DIM AS INTEGER ax,ay
  DIM AS INTEGER PTR ipt
  DIM AS STRING texname
  DIM AS UINTEGER col
  DIM AS STRING used
  DIM AS STRING txdir
  DIM AS STRING txplane

  'Clear box
  LINE (FCol(x)+3,FRow(y+2)-6)-STEP(63,63),0,BF
  PrintStrBkg FCol(x),FRow(y), "                       ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+1), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+2), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+3), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+4), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+5), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+6), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+7), "           ",COL_TEXT, 1, 2
  PrintStrBkg FCol(x+12),FRow(y+8), "           ",COL_TEXT, 1, 2
  
  'Information for mode 3 (cells)
  IF ovtex.texname <> "" THEN
    SELECT CASE ovtex.used
      CASE 1: used = "Yes": col = RGB(255,50,50)
      CASE 0: used = "No ": col = RGB(255,255,255)
    END SELECT
    SELECT CASE ovtex.texdir
      CASE 1: txdir = "Wall"
      CASE 1: txdir = "Ground"
    END SELECT
    SELECT CASE ovtex.plane
      CASE ISOXPLANE: txplane = "X    "
      CASE ISOYPLANE: txplane = "Y    "
      CASE ISOZPLANE: txplane = "Z    "
      CASE ISOFPLANE: txplane = "45deg"
    END SELECT
    texname = MID$(ovtex.texname,1,LEN(ovtex.texname)-4)
    ReadTexture ovtex.texdir, ovtex.texname, ax, ay, ipt
    PrintStrBkg FCol(x),FRow(y+0), ovtex.texname + " (" + STR$(ax) + "x" + STR$(ay) + ")",col, 1, 2
    ImgDisplayClip FCol(x)+3, FRow(y+2)-6, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
    PrintStrBkg FCol(x+12),FRow(y+1), "TDir:" + txdir,col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+2), "Plan:" + txplane,col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+3), "Size:" + FORMAT(ovtex.ax,"00") + "x" + FORMAT(ovtex.ay,"00"),col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+4), "Grp.:" + STR$(ovtex.group), col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+5), "Used:" + used, col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+6), "X:" + FORMAT(ovtex.x,"00") + "(" + FORMAT(ovtex.dx,"00") + ")",col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+7), "Y:" + FORMAT(ovtex.y,"00") + "(" + FORMAT(ovtex.dy,"00") + ")",col, 1, 2
    PrintStrBkg FCol(x+12),FRow(y+8), "Z:" + FORMAT(ovtex.z,"00") + "(" + FORMAT(ovtex.dz,"00") + ")",col, 1, 2
  ELSE
    LINE (FCol(x)+3,FRow(y+2)-6)-STEP(63,63),RGB(0,0,40),BF
  ENDIF

END SUB

'Print display mode
SUB PrintDispMode
  
  'Variables
  DIM AS INTEGER xc
  DIM AS INTEGER i
  DIM AS INTEGER mode
  DIM AS UINTEGER col
  DIM AS STRING mst
  
  'Texture mode
  xc = 39
  mode = GetDispMode(TEXT_MODE)
  FOR i=0 TO 5
    SELECT CASE i
      CASE 0: mst = "Textures:": col = COL_TEXT
      CASE 1: mst = "On"   : IF mode=MODE_ON    THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 2: mst = "Fix"  : IF mode=MODE_FIXED THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 3: mst = "Sol"  : IF mode=MODE_SOLID THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 4: mst = "Box"  : IF mode=MODE_BOX   THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 5: mst = "__"   : IF mode=MODE_OFF   THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
    END SELECT
    PrintStrBkg FCol(xc), FRow(0),mst,col,1,2
    xc = xc + LEN(mst)
  NEXT i
  
  'Object mode
  xc = xc + 2
  mode = GetDispMode(OBJS_MODE)
  FOR i=0 TO 5
    SELECT CASE i
      CASE 0: mst = "Objects:": col = COL_TEXT
      CASE 1: mst = "On"   : IF mode = MODE_ON    THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 2: mst = "Fix"  : IF mode = MODE_FIXED THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 3: mst = "Sol"  : IF mode = MODE_SOLID THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 4: mst = "Box"  : IF mode = MODE_BOX  THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 5: mst = "__"   : IF mode = MODE_OFF  THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
    END SELECT
    PrintStrBkg FCol(xc), FRow(0),mst,col,1,2
    xc = xc + LEN(mst)
  NEXT i
  
  'light mode
  xc = xc + 2
  mode = GetDispMode(LIGHT_MODE)
  FOR i=0 TO 3
    SELECT CASE i
      CASE 0: mst = "Lights:": col = COL_TEXT
      CASE 1: mst = "Dyn"    : IF mode = MODE_LDYN  THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 2: mst = "Map"    : IF mode = MODE_LMAP  THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
      CASE 3: mst = "__"     : IF mode = MODE_OFF   THEN: col=COL_DSEL: ELSE: col=COL_NSEL: ENDIF
    END SELECT
    PrintStrBkg FCol(xc), FRow(0),mst,col,1,2
    xc = xc + LEN(mst)
  NEXT i

END SUB

'Help Screen
SUB HelpScreen(page AS INTEGER)
  
  'Variables
  DIM AS INTEGER col,row
  DIM AS INTEGER file
  DIM AS STRING helpline
  
  'Display page number at the top
  PrintStr FCol(100), FRow(2), FORMAT(page,"0") + "/" + FORMAT(HELPAGES,"0"), COL_TEXT

  'Open help file and dump on screen
  row = 2
  col = 24
  file = FREEFILE
  SELECT CASE page
    CASE 1: OPEN "help1.txt" FOR INPUT AS #file
    CASE 2: OPEN "help2.txt" FOR INPUT AS #file
    CASE 3: OPEN "help3.txt" FOR INPUT AS #file
  END SELECT
  DO
    LINE INPUT #file, helpline
    PrintStr FCol(col), FRow(row), helpline, COL_TEXT
    row = row + 1
  LOOP WHILE EOF(file) <> -1
  CLOSE #file

END SUB

'Texture browser
FUNCTION TextureBrowser(texdir AS INTEGER) AS STRING
  
  'Constants
  CONST TEXAX = 7
  CONST TEXAY = 5
  
  'Variables
  STATIC AS INTEGER xd(4),yd(4),topd(4),catd(4)
  STATIC AS INTEGER xdi,ydi,topdi
  STATIC AS STRING folder,previmpdir
  STATIC AS INTEGER texprv,catprv
  STATIC AS STRING prefix
  STATIC AS INTEGER scale
  DIM AS STRING importdir
  DIM AS STRING cname,catname
  DIM AS INTEGER x,y,top,txd,cat
  DIM AS INTEGER ax,ay,atop,atxd,acat
  DIM AS INTEGER i,j,k,numtex, totnumtex
  DIM AS STRING filename
  DIM AS INTEGER file
  DIM AS STRING pipecom
  DIM AS INTEGER code,ecode,finish,redraw,browser,reselect
  DIM AS INTEGER objimport
  REDIM AS STRING * 15 textable(MAXTEXTUR)
  DIM AS INTEGER tax, tay
  DIM AS UINTEGER PTR ipt
    
  'Default texture loader configuration
  TexLoaderConfig 0,""

  'Call from texture browser?
  IF texdir = 0 THEN 
    browser = 1
  ELSE
    browser = 0
  ENDIF

  'Set menu bar
  IF browser = 1 THEN PrintMenuBar MBAR,MENUTXB1,0
  IF browser = 0 THEN PrintMenuBar MBAR,MENUTXB2,0

  'Get directory
  IF browser = 1 THEN 
    txd = texprv
  ELSE
    txd = texdir
  ENDIF
  IF txd = 0 THEN txd = 1
  cat = catprv
  
  'Read texture classification file
  TexClasif "READ", 0, "", "",0
  
  'Initialize screen
  LINE (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
  PrintStrBkg FCol(25), FRow(0), SPACEBAR, COL_TEXT, 0, 2
  PrintStrBkg FCol(25), FRow(0), TexMemGetSpace, COL_TEXT, 0, 2

  'Texture selection loop
  finish = 0: atop = 0: ax = 0:ay = 0: atxd = 0: acat = 0
  DO
    
    'Update directory
    IF txd <> atxd OR cat <> acat OR reselect = 1 THEN
    
      'Get parameters for directory
      IF importdir = "" THEN
        IF txd <> atxd THEN
          x = xd(txd): y = yd(txd): top = topd(txd): cat = catd(txd)
        ENDIF
        folder = GetTexPath(txd)
        cname = GetTexCat(cat)
        PrintStrBkg FCol(25), FRow(0), SPACEBAR, COL_TEXT, 0, 2
        PrintStrBkg FCol(40), FRow(0), "Texture Directory:" + folder + STRING(30-LEN(folder)," "), COL_TEXT, 0, 2
      ELSE
        IF previmpdir = importdir THEN
          x=xdi:y=ydi:top=topdi
        ELSE
          x=0:y=0:top=0
        ENDIF
        PrintStrBkg FCol(25), FRow(0), SPACEBAR, COL_TEXT, 0, 2
        PrintStrBkg FCol(40), FRow(0), "Import Dir:" + importdir + STRING(40-LEN(importdir)," "), COL_TEXT, 0, 2
      ENDIF
      PrintStrBkg FCol(25), FRow(0), TexMemGetSpace, COL_TEXT, 0, 2

      'Read texture directory
      k = 0
      totnumtex = 0
      file = FREEFILE
      IF importdir = "" THEN
        pipecom = "dir " + folder + "*.pcx" + " " + folder + "*.px2" + " /b /on"
      ELSE
        pipecom = "dir " + importdir + "*.pcx" + " /a /b /on"
      ENDIF
      OPEN PIPE pipecom FOR INPUT AS #file 
      WHILE NOT EOF(file) AND k < MAXTEXTUR
        LINE INPUT #file, filename
        IF cat > 0 AND importdir = "" THEN TexClasif "GET",txd,filename,catname,0
        IF ( importdir <> "" AND filename <> "" ) _
        OR ( cat = 0 AND filename <> "" ) _
        OR ( cat = 99 AND filename <> "" AND catname = "" ) _
        OR ( cat > 0 AND cat < 99 AND cname = catname ) THEN
          textable(k) = filename
          k = k + 1
        ENDIF
        IF filename <> "" THEN totnumtex = totnumtex + 1
      WEND
      CLOSE #file
      numtex = k
      
      'Adjust cursor
      IF (y*TEXAX + top + x) > numtex - 1 THEN
        top = 0: x = 0: y = 0
      ENDIF

      'Clear selection flag
      reselect = 0
      
      'Redraw screen
      redraw = 1
    
    ENDIF
  
    'Redraw screen
    IF top <> atop OR redraw = 1 THEN
      LINE (WINDOWPX,WINDOWPY + 22)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
      IF importdir <> "" THEN 
        PrintStrBkg FCol(97), FRow(0), "Scale:" + STR$(scale), COL_TEXT, 0, 2
      ENDIF
      k = top
      FOR j = 0 TO TEXAY - 1
      FOR i = 0 TO TEXAX - 1
        IF k < numtex THEN
          IF importdir = "" THEN
            PrintStrBkg FCol(25), FRow(0), TexMemGetSpace, COL_TEXT, 0, 2
            TexClasif "GET",txd,textable(k),cname,0
            DrawTextureBrowser txd, textable(k),cname,i,j,0
          ELSE
            PrintStrBkg FCol(25), FRow(0), TexMemGetSpace, COL_TEXT, 0, 2
            DrawTextureBrowser txd, textable(k),"",i,j,0
          ENDIF
          k = k + 1
        ELSE
          EXIT FOR
        ENDIF
      NEXT i
      NEXT j
      DrawTextureBrowser txd, "","",x,y,1
    ENDIF
    
    'Redraw cursor
    IF ax <> x OR ay <> y OR redraw = 1 THEN
      DrawTextureBrowser txd, "","",ax,ay,2
      DrawTextureBrowser txd, "","",x,y,1
    ENDIF
   
   'Print categories menu
    IF importdir = "" AND ( redraw = 1 OR reselect = 1 ) THEN
      PrintCatMenu cat,txd,totnumtex,0
    ENDIF

    'Clear flags
    redraw = 0
    reselect = 0
  
    'Save static screen parameters
    xd(txd) = x: yd(txd) = y: topd(txd) = top: texprv = txd: catd(txd) = cat
  
    'Save previous cursor location
    ax = x: ay = y: atop = top: atxd = txd: acat = cat
   
    'Get keyboard
    code = GetKeyb(1)
    
    'Switch on key
    SELECT CASE code
    
      CASE 13 'Enter -> select texture
        IF textable(y*TEXAX + top + x) <> "" _
        AND browser = 0 AND importdir = "" THEN
          finish = 1
          TextureBrowser = textable(y*TEXAX + top + x)
        ENDIF
        
      CASE 27 'Escape -> exit
        IF importdir = "" THEN
          finish = 1
          TextureBrowser = "ESC"
        ELSE
          previmpdir = importdir
          xdi=x:ydi=y:topdi=top
          importdir = ""
          TexLoaderConfig 0,""
          PrintMenuBar MBAR,MENUTXB1,0
          PrintCatMenu 0,0,0,1
          KillTexMemory
          IF objimport = 1 THEN
            ScanNewObjects
            SaveObjectList
          ENDIF
          reselect = 1
        ENDIF
        
      CASE -17,-31,-50,-32,-20,-34,-30,-47,-19,-35,-24 'Alt-wsmdtgavrho -> Relocate texture
        IF browser = 1 AND importdir = "" THEN
          SELECT CASE code
            CASE -17: cname = GetTexCat(01)
            CASE -31: cname = GetTexCat(02)
            CASE -50: cname = GetTexCat(03)
            CASE -32: cname = GetTexCat(04)
            CASE -20: cname = GetTexCat(05)
            CASE -34: cname = GetTexCat(06)
            CASE -30: cname = GetTexCat(07)
            CASE -47: cname = GetTexCat(08)
            CASE -19: cname = GetTexCat(09)
            CASE -35: cname = GetTexCat(10)
            CASE -24: cname = GetTexCat(11)
          END SELECT
          TexClasif "SET", txd, textable(y*TEXAX + top + x), cname,0
          redraw = 1
        ENDIF
      
      CASE -60 'F2 -> Change texture directory / Change scale
        IF browser = 1 AND importdir = "" THEN
          txd = txd + 1
          if txd > 4 THEN txd = 1
        ELSEIF browser = 1 AND importdir <> "" THEN
          IF scale = 1 THEN
            scale = 2
          ELSE
            scale = 1
          ENDIF
          KillTexMemory
          TexLoaderConfig scale,importdir
          redraw = 1
        ENDIF
        
      CASE -61 'F3 -> Change category
        IF importdir = "" THEN
          IF cat = 99 THEN
            cat = 0
          ELSE
            cat = cat + 1
            IF cat >= NUMCATEG THEN 
              cat = 99
            ENDIF
          ENDIF
        ENDIF

      CASE -62 'F4 -> Import textures
        IF browser = 1 AND importdir = "" THEN
          PrintMenuBar MBAR,SPACEBAR,0
          PrintStrBkg FCol(24),FRow(52),"Enter directory:",COL_TEXT,0,2
          IF importdir = "" THEN importdir = previmpdir
          IF importdir = "" THEN importdir = "c:\usr\pictures\games\heretic"
          importdir = GetStringBkg(40,52,60,COL_TEXT,2,importdir,ecode)
          IF importdir = "" THEN
            PrintMenuBar MBAR,MENUTXB1,0
          ELSE
            IF RIGHT$(importdir,1) <> "\" THEN importdir = importdir + "\"
            IF scale = 0 THEN scale = 1
            TexLoaderConfig scale,importdir
            PrintMenuBar MBAR,MENUTXB3,0
            PrintCatMenu 0,0,0,1
            PrintStr FCol(24), FRow(2), "Select textures to import:", COL_TEXT
            KillTexMemory
            objimport = 0
            reselect = 1
          ENDIF
        ENDIF

        CASE ASC("w"),ASC("g"),ASC("o"),ASC("e") 'wgoe -> Import textures
          PrintMenuBar MBAR,SPACEBAR,0
          PrintStrBkg FCol(24),FRow(52),"Filename prefix:",COL_TEXT,0,2
          IF prefix = "" THEN prefix = "IM"
          prefix = GetStringBkg(40,52,5,COL_TEXT,2,prefix,ecode)
          IF prefix = "" THEN prefix = ""
          PrintMenuBar MBAR,MENUTXB3,0
          IF prefix <> "" THEN
            filename = textable(y*TEXAX + top + x)
            filename = MID$(filename,1,INSTR(filename,".")-1)
            filename = prefix + filename
            IF scale = 1 THEN filename = filename + ".PCX"
            IF scale = 2 THEN filename = filename + ".PX2"
            IF LEN(filename) > 14 THEN
              PrintMenuBar MBAR,SPACEBAR,0
              PrintStrBkg FCol(24),FRow(52),"Filename is too long!",COL_TEXT,0,2
              code = GetKeyb(1)
              PrintMenuBar MBAR,MENUTXB3,0
            ELSE
              SELECT CASE code
                CASE ASC("w"): filename = GetTexPath(1) + filename
                CASE ASC("g"): filename = GetTexPath(2) + filename
                CASE ASC("o"): filename = GetTexPath(3) + filename
                CASE ASC("e"): filename = GetTexPath(4) + filename
              END SELECT
              SHELL "COPY " + importdir + textable(y*TEXAX + top + x) + " " + filename
              SHELL "ATTRIB +H " + importdir + textable(y*TEXAX + top + x)
              IF code = ASC("o") THEN objimport = 1
            ENDIF
          ENDIF
              
      CASE -83 'Delete -> exit an return null texture
        IF browser = 0 AND importdir = "" THEN
          finish = 1
          TextureBrowser = ""
        ENDIF   
        
      CASE -75 'Cursor left -> x--
        x = x - 1
        IF x < 0 THEN x = 0

      CASE -77 'Cursor right -> x++
        x = x + 1
        IF ( top + y * TEXAX + x ) > numtex - 1 THEN
          x = x - 1
        ELSEIF x > TEXAX - 1 THEN
          x = TEXAX - 1
        ENDIF
    
      CASE -72 'Cursor up -> y --
        y = y - 1
        IF y < 0 THEN
          y = 0
          top = top - TEXAX
          IF top < 0 THEN top = 0
        ENDIF
        
      CASE -80 'Cursor down -> y ++
        y = y + 1
        IF ( top + y * TEXAX + x ) > numtex - 1 THEN
          y = y - 1
        ELSEIF y > TEXAY - 1 THEN
          y = TEXAY - 1
          top = top + TEXAX
          IF ( top + TEXAY * TEXAX - TEXAX ) >= numtex THEN
            top = top - TEXAX
          ENDIF
        ENDIF
      
      CASE -73 'Page up -> y = y - TEXAY
        top = top - TEXAX * TEXAY
        IF top < 0 THEN top = 0
        
      CASE -81 'Page down -> y = y + TEXAY
        top = top + TEXAX * TEXAY
        IF ( top + TEXAY * TEXAX - TEXAX ) >= numtex THEN
          top = top - TEXAX * TEXAY
        ENDIF
      
    END SELECT
    
  LOOP WHILE finish = 0

  'Save texture clasification file
  TexClasif "WRITE", 0, "", "",0
  
END FUNCTION

' Print categories menu
SUB PrintCatMenu(cat AS INTEGER, txd AS INTEGER,numtex AS INTEGER,mode AS INTEGER)
  PrintSingleCategory 00,0,00,cat,txd,numtex,mode
  PrintSingleCategory 01,0,01,cat,txd,numtex,mode
  PrintSingleCategory 02,0,02,cat,txd,numtex,mode
  PrintSingleCategory 03,0,03,cat,txd,numtex,mode
  PrintSingleCategory 04,0,04,cat,txd,numtex,mode
  PrintSingleCategory 05,0,05,cat,txd,numtex,mode
  PrintSingleCategory 06,0,06,cat,txd,numtex,mode
  PrintSingleCategory 07,0,07,cat,txd,numtex,mode
  PrintSingleCategory 08,0,08,cat,txd,numtex,mode
  PrintSingleCategory 09,0,09,cat,txd,numtex,mode
  PrintSingleCategory 10,0,10,cat,txd,numtex,mode
  PrintSingleCategory 11,0,11,cat,txd,numtex,mode
  PrintSingleCategory 12,0,99,cat,txd,numtex,mode
END SUB

' Print single category
SUB PrintSingleCategory(x AS INTEGER, y AS INTEGER, cat AS INTEGER,catsel AS INTEGER, txd AS INTEGER, numtex AS INTEGER, mode AS INTEGER)
  
  'Variables
  STATIC AS INTEGER wocats
  DIM AS UINTEGER fcolor, bcolor
  DIM AS INTEGER px,py
  DIM AS STRING categ
  DIM AS STRING strcount
  DIM AS INTEGER count
  
  'Calculate screen coordinates
  px = WINDOWPX + 6 + x * ( 37 )
  py = WINDOWPY + 6 + y * ( 8 )
  
  'Clear and exit
  IF mode = 1 THEN
    LINE (px-3,py-3)-STEP(36,18),0,BF
    EXIT SUB
  ENDIF
  
  'Get category text
  categ = GetTexCat(cat)

  'Count items in category
  IF cat = 0 THEN
    count = numtex
    wocats = numtex
  ELSEIF cat > 0 AND cat < 99 THEN
    TexClasif "COUNT", txd, "", categ, count
    wocats = wocats - count
  ELSEIF cat = 99 THEN
    count = wocats
  ENDIF
    
  'Counter string
  IF count = 0 THEN
    strcount = ""
  ELSE
    strcount = "(" + STR$(count) + ")"
  ENDIF

  'Item colors
  IF cat <> catsel AND count = 0 THEN
    bcolor = RGB(30,30,30): fcolor = RGB(150,150,150)
  ELSEIF cat <> catsel AND count <> 0 THEN
    bcolor = RGB(60,0,0): fcolor = RGB(150,150,150)
  ELSE
    bcolor = RGB(180,0,0): fcolor = COL_TEXT
  ENDIF
  
  'Print category
  LINE (px-3,py-3)-STEP(36,18),bcolor,BF
  PrintStr px+2,py-1,categ,fcolor
  PrintStr px+2,py+7,strcount,fcolor
  IF cat = catsel THEN
    LINE (px-3,py-3)-STEP(36,18),RGB(255,0,0),B
  ENDIF

END SUB

'Draw single texture (for texture browser)
SUB DrawTextureBrowser(TexDir AS INTEGER, Texture AS STRING, cname AS STRING, x AS INTEGER, y AS INTEGER, mode AS INTEGER)
 
  'Variables
  DIM AS INTEGER ax,ay
  DIM AS INTEGER px,py
  DIM AS STRING texname,categ
  DIM AS UINTEGER PTR ipt

  'Calculate screen coordinates
  px = WINDOWPX + 04 + x * ( 64 + 5 )
  py = WINDOWPY + 26 + y * ( 64 + 22 )
  
  'Get Texture name and category
  IF mode = 0 THEN
    texname = MID$(Texture,1,LEN(Texture)-4)
    IF cname <> "" THEN
      categ = "(" + MID$(cname,1,2) + ")"
    ELSE
      categ = ""
    ENDIF
  ENDIF
  
  'Switch on mode
  SELECT CASE mode
  
    'Print texture (for texture browser)
    CASE 0
      ReadTexture TexDir, Texture, ax, ay, ipt
      DrawOccupiedMemory 0
      PrintStr px + 2,py + 1, texname,COL_TEXT
      PrintStr px + 2,py + 9, STR$(ax) + "x" + STR$(ay) + " " + categ,COL_TEXT
      ImgDisplayClip px, py + 18, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
  
    'Cursor box set
    CASE 1
      LINE (px-1,py-1)-STEP(64+1,64+19),COL_CURS,B
  
    'Cursor box clear
    CASE 2
      LINE (px-1,py-1)-STEP(64+1,64+19),0,B

  END SELECT
  
END SUB

'Set Menu Bar
SUB PrintMenuBar(mode AS INTEGER, MenuText AS STRING, cur AS INTEGER)
  SELECT CASE mode
    CASE MBAR: PrintStrBkg  FCol(24),FRow(52),SPACEBAR,COL_TEXT,cur,2
    CASE EBAR: PrintStrBCol FCol(24),FRow(52),SPACEBAR,COL_TEXT,RGB(100,000,000)
    CASE WBAR: PrintStrBCol FCol(24),FRow(52),SPACEBAR,COL_TEXT,RGB(100,100,000)
    CASE SBAR: PrintStrBCol FCol(24),FRow(52),SPACEBAR,COL_TEXT,RGB(000,100,000)
    CASE IBAR: PrintStrBCol FCol(24),FRow(52),SPACEBAR,COL_TEXT,RGB(000,000,100)
  END SELECT
  PrintStr FCol(24), FRow(52), MenuText,COL_TEXT
END SUB

'Save file 
FUNCTION FileSave( rattr AS roomattr, cmap() AS mapcell, light() AS maplight, _
                   ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio, _
                   deffile AS STRING, BYREF cancel AS INTEGER ) AS STRING
  
  'Variables
  DIM AS STRING newfile
  DIM AS INTEGER i,ecode
  
  'Clear menu bar
  PrintMenuBar MBAR,SPACEBAR,0

  'Check light updated if file has light map store flag
  IF rattr.lmapstore = 1 AND ISOLMapStatus() = 0 THEN
    PrintStrBkg FCol(24),FRow(52),"Update light map before saving this file!",COL_TEXT,0,2
    SLEEP
    FileSave = deffile
    cancel = 1
    EXIT FUNCTION
  ENDIF
  
  'Prepare screen
  PrintStrBkg FCol(24),FRow(52),"Enter filename:",COL_TEXT,0,2
  
  'Get file name
  newfile = GetStringBkg(39,52,15,COL_TEXT,2,deffile,ecode)
  IF newfile = "" THEN 
    FileSave = deffile
    cancel = 1
    EXIT FUNCTION
  ENDIF
  
  'Eliminate bars
  FOR i=0 TO LEN(newfile)-1
    IF CHR$(newfile[i]) = "\" _
    OR CHR$(newfile[i]) = "/" THEN
      newfile[i] = ASC("_")
    ENDIF
  NEXT i

 'Force .map extension
  newfile = MID$(newfile,1,INSTR(newfile,".")-1) + ".map"
  
  'Write file data
  MapWrite newfile, rattr, cmap(), light(), ovtex(), event(), actio()

  'Return file name
  FileSave = newfile
  cancel = 0
  
END FUNCTION

'Load file
FUNCTION FileLoad( rattr AS roomattr, cmap() AS mapcell, light() AS maplight, _
                   ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio, _
                   deffile AS STRING, gotomap AS STRING, BYREF cancel AS INTEGER ) AS STRING
  
  'Constants
  CONST MAXFILES = 100
  
  'Variables
  STATIC AS INTEGER x,y,top,filenr
  DIM AS INTEGER x0,y0,top0
  DIM AS INTEGER filesax,filesay,filesx,filesy
  DIM AS INTEGER ax,ay,atop
  DIM AS INTEGER i,j,k,numfil
  DIM AS STRING filename
  DIM AS INTEGER code,finish,redraw,deleted
  DIM AS INTEGER pipefile
  DIM AS roomattr rattr0
  DIM AS SINGLE devx, devy
  DIM AS INTEGER centerpx, centerpy
  REDIM AS mapcell cmap0(MAXCELLX,MAXCELLY)
  REDIM AS maplight light0(MAXLIGHT)
  REDIM AS mapovtex ovtex0(MAXOVTEX)
  REDIM AS mapevent event0(MAXEVENT)
  REDIM AS mapactio actio0(MAXACTIO)
  REDIM AS STRING * 50 files(MAXFILES)
  
  'Set map display mode
  StoreDispMode
  SetDispMode(TEXT_MODE, MODE_FIXED)
  SetDispMode(OBJS_MODE, MODE_FIXED)
  SetDispMode(LIGHT_MODE, MODE_LDYN)
  SetDispMode(ENTS_MODE, MODE_OFF)

  'Initialize number of files per row
  IF filenr = 0 THEN filenr = 3

  'Read map directory
  k = 0
  pipefile = FREEFILE
  OPEN PIPE "dir maps\*.map /b /on" FOR INPUT AS #pipefile 
  WHILE NOT EOF(pipefile) AND k < MAXFILES
    LINE INPUT #pipefile, files(k)
    k = k + 1
  WEND
  CLOSE #pipefile
  numfil = k - 1

  'Initialize cursor if go to map is set
  IF gotomap <> "" THEN
    x0 = x: y0 = y: top0 = top
    FOR k = 0 TO numfil - 1
      IF files(k) = gotomap THEN
        x = k MOD filenr
        y = 0
        top = k - x
        EXIT FOR
      ENDIF
    NEXT k
  ENDIF
  
  'Initialize screen
  LINE (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF

  'Texture selection loop
  finish=0:redraw=1:deleted=0 
  atop=0:ax=0:ay=0
  DO
    
    'Redraw screen
    IF top <> atop OR redraw = 1 THEN
      filesax = filenr
      filesay = filenr
      filesx = ((AX0-WINBORDR-WINDOWPX)/filesax)
      filesy = ((AY0-WINBORDR-WINDOWPY)/filesay)
      DO
        IF  filesx * filesax <= AX0-WINBORDR-WINDOWPX _
        AND filesy * filesay <= AY0-WINBORDR-WINDOWPY THEN
          EXIT DO
        ELSE
           IF filesx * filesax > AX0-WINBORDR-WINDOWPX THEN filesx = filesx -1
           IF filesy * filesay > AY0-WINBORDR-WINDOWPY THEN filesy = filesy -1
        ENDIF
      LOOP
      LINE (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
      k = top
      FOR j = 0 TO filesay - 1
      FOR i = 0 TO filesax - 1
        IF k < numfil THEN
          IF files(k) <> "" THEN
            IF _MapImgRestore(files(k),filenr,WINDOWPX+i*filesx,WINDOWPY+j*filesy) = 0 THEN
              ISOLMapIncBuffer()
              MapRead files(k), rattr0, cmap0(), light0(), ovtex0(), event0(), actio0()
              IF rattr0.lmapstore = 0 THEN
                SetDispMode(LIGHT_MODE, MODE_LDYN)
              ELSE
                SetDispMode(LIGHT_MODE, MODE_LMAP)
              ENDIF
              devx = (rattr0.px - WINDOWPX)/(AX0 - WINDOWPX - WINBORDR)
              devy = (rattr0.py - WINBORDR)/(AY0 - WINDOWPY - WINBORDR)
              centerpx = (WINDOWPX+(i+devx)*filesx)
              centerpy = (WINDOWPY+(filesay-j+devy-1)*filesy)
              centerpx = centerpx + (rattr0.px MOD 2)
              centerpy = centerpy + (rattr0.py MOD 2)
              ISOLMapOffset(rattr0.px-centerpx,-(rattr0.py-centerpy))
              rattr0.px = centerpx
              rattr0.py = centerpy
              VIEW SCREEN (WINDOWPX+(i+0)*filesx,WINDOWPY+(j+0)*filesy)- _ 
              (WINDOWPX-1+(i+1)*filesx,WINDOWPY-1+(j+1)*filesy)
              ISOScale(CAST(SINGLE,1/filenr))
              RoomMap rattr0, cmap0(), light0(), ovtex0(), 0
              ISOScale(1)
              ISOLMapOffset(0,0)
              ISOLMapDecBuffer()
              'PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+3,files(k),COL_TEXT
              'PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+FRow(1)+3, _ 
              'STR$(rattr0.ax)+"x"+STR$(rattr0.ay),COL_TEXT
              IF rattr0.epmap = 1 THEN CIRCLE (WINDOWPX+(i+0)*filesx+filesx-6,WINDOWPY+(j+0)*filesy+6),3,RGB(0,255,0),,,,F
              SetFont(1)
              PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+3,UCASE(files(k)),COL_TEXT
              IF rattr0.mname = "" THEN
                PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+FRow(1)+2,UCASE("(no desc.)"),COL_TEXT
              ELSE
                PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+FRow(1)+2,UCASE(rattr0.mname),COL_TEXT
              ENDIF
              PrintStr WINDOWPX+(i+0)*filesx+3,WINDOWPY+(j+0)*filesy+FRow(2)+3, _ 
              UCASE(STR$(rattr0.ax)+"x"+STR$(rattr0.ay)),COL_TEXT
              SetFont(2)
              VIEW
              LINE (WINDOWPX+i*filesx,WINDOWPY+j*filesy)-STEP(filesx,filesy),RGB(50,50,50),B
              _MapImgStore(files(k),filenr,WINDOWPX+i*filesx,WINDOWPY+j*filesy,filesx+1,filesy+1)
            ENDIF
            DrawOccupiedMemory 0
          ELSE
            LINE (WINDOWPX+i*filesx,WINDOWPY+j*filesy)-STEP(filesx,filesy),RGB(50,50,50),BF
          ENDIF
          k = k + 1
        ELSE
          EXIT FOR
        ENDIF
      NEXT i
      NEXT j
      LINE (WINDOWPX+x*filesx,WINDOWPY+y*filesy)-STEP(filesx,filesy),COL_TEXT,B
    ENDIF
    
    'Redraw cursor
    IF ax <> x OR ay <> y OR redraw = 1 OR redraw = 2 THEN
      LINE (WINDOWPX+ax*filesx,WINDOWPY+ay*filesy)-STEP(filesx,filesy),RGB(50,50,50),B
      LINE (WINDOWPX+x*filesx,WINDOWPY+y*filesy)-STEP(filesx,filesy),COL_TEXT,B
    ENDIF
    
    'Redraw menu
    IF redraw <> 0 THEN PrintMenuBar MBAR,MENUOPEN,0
    
    'Clear redraw flag
    redraw = 0
    
    'Save previous cursor location
    ax = x: ay = y: atop = top
   
    'Get keyboard
    code = GetKeyb(1)
    
    'Switch on key
    SELECT CASE code
    
      CASE 13 'Enter -> select file
        IF files(y*filesax + top + x) <> "" THEN
          finish = 1
          MapRead files(y*filesax + top + x), rattr, cmap(), light(), ovtex(), event(), actio()
          FileLoad = files(y*filesax + top + x)
          cancel = 0
          IF deleted=1 THEN 
            x=0:y=0:top=0
          ENDIF
        ENDIF
        
      CASE 27 'Escape -> exit
        finish = 1
        FileLoad = deffile
        cancel = 1
        IF deleted=1 THEN 
          x=0:y=0:top=0
        ENDIF
        
      CASE -147 'Ctrl-Del -> Delete map file
        IF deffile <> files(y*filesax + top + x) THEN
          PrintMenuBar MBAR,MENUDFIL,0
          code = GetKeyb(1)
          IF code = ASC("Y") OR code = ASC("y") OR code = 13 THEN
            deleted = 1
            KILL "maps\" + files(y*filesax + top + x)
            files(y*filesax + top + x) = ""
            LINE (WINDOWPX+x*filesx,WINDOWPY+y*filesy)-STEP(filesx,filesy),RGB(50,50,50),BF
          ENDIF
          redraw = 2
        ENDIF
        
      CASE -75 'Cursor left -> x--
        x = x - 1
        IF x < 0 THEN x = 0

      CASE -77 'Cursor right -> x++
        x = x + 1
        IF ( top + y * filesax + x ) > numfil - 1 THEN
          x = x - 1
        ELSEIF x > filesax - 1 THEN
          x = filesax - 1
        ENDIF
    
      CASE -72 'Cursor up -> y --
        y = y - 1
        IF y < 0 THEN
          y = 0
          top = top - filesax
          IF top < 0 THEN top = 0
        ENDIF
        
      CASE -80 'Cursor down -> y ++
        y = y + 1
        IF ( top + y * filesax + x ) > numfil - 1 THEN
          y = y - 1
        ELSEIF y > filesay - 1 THEN
          y = filesay - 1
          top = top + filesax
          IF ( top + filesay * filesax - filesax ) >= numfil THEN
            top = top - filesax
          ENDIF
        ENDIF
      
      CASE -73 'Page up -> filenr--
        IF filenr > 1 THEN
          filenr = filenr - 1
          x = 0: y = 0: top = 0
          redraw = 1
        ENDIF
        
      CASE -81 'Page down -> filenr++
        IF filenr < 8 THEN
          filenr = filenr + 1
          x = 0: y = 0: top = 0
          redraw = 1
        ENDIF
        
    END SELECT
    
  LOOP WHILE finish = 0

  'Restore display mode
  RestoreDispMode

  'Restore static variables if go to map was set
  IF gotomap <> "" THEN
    x = x0: y = y0: top = top0
  ENDIF

END FUNCTION

'Object browser
FUNCTION ObjectBrowser(browser AS INTEGER) AS STRING
  
  'Constants
  CONST OBJAX = 7
  CONST OBJAY = 5
  
  'Variables
  STATIC AS INTEGER xd,yd,topd
  DIM AS INTEGER rfcount
  DIM AS INTEGER x,y,top
  DIM AS INTEGER ax,ay,atop
  DIM AS INTEGER i,j,k,index
  DIM AS INTEGER code,ecode,finish
  DIM AS INTEGER redraw,changes
  DIM AS STRING * 10 newname
  DIM AS STRING * 10 minimum
  DIM AS STRING folder
  REDIM AS objdef objaux(MAXOBJEC)
    
  'Default texture loader configuration
  TexLoaderConfig 0,""

  'Init screen
  x=xd: y=yd: top = topd

  'Initialize screen
  LINE (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
  PrintStrBkg FCol(25), FRow(0), SPACEBAR, COL_TEXT, 0, 2
  PrintStrBkg FCol(25), FRow(0), "Object browser", COL_TEXT, 0, 2

  'Texture selection loop
  finish = 0: redraw = 1
  rfcount = 0: changes = 0
  atop = 0: ax = 0:ay = 0: 
  DO
    
    'Redraw screen
    IF top <> atop OR redraw = 1 THEN
      ObjRefreshKill
      LINE (WINDOWPX,WINDOWPY + 22)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
      k = top
      FOR j = 0 TO OBJAY - 1
      FOR i = 0 TO OBJAX - 1
        IF k < numobj THEN
          DrawObjectBrowser obj(k),i,j,0
          k = k + 1
        ELSE
          EXIT FOR
        ENDIF
      NEXT i
      NEXT j
      DrawObjectBrowser obj(0),x,y,1
      IF browser = 1 THEN PrintMenuBar MBAR,MENUOBJ1,0
      IF browser = 0 THEN PrintMenuBar MBAR,MENUOBJ2,0
    ENDIF
    
    'Redraw cursor
    IF ax <> x OR ay <> y THEN
      DrawObjectBrowser obj(0),ax,ay,2
      DrawObjectBrowser obj(0),x,y,1
    ENDIF
    
    'Clear flags
    redraw = 0
   
    'Save static screen parameters
    xd = x: yd = y: topd = top
  
    'Save previous cursor location
    ax = x: ay = y: atop = top
   
    'Get keyboard key & refresh objects
    DO
      IF rfcount = 0 THEN 
        ObjRefreshMode 2
        ObjRefreshDraw
      ENDIF
      code = GetKeyb(0): IF code <> 0 THEN EXIT DO
      SLEEP 50
      rfcount = rfcount + 1
      IF rfcount = 3 THEN rfcount = 0
    LOOP

    'Switch on key
    SELECT CASE code
    
      CASE 13 'Enter -> select texture
        IF obj(y*OBJAX + top + x).objname <> "" AND browser = 0 THEN
          finish = 1
          ObjectBrowser = obj(y*OBJAX + top + x).objname
        ENDIF
        
      CASE 27 'Escape -> exit
        finish = 1
        ObjectBrowser = "ESC"
        
      CASE -83 'Delete -> exit an return null texture
        IF browser = 0 THEN
          finish = 1
          ObjectBrowser = ""
        ENDIF   
        
      CASE -60 'F2: Rename object
        IF browser = 1 THEN
          PrintMenuBar MBAR,SPACEBAR,0
          PrintStrBkg FCol(24),FRow(52),"Enter new name:",COL_TEXT,0,2
          newname = GetStringBkg(39,52,10,COL_TEXT,2,obj(y*OBJAX + top + x).objname,ecode)
          IF newname <> "" THEN
            obj(y*OBJAX + top + x).objname = newname
            changes = 1
          ENDIF
          redraw = 1
        ENDIF
        
      CASE -61 'F3: Delete object
        IF browser = 1 THEN
          PrintMenuBar MBAR,SPACEBAR,0
          PrintStrBkg FCol(24),FRow(52),"Delete this object and all its sprites totaly? (y/n)", COL_TEXT,0,2
          code = GetKeyb(1)
          IF code = ASC("Y") OR code = ASC("y") OR code = 13 THEN
            folder = GetTexPath(3)
            FOR i=0 to obj(y*OBJAX + top + x).frames - 1
              KILL folder + obj(y*OBJAX + top + x).txname(i)
            NEXT i
            obj(y*OBJAX + top + x).delet = 1
            changes = 1
          ENDIF
          redraw = 1
        ENDIF
        
      CASE -62 'F4: Sort object list
        IF browser = 1 THEN
          FOR i=0 TO numobj-1
            objaux(i) = obj(i)
          NEXT i
          FOR i=0 TO numobj-1
            minimum = ""
            FOR j=0 TO numobj-1
              IF minimum = "" AND obj(j).objname <> "" THEN minimum = obj(j).objname
              IF obj(j).objname <= minimum AND obj(j).objname <> "" THEN
                minimum = obj(j).objname
                index = j
              ENDIF
            NEXT j
            objaux(i) = obj(index)
            obj(index).objname = ""
          NEXT i
          FOR i=0 TO numobj-1
            obj(i) = objaux(i)
          NEXT i
          changes = 1
          redraw = 1
        ENDIF
        
      CASE -75 'Cursor left -> x--
        x = x - 1
        IF x < 0 THEN x = 0

      CASE -77 'Cursor right -> x++
        x = x + 1
        IF ( top + y * OBJAX + x ) > numobj - 1 THEN
          x = x - 1
        ELSEIF x > OBJAX - 1 THEN
          x = OBJAX - 1
        ENDIF
    
      CASE -72 'Cursor up -> y --
        y = y - 1
        IF y < 0 THEN
          y = 0
          top = top - OBJAX
          IF top < 0 THEN top = 0
        ENDIF
        
      CASE -80 'Cursor down -> y ++
        y = y + 1
        IF ( top + y * OBJAX + x ) > numobj - 1 THEN
          y = y - 1
        ELSEIF y > OBJAY - 1 THEN
          y = OBJAY - 1
          top = top + OBJAX
          IF ( top + OBJAY * OBJAX - OBJAX ) >= numobj THEN
            top = top - OBJAX
          ENDIF
        ENDIF
      
      CASE -73 'Page up -> y = y - OBJAY
        top = top - OBJAX * OBJAY
        IF top < 0 THEN top = 0
        
      CASE -81 'Page down down -> y = y + OBJAY
        top = top + OBJAX * OBJAY
        IF ( top + OBJAY * OBJAX - OBJAX ) >= numobj THEN
          top = top - OBJAX * OBJAY
        ENDIF
      
    END SELECT
    
  LOOP WHILE finish = 0

  'Clear object refresh table
  ObjRefreshKill
  
  'Saves changes to file
  IF changes = 1 THEN
    SaveObjectList
    LoadObjectList
  ENDIF
  
END FUNCTION

'Draw object (for object browser)
SUB DrawObjectBrowser(Object AS objdef, x AS INTEGER, y AS INTEGER, mode AS INTEGER)
 
  'Variables
  DIM AS INTEGER posy
  DIM AS INTEGER ax,ay
  DIM AS INTEGER px,py
  DIM AS UINTEGER PTR ipt

  'Calculate screen coordinates
  px = WINDOWPX + 04 + x * ( 64 + 5 )
  py = WINDOWPY + 26 + y * ( 64 + 22 )
  
  'Switch on mode
  SELECT CASE mode
  
    'Print object texture
    CASE 0
      IF Object.delet = 0 THEN
        ReadTexture 3, Object.txname(0), ax, ay, ipt
        DrawOccupiedMemory 0
        PrintStr px + 2,py + 66, Object.objname,COL_TEXT
        PrintStr px + 2,py + 74, "(" + STR$(object.frames) + ")",COL_TEXT
        IF ay > CLIPAYTEX THEN
          posy = py
        ELSE
          posy = py + CLIPAYTEX - ay
        ENDIF
        ImgDisplayClip px, posy, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
        IF Object.frames > 1 THEN
          ObjRefreshEnqueue px, py, 0, Object
        ENDIF
      ELSE
        PrintStr px + 2,py + 1, "[Deleted]",COL_DELE
      ENDIF
  
    'Cursor box set
    CASE 1
      LINE (px-1,py-1)-STEP(64+1,64+19),COL_CURS,B
  
    'Cursor box clear
    CASE 2
      LINE (px-1,py-1)-STEP(64+1,64+19),0,B

  END SELECT
  
END SUB

'Entity type browser
FUNCTION ETypeBrowser(browser AS INTEGER) AS STRING
  
  'Variables
  STATIC AS INTEGER etypenumd,stindexd
  DIM AS INTEGER etypenum,stindex
  DIM AS INTEGER aetypenum,astindex
  DIM AS INTEGER stmin,stmax
  DIM AS INTEGER i,code,finish,redraw
    
  'Default texture loader configuration
  TexLoaderConfig 0,""

  'Init screen
  etypenum=etypenumd: stindex=stindexd

  'Initialize screen
  LINE (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
  PrintStrBkg FCol(25), FRow(0), SPACEBAR, COL_TEXT, 0, 2
  PrintStrBkg FCol(25), FRow(0), "Entity types", COL_TEXT, 0, 2

  'Texture selection loop
  finish = 0
  etypenumd = 0: stindexd = 0 
  aetypenum =-1: astindex =-1 
  DO
    
    'Get maximum and minimum indexes in the entity
    IF aetypenum <> etypenum THEN
      stmin = 999999
      stmax = 000000
      FOR i=0 TO MAXETYST-1
        IF etypestate(i).etypename = etype(etypenum).etypename THEN
          IF i < stmin THEN stmin = i
          IF i > stmax THEN stmax = i
        ENDIF
      NEXT i
    ENDIF

    'Redraw screen
    IF aetypenum <> etypenum OR astindex <> stindex THEN
      LINE (WINDOWPX,WINDOWPY + 22)-(AX0 - WINBORDR, AY0 - WINBORDR),0,BF
      DrawETypeBrowser etype(etypenum), etypestate(), stmin + stindex
      IF browser = 0 THEN PrintMenuBar MBAR,MENUETY1,0
      IF browser = 1 THEN PrintMenuBar MBAR,MENUETY2,0
    ENDIF
    
    'Clear flags
    redraw = 0
   
    'Save screen parameters
    aetypenum=etypenum: astindex=stindex

   'Save static screen parameters
    etypenumd=etypenum: stindexd=stindex
  
    'Get keyboard key
    code = GetKeyb(1)

    'Switch on key
    SELECT CASE code
    
      CASE 13 'ENTER -> select
        IF browser = 1 THEN
          finish = 1
          Etypebrowser = etype(etypenum).etypename
        ENDIF
        
      CASE 27 'Escape -> exit
        finish = 1
        Etypebrowser = "ESC"
        
      CASE -75 'Cursor left -> etypenum--
        etypenum = etypenum - 1
        IF etypenum < 0 THEN etypenum = 0

      CASE -77 'Cursor right -> etypenum++
        etypenum = etypenum + 1
        IF etypenum > numetype - 1 THEN etypenum = numetype - 1
    
      CASE -72 'Cursor up -> y --
        stindex = stindex - 1
        IF stindex < 0 THEN stindex = 0
        
      CASE -80 'Cursor down -> y ++
        stindex = stindex + 1
        IF stmin + stindex + 3 > stmax THEN stindex = stindex - 1
      
    END SELECT
    
  LOOP WHILE finish = 0

END FUNCTION

'Draw entity type
SUB DrawETypeBrowser(etype AS etypedef, etypestate() AS etypestdef, stindex AS INTEGER)
 
  'Variables
  DIM AS INTEGER i,x,y
  DIM AS INTEGER ax,ay,px,py
  DIM AS INTEGER posy
  DIM AS UINTEGER PTR ipt

  'Print header data
  x = 0: px = WINDOWPX + 04 + x * ( 64 + 5 )
  y = 0: py = WINDOWPY + 26 + y * ( 64 + 22 )
  ReadTexture 4, etype.etypeimg, ax, ay, ipt
  IF ay > CLIPAYTEX THEN
    posy = py
  ELSE
    posy = py + CLIPAYTEX - ay
  ENDIF
  ImgDisplayClip px, posy, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
  PrintStr FCol(34),FRow(7), etype.etypename, COL_TEXT
  
  'Draw frane headings
  DrawETypeStateFrame(etypestate(0), 0, 0, 0, "0Deg")
  DrawETypeStateFrame(etypestate(0), 1, 0, 0, "45Deg")
  DrawETypeStateFrame(etypestate(0), 2, 0, 0, "90Deg")
  DrawETypeStateFrame(etypestate(0), 3, 0, 0, "135Deg")
  DrawETypeStateFrame(etypestate(0), 4, 0, 0, "180Deg")
  DrawETypeStateFrame(etypestate(0), 5, 0, 0, "225Deg")
  DrawETypeStateFrame(etypestate(0), 6, 0, 0, "270Deg")
  DrawETypeStateFrame(etypestate(0), 7, 0, 0, "135Deg")

  'Draw state textures
  FOR i=0 TO 3
    DrawETypeStateFrame(etypestate(i+stindex), 0, i+1, 0, "")
    DrawETypeStateFrame(etypestate(i+stindex), 1, i+1, 1, "")
    DrawETypeStateFrame(etypestate(i+stindex), 2, i+1, 2, "")
    DrawETypeStateFrame(etypestate(i+stindex), 3, i+1, 3, "")
    DrawETypeStateFrame(etypestate(i+stindex), 4, i+1, 4, "")
    DrawETypeStateFrame(etypestate(i+stindex), 5, i+1, 5, "")
    DrawETypeStateFrame(etypestate(i+stindex), 6, i+1, 6, "")
    DrawETypeStateFrame(etypestate(i+stindex), 7, i+1, 7, "")
  NEXT i

END SUB

'Draw state frames
SUB DrawETypeStateFrame(etypestate AS etypestdef, x AS INTEGER, y AS INTEGER, frame AS INTEGER, heading AS STRING)
 
  'Variables
  DIM AS STRING frame00,fname
  DIM AS INTEGER ax,ay,px,py
  DIM AS UINTEGER PTR ipt
  DIM AS INTEGER posy

  'Calculate screen coordinates
  px = WINDOWPX + 04 + x * ( 55 + 5 )
  py = WINDOWPY + 26 + y * ( 64 + 22 )
  
  'Normal frame drawing
  IF heading = "" THEN
  
    'Exit if frame not populated
    IF etypestate.frame(frame) = "" THEN EXIT SUB
  
    'Frame printable name
    fname = MID$(etypestate.frame(frame),1,LEN(frame00)-4)

    'Draw frame
    SetFont 1
    LINE (px+1,py+65)-(px+57,py+82),RGB(50,0,0),BF
    ReadTexture 4, etypestate.frame(frame), ax, ay, ipt
    PrintStr px + 2,py + 66, fname, COL_TEXT
    PrintStr px + 2,py + 74, "(" + FORMAT(ax,"00") + "x" + FORMAT(ay,"00") + ")",COL_TEXT
    IF ay > CLIPAYTEX THEN
      posy = py
    ELSE
      posy = py + CLIPAYTEX - ay
    ENDIF
    ImgDisplayClip px, posy, ax, ay, ipt, etypestate.mirror(frame), 57, CLIPAYTEX
    SetFont 2
    DrawOccupiedMemory 0
  
  'Frame headings
  ELSE
    LINE (px+1,py+74)-(px+57,py+83),RGB(0,0,50),BF
    PrintStr px + 2,py + 75, heading, COL_TEXT
  ENDIF
  
END SUB

SUB DrawOccupiedMemory(page AS INTEGER)
  
  'Variables
  DIM AS INTEGER recused, memused
  DIM AS INTEGER px, py
  
  'Get occupied memory
  TexMemGetOccupied recused, memused
  
  'Calculate screen location
  px = FCol(104)+2
  py = FRow(045)+1
  
  'Background bars
  LINE (px+00,py)-(px+03,py+50),RGB(0,0,90),BF
  LINE (px+06,py)-(px+09,py+50),RGB(0,90,0),BF
  
  'Memory bars
  LINE (px+00,py+50-recused/2)-(px+03,py+50),RGB(0,0,210),BF
  LINE (px+06,py+50-memused/2)-(px+09,py+50),RGB(0,210,0),BF
  
  'Letters
  PrintStrBkg FCol(104)+1, FRow(51), "RM", COL_TEXT, page, 2

END SUB

'Set link point
FUNCTION ChooseLinkPoint(BYREF linkmap AS STRING, BYREF linkcx AS INTEGER, _
                         BYREF linkcy AS INTEGER) AS INTEGER
  
  'Variables
  DIM AS INTEGER x,y,ax,ay
  DIM AS INTEGER code,finish,redraw,cancel
  DIM AS roomattr rattr0
  REDIM AS mapcell cmap0(MAXCELLX,MAXCELLY)
  REDIM AS maplight light0(MAXLIGHT)
  REDIM AS mapovtex ovtex0(MAXOVTEX)
  REDIM AS mapevent event0(MAXEVENT)
  REDIM AS mapactio actio0(MAXACTIO)
  REDIM AS INTEGER mapscr(WINPIXEL)
  
  'Increase light map buffer
  ISOLMapIncBuffer()

  'Call map browser to choose map
  IF linkmap = "" THEN
    linkmap = FileLoad(rattr0,cmap0(),light0(),ovtex0(),event0(),actio0(),"","",cancel)
    IF cancel = 1 THEN
      ChooseLinkPoint = 0
      ISOLMapDecBuffer()
      EXIT FUNCTION
    ENDIF
  ELSE
    MapRead(linkmap,rattr0,cmap0(),light0(),ovtex0(),event0(),actio0())
  ENDIF

  'Set map display mode
  StoreDispMode
  SetDispMode(TEXT_MODE, MODE_FIXED)
  SetDispMode(OBJS_MODE, MODE_FIXED)
  SetDispMode(ENTS_MODE, MODE_OFF)
  IF rattr0.lmapstore = 1 THEN 
    SetDispMode(LIGHT_MODE, MODE_LMAP)
  ELSE
    SetDispMode(LIGHT_MODE, MODE_LDYN)
  ENDIF

  'Print map
  LINE (WINDOWPX+0,WINDOWPY+0)-(AX0 - WINBORDR-0, AY0 - WINBORDR-0),RGB(255,0,0),B
  LINE (WINDOWPX+1,WINDOWPY+1)-(AX0 - WINBORDR-1, AY0 - WINBORDR-1),RGB(180,0,0),B
  LINE (WINDOWPX+2,WINDOWPY+2)-(AX0 - WINBORDR-2, AY0 - WINBORDR-2),RGB(120,0,0),B
  VIEW SCREEN (WINDOWPX+3,WINDOWPY+3)-(AX0 - WINBORDR-3, AY0 - WINBORDR-3),0
  RoomMap rattr0, cmap0(), light0(), ovtex0(), 0
  RoomLinkPoints linkmap, rattr0, cmap0()
  VIEW SCREEN
  PrintInfoBox EDIT_LNK,linkcx,linkcy,linkmap,rattr0,cmap0(),_
  light0(),ovtex0(),event0(),actio0(),99,0,0,1,0,0,0,0,0,0,0,0,9,0,0
  PrintStr FCol(25),FRow(2),"Set link point location", RGB(255,255,255)
  GET (WINDOWPX,WINDOWPY)-(AX0 - WINBORDR,AY0 - WINBORDR), mapscr

  'Menu bar
  PrintMenuBar MBAR,MENULINK,0  
  
  'Set default cursor location
  x = linkcx
  y = linkcy
  
  'Texture selection loop
  finish=0:redraw=1:ax=0:ay=0
  DO
    
    'Print cursor
    IF ax <> x OR ay <> y OR redraw = 1 THEN
      PUT (WINDOWPX,WINDOWPY), mapscr, PSET
      PrintInfoBox EDIT_LNK,x,y,linkmap,rattr0,cmap0(),_
      light0(),ovtex0(),event0(),actio0(),99,0,0,1,0,0,0,0,0,0,0,0,9,0,0
      RoomCursor x, y, rattr0, cmap0(), light0()
      PrintStr FCol(98), FRow(50),FORMAT(x,"00")+","+FORMAT(y,"00"), COL_TEXT
      redraw = 0
    ENDIF
    
    'Save previous cursor location
    ax = x: ay = y
   
    'Get keyboard
    code = GetKeyb(1)
    
    'Switch on key
    SELECT CASE code
    
      CASE 13 'Enter -> select link point
        finish = 1
        linkcx = x
        linkcy = y
        ChooseLinkPoint = 1
        
      CASE 27 'Escape -> exit
        finish = 1
        ChooseLinkPoint = 0
        
      CASE -77 'Cursor left -> x--
        x = x - 1
        IF x < 0 THEN x = 0

      CASE -75 'Cursor right -> x++
        x = x + 1
        IF x >= rattr0.ax - 1 THEN
          x = rattr0.ax - 1
        ENDIF
    
      CASE -72 'Cursor up -> y --
        y = y - 1
        IF y < 0 THEN y = 0
        
      CASE -80 'Cursor down -> y ++
        y = y + 1
        IF y >= rattr0.ay - 1 THEN
          y = rattr0.ay - 1
        ENDIF
      
    END SELECT
    
  LOOP WHILE finish = 0

  'Restore display mode
  RestoreDispMode
  ISOLMapDecBuffer()

END FUNCTION

'Minimum
FUNCTION Min(a AS INTEGER, b AS INTEGER) AS INTEGER
  IF a <= b THEN 
    Min = a
  ELSE
    Min = b
  ENDIF
END FUNCTION

'Maximum
FUNCTION Max(a AS INTEGER, b AS INTEGER) AS INTEGER
  IF a >= b THEN 
    Max = a
  ELSE
    Max = b
  ENDIF
END FUNCTION


'*** End of gamlib.bas ***


'*** Begin of maplib.bi ***

' Colors
CONST COL_LINE = RGB(&H63,&H63,&H63)
CONST COL_GRND = RGB(&H1f,&H1b,&H2f)
CONST COL_LEFT = RGB(&H4b,&H5b,&H43)
CONST COL_RIGHT= RGB(&H37,&H43,&H37)
CONST COL_CURS = RGB(255,255,255)
CONST COL_FLOR = RGB(255,255,255)
CONST COL_HEIG = RGB(255,0,0)
CONST COL_CEIL = RGB(255,255,255)
CONST COL_OBJ  = RGB(100,100,100)
CONST COL_OVL  = RGB(180,180,0)

' Sizes
CONST MAXCELLX  = 40      'Maximum map size X
CONST MAXCELLY  = 40      'Maximum map size Y
CONST MAXOBJTX  = 50      'Maximum object frames
CONST MAXOBJEC  = 500     'Maximum defined objects
CONST MAXOBJRF  = 50      'Maximum objects in refresh table
CONST MAXOVTEX  = 38      'Maximum overlay textures per map
CONST MAXEVENT  = 36      'Maximum events per map
CONST MAXACTIO  = 500     'Maximum actions per map
CONST MAXWALKST = 10      'Maximum states per walk sequence
CONST MAXATAKST = 10      'Maximum states per atack sequence
CONST MAXDEADST = 10      'Maximum states per dead sequence
CONST MAXETYPE  = 20      'Maximum defined entity types
CONST MAXENTRF  = 50      'Maximum entities in refresh table
CONST MAXCOMMD  = 100     'Maximum entity commands in queue
CONST MAXLINKP  = 3000    'Maximum link points
CONST DSMSTACK  = 30      'Display modes stack
CONST MAPIMGSZ  = 3000000 'Size of the map image cache memory
CONST MAPIMGRE  = 200     'Size of the map image cache memory
CONST MAXPARMS  = 5       'Max parameters per action
CONST DEFACTIO  = 7       'Defined actions

'Total entity type states
CONST MAXETYST  = MAXETYPE * (MAXWALKST + MAXATAKST + MAXDEADST)

'Entity type states
CONST ENTWALK   = "WALK"
CONST ENTATAK   = "ATAK"
CONST ENTDEAD   = "DEAD"

'Entity commands
CONST ENCSTOP   = "STOP"
CONST ENCMOVL   = "MOVL"
CONST ENCMOVR   = "MOVR"
CONST ENCMOVU   = "MOVU"
CONST ENCMOVD   = "MOVD"
CONST ENCATAK   = "ATAK"
CONST ENCDEAD   = "DEAD"
CONST ENCTURL   = "TURL"
CONST ENCTURR   = "TURR"
CONST ENCHEIG   = "HEIG"

'Entity parameters
CONST ENTSTEP   = 8

'Display modes
'Grid: on/off (&h000F)
'Textures: On/Off/Solid/Fixed (&h00F0)
'Objects: on/off/fixed (&h0F00)
CONST TEXT_MODE  = 1
CONST OBJS_MODE  = 2
CONST LIGHT_MODE = 3
CONST ENTS_MODE  = 4
CONST MODE_OFF   = 0
CONST MODE_ON    = 1  
CONST MODE_SOLID = 2
CONST MODE_FIXED = 3
CONST MODE_FRAME = 4
CONST MODE_BOX   = 5
CONST MODE_LDYN  = 6
CONST MODE_LMAP  = 7
CONST MODE_CHANG = 99

'Event types
CONST EVT_LINKPO  = "LinkPnt"
CONST EVT_SWITCH  = "Switch "
CONST EVT_TRIGGER = "Trigger"

'Action types
CONST ACT_BLANKLINE     = "blankline"
CONST ACT_COMMENT       = "comment"
CONST ACT_EVENT         = "event"
CONST ACT_OVTEX_ENABLE  = "ovtex_enable"
CONST ACT_OVTEX_DISABLE = "ovtex_disable" 
CONST ACT_PASS_ENABLE   = "pass_enable"
CONST ACT_PASS_DISABLE  = "pass_disable"

'Argument types for actions
CONST ARG_INTEGER = 1
CONST ARG_STRING  = 2

'Room generic attributes
TYPE roomattr
  px AS INTEGER
  py AS INTEGER
  ax AS INTEGER
  ay AS INTEGER
  cellsize AS INTEGER
  lightambl AS SINGLE
  lightambr AS INTEGER
  lightambg AS INTEGER
  lightambb AS INTEGER
  lightaprox AS INTEGER
  lmapdifuse AS INTEGER
  lmapstore AS INTEGER
  depx AS INTEGER
  depy AS INTEGER
  numevt AS INTEGER
  numact AS INTEGER
  epmap AS INTEGER
  mname AS STRING * 23
END TYPE

'Ground cell map
TYPE mapcell
  height AS INTEGER
  floor AS INTEGER
  ceiling AS INTEGER
  texfb AS STRING * 15
  texcb AS STRING * 15
  texcr AS STRING * 15
  texcl AS STRING * 15
  texmr AS STRING * 15
  texml AS STRING * 15
  texfr AS STRING * 15
  texfl AS STRING * 15
  ofcrx AS SHORT
  ofclx AS SHORT
  ofmrx AS SHORT
  ofmlx AS SHORT
  offrx AS SHORT
  offlx AS SHORT
  ofcry AS SHORT
  ofcly AS SHORT
  ofmry AS SHORT
  ofmly AS SHORT
  offry AS SHORT
  offly AS SHORT
  confb AS SHORT
  concb AS SHORT
  concr AS SHORT
  concl AS SHORT
  conmr AS SHORT
  conml AS SHORT
  confr AS SHORT
  confl AS SHORT
  pasmr AS SHORT
  pasml AS SHORT
  object AS STRING * 10
  objheight AS SINGLE
  eventid AS INTEGER
END TYPE

'Object definition
TYPE objdef
  objname AS STRING * 10
  frames AS INTEGER
  txname(MAXOBJTX) AS STRING * 15
  delet AS INTEGER
END TYPE

'Object refresh table
TYPE objrefresh
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  frame AS INTEGER
  obj AS objdef
END TYPE

'Entity type definition
TYPE etypedef
  etypename AS STRING * 10
  etypeimg AS STRING * 15
  walksnr AS INTEGER
  ataksnr AS INTEGER
  deadsnr AS INTEGER
END TYPE

'Entity type states
TYPE etypestdef
  etypename AS STRING * 10
  state AS STRING * 10
  stnum AS INTEGER
  multidir AS INTEGER
  frame(8) AS STRING * 15 
  mirror(8) AS INTEGER
END TYPE

'Entity refresh table
TYPE entrefresh
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  x0 AS INTEGER
  y0 AS INTEGER
  z0 AS INTEGER
  etype AS etypedef
  state AS STRING * 10
  stnum AS INTEGER
  direc AS INTEGER
  cindex AS INTEGER
  commd(MAXCOMMD) AS STRING * 4
  arg(MAXCOMMD) AS INTEGER
  acomm AS STRING * 4
  used AS INTEGER
END TYPE

' Light source
TYPE maplight
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  dx AS INTEGER
  dy AS INTEGER
  dz AS INTEGER
  lum AS SINGLE
  r AS INTEGER
  g AS INTEGER
  b AS INTEGER
  used AS INTEGER
END TYPE

'Array for object files
TYPE obfile
  file AS STRING * 15
  action AS INTEGER
END TYPE

'Overlay textures
TYPE mapovtex
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  dx AS INTEGER
  dy AS INTEGER
  dz AS INTEGER
  texdir AS INTEGER
  texname AS STRING * 15
  ax AS INTEGER
  ay AS INTEGER
  plane AS INTEGER
  group AS INTEGER
  used AS INTEGER
  active AS INTEGER
END TYPE

'Link point file structure
TYPE lpfile
  map1 AS STRING * 15
  x1 AS INTEGER
  y1 AS INTEGER
  map2 AS STRING * 15
  x2 AS INTEGER
  y2 AS INTEGER
  way AS INTEGER
  delet AS INTEGER
END TYPE

'Map event
TYPE mapevent
  id AS INTEGER
  evname AS STRING * 7
  evtype AS STRING * 7
  value AS INTEGER
END TYPE

'Map action
TYPE mapactio
  oper AS STRING * 32
  parm(MAXPARMS) AS STRING * 80
END TYPE

'Action definition
TYPE actdefin
  oper AS STRING * 32
  argnum AS INTEGER
  ptype(MAXPARMS) AS INTEGER
END TYPE

'Map images memory records
TYPE mapimgrec
  file AS STRING * 15
  scale AS INTEGER
  ax AS INTEGER
  ay AS INTEGER
  imgptr AS INTEGER
END TYPE

'Link point macros
#MACRO _LinkLoadFile()
  LinkPnt("LINK_LOAD_FILE","",0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkSyncFile()
  LinkPnt("LINK_SYNC_FILE","",0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkStorePoint(map1,x1,y1,map2,x2,y2,way)
  LinkPnt("LINK_STORE_POINT",map1,x1,y1,map2,x2,y2,way)
#ENDMACRO
#MACRO _LinkDeletePoint(map,x,y)
  LinkPnt("LINK_DELETE_POINT",map,x,y,"",0,0,0)
#ENDMACRO
#MACRO _LinkSearchPoint(map1,x1,y1,map2,x2,y2,way,found)
  found=LinkPnt("LINK_SEARCH_POINT",map1,x1,y1,map2,x2,y2,way)
#ENDMACRO
#MACRO _LinkGetNumber()
  LinkPnt("LINK_GET_NUMBER","",0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkListStart(map)
  LinkPnt("LINK_LIST_START",map,0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkListNext(map1,x1,y1,map2,x2,y2,way,finish)
  finish=LinkPnt("LINK_LIST_NEXT",map1,x1,y1,map2,x2,y2,way)
#ENDMACRO

'Map images macros
#MACRO _MapImgInit()
  MapImage("INIT_MEMORY","",0,0,0,0,0)
#ENDMACRO  
#MACRO _MapImgStore(file,scale,px,py,ax,ay)
  MapImage("STORE_IMAGE",file,scale,px,py,ax,ay)
#ENDMACRO  
#MACRO _MapImgRestore(file,scale,px,py)
  MapImage("RESTORE_IMAGE",file,scale,px,py,0,0)
#ENDMACRO  
#MACRO _MapImgDelete(file)
  MapImage("DELETE_IMAGE",file,0,0,0,0,0)
#ENDMACRO  

' Subroutine declarations
DECLARE SUB ActionTable
DECLARE SUB RoomCursor (x AS INTEGER, y AS INTEGER, rattr AS roomattr, cmap() AS mapcell, light() as maplight)
DECLARE SUB RoomMap(rattr as roomattr, cmap() as mapcell, light() as maplight, ovtex() AS mapovtex, lmapinit AS INTEGER)
DECLARE SUB RoomGCell (rattr AS roomattr, x AS INTEGER, y AS INTEGER, cmap() AS mapcell, lowhigh AS INTEGER)
DECLARE SUB MapInit(rattr as roomattr, cmap() as mapcell, light() as maplight, ovtex() AS mapovtex, event() AS mapevent, actio() as mapactio)
DECLARE SUB MapWrite( filename AS STRING, rattr AS roomattr, cmap() AS mapcell, light() as maplight, ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio)
DECLARE SUB MapRead( filename AS STRING, rattr AS roomattr, cmap() AS mapcell, light() as maplight, ovtex() AS mapovtex, event() AS mapevent, actio() AS mapactio)
DECLARE SUB MapReadAttr(filename AS STRING, rattr AS roomattr)
DECLARE SUB LoadObjectList
DECLARE SUB SaveObjectList
DECLARE SUB ScanNewObjects
DECLARE SUB ObjRefreshEnqueue(x AS INTEGER, y AS INTEGER, z AS INTEGER, obj AS objdef)
DECLARE SUB ObjRefreshMode(mode AS INTEGER)
DECLARE SUB ObjRefreshDraw
DECLARE SUB ObjRefreshKill
DECLARE SUB SetDispMode(param AS INTEGER, value AS INTEGER)
DECLARE SUB StoreDispMode
DECLARE SUB RestoreDispMode
DECLARE FUNCTION GetDispMode(param AS INTEGER) AS INTEGER
DECLARE SUB LightCursor (index AS INTEGER, light() AS maplight, rattr AS roomattr)
DECLARE SUB SaveEntityTypes
DECLARE SUB LoadEntityTypes
DECLARE FUNCTION EntRefreshEnqueue(x AS INTEGER, y AS INTEGER, z AS INTEGER, etype AS etypedef) AS INTEGER
DECLARE SUB EntRefreshCommand(index AS INTEGER, commd AS STRING, arg AS INTEGER)
DECLARE SUB EntRefreshKill
DECLARE SUB EntRefreshDraw
DECLARE SUB EntRefreshSetMapPointer(rattrp AS roomattr PTR, cmapp AS mapcell PTR)
DECLARE FUNCTION EntityValidMovement(BYVAL entref AS entrefresh) AS INTEGER
DECLARE SUB SetTextureMode(disp AS INTEGER)
DECLARE SUB RoomLinkPoints(mapname AS STRING, rattr AS roomattr, cmap() AS mapcell)
DECLARE SUB RoomEvents(rattr AS roomattr, cmap() AS mapcell)
DECLARE FUNCTION LinkPnt(commd AS STRING, BYREF map1 AS STRING, BYREF x1 AS INTEGER, BYREF y1 AS INTEGER, BYREF map2 AS STRING, BYREF x2 AS INTEGER, BYREF y2 AS INTEGER, BYREF way AS INTEGER) AS INTEGER
DECLARE FUNCTION MapImage(commd AS STRING, file AS STRING, scale AS INTEGER, px AS INTEGER, py AS INTEGER, ax AS INTEGER, ay AS INTEGER ) AS INTEGER
DECLARE SUB CellSample(px AS INTEGER, py AS INTEGER, BYVAL rattr AS roomattr, BYVAL cell AS mapcell)
DECLARE SUB EventPrint(event AS mapevent, evtstr AS STRING)
DECLARE SUB ActionPrint(actio AS mapactio, actstr AS STRING)
DECLARE FUNCTION ActionParse(actio AS mapactio, actstr AS STRING, perror AS STRING) AS INTEGER
DECLARE SUB MapExecute(mapname AS STRING)

'*** End of maplib.bi ***


'*** Begin of maplib.bas ***

#INCLUDE "maplib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "texlib.bi"
#INCLUDE "vbcompat.bi"

'Display modes
DIM SHARED disp_text AS INTEGER = MODE_FIXED
DIM SHARED disp_objs AS INTEGER = MODE_FIXED
DIM SHARED disp_light AS INTEGER = MODE_LDYN
DIM SHARED disp_ents AS INTEGER = MODE_ON

'Display mode stack (for store and restore functions)
DIM SHARED disp_stindex AS INTEGER = 0
DIM SHARED disp_text_bk(DSMSTACK) AS INTEGER
DIM SHARED disp_objs_bk(DSMSTACK) AS INTEGER
DIM SHARED disp_light_bk(DSMSTACK) AS INTEGER
DIM SHARED disp_ents_bk(DSMSTACK) AS INTEGER

'Object definition
COMMON SHARED numobj AS INTEGER
EXTERN obj() AS objdef
REDIM SHARED obj(MAXOBJEC) AS objdef

'Automatic object refresh
DIM SHARED numobjref AS INTEGER
DIM SHARED objrefmode AS INTEGER = 1
REDIM SHARED objref(MAXOBJRF) AS objrefresh

'Entity definition
COMMON SHARED numetype AS INTEGER
EXTERN etype() AS etypedef
REDIM SHARED etype(MAXETYPE) AS etypedef
EXTERN etypestate() AS etypestdef
REDIM SHARED etypestate(MAXETYST) AS etypestdef

'Automatic entity refresh
REDIM SHARED entref(MAXENTRF) AS entrefresh

'Generic image buffer for textures
DIM SHARED imgfl AS UINTEGER PTR 'Image buffer floor left
DIM SHARED imgfr AS UINTEGER PTR 'Image buffer floor right
DIM SHARED imgml AS UINTEGER PTR 'Image buffer middle left
DIM SHARED imgmr AS UINTEGER PTR 'Image buffer middle right
DIM SHARED imgcl AS UINTEGER PTR 'Image buffer ceiling left
DIM SHARED imgcr AS UINTEGER PTR 'Image buffer ceiling right
DIM SHARED imgfb AS UINTEGER PTR 'Image buffer floor base
DIM SHARED imgcb AS UINTEGER PTR 'Image buffer ceiling base
DIM SHARED imgob AS UINTEGER PTR 'Image buffer object refresh
DIM SHARED imgov AS UINTEGER PTR 'Image buffer overlay texture
DIM SHARED imgen AS UINTEGER PTR 'Image buffer entity sprite

'Pointer to map data
DIM SHARED rattrptr AS roomattr PTR
DIM SHARED cmapptr AS mapcell PTR

'Light map to store / restore from files
DIM SHARED lmaplen AS INTEGER
REDIM SHARED lmap(AX0*AY0) AS RGBi

'Link poit variables
DIM SHARED numlink AS INTEGER
REDIM SHARED linkpoint(MAXLINKP) AS lpfile 

'Map images
DIM SHARED mapptr AS INTEGER = 0
DIM SHARED numimr AS INTEGER = 0
REDIM SHARED mapimr(MAPIMGRE) AS mapimgrec
REDIM SHARED mapimg(MAPIMGSZ) AS UINTEGER

'Defined actions table
REDIM SHARED AS actdefin actdef(DEFACTIO)

'Init action definition table
SUB ActionTable

  'Variables
  DIM AS INTEGER i
  
  'Starting index
  i = 0
  
  'Blank line
  actdef(i).oper     = ACT_BLANKLINE
  actdef(i).argnum   = 0
  i = i + 1
  
  'Event
  actdef(i).oper     = ACT_COMMENT
  actdef(i).argnum   = 1
  actdef(i).ptype(0) = ARG_STRING
  i = i + 1
  
  'Event
  actdef(i).oper     = ACT_EVENT
  actdef(i).argnum   = 2
  actdef(i).ptype(0) = ARG_INTEGER
  actdef(i).ptype(1) = ARG_INTEGER
  i = i + 1
  
  'Overlay texture enable group
  actdef(i).oper     = ACT_OVTEX_ENABLE
  actdef(i).argnum   = 1
  actdef(i).ptype(0) = ARG_INTEGER
  i = i + 1
  
  'Overlay texture disable group
  actdef(i).oper     = ACT_OVTEX_DISABLE
  actdef(i).argnum   = 1
  actdef(i).ptype(0) = ARG_INTEGER
  i = i + 1
  
  'Cell pass enable
  actdef(i).oper     = ACT_PASS_ENABLE
  actdef(i).argnum   = 2
  actdef(i).ptype(0) = ARG_INTEGER
  actdef(i).ptype(1) = ARG_INTEGER
  i = i + 1

  'Cell pass disable
  actdef(i).oper     = ACT_PASS_DISABLE
  actdef(i).argnum   = 2
  actdef(i).ptype(0) = ARG_INTEGER
  actdef(i).ptype(1) = ARG_INTEGER
  i = i + 1

END SUB

'Set display mode
SUB SetDispMode(param AS INTEGER, value AS INTEGER)
  
  'Switch on parameter
  SELECT CASE param
    
    'Texture mode
    CASE TEXT_MODE
      IF value = MODE_CHANG THEN
        IF     disp_text = MODE_ON    THEN: disp_text = MODE_FIXED
        ELSEIF disp_text = MODE_FIXED THEN: disp_text = MODE_SOLID
        ELSEIF disp_text = MODE_SOLID THEN: disp_text = MODE_BOX
        ELSEIF disp_text = MODE_BOX   THEN: disp_text = MODE_OFF
        ELSEIF disp_text = MODE_OFF   THEN: disp_text = MODE_ON
        ENDIF
      ELSE
        disp_text = value
      ENDIF
    
    'Object mode
    CASE OBJS_MODE
      IF value = MODE_CHANG THEN
        IF     disp_objs = MODE_ON    THEN: disp_objs = MODE_FIXED
        ELSEIF disp_objs = MODE_FIXED THEN: disp_objs = MODE_SOLID
        ELSEIF disp_objs = MODE_SOLID THEN: disp_objs = MODE_BOX
        ELSEIF disp_objs = MODE_BOX   THEN: disp_objs = MODE_OFF
        ELSEIF disp_objs = MODE_OFF   THEN: disp_objs = MODE_ON
        ENDIF
      ELSE
        disp_objs = value
      ENDIF
      
    'Light mode
    CASE LIGHT_MODE
      IF value = MODE_CHANG THEN
        IF     disp_light = MODE_LDYN  THEN: disp_light = MODE_LMAP
        ELSEIF disp_light = MODE_LMAP  THEN: disp_light = MODE_OFF
        ELSEIF disp_light = MODE_OFF   THEN: disp_light = MODE_LDYN
        ENDIF
      ELSE
        disp_light = value
      ENDIF
    
    'Entity mode
    CASE ENTS_MODE
      IF value = MODE_CHANG THEN
        IF     disp_ents = MODE_ON  THEN: disp_ents = MODE_OFF
        ELSEIF disp_ents = MODE_OFF THEN: disp_ents = MODE_ON
        ENDIF
      ELSE
        disp_ents = value
      ENDIF
    
  END SELECT
  
END SUB

'Get display mode
FUNCTION GetDispMode(param AS INTEGER) AS INTEGER
  
  'Switch on parameter
  SELECT CASE param
    CASE TEXT_MODE: GetDispMode = disp_text
    CASE OBJS_MODE: GetDispMode = disp_objs
    CASE LIGHT_MODE:GetDispMode = disp_light
    CASE ENTS_MODE: GetDispMode = disp_ents
  END SELECT
  
END FUNCTION

'Store display mode
SUB StoreDispMode
  disp_text_bk(disp_stindex) = disp_text
  disp_objs_bk(disp_stindex) = disp_objs
  disp_light_bk(disp_stindex) = disp_light
  disp_ents_bk(disp_stindex) = disp_ents
  disp_stindex = disp_stindex + 1
END SUB

'Restore display mode
SUB RestoreDispMode
  IF disp_stindex > 0 THEN
    disp_stindex = disp_stindex - 1
    disp_text = disp_text_bk(disp_stindex)
    disp_objs = disp_objs_bk(disp_stindex)
    disp_light = disp_light_bk(disp_stindex)
    disp_ents = disp_ents_bk(disp_stindex)
  ENDIF
END SUB

' Set texture mode
SUB SetTextureMode(disp AS INTEGER)

  'Texture mapping mode
  IF disp = MODE_ON THEN
    ISOTexMappingMode TEX_NORMAL
  ELSEIF disp = MODE_FIXED THEN
    ISOTexMappingMode TEX_NORMAL
  ELSEIF disp = MODE_SOLID THEN
    ISOTexMappingMode TEX_SOLID
  ELSEIF disp = MODE_FRAME THEN
    ISOTexMappingMode TEX_FRAME
  ELSEIF disp = MODE_BOX THEN
    ISOTexMappingMode TEX_BOX
  ENDIF
  
  'ZBuffer overwrite (otherwise we have missing lines)
  IF disp = MODE_BOX OR disp = MODE_FRAME THEN
    ISOZBufferOverwrite 1
  ELSE
    ISOZBufferOverwrite 0
  ENDIF

END SUB

' Print room groung cell
SUB RoomGCell (rattr AS roomattr, x AS INTEGER, y AS INTEGER, cmap() AS mapcell, mode AS INTEGER)

  'Variables
  DIM AS INTEGER i                              'Counter
  DIM AS INTEGER found                          'Found flag
  DIM AS INTEGER a1, b1, c1, d1, e1, f1, g1, h1 'Cell points
  DIM AS INTEGER a2, b2, c2, d2, e2, f2, g2, h2 'Cell points
  DIM AS INTEGER a3, b3, c3, d3, e3, f3, g3, h3 'Cell points
  Dim AS INTEGER hX0Ym,hX0Y0,hX0Yp,hXpY0,hXmY0  'Near cell heights
  DIM AS INTEGER LSide, RSide                   'Side faces
  DIM AS INTEGER abs_height                     'Absolute ground height
  DIM AS INTEGER px, py                         'Pixel coordinates
  DIM AS INTEGER tax, tay                       'Texture size
  DIM AS INTEGER offsx, offsy                   'Texture offset
  DIM AS UINTEGER col                           'Line color
  
  'Texture loader configuration (Readonce=1, Factor=0)
  TexLoaderConfig 0,""

  'Calculate cell heights (floor)
  If mode = 0 Then
    hX0Y0=0:hX0Ym=0:hX0Yp=0:hXpY0=0:hXmY0=0 
    hX0Y0 = cmap(x + 0, y + 0).floor
    If x - 1 >= 0 Then hXmY0 = cmap(x - 1, y + 0).floor
    If y - 1 >= 0 Then hX0Ym = cmap(x + 0, y - 1).floor
    If y + 1 < rattr.ay Then hX0Yp = cmap(x + 0, y + 1).floor
    If x + 1 < rattr.ax Then hXpY0 = cmap(x + 1, y + 0).floor
  
  'Calculate cell heights (middle)
  ElseIf mode = 1 Then
    If cmap(x, y).height = 0 Then Exit Sub
    hX0Y0=0:hX0Ym=0:hX0Yp=0:hXpY0=0:hXmY0=0 
    hX0Y0 = cmap(x + 0, y + 0).height - cmap(x + 0, y + 0).floor
    If x - 1 >= 0 Then hXmY0 = cmap(x - 1, y + 0).floor
    If y - 1 >= 0 Then hX0Ym = cmap(x + 0, y - 1).floor
    If y + 1 < rattr.ay Then 
      IF cmap(x + 0, y + 0).floor >= cmap(x + 0, y + 1).floor THEN
        hX0Yp = 0
      ELSE
        hX0Yp = cmap(x + 0, y + 1).floor - cmap(x + 0, y + 0).floor
      ENDIF
    ENDIF    
    If x + 1 < rattr.ax Then 
      IF cmap(x + 0, y + 0).floor >= cmap(x + 1, y + 0).floor THEN
        hXpY0 = 0
      ELSE
        hXpY0 = cmap(x + 1, y + 0).floor - cmap(x + 0, y + 0).floor
      ENDIF
    ENDIF

  'Calculate cell heights (ceiling)
  ElseIf mode = 2 Then
    If cmap(x, y).ceiling = 0 or cmap(x, y).height = 0 Then Exit Sub
    hX0Y0=0:hX0Ym=0:hX0Yp=0:hXpY0=0:hXmY0=0 
    hX0Y0 = cmap(x + 0, y + 0).ceiling
    If x - 1 >= 0 Then 
    	If cmap(x - 1, y + 0).height > 0 Then
        hXmY0 = cmap(x - 1, y + 0).ceiling + cmap(x - 1, y + 0).height - cmap(x, y).height
      Else
        hXmY0 = cmap(x - 1, y + 0).ceiling
      endif
    endif
    If y - 1 >= 0 Then
    	If cmap(x + 0, y - 1).height > 0 Then
        hX0Ym = cmap(x + 0, y - 1).ceiling + cmap(x + 0, y - 1).height - cmap(x, y).height
      Else
        hX0Ym = cmap(x + 0, y - 1).ceiling
      endif
    endif
    If y + 1 < rattr.ay Then
    	If cmap(x + 0, y + 1).height > 0 Then
        hX0Yp = cmap(x + 0, y + 1).ceiling + cmap(x + 0, y + 1).height - cmap(x, y).height
      Else
        hX0Yp = cmap(x + 0, y + 1).ceiling
      endif
    endif
    If x + 1 < rattr.ax Then 
      If cmap(x + 1, y + 0).height > 0 Then
        hXpY0 = cmap(x + 1, y + 0).ceiling + cmap(x + 1, y + 0).height - cmap(x, y).height
      Else
        hXpY0 = cmap(x + 1, y + 0).ceiling
      endif
    endif
  endif
  
  ' Calculate ground cell points
  a1 = (x + 0) * rattr.cellsize: a2 = (y + 0) * rattr.cellsize: a3 = hX0Y0 * rattr.cellsize
  b1 = (x + 1) * rattr.cellsize: b2 = (y + 0) * rattr.cellsize: b3 = hX0Y0 * rattr.cellsize
  c1 = (x + 1) * rattr.cellsize: c2 = (y + 1) * rattr.cellsize: c3 = hX0Y0 * rattr.cellsize
  d1 = (x + 0) * rattr.cellsize: d2 = (y + 1) * rattr.cellsize: d3 = hX0Y0 * rattr.cellsize

  'Calculate left side points
  IF hX0Y0 <> hXpY0 AND x + 1 < rattr.ax THEN
    If hX0Y0 > hXpY0 Then LSide = +1
    If hX0Y0 < hXpY0 Then LSide = -1
    e1 = (x + 1) * rattr.cellsize: e2 = (y + 0) * rattr.cellsize: e3 = hXpY0 * rattr.cellsize
    f1 = (x + 1) * rattr.cellsize: f2 = (y + 1) * rattr.cellsize: f3 = hXpY0 * rattr.cellsize
  ElseIf x = rattr.ax - 1 and hX0Y0 > 0 THEN
    LSide = +1
    e1 = (x + 1) * rattr.cellsize: e2 = (y + 0) * rattr.cellsize: e3 = 0
    f1 = (x + 1) * rattr.cellsize: f2 = (y + 1) * rattr.cellsize: f3 = 0
  ELSE
    LSide = 0
  END IF

  'Calculate right side points
  IF hX0Y0 <> hX0Yp AND y + 1 < rattr.ay THEN
    If hX0Y0 > hX0Yp Then RSide = +1
    If hX0Y0 < hX0Yp Then RSide = -1
    g1 = (x + 1) * rattr.cellsize: g2 = (y + 1) * rattr.cellsize: g3 = hX0Yp * rattr.cellsize
    h1 = (x + 0) * rattr.cellsize: h2 = (y + 1) * rattr.cellsize: h3 = hX0Yp * rattr.cellsize
  ElseIf y = rattr.ay - 1 and hX0Y0 > 0 THEN
    RSide = +1
    g1 = (x + 1) * rattr.cellsize: g2 = (y + 1) * rattr.cellsize: g3 = 0
    h1 = (x + 0) * rattr.cellsize: h2 = (y + 1) * rattr.cellsize: h3 = 0
  ELSE
    RSide = 0
  END IF
 
  'Increase drawing poings for midle & ceiling modes
  IF mode = 1 THEN
    a3 = a3 + cmap(x,y).floor * rattr.cellsize
    b3 = b3 + cmap(x,y).floor * rattr.cellsize
    c3 = c3 + cmap(x,y).floor * rattr.cellsize
    d3 = d3 + cmap(x,y).floor * rattr.cellsize
    e3 = e3 + cmap(x,y).floor * rattr.cellsize
    f3 = f3 + cmap(x,y).floor * rattr.cellsize
    g3 = g3 + cmap(x,y).floor * rattr.cellsize
    h3 = h3 + cmap(x,y).floor * rattr.cellsize
  ElseIf mode = 2 THEN
    a3 = a3 + cmap(x,y).height * rattr.cellsize
    b3 = b3 + cmap(x,y).height * rattr.cellsize
    c3 = c3 + cmap(x,y).height * rattr.cellsize
    d3 = d3 + cmap(x,y).height * rattr.cellsize
    e3 = e3 + cmap(x,y).height * rattr.cellsize
    f3 = f3 + cmap(x,y).height * rattr.cellsize
    g3 = g3 + cmap(x,y).height * rattr.cellsize
    h3 = h3 + cmap(x,y).height * rattr.cellsize
  ENDIF

  'Draw textures (ground face & positive faces)
  IF disp_text <> MODE_OFF THEN
    
    'Set texture mapping mode
    SetTextureMode(disp_text)

    'Calculate ground color
    IF disp_text = MODE_SOLID OR disp_text = MODE_BOX THEN
      IF mode = 0 THEN
        abs_height = hX0Y0
      ELSEIF mode = 2 THEN
        abs_height = hX0Y0 + cmap(x,y).height
      ENDIF
      abs_height = 128 + abs_height * 10
      IF     hX0Y0 > 0 THEN: ISOTexMappingColor(RGB(128,128,abs_height))
      ELSEIF hX0Y0 < 0 THEN: ISOTexMappingColor(RGB(abs_height,0,0))
      ELSEIF hX0Y0 = 0 THEN: ISOTexMappingColor(COL_GRND)
      ENDIF
    ENDIF
    
    'Draw ground textures
    if mode = 0 and cmap(x,y).texfb <> "" then
      ReadTextureOnce 2, cmap(x,y).texfb, tax, tay, imgfb, 0
      offsx = (x * rattr.cellsize) MOD tax
      offsy = (y * rattr.cellsize) MOD tay
      ISOMap a1, a2, a3, rattr.cellsize, rattr.cellsize, tax, tay, offsx, offsy, imgfb, ISOZPLANE, 0
    elseif mode = 2 and cmap(x,y).texcb <> "" then
      ReadTextureOnce 2, cmap(x,y).texcb, tax, tay, imgcb, 1
      offsx = (x * rattr.cellsize) MOD tax
      offsy = (y * rattr.cellsize) MOD tay
      ISOMap a1, a2, a3, rattr.cellsize, rattr.cellsize, tax, tay, offsx, offsy, imgcb, ISOZPLANE, 0
    endif
    
    'Draw left side (positive)
    If LSide = +1 THEN
      ISOTexMappingColor(COL_LEFT)
      if mode = 0 and cmap(x,y).texfl <> "" then
        ReadTextureOnce 1, cmap(x,y).texfl, tax, tay, imgfl, 2
        offsx = (y * rattr.cellsize) MOD tax
        offsy = (e3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).offlx * rattr.cellsize
        offsy = offsy + cmap(x,y).offly * rattr.cellsize
        ISOMap e1, e2, e3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgfl, ISOXPLANE, 0
      elseif mode = 1 and cmap(x,y).texml <> "" then
        ReadTextureOnce 1, cmap(x,y).texml, tax, tay, imgml, 3
        offsx = (y * rattr.cellsize) MOD tax
        offsy = 0
        offsx = offsx + cmap(x,y).ofmlx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofmly * rattr.cellsize
        ISOMap e1, e2, e3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgml, ISOXPLANE, 0
      elseif mode = 2 and cmap(x,y).texcl <> "" then
        ReadTextureOnce 1, cmap(x,y).texcl, tax, tay, imgcl, 3
        offsx = (y * rattr.cellsize) MOD tax
        offsy = (e3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).ofclx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofcly * rattr.cellsize
        ISOMap e1, e2, e3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgcl, ISOXPLANE, 0
      endif
    END IF

    'Draw right side (positive)
    IF RSide = +1 THEN
      ISOTexMappingColor(COL_RIGHT)
      if mode = 0 and cmap(x,y).texfr <> "" then
        ReadTextureOnce 1, cmap(x,y).texfr, tax, tay, imgfr, 4
        offsx = (x * rattr.cellsize) MOD tax
        offsy = (h3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).offrx * rattr.cellsize
        offsy = offsy + cmap(x,y).offry * rattr.cellsize
        ISOMap h1, h2, h3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgfr, ISOYPLANE, 0
      elseif mode = 1 and cmap(x,y).texmr <> "" then
        ReadTextureOnce 1, cmap(x,y).texmr, tax, tay, imgmr, 5
        offsx = (x * rattr.cellsize) MOD tax
        offsy = 0
        offsx = offsx + cmap(x,y).ofmrx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofmry * rattr.cellsize
        ISOMap h1, h2, h3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgmr, ISOYPLANE, 0
      elseif mode = 2 and cmap(x,y).texcr <> "" then
        ReadTextureOnce 1, cmap(x,y).texcr, tax, tay, imgcr, 5
        offsx = (x * rattr.cellsize) MOD tax
        offsy = (h3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).ofcrx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofcry * rattr.cellsize
        ISOMap h1, h2, h3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgcr, ISOYPLANE, 0
      endif
    END IF
    
    'Draw left side (negative)
    If LSide = -1 Then
      ISOTexMappingColor(COL_LEFT)
      if mode = 0 and cmap(x,y).texfl <> "" then
        ReadTextureOnce 1, cmap(x,y).texfl, tax, tay, imgfl, 2
        offsx = (y * rattr.cellsize) MOD tax
        offsy = (b3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).offlx * rattr.cellsize
        offsy = offsy + cmap(x,y).offly * rattr.cellsize
        ISOMap b1, b2, b3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgfl, ISOXPLANE, 0
      elseif mode = 1 and cmap(x,y).texml <> "" then
        ReadTextureOnce 1, cmap(x,y).texml, tax, tay, imgml, 3
        offsx = (y * rattr.cellsize) MOD tax
        offsy = 0
        offsx = offsx + cmap(x,y).ofmlx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofmly * rattr.cellsize
        ISOMap b1, b2, b3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgml, ISOXPLANE, 0
      elseif mode = 2 and cmap(x,y).texcl <> "" then
        ReadTextureOnce 1, cmap(x,y).texcl, tax, tay, imgcl, 3
        offsx = (y * rattr.cellsize) MOD tax
        offsy = (b3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).ofclx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofcly * rattr.cellsize
        ISOMap b1, b2, b3, rattr.cellsize, Abs(hX0Y0 - hXpY0) * rattr.cellsize, tax, tay, offsx, offsy, imgcl, ISOXPLANE, 0
      endif
    END IF
    
    'Draw right side (negative)
    If RSide = -1 Then
      ISOTexMappingColor(COL_RIGHT)
      if mode = 0 and cmap(x,y).texfr <> "" then
        ReadTextureOnce 1, cmap(x,y).texfr, tax, tay, imgfr, 4
        offsx = (x * rattr.cellsize) MOD tax
        offsy = (d3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).offrx * rattr.cellsize
        offsy = offsy + cmap(x,y).offry * rattr.cellsize
        ISOMap d1, d2, d3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgfr, ISOYPLANE, 0
      elseif mode = 1 and cmap(x,y).texmr <> "" then
        ReadTextureOnce 1, cmap(x,y).texmr, tax, tay, imgmr, 5
        offsx = (x * rattr.cellsize) MOD tax
        offsy = 0
        offsx = offsx + cmap(x,y).ofmrx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofmry * rattr.cellsize
        ISOMap d1, d2, d3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgmr, ISOYPLANE, 0
      elseif mode = 2 and cmap(x,y).texcr <> "" then
        ReadTextureOnce 1, cmap(x,y).texcr, tax, tay, imgcr, 5
        offsx = (x * rattr.cellsize) MOD tax
        offsy = (d3 * rattr.cellsize) MOD tay
        offsx = offsx + cmap(x,y).ofcrx * rattr.cellsize
        offsy = offsy + cmap(x,y).ofcry * rattr.cellsize
        ISOMap d1, d2, d3, rattr.cellsize, Abs(hX0Y0 - hX0Yp) * rattr.cellsize, tax, tay, offsx, offsy, imgcr, ISOYPLANE, 0
      endif
    END IF
  END IF
  
  'Print static objects
  IF disp_objs <> MODE_OFF THEN
    SetTextureMode(disp_objs)
    ISOTexMappingColor(COL_OBJ)
    IF mode = 0 AND cmap(x,y).object <> "" THEN
      found = 0
      FOR i=0 TO numobj - 1
        IF cmap(x,y).object = obj(i).objname THEN
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        ReadTexture 3, obj(i).txname(0), tax, tay, imgob
        IF obj(i).frames = 1 OR disp_objs <> MODE_ON THEN
          ISOMap a1 + rattr.cellsize/2 + tax/4, a2 + rattr.cellsize/2 - tax/4, a3 + rattr.cellsize * cmap(x,y).objheight, tax, tay, tax, tay, 0, 0, imgob, ISOFPLANE, 0
        ELSE
          ObjRefreshEnqueue a1 + rattr.cellsize/2 + tax/4, a2 + rattr.cellsize/2 - tax/4, a3 + rattr.cellsize * cmap(x,y).objheight, obj(i)
          ISOLMapSetPlane a1 + rattr.cellsize/2 + tax/4, a2 + rattr.cellsize/2 - tax/4, a3 + rattr.cellsize * cmap(x,y).objheight, tax, tay, tax, tay, 0, 0, imgob, ISOFPLANE, 1, 1
        END IF
      END IF
    END IF
  
  END IF

END SUB

' Print room ground
SUB RoomMap (rattr AS roomattr, cmap() AS mapcell, light() AS maplight, _
             ovtex() AS mapovtex, lmapinit AS INTEGER)

  'Variables
  DIM AS INTEGER maxk, k, t, i, j
  DIM AS INTEGER tax, tay
  
  'Set isometric center (First cell)
  ISOCenter rattr.px, rattr.py

  'Set z-Buffer
  ISOZBufferEnable
  ISOZBufferLayer 1
  ISOZBufferClear
  
  'Set up light sources
  ISOLightClearAll
  ISOLightAproxFactor rattr.lightaprox
  ISOLightAmbient rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb
  IF disp_light = MODE_LDYN OR disp_light = MODE_LMAP THEN
    FOR i = 0 TO MAXLIGHT - 1
      IF light(i).used = 1 THEN
        ISOLightSource rattr.cellsize*light(i).x + rattr.cellsize/2 + light(i).dx,_
                       rattr.cellsize*light(i).y + rattr.cellsize/2 + light(i).dy,_
                       rattr.cellsize*light(i).z + rattr.cellsize/2 + light(i).dz,_
                       light(i).lum, light(i).r, light(i).g, light(i).b
      ENDIF
    NEXT i
  ENDIF
  IF disp_light = MODE_LMAP THEN
    ISOLMapUseFLag(1)
  ELSE
    ISOLMapUseFLag(0)
  ENDIF

  'Init light map and shadow planes
  IF lmapinit = 1 THEN
    ISOLMapInit
    ISOLMapStorePlanes(1)
  ENDIF
  
  'Kill object refresh table
  ObjRefreshKill
  
  'Cell loop
  maxk = rattr.ax + rattr.ay
  FOR k = maxk TO 0 STEP -1
  FOR t = rattr.ax - 1 TO 0 STEP - 1
	i = t: j = -t + k
	IF (k >= t) AND (i < rattr.ax) AND (j < rattr.ay) THEN
	  RoomGCell rattr, i, j, cmap(), 0
	  RoomGCell rattr, i, j, cmap(), 1	    
	  RoomGCell rattr, i, j, cmap(), 2
    END IF
  NEXT t
  NEXT k
  
  'Draw overlay textures
  IF disp_text <> MODE_OFF THEN
    SetTextureMode(disp_text)
    ISOTexMappingColor(COL_OVL)
    ISOZBufferOverwrite 1
    FOR i=0 TO MAXOVTEX - 1
      IF ovtex(i).used = 1 AND ovtex(i).active = 1 THEN
        IF ReadTexture(ovtex(i).texdir, ovtex(i).texname, tax, tay, imgov) = 0 THEN
          ISOMap rattr.cellsize * ovtex(i).x + ovtex(i).dx, _
          rattr.cellsize * ovtex(i).y + ovtex(i).dy, _
          rattr.cellsize * ovtex(i).z + ovtex(i).dz, _
          (ovtex(i).ax * rattr.cellsize) / 16, _
          (ovtex(i).ay * rattr.cellsize) / 16, _
          tax, tay, 0, 0, imgov, ovtex(i).plane, 0
        ENDIF
      ENDIF
    NEXT i
    ISOZBufferOverwrite 0
  ENDIF
 
  'Finish plane storage for shadow calculations
  ISOLMapStorePlanes(0)

END SUB

' Init map data
SUB MapInit(rattr AS roomattr, cmap() AS mapcell, light() AS maplight, _
            ovtex() AS mapovtex, event() AS mapevent, actio() as mapactio )

  'Variables
  DIM AS INTEGER i,j

  'Init map attributes
  rattr.ax = 13
  rattr.ay = 13
  rattr.px = (AX0 / 2) + 62
  rattr.py = (AY0 / 2) + 20
  rattr.cellsize   = 16
  rattr.lightambl  = 0.75
  rattr.lightambr  = 100
  rattr.lightambg  = 100
  rattr.lightambb  = 100
  rattr.lightaprox = 7
  rattr.lmapdifuse = 4
  rattr.lmapstore  = 0
  rattr.depx = 1
  rattr.depy = 1
  rattr.epmap = 0
  rattr.mname = ""

  'Init cell data
  FOR i = 0 TO MAXCELLX - 1
  FOR j = 0 TO MAXCELLY - 1
    cmap(i,j).height    = 0         
    cmap(i,j).floor     = 0         
    cmap(i,j).ceiling   = 0         
    cmap(i,j).texfl = "wall0001.pcx"
    cmap(i,j).texfr = "wall0001.pcx"
    cmap(i,j).texfb = "wall0002.pcx"
    cmap(i,j).texcl = "wall0001.pcx"
    cmap(i,j).texcr = "wall0001.pcx"
    cmap(i,j).texcb = "wall0002.pcx"
    cmap(i,j).texmr     = ""        
    cmap(i,j).texml     = ""        
    cmap(i,j).ofcrx     = 0         
    cmap(i,j).ofclx     = 0         
    cmap(i,j).ofmrx     = 0         
    cmap(i,j).ofmlx     = 0         
    cmap(i,j).offrx     = 0         
    cmap(i,j).offlx     = 0         
    cmap(i,j).ofcry     = 0         
    cmap(i,j).ofcly     = 0         
    cmap(i,j).ofmry     = 0         
    cmap(i,j).ofmly     = 0         
    cmap(i,j).offry     = 0         
    cmap(i,j).offly     = 0         
    cmap(i,j).confb     = 0         
    cmap(i,j).concb     = 0         
    cmap(i,j).concr     = 0         
    cmap(i,j).concl     = 0         
    cmap(i,j).conmr     = 0         
    cmap(i,j).conml     = 0         
    cmap(i,j).confr     = 0         
    cmap(i,j).confl     = 0         
    cmap(i,j).pasmr     = 0         
    cmap(i,j).pasml     = 0         
    cmap(i,j).object    = ""        
    cmap(i,j).objheight = 0         
    cmap(i,j).eventid   = 0
  NEXT j
  NEXT i

  'Init light data
  FOR i=0 TO MAXLIGHT - 1
    light(i).x    = 0
    light(i).y    = 0
    light(i).z    = 0
    light(i).dx   = 0
    light(i).dy   = 0
    light(i).dz   = 0
    light(i).lum  = 0
    light(i).r    = 0
    light(i).g    = 0
    light(i).b    = 0
    light(i).used = 0
  NEXT i
  
  'Init overlay texture data
  FOR i=0 TO MAXOVTEX - 1
    ovtex(i).x       = 0 
    ovtex(i).y       = 0 
    ovtex(i).z       = 0 
    ovtex(i).dx      = 0 
    ovtex(i).dy      = 0 
    ovtex(i).dz      = 0 
    ovtex(i).texdir  = 1
    ovtex(i).texname = ""
    ovtex(i).ax      = 0 
    ovtex(i).ay      = 0 
    ovtex(i).plane   = ISOXPLANE
    ovtex(i).group   = 0 
    ovtex(i).used    = 0 
    ovtex(i).active  = 1 
  NEXT i

  'Init events
  FOR i=0 TO MAXEVENT - 1
    event(i).id = 0
    event(i).evtype = ""
    event(i).value = 0
    event(i).evname = ""
  NEXT i

  'Actions
  FOR i=0 TO MAXACTIO-1
    actio(i).oper = ""
    FOR j=0 TO MAXPARMS-1
      actio(i).parm(j) = ""
    NEXT j
  NEXT i 
  
  'Init action script
  rattr.numact = 12
  actio(00).oper = ACT_COMMENT      :actio(00).parm(0) = "Action script"
  actio(01).oper = ACT_BLANKLINE           
  actio(02).oper = ACT_COMMENT      :actio(02).parm(0) = "Event door - value 0"
  actio(03).oper = ACT_EVENT        :actio(03).parm(0) = "01":actio(03).parm(1) = "00"
  actio(04).oper = ACT_OVTEX_ENABLE :actio(04).parm(0) = "01"
  actio(05).oper = ACT_OVTEX_DISABLE:actio(05).parm(0) = "02"
  actio(06).oper = ACT_BLANKLINE           
  actio(07).oper = ACT_COMMENT      :actio(07).parm(0) = "Event door - value 1"
  actio(08).oper = ACT_EVENT        :actio(08).parm(0) = "01":actio(08).parm(1) = "01"
  actio(09).oper = ACT_OVTEX_DISABLE:actio(09).parm(0) = "01"
  actio(10).oper = ACT_OVTEX_ENABLE :actio(10).parm(0) = "02"
  actio(11).oper = ACT_BLANKLINE           

END SUB

' Isometric cursor drawing
SUB RoomCursor (x AS INTEGER, y AS INTEGER, rattr AS roomattr, cmap() AS mapcell, light() AS maplight)
  
  'Init graphics
  StoreDispMode
  SetDispMode(TEXT_MODE,MODE_BOX)
  SetDispMode(OBJS_MODE,MODE_FIXED)
  ISOLightAmbient rattr.lightambl+1.5, rattr.lightambr, rattr.lightambg, rattr.lightambb
  ISOZBufferLayer 2
  ISOZBufferOverwrite 1
  ISOLMapStorePlanes(0)
  ISOLMapUseFlag(0)
  RoomGCell rattr, x, y, cmap(), 0
  RoomGCell rattr, x, y, cmap(), 1
  RoomGCell rattr, x, y, cmap(), 2
  RestoreDispMode
  ISOLightAmbient rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb
  ISOZBufferOverwrite 0
  
  'Draw height
  ISOSquare x*rattr.cellsize,y*rattr.cellsize,0,rattr.cellsize,rattr.cellsize,ISOZPLANE, COL_CURS
  IF cmap(x,y).height <> 0 THEN 
    ISOSquare x*rattr.cellsize,y*rattr.cellsize,cmap(x,y).height*rattr.cellsize,_
               rattr.cellsize,rattr.cellsize,ISOZPLANE, COL_HEIG
  ENDIF

END SUB

' Sample cell print
SUB CellSample(px AS INTEGER, py AS INTEGER, BYVAL rattr AS roomattr, BYVAL cell AS mapcell)
  
  'Variables
  DIM AS mapcell cmap1(1,1)
  
  'Prepare sample cell
  cmap1(0,0) = cell

  'Init graphics
  StoreDispMode
  SetDispMode(TEXT_MODE,MODE_ON)
  SetDispMode(OBJS_MODE,MODE_FIXED)
  SetDispMode(LIGHT_MODE,MODE_LDYN)
  ISOLightAmbient rattr.lightambl+1, rattr.lightambr, rattr.lightambg, rattr.lightambb
  ISOZBufferOverwrite(1)
  ISOZBufferLayer(2)
  ISOLMapStorePlanes(0)
  ISOLMapUseFlag(0)
  VIEW SCREEN (FCol(00)-1,FRow(03)-3)-(FCol(8)-1,FRow(12)-1),0
  ISOCenter(px,py)
  
  'Print cell sample
  RoomGCell rattr, 0, 0, cmap1(), 0
  RoomGCell rattr, 0, 0, cmap1(), 1
  RoomGCell rattr, 0, 0, cmap1(), 2
  
  'Restore graphics
  VIEW SCREEN
  ISOZBufferOverwrite(0)
  ISOCenter(rattr.px,rattr.py)
  RestoreDispMode
  ISOLightAmbient rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb
  
END SUB

' Print room cells that contain link points
SUB RoomLinkPoints(mapname AS STRING, rattr AS roomattr, cmap() AS mapcell)
  
  'Variables
  DIM AS INTEGER i,j,finish
  DIM AS STRING map1,map2
  DIM AS INTEGER x1,y1,x2,y2,way
  
  'Modify graphics
  StoreDispMode
  SetDispMode(TEXT_MODE,MODE_ON)
  SetDispMode(OBJS_MODE,MODE_FIXED)
  SetDispMode(LIGHT_MODE,MODE_LDYN)
  ISOZBufferLayer(2)
  ISOZBufferOverwrite(1)
  ISOLMapStorePlanes(0)
  ISOLMapUseFlag(0)
  
  'Default entry point
  ISOLightAmbient rattr.lightambl+1.5,100,100,0
  RoomGCell rattr, rattr.depx, rattr.depy, cmap(), 0
  
  'Link point loop
  _LinkListStart(mapname)
  _LinkListNext(map1,x1,y1,map2,x2,y2,way,finish)
  WHILE finish = 0
    IF way = +1 THEN 'One way exit
      ISOLightAmbient rattr.lightambl+1.5,0,0,100
    ELSEIF way = -1 THEN 'One way entry
      ISOLightAmbient rattr.lightambl+1.5,100,0,0
    ELSEIF way = 0 THEN 'Two way link
      ISOLightAmbient rattr.lightambl+1.5,0,100,0
    ENDIF
    RoomGCell rattr, x1, y1, cmap(), 0
    _LinkListNext(map1,x1,y1,map2,x2,y2,way,finish)
  WEND

  'Restore graphics
  RestoreDispMode
  ISOLightAmbient rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb
  ISOZBufferOverwrite 0

END SUB

' Print room cells that contain link points
SUB RoomEvents(rattr AS roomattr, cmap() AS mapcell)
  
  'Variables
  DIM AS INTEGER i,j,finish
  DIM AS STRING map1,map2
  DIM AS INTEGER x1,y1,x2,y2,way
  
  'Modify graphics
  StoreDispMode
  SetDispMode(TEXT_MODE,MODE_ON)
  SetDispMode(OBJS_MODE,MODE_FIXED)
  SetDispMode(LIGHT_MODE,MODE_LDYN)
  ISOZBufferLayer(2)
  ISOZBufferOverwrite(1)
  ISOLMapStorePlanes(0)
  ISOLMapUseFlag(0)
  
  'Cell loop
  FOR i=0 TO rattr.ax-1
  FOR j=0 TO rattr.ay-1
    IF cmap(i,j).eventid <> 0 THEN
      ISOLightAmbient rattr.lightambl+1.5,100,50,0
      RoomGCell rattr, i, j, cmap(), 0
    ENDIF
  NEXT j
  NEXT i

  'Restore graphics
  RestoreDispMode
  ISOLightAmbient rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb
  ISOZBufferOverwrite 0

END SUB

' Isometric cursor drawing for light sources
SUB LightCursor (index AS INTEGER, light() AS maplight, rattr AS roomattr)
  
  'Variables
  DIM AS UINTEGER col
  
  'Cursor color
  IF index = 99 THEN
    col = RGB(255,0,0)
  ELSEIF light(index).used = 1 THEN
    col = RGB(255,0,0)
  ELSE
    col = COL_CURS
  ENDIF
  
  'Exit if selected light is ambien light
  IF index = 99 THEN EXIT SUB

  'Draw Cell and height frames
  ISOLine rattr.cellsize*light(index).x,rattr.cellsize*light(index).y,0,_
          rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y,0,col  
  
  ISOLine rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y,0,_
          rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
          0,col
  
  ISOLine rattr.cellsize*light(index).x,rattr.cellsize*light(index).y,0,_
          rattr.cellsize*light(index).x,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
          0,col
  
  ISOLine rattr.cellsize*light(index).x,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,0,_
          rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
          0,col

  ISOSquare light(index).x*rattr.cellsize,light(index).y*rattr.cellsize,0,_
            rattr.cellsize,rattr.cellsize,ISOZPLANE, col

  ISOLine rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
          0,_
          rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
          rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
          rattr.cellsize*light(index).z + rattr.cellsize/2 + light(index).dz,_
          col
  
  ISOCircle rattr.cellsize*light(index).x + rattr.cellsize/2 + light(index).dx,_
            rattr.cellsize*light(index).y + rattr.cellsize/2 + light(index).dy,_
            rattr.cellsize*light(index).z + rattr.cellsize/2 + light(index).dz,_
            3,col,1
END SUB

'Write file 
SUB MapWrite( filename AS STRING, rattr AS roomattr, cmap() AS mapcell, _
              light() AS maplight, ovtex() AS mapovtex, event() AS mapevent, _
              actio() AS mapactio)
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER file
  
  'Delete previous file
  KILL "maps\" + filename

  'Open file
  file = FREEFILE
  OPEN "maps\" + filename FOR BINARY AS #file

  'Save header data
  WRITE# #file, rattr.px, rattr.py, rattr.ax, rattr.ay, rattr.cellsize, _
  rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb, _
  rattr.lmapdifuse, rattr.lmapstore, rattr.depx, rattr.depy, _
  rattr.numevt, rattr.numact, rattr.epmap, rattr.mname
  
  'Save light sources
  FOR i=0 TO MAXLIGHT - 1
    WRITE# #file, light(i).x, light(i).y, light(i).z, light(i).dx, light(i).dy, _
    light(i).dz, light(i).lum, light(i).r, light(i).g, light(i).b, light(i).used
  NEXT i
 
  'Save overlay textures
  FOR i=0 TO MAXOVTEX - 1
    WRITE# #file, ovtex(i).x, ovtex(i).y, ovtex(i).z, ovtex(i).dx, ovtex(i).dy, _
    ovtex(i).dz, ovtex(i).texdir, ovtex(i).texname, ovtex(i).ax, ovtex(i).ay, _
    ovtex(i).plane, ovtex(i).group, ovtex(i).used, ovtex(i).active
  NEXT i
 
  'Save events
  FOR i=0 TO rattr.numevt-1
    WRITE# #file, event(i).id, event(i).evname, event(i).evtype, event(i).value
  NEXT i
  
  'Save actions
  FOR i=0 TO rattr.numact-1
    WRITE# #file, actio(i).oper
    FOR j=0 TO MAXPARMS-1
      WRITE# #file, actio(i).parm(j)
    NEXT j
  NEXT i
  
  'Save cell data
  FOR i=0 TO rattr.ax - 1
  FOR j=0 TO rattr.ay - 1
    WRITE# #file,i,j,cmap(i,j).height,cmap(i,j).floor,cmap(i,j).ceiling,cmap(i,j).object,cmap(i,j).objheight
    WRITE# #file,i,j,cmap(i,j).texfb,cmap(i,j).texcb,cmap(i,j).texcr,cmap(i,j).texcl
    WRITE# #file,i,j,cmap(i,j).texmr,cmap(i,j).texml,cmap(i,j).texfr,cmap(i,j).texfl
    WRITE# #file,i,j,cmap(i,j).ofcrx,cmap(i,j).ofclx,cmap(i,j).ofmrx,cmap(i,j).ofmlx,cmap(i,j).offrx,cmap(i,j).offlx
    WRITE# #file,i,j,cmap(i,j).ofcry,cmap(i,j).ofcly,cmap(i,j).ofmry,cmap(i,j).ofmly,cmap(i,j).offry,cmap(i,j).offly
    WRITE# #file,i,j,cmap(i,j).confb,cmap(i,j).concb,cmap(i,j).concr,cmap(i,j).concl
    WRITE# #file,i,j,cmap(i,j).conmr,cmap(i,j).conml,cmap(i,j).confr,cmap(i,j).confl
    WRITE# #file,i,j,cmap(i,j).pasmr,cmap(i,j).pasml, cmap(i,j).eventid
  NEXT j
  NEXT i

  'Save light map
  IF rattr.lmapstore = 1 THEN
    ISOLMapExport lmap(), lmaplen
    PUT #file,,lmaplen
    FOR i=0 TO lmaplen-1
      PUT #file,,lmap(i)
    NEXT i
  ENDIF
  
  'Close file
  CLOSE #File

END SUB

'Load file 
SUB MapRead(filename AS STRING, rattr AS roomattr, cmap() AS mapcell, _
            light() AS maplight, ovtex() AS mapovtex, event() AS mapevent, _
            actio() AS mapactio)

  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER a,b
  DIM AS INTEGER file
  DIM AS mapovtex ovtex1

  'Open file
  file = FREEFILE
  OPEN "maps\" + filename FOR BINARY AS #file

  'Get header data
  INPUT# #file, rattr.px, rattr.py, rattr.ax, rattr.ay, rattr.cellsize, _ 
  rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb, _
  rattr.lmapdifuse, rattr.lmapstore, rattr.depx, rattr.depy, _
  rattr.numevt, rattr.numact, rattr.epmap, rattr.mname
   
  'Get light sources
  FOR i=0 TO MAXLIGHT - 1
    INPUT# #file, light(i).x, light(i).y, light(i).z, light(i).dx, light(i).dy, _
    light(i).dz, light(i).lum, light(i).r, light(i).g, light(i).b, light(i).used
  NEXT i

  'Get overlay textures
  FOR i=0 TO MAXOVTEX - 1
    INPUT# #file, ovtex(i).x, ovtex(i).y, ovtex(i).z, ovtex(i).dx, ovtex(i).dy, _
    ovtex(i).dz, ovtex(i).texdir, ovtex(i).texname, ovtex(i).ax, ovtex(i).ay, _
    ovtex(i).plane, ovtex(i).group, ovtex(i).used, ovtex(i).active
  NEXT i
 
  'Save events
  FOR i=0 TO rattr.numevt-1
    INPUT# #file, event(i).id, event(i).evname, event(i).evtype, event(i).value
  NEXT i
  
  'Save actions
  FOR i=0 TO rattr.numact-1
    INPUT# #file, actio(i).oper
    FOR j=0 TO MAXPARMS-1
      INPUT# #file, actio(i).parm(j)
    NEXT j
  NEXT i
  
  'Get cell data
  FOR i=0 TO rattr.ax - 1
  FOR j=0 TO rattr.ay - 1
    INPUT# #file,a,b,cmap(i,j).height,cmap(i,j).floor,cmap(i,j).ceiling,cmap(i,j).object,cmap(i,j).objheight
    INPUT# #file,a,b,cmap(i,j).texfb,cmap(i,j).texcb,cmap(i,j).texcr,cmap(i,j).texcl
    INPUT# #file,a,b,cmap(i,j).texmr,cmap(i,j).texml,cmap(i,j).texfr,cmap(i,j).texfl
    INPUT# #file,a,b,cmap(i,j).ofcrx,cmap(i,j).ofclx,cmap(i,j).ofmrx,cmap(i,j).ofmlx,cmap(i,j).offrx,cmap(i,j).offlx
    INPUT# #file,a,b,cmap(i,j).ofcry,cmap(i,j).ofcly,cmap(i,j).ofmry,cmap(i,j).ofmly,cmap(i,j).offry,cmap(i,j).offly
    INPUT# #file,i,j,cmap(i,j).confb,cmap(i,j).concb,cmap(i,j).concr,cmap(i,j).concl
    INPUT# #file,i,j,cmap(i,j).conmr,cmap(i,j).conml,cmap(i,j).confr,cmap(i,j).confl
    INPUT# #file,i,j,cmap(i,j).pasmr,cmap(i,j).pasml, cmap(i,j).eventid
  NEXT j
  NEXT i
  
  'Save light map
  IF rattr.lmapstore = 1 THEN
    GET #file,,lmaplen
    FOR i=0 TO lmaplen-1
      GET #file,,lmap(i)
    NEXT i
    ISOLMapImport lmap(), lmaplen
  ENDIF
  
  'Close file
  CLOSE #file

END SUB

'Load file attributes
SUB MapReadAttr(filename AS STRING, rattr AS roomattr)

  'Variables
  DIM AS INTEGER file

  'Open file
  file = FREEFILE
  OPEN "maps\" + filename FOR BINARY AS #file

  'Get header data
  INPUT# #file, rattr.px, rattr.py, rattr.ax, rattr.ay, rattr.cellsize, _ 
  rattr.lightambl, rattr.lightambr, rattr.lightambg, rattr.lightambb, _
  rattr.lmapdifuse, rattr.lmapstore, rattr.depx, rattr.depy, _
  rattr.numevt, rattr.numact, rattr.epmap, rattr.mname
   
  'Close file
  CLOSE #file

END SUB

'Object list load
SUB LoadObjectList
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER file
  
  'Open object list file
  file = FREEFILE
  OPEN "objects.dat" FOR INPUT AS #file
  
  'Get number of objects
  INPUT# #file, numobj
  
  'Load loop
  i=0
  WHILE NOT EOF(file) AND i < numobj
    INPUT# #file, obj(i).objname, obj(i).frames
    obj(i).delet = 0
    FOR j=0 TO obj(i).frames - 1
      INPUT# #file, obj(i).txname(j)
    NEXT j
    i = i + 1
  WEND
  
  'Close file
  CLOSE #file

END SUB

'Object list save
SUB SaveObjectList
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER file
  DIM AS INTEGER totalobj
  
  'Open object list file
  file = FREEFILE
  OPEN "objects.dat" FOR OUTPUT AS #file
  
  'Calculate number of objects
  totalobj = 0
  FOR i=0 TO numobj - 1
    IF obj(i).delet = 0 THEN
      totalobj = totalobj + 1
    ENDIF
  NEXT i
  
  'Write number of objects
  WRITE# #file, totalobj
  
  'Save loop
  i=0
  FOR i=0 TO numobj - 1
    IF obj(i).delet = 0 AND obj(i).objname <> "" THEN
      WRITE# #file, obj(i).objname, obj(i).frames
      FOR j=0 TO obj(i).frames - 1
        WRITE# #file, obj(i).txname(j)
      NEXT j
    ENDIF
  NEXT i
  
  'Close file
  CLOSE #file
  
  'Modify total number of objects
  numobj = totalobj

END SUB

'Scan new objects in directory
SUB ScanNewObjects
  
  'Constants
  CONST MAXFILES = 4000
  
  'Variables
  DIM AS INTEGER i,j,k
  DIM AS INTEGER objindex
  DIM AS INTEGER pipefile
  DIM AS STRING folder
  DIM AS INTEGER numfil
  DIM AS INTEGER used
  DIM AS INTEGER reserved
  DIM curobj AS STRING * 15
  DIM prvobj AS STRING * 15
  DIM AS INTEGER frame
  DIM AS INTEGER found
  DIM AS objdef newobj
  DIM AS obfile obfiles(MAXFILES)
  
  'Get object directory
  folder = GetTexPath(3)
 
  'Read object directory
  i = 0
  pipefile = FREEFILE
  OPEN PIPE "dir " + folder + "*.pcx " + folder + "*.px2 /b /on" FOR INPUT AS #pipefile 
  WHILE NOT EOF(pipefile) AND k < MAXFILES
    LINE INPUT #pipefile, obfiles(i).file
    i = i + 1
  WEND
  CLOSE #pipefile
  numfil = i - 1
  
  'Check actions to be performed on files
  FOR i=0 TO numfil - 1
    used=0
    reserved = 0
    FOR j=0 TO numobj
    FOR k=0 TO obj(j).frames - 1
      IF obfiles(i).file = obj(j).txname(k) THEN used = 1
      IF MID$(obfiles(i).file,1,6) = MID$(obj(j).txname(k),1,6) THEN reserved = 1
    NEXT k
    NEXT j
    IF used=0 AND reserved=0 THEN 
      obfiles(i).action = 1 'Insert new object
    ELSEIF used=0 AND reserved=1 THEN 
      obfiles(i).action = 2 'Insert new frame
    ELSE
      obfiles(i).action = 0'Already used
    ENDIF
  NEXT i
  
  'Insert new objects
  prvobj = ""
  frame = 0
  FOR i=0 to numfil - 1
    IF obfiles(i).action = 1 THEN
      curobj = MID$(obfiles(i).file,1,6)  
      IF prvobj <> curobj AND frame > 0 THEN
        obj(numobj) = newobj
        numobj = numobj + 1
        frame = 0
      ENDIF
      newobj.objname = curobj
      newobj.txname(frame) = obfiles(i).file
      newobj.frames = frame + 1
      frame = frame + 1
      prvobj = MID$(obfiles(i).file,1,6)  
    ENDIF
  NEXT i
  IF frame > 0 THEN
    obj(numobj) = newobj
    numobj = numobj + 1
  ENDIF
  
  'Insert new frames
  prvobj = ""
  frame = 0
  FOR i=0 to numfil - 1
    IF obfiles(i).action = 2 THEN
      found = 0
      FOR j=0 TO numobj - 1
      FOR k=0 TO obj(j).frames - 1
        IF MID$(obfiles(i).file,1,6) = MID$(obj(j).txname(k),1,6) THEN
          found = 1: objindex = j
        ENDIF
      NEXT k
      NEXT j
      IF found = 1 THEN
        frame = obj(objindex).frames
        obj(objindex).txname(frame) = obfiles(i).file
        obj(objindex).frames = obj(objindex).frames + 1
      ENDIF
    ENDIF
  NEXT i
  
END SUB

'Object refresh enqueue
SUB ObjRefreshEnqueue(x AS INTEGER, y AS INTEGER, z AS INTEGER, obj AS objdef)
  
  'Variables
  DIM AS INTEGER i,j
  
  'Exit if enqueue table is full
  IF numobjref - 1 = MAXOBJRF THEN EXIT SUB
  
  'Enqueue new object in refresh table
  i = numobjref
  objref(i).x = x
  objref(i).y = y
  objref(i).z = z
  objref(i).obj = obj
  objref(i).frame = RND * (obj.frames-1)
  
  'Increase counter
  numobjref = numobjref + 1

END SUB

'Kill Object refresh table
SUB ObjRefreshKill
  
  'Variables
  DIM AS INTEGER i,j
  
  'Clear object refresh table
  FOR i=0 TO numobjref - 1
    objref(i).x = 0
    objref(i).y = 0
    objref(i).z = 0
    objref(i).frame = 0
    objref(i).obj.objname = ""
    FOR j=0 TO objref(i).obj.frames - 1
      objref(i).obj.txname(j) = ""
    NEXT j
    objref(i).obj.frames = 0
  NEXT i
  
  'Clear counter
  numobjref = 0

END SUB

'Object refresh mode
SUB ObjRefreshMode(mode AS INTEGER)
  objrefmode = mode
END SUB

'Object refresh routine
SUB ObjRefreshDraw
  
  'Variables
  DIM AS INTEGER i
  DIM AS INTEGER posy
  DIM AS INTEGER ax,ay
  DIM AS UINTEGER PTR ipt
  
  'Texture mapping mode
  ISOTexMappingMode(TEX_NORMAL)

  'Object loop
  FOR i=0 TO numobjref - 1
    
    'Switch on draw mode
    SELECT CASE objrefmode
    
      'Map display mode
      CASE 1
        ReadTexture 3, objref(i).obj.txname(objref(i).frame), ax, ay, imgob
        IF ax > 0 AND ay > 0 THEN
          ISOMap objref(i).x, objref(i).y, objref(i).z, ax, ay, ax, ay, 0, 0, imgob, ISOFPLANE, 0
        ENDIF
        objref(i).frame = objref(i).frame + 1
        IF objref(i).frame = objref(i).obj.frames THEN objref(i).frame = 0

     'Object browser mode
      CASE 2
        LINE (objref(i).x,objref(i).y)-STEP(CLIPAXTEX-1,CLIPAYTEX-1),0,BF
        ReadTexture 3, objref(i).obj.txname(objref(i).frame), ax, ay, ipt
        IF ay >  CLIPAYTEX THEN 
          posy = objref(i).y
        ELSE 
          posy = objref(i).y+CLIPAYTEX-ay
        ENDIF
        ImgDisplayClip objref(i).x, posy, ax, ay, ipt, 0, CLIPAXTEX, CLIPAYTEX
        objref(i).frame = objref(i).frame + 1
        IF objref(i).frame = objref(i).obj.frames THEN objref(i).frame = 0
  
    END SELECT
  
  NEXT i
  
END SUB

'Entity refresh enqueue
FUNCTION EntRefreshEnqueue(x AS INTEGER, y AS INTEGER, z AS INTEGER, _
                           etype AS etypedef) AS INTEGER
  
  'Variables
  DIM AS INTEGER i,found,index
  
  'Search for a free record
  found = 0
  FOR i=0 TO MAXENTRF - 1
    IF entref(i).used = 0 THEN 
      found = 1
      index = i
      EXIT FOR
    ENDIF
  NEXT i
  IF found = 0 THEN
    EntRefreshEnqueue = -1
    EXIT FUNCTION
  ENDIF
  
  'Enqueue new entity in refresh table
  entref(index).x = x
  entref(index).y = y
  entref(index).z = z
  entref(index).x0 = x
  entref(index).y0 = y
  entref(index).z0 = z
  entref(index).etype = etype
  entref(index).state = ENTWALK
  entref(index).stnum = 1
  entref(index).direc = 1
  entref(index).cindex = -1
  entref(index).commd(0) = ""
  entref(index).arg(0)   = 0
  entref(index).acomm    = ""
  entref(index).used  = 1
  
  'Return entity index
  EntRefreshEnqueue = index

END FUNCTION

'Entity refresh enqueue
SUB EntRefreshCommand(index AS INTEGER, commd AS STRING, arg AS INTEGER)
  
  'Variables
  DIM AS INTEGER i
  
  'Exit in command queue is full
  IF entref(index).cindex = MAXCOMMD - 1 THEN EXIT SUB
  
  'Increase index
  entref(index).cindex = entref(index).cindex + 1
  
  'Save command
  i = entref(index).cindex
  entref(index).commd(i) = commd
  entref(index).arg(i) = arg
  
END SUB

'Entity get position
SUB EntRefreshSetMapPointer(rattrp AS roomattr PTR, cmapp AS mapcell PTR)
  rattrptr = rattrp
  cmapptr = cmapp
END SUB

'Kill emtity refresh table
SUB EntRefreshKill
  
  'Variables
  DIM AS INTEGER i,j
  
  'Clear object refresh table
  FOR i=0 TO MAXENTRF - 1
    entref(i).x = 0
    entref(i).y = 0
    entref(i).z = 0
    entref(i).x0 = 0
    entref(i).y0 = 0
    entref(i).z0 = 0
    entref(i).etype.etypename = ""
    entref(i).etype.etypeimg = ""
    entref(i).etype.walksnr = 0
    entref(i).etype.ataksnr = 0
    entref(i).etype.deadsnr = 0
    entref(i).state = ""
    entref(i).stnum = 1
    entref(i).direc = 1
    entref(i).cindex = -1
    FOR j=0 TO MAXCOMMD-1
      entref(i).commd(j) = ""
      entref(i).arg(j)   = 0
    NEXT j
    entref(i).acomm = ""
    entref(i).used  = 0
  NEXT i
  
END SUB

'Entity refresh routine
SUB EntRefreshDraw

  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER cx,cy
  DIM AS INTEGER ax,ay
  DIM AS INTEGER found
  DIM AS INTEGER delet
  DIM AS INTEGER floor1, floor2
  DIM AS UINTEGER PTR ipt

  'Exit if entities are not to be displayed
  IF disp_ents = MODE_OFF THEN EXIT SUB

  'Texture mapping mode
  ISOTexMappingMode(TEX_NORMAL)

  'Object loop
  FOR i=0 TO MAXENTRF - 1
    
    'Process used records
    IF entref(i).used = 1 THEN
    
      'Process command
      IF entref(i).cindex > -1 THEN
      
        'Clear delete command flag
        delet = 0
      
        'Save current position on command change
        IF entref(i).commd(0) <> entref(i).acomm THEN
          entref(i).x0 = entref(i).x
          entref(i).y0 = entref(i).y
          entref(i).z0 = entref(i).z
        ENDIF
        
        'Command translation
        SELECT CASE entref(i).commd(0)
          
          'Move left -> Change direction
          CASE ENCMOVL
            IF entref(i).direc <> 2 THEN
              IF entref(i).direc >= 3 AND entref(i).direc <= 6 THEN
                entref(i).commd(0) = ENCTURR
                entref(i).arg(0) = 2
              ELSE
                entref(i).commd(0) = ENCTURL
                entref(i).arg(0) = 2
              ENDIF
            ENDIF
        
          'Move right -> Change direction
          CASE ENCMOVR
            IF entref(i).direc <> 6 THEN
              IF entref(i).direc >= 2 AND entref(i).direc <= 5 THEN
                entref(i).commd(0) = ENCTURL
                entref(i).arg(0) = 6
              ELSE
                entref(i).commd(0) = ENCTURR
                entref(i).arg(0) = 6
              ENDIF
            ENDIF

          'Move down -> Change direction
          CASE ENCMOVD
            IF entref(i).direc <> 8 THEN
              IF entref(i).direc >= 4 AND entref(i).direc <= 7 THEN
                entref(i).commd(0) = ENCTURL
                entref(i).arg(0) = 8
              ELSE
                entref(i).commd(0) = ENCTURR
                entref(i).arg(0) = 8
              ENDIF
            ENDIF
        
          'Move up -> Change direction
          CASE ENCMOVU
            IF entref(i).direc <> 4 THEN
              IF entref(i).direc >= 4 AND entref(i).direc <= 7 THEN
                entref(i).commd(0) = ENCTURR
                entref(i).arg(0) = 4
              ELSE
                entref(i).commd(0) = ENCTURL
                entref(i).arg(0) = 4
              ENDIF
            ENDIF
        
        END SELECT
        
        'Check valid movement
        IF EntityValidMovement(entref(i)) = 1 THEN
 
          'Process commands
          SELECT CASE entref(i).commd(0)
        
            'Stop
            CASE ENCSTOP
        
            'Turn left
            CASE ENCTURL
              IF entref(i).direc = entref(i).arg(0) THEN 
                delet = 1
              ELSE
                IF entref(i).state <> ENTWALK THEN
                entref(i).state = ENTWALK
                  entref(i).stnum = 1
                ENDIF
                entref(i).direc = entref(i).direc + 1
                IF entref(i).direc > 8 THEN entref(i).direc = 1
                IF entref(i).state <> ENTWALK THEN
                  entref(i).state = ENTWALK
                  entref(i).stnum = 1
                ELSE
                  entref(i).stnum = entref(i).stnum + 1
                  IF entref(i).stnum > entref(i).etype.walksnr THEN
                    entref(i).stnum = 1
                  ENDIF
                ENDIF
              ENDIF
          
            'Turn right
            CASE ENCTURR
              IF entref(i).direc = entref(i).arg(0) THEN 
                delet = 1
              ELSE
                IF entref(i).state <> ENTWALK THEN
                  entref(i).state = ENTWALK
                  entref(i).stnum = 1
                ENDIF
                entref(i).direc = entref(i).direc - 1
                IF entref(i).direc < 1 THEN entref(i).direc = 8
                IF entref(i).state <> ENTWALK THEN
                  entref(i).state = ENTWALK
                  entref(i).stnum = 1
                ELSE
                  entref(i).stnum = entref(i).stnum + 1
                  IF entref(i).stnum > entref(i).etype.walksnr THEN
                    entref(i).stnum = 1
                  ENDIF
                ENDIF
              ENDIF
          
            'Move left
            CASE ENCMOVL
              entref(i).x = entref(i).x + ENTSTEP
              IF entref(i).state <> ENTWALK THEN
                entref(i).state = ENTWALK
                entref(i).stnum = 1
              ELSE
                entref(i).stnum = entref(i).stnum + 1
                IF entref(i).stnum > entref(i).etype.walksnr THEN
                  entref(i).stnum = 1
                ENDIF
              ENDIF
              IF entref(i).x >= entref(i).x0 + ENTSTEP * entref(i).arg(0) THEN delet = 1
          
            'Move right
            CASE ENCMOVR
              entref(i).x = entref(i).x - ENTSTEP
              IF entref(i).state <> ENTWALK THEN
                entref(i).state = ENTWALK
                entref(i).stnum = 1
              ELSE
                entref(i).stnum = entref(i).stnum + 1
                IF entref(i).stnum > entref(i).etype.walksnr THEN
                  entref(i).stnum = 1
                ENDIF
              ENDIF
              IF entref(i).x <= entref(i).x0 - ENTSTEP * entref(i).arg(0) THEN delet = 1
          
            'Move up
            CASE ENCMOVU
              entref(i).y = entref(i).y - ENTSTEP
              IF entref(i).state <> ENTWALK THEN
                entref(i).state = ENTWALK
                entref(i).stnum = 1
              ELSE
                entref(i).stnum = entref(i).stnum + 1
                IF entref(i).stnum > entref(i).etype.walksnr THEN
                  entref(i).stnum = 1
                ENDIF
              ENDIF
              IF entref(i).y <= entref(i).y0 - ENTSTEP * entref(i).arg(0) THEN delet = 1
        
            'Move down
            CASE ENCMOVD
              entref(i).y = entref(i).y + ENTSTEP
              IF entref(i).state <> ENTWALK THEN
                entref(i).state = ENTWALK
                entref(i).stnum = 1
              ELSE
                entref(i).stnum = entref(i).stnum + 1
                IF entref(i).stnum > entref(i).etype.walksnr THEN
                  entref(i).stnum = 1
                ENDIF
              ENDIF
              IF entref(i).y >= entref(i).y0 + ENTSTEP * entref(i).arg(0) THEN delet = 1
        
            'Set height
            CASE ENCHEIG
              entref(i).z = entref(i).arg(0)
          
            'Atack
            CASE ENCATAK
        
            'Dead
            CASE ENCDEAD
      
          END SELECT
        
        'Command is not valid: delete
        ELSE
          delet = 1
        ENDIF
      
        'Set sprite height according to corresponding cell floor
        cx = entref(i).x / rattrptr->cellsize
        cy = entref(i).y / rattrptr->cellsize
        entref(i).z = cmapptr[cx*(MAXCELLY+1)+cy].floor * (*rattrptr).cellsize 
        
        'Save last processed command
        entref(i).acomm = entref(i).commd(0)
        IF delet = 1 THEN entref(i).acomm = ""

        'Delete processed command from queue
        IF delet = 1 THEN
          FOR j=0 TO entref(i).cindex-1
            entref(i).commd(j) = entref(i).commd(j+1)
            entref(i).arg(j) = entref(i).arg(j+1)
          NEXT j
          j=entref(i).cindex
          entref(i).commd(j) = ""
          entref(i).arg(j) = 0
          entref(i).cindex = entref(i).cindex - 1
        ENDIF
      
      ENDIF
      
      'Get entity type state definition
      j=0
      found=0
      DO
        IF  etypestate(j).etypename = entref(i).etype.etypename _
        AND etypestate(j).state     = entref(i).state _
        AND etypestate(j).stnum     = entref(i).stnum THEN
          found = 1
          EXIT DO
        ENDIF
        j = j + 1
      LOOP
      
      'Print sprite
      ReadTexture 4, etypestate(j).frame(entref(i).direc-1), ax, ay, imgen
      IF ax > 0 AND ay > 0 THEN
        ISOMap entref(i).x + rattrptr->cellsize/2 + ax/4, _
               entref(i).y + rattrptr->cellsize/2 - ax/4, _
               entref(i).z, ax, ay, ax, ay, 0, 0, imgen, _
               ISOFPLANE, etypestate(j).mirror(entref(i).direc-1)
      ENDIF
      
      'Entity position and frame
      cx = entref(i).x / rattrptr->cellsize
      cy = entref(i).y / rattrptr->cellsize
      'PrintStrBkg FCol(98),FRow(50), FORMAT(cx,"00") + "," + FORMAT(cy,"00"), RGB(255,255,255),1,1
      'ISOSquare cx*rattrptr->cellsize,cy*rattrptr->cellsize,_
      '          cmapptr[cx*(MAXCELLY+1)+cy].floor*rattrptr->cellsize,_
      '          rattrptr->cellsize,rattrptr->cellsize,ISOZPLANE,RGB(0,255,0)

    ENDIF

  NEXT i
  
END SUB

'Check entity valid movement
FUNCTION EntityValidMovement(BYVAL entref AS entrefresh) AS INTEGER

  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER cx,cy
  DIM AS INTEGER ax,ay
  DIM AS INTEGER found
  DIM AS INTEGER delet
  DIM AS INTEGER floor
  DIM AS mapcell cmap
  DIM AS UINTEGER PTR ipt

  'Get floor level before movement
  cx = entref.x / rattrptr->cellsize
  cy = entref.y / rattrptr->cellsize
  floor = cmapptr[cx*(MAXCELLY+1)+cy].floor
  
  'Move
  SELECT CASE entref.commd(0)
    CASE ENCMOVL: entref.x = entref.x + ENTSTEP
    CASE ENCMOVR: entref.x = entref.x - ENTSTEP
    CASE ENCMOVU: entref.y = entref.y - ENTSTEP
    CASE ENCMOVD: entref.y = entref.y + ENTSTEP
  END SELECT
  
  'Get destination cell
  cx = entref.x / rattrptr->cellsize
  cy = entref.y / rattrptr->cellsize
  cmap = cmapptr[cx*(MAXCELLY+1)+cy]
  
  'Check floor level
  IF cmap.floor - floor > 1 THEN
    EntityValidMovement = 0
    EXIT FUNCTION
  ENDIF
  
  'Check height
  IF cmap.height > 0 AND cmap.height - cmap.floor < 4 THEN
    EntityValidMovement = 0
    EXIT FUNCTION
  ENDIF
  
  'Check that cell is not occupied by an object
  IF cmap.object <> "" AND cmap.objheight < 2 THEN
    EntityValidMovement = 0
    EXIT FUNCTION
  ENDIF
  
  'Check that entity does not go outside map
  IF cx < 0 OR cy < 0 OR cx > rattrptr->ax-1 OR cy > rattrptr->ay-1 THEN
    EntityValidMovement = 0
    EXIT FUNCTION
  ENDIF 

  'Valid movement
  EntityValidMovement = 1

END FUNCTION

'Save entity types
SUB SaveEntityTypes
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER file
  DIM AS STRING fline
  
  'Open object list file
  file = FREEFILE
  OPEN "etypes.dat" FOR OUTPUT AS #file
  
  'Save number of entity types
  WRITE# #file, numetype
  
  'Save entity types
  FOR i=0 TO numetype - 1
    PRINT# #file, etype(i).etypename,",",etype(i).etypeimg,_
    ",",STR$(etype(i).walksnr),",",STR$(etype(i).ataksnr),",",STR$(etype(i).deadsnr)
  NEXT i

  'Save entity type states
  FOR i=0 TO MAXETYST - 1
    IF etypestate(i).etypename <> "" THEN
      fline = etypestate(i).etypename + "," + etypestate(i).state + "," + _
      STR$(etypestate(i).stnum) + "," + STR$(etypestate(i).multidir)
      FOR j=0 TO 7: fline = fline + "," + etypestate(i).frame(j): NEXT j
      FOR j=0 TO 7: fline = fline + "," + STR$(etypestate(i).mirror(j)): NEXT j
      PRINT# #file, fline
    ENDIF
  NEXT i  
  
  'Close file
  CLOSE #file

END SUB

'Load entity types
SUB LoadEntityTypes
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER file
  DIM AS STRING fline
  
  'Open object list file
  file = FREEFILE
  OPEN "etypes.dat" FOR INPUT AS #file
  
  'Get number of objects
  INPUT# #file, numetype
  
  'Save entity types
  FOR i=0 TO numetype - 1
    INPUT# #file, etype(i).etypename, etype(i).etypeimg, _
    etype(i).walksnr, etype(i).ataksnr, etype(i).deadsnr
  NEXT i

  'Save entity type states
  FOR i=0 TO MAXETYST - 1
    INPUT# #file, etypestate(i).etypename, etypestate(i).state, _
    etypestate(i).stnum, etypestate(i).multidir
    FOR j=0 TO 7: INPUT# #file, etypestate(i).frame(j): NEXT j
    FOR j=0 TO 7: INPUT# #file, etypestate(i).mirror(j): NEXT j
    IF EOF(file) = -1 THEN EXIT FOR
  NEXT i  
  
  'Close file
  CLOSE #file

END SUB

'Link point management
FUNCTION LinkPnt(commd AS STRING, BYREF map1 AS STRING, BYREF x1 AS INTEGER, BYREF y1 AS INTEGER, _
                 BYREF map2 AS STRING, BYREF x2 AS INTEGER, BYREF y2 AS INTEGER, _
                 BYREF way AS INTEGER) AS INTEGER
  
  'Variables
  STATIC AS STRING map
  STATIC AS INTEGER index
  DIM AS INTEGER k,file
  DIM AS INTEGER duplicated,found
  
  'Switch on command
  SELECT CASE commd
  
    'Read link point definition file
    CASE "LINK_LOAD_FILE"
      file = FREEFILE
      OPEN "linkpoints.dat" FOR RANDOM AS #file Len = SIZEOF(lpfile)
      k=0
      WHILE NOT EOF(file) AND k < MAXLINKP
        GET #file,,linkpoint(k)
        k = k + 1
      WEND
      CLOSE #file
      numlink = k

    'Save texture clasification file
    CASE "LINK_SYNC_FILE"
      file = FREEFILE
      OPEN "linkpoints.dat" FOR RANDOM AS #file Len = SIZEOF(lpfile)
      k=0
      WHILE k < numlink
        IF linkpoint(k).delet = 0 THEN 
          PUT #file,,linkpoint(k)
        ENDIF
        k = k + 1
      WEND
      CLOSE #file
      LinkPnt("LINK_LOAD_FILE","",0,0,"",0,0,0)

    'Set source point
    CASE "LINK_STORE_POINT"
      duplicated = 0
      FOR k=0 TO numlink-1
        IF linkpoint(k).delet = 0 THEN
          IF     ( linkpoint(k).map1 = map1 AND linkpoint(k).x1 = x1 AND linkpoint(k).y1 = y1 _
          AND      linkpoint(k).map2 = map2 AND linkpoint(k).x2 = x2 AND linkpoint(k).y2 = y2 ) THEN
            IF ( way = +1 AND linkpoint(k).way = +1 ) _
            OR ( way = -1 AND linkpoint(k).way = -1 ) _
            OR ( way = +0 AND linkpoint(k).way = +0 ) THEN
              duplicated = 1
              EXIT FOR
            ENDIF
          ELSEIF ( linkpoint(k).map1 = map2 AND linkpoint(k).x1 = x2 AND linkpoint(k).y1 = y2 _
          AND      linkpoint(k).map2 = map1 AND linkpoint(k).x2 = x1 AND linkpoint(k).y2 = y1 ) THEN
            IF ( way = +1 AND linkpoint(k).way = -1 ) _
            OR ( way = -1 AND linkpoint(k).way = +1 ) _
            OR ( way = +0 AND linkpoint(k).way = +0 ) THEN
              duplicated = 1
              EXIT FOR
            ENDIF
          ENDIF
        ENDIF
      NEXT k
      IF duplicated = 0 THEN
        linkpoint(numlink).map1 = map1
        linkpoint(numlink).x1 = x1
        linkpoint(numlink).y1 = y1
        linkpoint(numlink).map2 = map2
        linkpoint(numlink).x2 = x2
        linkpoint(numlink).y2 = y2
        linkpoint(numlink).way = way
        linkpoint(numlink).delet = 0
        numlink = numlink + 1
      ENDIF
    
    'Delete link point from source
    CASE "LINK_DELETE_POINT"
      FOR k=0 TO numlink-1
        IF linkpoint(k).delet = 0 THEN
          IF  ( linkpoint(k).map1 = map1 _
          AND   linkpoint(k).x1   = x1 _
          AND   linkpoint(k).y1   = y1 ) _
          OR  ( linkpoint(k).map2 = map1 _
          AND   linkpoint(k).x2   = x1 _
          AND   linkpoint(k).y2   = y1 ) THEN
            linkpoint(k).delet = 1
          ENDIF
        ENDIF
      NEXT k

    'Search link point
    CASE "LINK_SEARCH_POINT"
      found = 0
      FOR k=0 TO numlink-1
        IF linkpoint(k).delet = 0 THEN
          IF  ( linkpoint(k).map1 = map1 _
          AND   linkpoint(k).x1   = x1 _
          AND   linkpoint(k).y1   = y1 ) THEN
            map2 = linkpoint(k).map2      
            x2 = linkpoint(k).x2      
            y2 = linkpoint(k).y2
            way = linkpoint(k).way
            found = 1
            EXIT FOR
          ELSEIF ( linkpoint(k).map2 = map1 _
          AND      linkpoint(k).x2   = x1 _
          AND      linkpoint(k).y2   = y1 ) THEN
            map2 = linkpoint(k).map1      
            x2 = linkpoint(k).x1      
            y2 = linkpoint(k).y1      
            way = -linkpoint(k).way
            found = 1
            EXIT FOR
          ENDIF
        ENDIF
      NEXT k
      LinkPnt = found

    'Get number of links
    CASE "LINK_GET_NUMBER"
      LinkPnt = numlink
      
    'Get link points for map
    CASE "LINK_LIST_START"
      map = map1
      index = 0
      
    'Get link points for map
    CASE "LINK_LIST_NEXT"
      found = 0
      FOR k=index TO numlink-1
        IF linkpoint(k).delet = 0 THEN
          IF linkpoint(k).map1 = map THEN
            map1 = linkpoint(k).map1
            x1   = linkpoint(k).x1
            y1   = linkpoint(k).y1
            map2 = linkpoint(k).map2
            x2   = linkpoint(k).x2
            y2   = linkpoint(k).y2
            way  = linkpoint(k).way
            found = 1
            EXIT FOR
          ELSEIF linkpoint(k).map2 = map THEN
            map1 = linkpoint(k).map2
            x1   = linkpoint(k).x2
            y1   = linkpoint(k).y2
            map2 = linkpoint(k).map1
            x2   = linkpoint(k).x1
            y2   = linkpoint(k).y1
            way  = -linkpoint(k).way
            found = 1
            EXIT FOR
          ENDIF
        ENDIF
      NEXT k
      IF found = 1 THEN
        index = k + 1
        LinkPnt = 0 
      ELSE
        LinkPnt = 1
      ENDIF
  
  END SELECT

END FUNCTION

'Map images cache memory management
FUNCTION MapImage(commd AS STRING, file AS STRING, scale AS INTEGER, _
                  px AS INTEGER, py AS INTEGER, ax AS INTEGER, ay AS INTEGER ) AS INTEGER
  
  'Variables
  DIM AS INTEGER i,j,k,l,found
  
  'Init return value
  MapImage = 0

  'Switch on function 
   SELECT CASE commd
   
     'Init memory
     CASE "INIT_MEMORY"
       CLEAR mapimr(0),0,LEN(mapimr(0))*MAPIMGRE
       numimr = 0
       mapptr = 0
     
     'Store map image
     CASE "STORE_IMAGE"
       IF mapptr + ax*ay < MAPIMGSZ _
       AND numimr < MAPIMGRE THEN
         mapimr(numimr).file = file             
         mapimr(numimr).scale = scale
         mapimr(numimr).ax = ax
         mapimr(numimr).ay = ay
         mapimr(numimr).imgptr = mapptr
         numimr = numimr + 1
         k = mapptr
         FOR i = 0 TO ax - 1
         FOR j = 0 TO ay - 1
           mapimg(k) = POINT(px+i,py+j)
           k = k + 1
         NEXT j
         NEXT i
         mapptr = mapptr + ax*ay
       ENDIF
       
     'Restore map image
     CASE "RESTORE_IMAGE"
       found = 0
       FOR l = 0 TO numimr - 1
         IF mapimr(l).file = file AND mapimr(l).scale = scale THEN
           found = 1
           EXIT FOR
         ENDIF
       NEXT l
       IF found = 1 THEN
         k = mapimr(l).imgptr
         FOR i = 0 TO mapimr(l).ax - 1
         FOR j = 0 TO mapimr(l).ay - 1
           PSET (px+i,py+j),mapimg(k)
           k = k + 1
         NEXT j
         NEXT i
         MapImage = 1
       ENDIF
   
     'Delete map image
     CASE "DELETE_IMAGE"
       FOR l = 0 TO numimr - 1
         IF mapimr(l).file = file THEN
           mapimr(l).file = ""
           mapimr(l).scale = 0
         ENDIF
       NEXT l
   
   END SELECT
   
END FUNCTION

'Print events
SUB EventPrint(event AS mapevent, evtstr AS STRING)

  'Variables
  DIM AS STRING evname, evtype

  'Exit of event not set
  IF event.id = 0 THEN: evtstr = "": EXIT SUB: ENDIF
  
  'Print event
  evname = event.evname
  evtype = event.evtype
  evtstr = FORMAT(event.id,"00") + " " + evname + SPACE(LEN(event.evname)-LEN(evname)) + _
  " " + evtype + SPACE(LEN(event.evtype)-LEN(evtype)) + " " + FORMAT(event.value,"00")

END SUB  

'Print parse actions
SUB ActionPrint(actio AS mapactio, actstr AS STRING)

  'Print action
  DIM AS INTEGER i,k,found
  
  'Find action in action table
  found = 0
  FOR k=0 TO DEFACTIO-1
    IF actio.oper = actdef(k).oper THEN
      found = 1
      EXIT FOR
    ENDIF
  NEXT k
  IF found = 0 THEN
    actstr = actio.oper + "?"
  ELSE
    actstr = actio.oper
  ENDIF
  
  'Print special commands blankline and comment
  IF actio.oper = ACT_BLANKLINE THEN
    actstr = ""
    EXIT SUB
  ELSEIF actio.oper = ACT_COMMENT THEN
    actstr = "'" + actio.parm(0)
    EXIT SUB
  ENDIF
  
  'Print arguments
  IF actdef(k).argnum > 0 THEN
    FOR i=0 TO actdef(k).argnum - 1
      IF actdef(k).argnum = 1 THEN
        actstr = actstr + "(" + actio.parm(i) + ")"
      ELSEIF i = 0 THEN
        actstr = actstr + "(" + actio.parm(i) + ","
      ELSEIF i = actdef(k).argnum - 1 THEN
        actstr = actstr + actio.parm(i) + ")"
      ELSE
        actstr = actstr + actio.parm(i) + ","
      ENDIF
    NEXT i
  ELSE
    actstr = actstr + "()"
  ENDIF

END SUB  

'Parse actions
FUNCTION ActionParse(actio AS mapactio, actstr AS STRING, perror AS STRING) AS INTEGER

  'Variables
  DIM AS INTEGER i,j,k,position,argerror
  DIM AS mapactio actio0
  DIM AS STRING argstr, argum
  
  'Parse blank lines
  IF TRIM(actstr) = "" THEN
    actio.oper = ACT_BLANKLINE
    ActionParse = 1
    EXIT FUNCTION
  ENDIF
  
  'Parse comnents
  IF LEFT(actstr,1) = "'" THEN
    actio.oper = ACT_COMMENT
    actio.parm(0) = MID(actstr,2,80)
    ActionParse = 1
    EXIT FUNCTION
  ENDIF
  
  'Get operation
  position = 0
  actio0.oper = ""
  FOR k=0 TO DEFACTIO-1
    position = INSTR(actstr,TRIM(actdef(k).oper)) 
    IF position <> 0 THEN 
      actio0.oper = actdef(k).oper
      EXIT FOR
    ENDIF
  NEXT k
  IF actio0.oper = "" THEN
    perror = "Operation in '" + actstr + "' is not valid!"
    ActionParse = 0
    EXIT FUNCTION
  ENDIF
  
  'Argument string
  argstr = MID(actstr,position+LEN(TRIM(actio0.oper)),255)

  'Check openning parenthesis
  IF LEFT(argstr,1) <> "(" THEN
    perror = "Opening parenthesis not found"
    ActionParse = 0
    EXIT FUNCTION
  ENDIF
  
  'Check closing parenthesis
  IF RIGHT(argstr,1) <> ")" THEN
    perror = "Closing parenthesis not found"
    ActionParse = 0
    EXIT FUNCTION
  ENDIF
  
  'Get argument loop
  i = 1
  j = 0
  argum = ""
  DO
    IF argstr[i] = ASC(",") THEN
      actio0.parm(j) = argum  
      argum = ""
      j = j + 1
    ELSEIF argstr[i] = ASC(")") THEN
      actio0.parm(j) = argum  
      EXIT DO
    ELSE
      argum = argum + CHR$(argstr[i])
    ENDIF
    i = i + 1
  LOOP
  
  'Check number of arguments
  IF actdef(k).argnum-1 <> j THEN
    perror = "Found arguments (" + STR$(j+1) + _
    ") does not match expected arguments (" + STR$(actdef(k).argnum) + ")"
    ActionParse = 0
    EXIT FUNCTION
  ENDIF
  
  'Check argument types
  argerror = 0
  FOR i=0 TO actdef(k).argnum-1
    IF actdef(k).ptype(i) = ARG_INTEGER THEN
      argum = TRIM(actio0.parm(i))
      FOR j=0 TO LEN(argum)-1
        IF VALINT(CHR$(actio0.parm(i)[j])) = 0 _ 
        AND actio0.parm(i)[j] <> ASC("0") _
        AND actio0.parm(i)[j] <> ASC(" ") THEN
          argerror = 1
          EXIT FOR
        ENDIF
      NEXT j
      IF argerror = 1 THEN
        perror = "Argument " + STR$(i+1) + " must be a number! (" + argum + ")"
        EXIT FOR
      ENDIF
    ENDIF
  NEXT i
  IF argerror = 1 THEN
    ActionParse = 0
    EXIT FUNCTION
  ENDIF

  'Return success
  actio = actio0
  ActionParse = 1
  

END FUNCTION


'*** End of maplib.bas ***


'*** Begin of isolib.bi ***

' Graphic constants
CONST AX0 = 640
CONST AY0 = 480

' Isometric planes
CONST ISOXPLANE = 1
CONST ISOYPLANE = 2
CONST ISOZPLANE = 3
CONST ISOFPLANE = 4

'Texture mapping modes
CONST TEX_NORMAL = 0
CONST TEX_FRAME  = 1
CONST TEX_SOLID  = 2
CONST TEX_BOX    = 3

'Constants
CONST MAXLIGHT = 9
CONST MAXPLANE = 10000
CONST MAXLMBUF = 3

'Math consts
CONST SQR02 = SQR(2)
CONST SQR12 = 1 / SQR(2)
CONST SQR13 = 1 / SQR(3)

'Substitution color
CONST COL_SUBST = RGB(100,100,100)

'RGB light color definition (floating point)
TYPE RGBf
  r AS SINGLE
  g AS SINGLE
  b AS SINGLE
END TYPE

'RGB light color definition (integer)
TYPE RGBi
  r AS USHORT
  g AS USHORT
  b AS USHORT
END TYPE

' Light source
TYPE LSource
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  lum AS SINGLE
  r AS INTEGER
  g AS INTEGER
  b AS INTEGER
  calc AS INTEGER
END TYPE

TYPE shadowplane
  a0 AS INTEGER
  b0 AS INTEGER
  c0 AS INTEGER
  ax AS INTEGER
  ay AS INTEGER
  tax AS INTEGER
  tay AS INTEGER
  ofx AS INTEGER
  ofy AS INTEGER
  ipt AS UINTEGER PTR
  plane AS INTEGER
  inverted AS INTEGER
  transp AS INTEGER
END TYPE

'Pixel 3D coordinates (light map calculation)
TYPE pixelcoord
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
  col AS UINTEGER
  plane AS INTEGER
END TYPE

'Light map for map storage
'TYPE RGBi
'  r AS SHORT
'  g AS SHORT
'  b AS SHORT
'END TYPE

' Public functions
DECLARE SUB ISOCenter(px AS INTEGER, py AS INTEGER)
DECLARE SUB ISOScale(scale AS SINGLE)
DECLARE SUB ISOCellSize(size AS INTEGER)
DECLARE SUB ISOGrid (a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, csize AS INTEGER, plane AS INTEGER, col AS UINTEGER)
DECLARE SUB ISOLine (a1 AS INTEGER, b1 AS INTEGER, c1 AS INTEGER, a2 AS INTEGER, b2 AS INTEGER, c2 AS INTEGER, col AS UINTEGER)
DECLARE SUB ISOCircle (a AS INTEGER, b AS INTEGER, c AS INTEGER, r AS INTEGER, col AS UINTEGER, fill AS INTEGER)
DECLARE SUB ISOMap (a AS INTEGER, b AS INTEGER, c AS INTEGER, ax AS INTEGER, ay AS INTEGER, tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, ipt AS UINTEGER PTR, plane AS INTEGER, mirror AS INTEGER)
DECLARE SUB ISOSquare(a AS INTEGER, b AS INTEGER, c AS INTEGER, ax AS INTEGER, ay AS INTEGER, plane AS INTEGER, col AS UINTEGER)
DECLARE SUB ISOZBufferClear
DECLARE SUB ISOZBufferEnable
DECLARE SUB ISOZBufferDisable
DECLARE SUB ISOZBufferLayer(layer AS INTEGER)
DECLARE SUB ISOZBufferOverwrite(mode AS INTEGER)
DECLARE SUB ISOPixel(px AS INTEGER, py AS INTEGER, col AS UINTEGER, colm AS UINTEGER, dist AS SHORT)
DECLARE SUB ISOLightAmbient(l AS SINGLE, r AS INTEGER, g AS INTEGER, b AS INTEGER)
DECLARE SUB ISOLightAproxFactor(f AS INTEGER)
DECLARE SUB ISOLightClearAll
DECLARE SUB ISOLightSource( x AS INTEGER, y AS INTEGER, z AS INTEGER, lum AS SINGLE, r AS INTEGER, g AS INTEGER, b AS INTEGER )
DECLARE SUB ISOTexMappingMode(mode AS INTEGER)
DECLARE SUB ISOTexMappingColor(col AS UINTEGER)
DECLARE SUB ISOLMapInit
DECLARE SUB ISOLMapStorePlanes(store AS INTEGER)
DECLARE SUB ISOLMapDrawPlanes(unified AS INTEGER)
DECLARE SUB ISOLMapUnifyPlanes(plane AS INTEGER, plane1() AS shadowplane, plane2() AS shadowplane, numplane1 AS INTEGER, BYREF numplane2 AS INTEGER)
DECLARE SUB ISOLMapUnifyAllPlanes
DECLARE SUB ISOLMapCalculate
DECLARE SUB ISOLMapDifuse(difuse AS INTEGER)
DECLARE FUNCTION ISOLMapStatus AS INTEGER
DECLARE SUB ISOLMapUseFlag(use AS INTEGER)
DECLARE SUB ISOLMapExport(lmap() AS RGBi, BYREF lmaplen AS INTEGER)
DECLARE SUB ISOLMapImport(lmap() AS RGBi, lmaplen AS INTEGER)
DECLARE SUB ISOLMapOffset(x AS INTEGER,y AS INTEGER)
DECLARE SUB ISOLMapIncBuffer
DECLARE SUB ISOLMapDecBuffer

' Private functions
DECLARE SUB ISOIMap (a AS INTEGER, b AS INTEGER, c AS INTEGER, ax AS INTEGER, ay AS INTEGER, tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, ipt AS UINTEGER PTR, plane AS INTEGER,inverted AS INTEGER, mode AS INTEGER, col AS UINTEGER)
DECLARE SUB ISODMap (a AS INTEGER, b AS INTEGER, c AS INTEGER, ax AS INTEGER, ay AS INTEGER, tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, ipt AS UINTEGER PTR, plane AS INTEGER,inverted AS INTEGER, mode AS INTEGER, col AS UINTEGER)
DECLARE SUB ISOLMapSetPlane(a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, ipt AS UINTEGER PTR, plane AS INTEGER, inverted AS INTEGER, transp AS INTEGER)
DECLARE FUNCTION ISOLightVisible(index AS INTEGER, a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, plane AS INTEGER) AS INTEGER
DECLARE FUNCTION ISOLightCalc (a AS INTEGER, b AS INTEGER, c AS INTEGER, plane AS INTEGER) AS RGBf
DECLARE FUNCTION ISOLMapShadowedRay(pix3d AS pixelcoord, light AS INTEGER) AS INTEGER

'*** End of isolib.bi ***


'*** Begin of isolib.bas ***

#INCLUDE "isolib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "matlib.bi"
#INCLUDE "vbcompat.bi"

'Isometric screen center & cellsize
DIM SHARED isopx0 AS INTEGER
DIM SHARED isopy0 AS INTEGER

'Scale parameter
DIM SHARED isoscale0 AS SINGLE = 1

'Texture mapping mode
DIM SHARED isotexmode AS INTEGER = TEX_NORMAL
DIM SHARED isotexcol AS UINTEGER = RGB(255,255,255)

'Z-Buffer
DIM SHARED ZBuffEnable AS INTEGER = 0
DIM SHARED ZBuffLayer AS INTEGER = 1
DIM SHARED ZBuffOvWrite AS INTEGER = 0
REDIM SHARED zbuffer1(AX0,AY0) AS SHORT
REDIM SHARED zbuffer2(AX0,AY0) AS SHORT

'Light sources
DIM SHARED isolightambr AS SINGLE
DIM SHARED isolightambg AS SINGLE
DIM SHARED isolightambb AS SINGLE
DIM SHARED isolightaprox AS INTEGER = 4
DIM SHARED isolightnum AS INTEGER = 0
REDIM SHARED isolight(MAXLIGHT) AS LSource

'Light map calculation
DIM SHARED isolmapbuffer AS INTEGER = 0
DIM SHARED isolmapupdated AS INTEGER = 0
DIM SHARED isolmapuse AS INTEGER = 0
DIM SHARED numsplane AS INTEGER = 0
DIM SHARED numuplane AS INTEGER = 0
DIM SHARED shadowstore AS INTEGER = 0
DIM SHARED isolmapoffx AS INTEGER = 0
DIM SHARED isolmapoffy AS INTEGER = 0
DIM SHARED splane(MAXPLANE) AS shadowplane
DIM SHARED uplane(MAXPLANE) AS shadowplane
DIM SHARED pixel3d(AX0,AY0) AS pixelcoord
DIM SHARED lightmap(AX0,AY0,MAXLMBUF) AS RGBf

'Shared variables to speed up performance (used in macros)
DIM SHARED AS INTEGER _a, _b, _c, _i, _j
DIM SHARED AS INTEGER _aplane,_ax,_ay,_az
DIM SHARED AS RGBf _alight
DIM SHARED AS INTEGER _xd,_yd,_zd
DIM SHARED AS INTEGER _px0,_py0
DIM SHARED AS UINTEGER _col

' Isometric direct pixel transformation
#MACRO _ISODTrans(a,b,c,px,py)
  px = -a + b + isopx0
  py = c - ((a+b) SHR 1) + isopy0
  py = AY0 - py - 1
#ENDMACRO

' Isometric direct plane proyection
#MACRO _ISODProy(a0,b0,c0,x,y,plane,px,py)

  'Z axis
  IF plane = ISOZPLANE THEN
    _ISODTrans((a0+x),(b0+y),c0,px,py)
    
  'X axis
  ELSEIF plane = ISOXPLANE THEN
    _ISODTrans(a0,(b0+x),(c0+y),px,py)
     
  'Y axis
  ELSEIF plane = ISOYPLANE THEN
    _ISODTrans((a0+x),b0,(c0+y),px,py)
    px = px - 1

  'Front plane
  ELSEIF plane = ISOFPLANE THEN
    _ISODTrans(a0,b0,c0,px,py)
    px = px + x
    py = py - y
  ENDIF  

#ENDMACRO

' Isometric inverse plane proyection
#MACRO _ISOIProy(a0,b0,c0,px,py,plane,x,y)

  'Z axis
  IF plane = ISOZPLANE THEN
    x = ((isopx0-px) SHR 1) - (AY0 - py) + isopy0 + c0 + 1 - a0
    y = ((px-isopx0) SHR 1) - (AY0 - py) + isopy0 + c0 + 1 - b0
  
  'X axis
  ELSEIF plane = ISOXPLANE THEN
    x = px - isopx0 + a0 - b0
    y = ((px-isopx0) SHR 1) + (AY0 - py) - isopy0 + a0 - 1 - c0

  'Y axis
  ELSEIF plane = ISOYPLANE THEN
    x = -px + isopx0 + b0 - a0
    y = ((isopx0-px) SHR 1) + (AY0 - py) - isopy0 + b0 - 1 - c0
  ENDIF

#ENDMACRO

'Get normal vector
#MACRO _ISONormalVector(plane,p1,p2,p3)
  IF plane = ISOZPLANE THEN
    p1 = 0: p2 = 0: p3 = 1    
  ELSEIF plane = ISOXPLANE THEN
    p1 = 1: p2 = 0: p3 = 0    
  ELSEIF plane = ISOYPLANE THEN
    p1 = 0: p2 = 1: p3 = 0    
  ELSEIF plane = ISOFPLANE THEN
    p1 = SQR12: p2 = SQR12: p3 = 0    
  ENDIF
#ENDMACRO

' Solve isometric coordinates
#MACRO _ISOSolve3D(a0,b0,c0,x,y,plane,a,b,c)
  IF plane = ISOZPLANE THEN
    a = a0 + x: b = b0 + y: c = c0
  ELSEIF plane = ISOXPLANE THEN
    a = a0: b = b0 + x: c = c0 + y
  ELSEIF plane = ISOYPLANE THEN
    a = a0 + x: b = b0: c = c0 + y
  ELSEIF plane = ISOFPLANE THEN
    a = a0 - (x SHR 1): b = b0 + (x SHR 1): c = c0 + y
  ENDIF
#ENDMACRO

'Z-Buffer distance calculation functions
#MACRO _ISOZBufferDist(a0,b0,c0,x,y,plane,dist)

  'Get ground coordinates
  _ISOSolve3D(a0,b0,c0,x,y,plane,_a,_b,_c)
  
  'Distance
  IF _a < _b THEN
    dist = _a * SQR02
  ELSE
    dist = _b * SQR02
  ENDIF

#ENDMACRO

'Z-Buffer distance calculation (with FPLANE case)
#MACRO _ISOZBufferDistF(a0,b0,c0,x,y,plane,dist,distf)
  IF plane = ISOFPLANE THEN
    dist = distf
  ELSE
    _ISOZBufferDist(a0,b0,c0,x,y,plane,dist)
  ENDIF
#ENDMACRO

' Light calculation macro with aproximation algorithm
#MACRO _ISOLightCalc (a,b,c,plane,light)
  IF isolightaprox > 0 THEN
    _xd = a-_ax: _yd = b-_ay: _zd = c-_az
    IF plane = _aplane AND (_xd*_xd+_yd*_yd+_zd*_zd) <= isolightaprox THEN
      light = _alight
    ELSE
      light = ISOLightCalc(a,b,c,plane)
      _alight = light
      _aplane = plane
      _ax = a
      _ay = b
      _az = c
    ENDIF
  ELSE
    light = ISOLightCalc(a,b,c,plane)
  ENDIF
#ENDMACRO

'Modify color according to light factor
#MACRO _ISOModulateColor(col0,light,col1)
  light.r = light.r * RGB_R(col0)
  light.g = light.g * RGB_G(col0)
  light.b = light.b * RGB_B(col0)
  IF light.r > 255 THEN light.r = 255
  IF light.g > 255 THEN light.g = 255
  IF light.b > 255 THEN light.b = 255
  col1 = RGB(CAST(INTEGER,light.r),CAST(INTEGER,light.g),CAST(INTEGER,light.b))
#ENDMACRO

#MACRO _ISOScalePixel(px,py,px0,py0)
  IF isoscale0 <> 1 THEN
    px0 = isopx0 + CAST(SINGLE,isoscale0 *(px - isopx0))
    py0 = (AY0 - isopy0) + CAST(SINGLE,isoscale0 * (py - (AY0 - isopy0)))
  ELSE
    px0 = px
    py0 = py
  ENDIF
#ENDMACRO

' Pixel set with Z-Buffer
#MACRO _ISOPixel(px,py,col,colm,dist,pixelset)

  'Modify coordinates according to scale
  _ISOScalePixel(px,py,_px0,_py0)

  'Init pixel set flag
  pixelset = 0
  
  'Check screen boundaries
  IF _px0 >= 0 AND _px0 < AX0 AND _py0 >= 0 AND _py0 < AY0 AND col <> TRANCOLHI THEN

    'Pixel set wth z-buffer
    IF ZBuffEnable = 1 THEN

      'Check z-buffer (layer 1)
      IF ZBuffLayer = 1 THEN
        IF ZBuffOvWrite = 0 THEN
          IF dist > zbuffer1(_px0,_py0) THEN
            PSET (_px0,_py0), colm
            zbuffer1(_px0,_py0) = dist
            pixelset = 1
          ENDIF
        ELSE
          IF dist >= zbuffer1(_px0,_py0) THEN
            PSET (_px0,_py0), colm
            zbuffer1(_px0,_py0) = dist
            pixelset = 1
          ENDIF
        ENDIF

      'Check z-buffer (layer 2)
      ELSEIF ZBuffLayer = 2 THEN
        IF dist >= zbuffer1(_px0,_py0) THEN
          IF dist >= zbuffer2(_px0,_py0) THEN
            PSET (_px0,_py0), colm
            zbuffer2(_px0,_py0) = dist
          ENDIF
        ENDIF
      ENDIF
  
    'Always set pixel if Z-Buffer is not enabled
    ELSE
      PSET (_px0,_py0), colm
    ENDIF

  ENDIF

#ENDMACRO

'Get textel for direct mapping routines
#MACRO _ISODTextelGet(x,y,tax,tay,ofx,ofy,ipt,inverted,pxcol)
  _i = x + ofx: IF _i > tax - 1 THEN _i = _i MOD tax
  _j = y + ofy: IF _j > tay - 1 THEN _j = _j MOD tay
  IF inverted = 0 THEN 
    pxcol = ipt[tax * (tay - _j - 1) + (tax - _i - 1)]
  ELSEIF inverted = 1 THEN 
    pxcol = ipt[tax * (tay - _j - 1) + _i]
  ENDIF
#ENDMACRO

'Get textel for inverse mapping routines
#MACRO _ISOITextelGet(x,y,tax,tay,ofx,ofy,ipt,inverted,pxcol)
  _i = x + ofx: IF _i > tax - 1 THEN _i = _i MOD tax
  _j = y + ofy: IF _j > tay - 1 THEN _j = _j MOD tay
  IF inverted = 0 THEN 
    pxcol = ipt[tax * (tay - _j - 1) + (tax - _i - 1)]
  ELSEIf inverted = 1 THEN 
    pxcol = ipt[tax * _j + (tax - _i - 1)]
  END IF
#ENDMACRO

' Set isometric screen center
SUB ISOCenter (px AS INTEGER, py AS INTEGER)
  isopx0 = px
  isopy0 = py
END SUB

' Set scale
SUB ISOScale (scale AS SINGLE)
  isoscale0 = scale
END SUB

'Set texture mapping mode
SUB ISOTexMappingMode(mode AS INTEGER)
  isotexmode = mode
END SUB

'Set texture mapping color
SUB ISOTexMappingColor(col AS UINTEGER)
  isotexcol = col
END SUB

' Isometric pixel map proyection (direct way)
' Input: a, b, c, ax, ay, img, plane
SUB ISODMap (a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
             tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, _
             ipt AS UINTEGER PTR, plane AS INTEGER, inverted AS INTEGER, _
             mode AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER x, y
  DIM AS INTEGER px, py
  DIM AS INTEGER i, j
  DIM AS UINTEGER pxcol, colm
  DIM AS SHORT dist, distf
  DIM AS RGBf light
  DIM AS INTEGER transp
  DIM AS INTEGER drawpixel
  DIM AS INTEGER pixelset
  DIM AS INTEGER a,b,c
  
  'Init transparency flag
  transp = 0
  
  'Calculate distance in the midle for 45 º plane
  IF plane = ISOFPLANE THEN
    distf = 0
    FOR x = 0 TO ax - 1
      _ISOZBufferDist(a0,b0,c0,x,y,plane,dist)
      IF dist > distf THEN distf = dist
    NEXT x
  ENDIF
  
  'Mapping loop
  FOR y = 0 TO ay - 1
  FOR x = 0 TO ax - 1
    
    'Draw pixel flag
    drawpixel = 0
    IF mode = TEX_NORMAL OR mode = TEX_SOLID THEN
      drawpixel = 1
    ELSEIF ( mode = TEX_FRAME OR mode = TEX_BOX ) _
    AND ( x = 0 OR y = 0 OR x = ax-1 OR y = ay-1 ) THEN
      drawpixel = 1
    ENDIF
    
    'Draw pixels
    IF drawpixel = 1 THEN
      _ISODProy(a0,b0,c0,x,y,plane,px,py)
      _ISOZBufferDistF(a0,b0,c0,x,y,plane,dist,distf)
      _ISOSolve3D(a0,b0,c0,x,y,plane,a,b,c)
      IF isolmapuse = 0 THEN
        _ISOLightCalc(a,b,c,plane,light)
      ELSE
        IF  px+isolmapoffx >= 0 AND px+isolmapoffx < AX0 _
        AND py+isolmapoffy >= 0 AND py+isolmapoffy < AY0 THEN
          light = lightmap(px+isolmapoffx,py+isolmapoffy,isolmapbuffer)
        ELSE
          light.r = 0
          light.g = 0
          light.b = 0
        ENDIF
      ENDIF
      _ISODTextelGet(x,y,tax,tay,ofx,ofy,ipt,inverted,pxcol)
      IF pxcol = TRANCOLHI THEN transp = 1
      IF pxcol <> TRANCOLHI OR mode = TEX_BOX OR mode = TEX_FRAME THEN
        IF mode = TEX_SOLID OR mode = TEX_BOX THEN
          pxcol = col
        ENDIF
        IF pxcol = TRANCOLHI THEN pxcol = COL_SUBST
        _ISOModulateColor(pxcol,light,colm)
        _ISOPixel(px,py,pxcol,colm,dist,pixelset)
        IF pixelset = 1 AND isoscale0 = 1 THEN
          _ISOSolve3D(a0,b0,c0,x,y,plane,a,b,c)
          pixel3d(px,py).x = a
          pixel3d(px,py).y = b
          pixel3d(px,py).z = c
          pixel3d(px,py).col = pxcol
          pixel3d(px,py).plane = plane
        ENDIF
      ENDIF
    ENDIF
  NEXT x
  NEXT y

  'Store shadow plane
  ISOLMapSetPlane a0,b0,c0,ax,ay,tax,tay,ofx,ofy,ipt,plane,inverted,transp

END SUB

' Isometric pixel map proyection (inverse way)
' Input: a, b, c, ax, ay, img, plane
SUB ISOIMap (a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
             tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, _
             ipt AS UINTEGER PTR, plane AS INTEGER, inverted AS INTEGER, _
             mode AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER i,j          'Counters
  DIM AS INTEGER x, y         'Map coordinates
  DIM AS INTEGER px, py       'Screen coordinates
  DIM AS INTEGER spx1, spy1   'Minimum coordinates in pixel space
  DIM AS INTEGER spx2, spy2   'Maximum coordinates in pixel space
  DIM AS INTEGER sx(4)        'Auxiliary variables
  DIM AS INTEGER sy(4)        'Auxiliary variables
  DIM AS UINTEGER pxcol, colm 'Pixel color
  DIM AS SHORT dist           'Pixel distance
  DIM AS RGBf light           'Pixel light
  DIM AS INTEGER transp       'Transparent flag
  DIM AS INTEGER drawpixel    'Draw pixel flag
  DIM AS INTEGER pixelset
  DIM AS INTEGER a,b,c
  
  'Init transparency flag
  transp = 0
  
  'Calculate map size and location in pixel space
  _ISODProy(a0,b0,c0,00,00,plane,sx(0),sy(0))
  _ISODProy(a0,b0,c0,ax,00,plane,sx(1),sy(1))
  _ISODProy(a0,b0,c0,ax,ay-1,plane,sx(2),sy(2))
  _ISODProy(a0,b0,c0,00,ay-1,plane,sx(3),sy(3))
  spx1 = sx(0): spx2 = sx(0): spy1 = sy(0): spy2 = sy(0)
  FOR i = 0 TO 3
    IF sx(i) < spx1 THEN spx1 = sx(i)
    IF sy(i) < spy1 THEN spy1 = sy(i)
    IF sx(i) > spx2 THEN spx2 = sx(i)
    IF sy(i) > spy2 THEN spy2 = sy(i)
  NEXT i
  
  'Proyection loop
  For py = spy1 TO spy2
  For px = spx1 TO spx2

    'Inverse proyection
    _ISOIProy(a0,b0,c0,px,py,plane,x,y)
    
    'Check texture boundaries
    IF x >= 0 AND y >= 0 AND x <= ax AND y < ay THEN
    
      'Draw pixel flag
      drawpixel = 0
      IF mode = TEX_NORMAL OR mode = TEX_SOLID THEN
        drawpixel = 1
      ELSEIF ( mode = TEX_FRAME OR mode = TEX_BOX ) _
      AND ( x = 0 OR x = ax OR y = 0 OR y = ay-1 ) THEN
        drawpixel = 1
      ENDIF
      
      'Draw pixels
      IF drawpixel = 1 THEN
        _ISOZBufferDist(a0,b0,c0,x,y,plane,dist)
        _ISOSolve3D(a0,b0,c0,x,y,plane,a,b,c)
        IF isolmapuse = 0 THEN
          _ISOLightCalc(a,b,c,plane,light)
        ELSE
          IF  px+isolmapoffx >= 0 AND px+isolmapoffx < AX0 _
          AND py+isolmapoffy >= 0 AND py+isolmapoffy < AY0 THEN
            light = lightmap(px+isolmapoffx,py+isolmapoffy,isolmapbuffer)
          ELSE
            light.r = 0
            light.g = 0
            light.b = 0
          ENDIF
        ENDIF
        _ISOITextelGet(x,y,tax,tay,ofx,ofy,ipt,inverted,pxcol)
        IF pxcol = TRANCOLHI THEN transp = 1
        IF pxcol <> TRANCOLHI OR mode = TEX_BOX OR mode = TEX_FRAME THEN
          IF mode = TEX_SOLID OR mode = TEX_BOX THEN
            pxcol = col
          ENDIF
          IF pxcol = TRANCOLHI THEN pxcol = COL_SUBST
          _ISOModulateColor(pxcol,light,colm)
          _ISOPixel(px,py,pxcol,colm,dist,pixelset)
          IF pixelset = 1  AND isoscale0 = 1 THEN
            _ISOSolve3D(a0,b0,c0,x,y,plane,a,b,c)
            pixel3d(px,py).x = a
            pixel3d(px,py).y = b
            pixel3d(px,py).z = c
            pixel3d(px,py).col = pxcol
            pixel3d(px,py).plane = plane
          ENDIF
        ENDIF
      ENDIF
    
    ENDIF
  
  NEXT px
  NEXT py

  'Store shadow plane
  ISOLMapSetPlane a0,b0,c0,ax,ay,tax,tay,ofx,ofy,ipt,plane,inverted,transp

END SUB

' Isometric pixel map proyection
' Input: a, b, c, ax, ay, img, plane
SUB ISOMap (a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
            tax AS INTEGER, tay AS INTEGER, ofx AS INTEGER, ofy AS INTEGER, _
            ipt AS UINTEGER PTR, plane AS INTEGER, mirror AS INTEGER)
  
  'Variables
  DIM AS INTEGER i
  DIM AS INTEGER inverted
  
  'Modify light sources according to visibility
  FOR i=0 TO isolightnum - 1
    isolight(i).calc = ISOLightVisible(i,a0,b0,c0,plane)
  NEXT i
  
  'Calculate inversion flag
  IF plane = ISOZPLANE THEN
    inverted = 1
  ELSEIF plane = ISOXPLANE THEN
    inverted = 1
  ELSEIF plane = ISOYPLANE THEN
    inverted = 0
  ELSEIF plane = ISOFPLANE THEN
    inverted = 1
  ENDIF
  IF mirror = 1 THEN
    IF inverted = 1 THEN 
      inverted = 0
    ELSE
      inverted = 1
    ENDIF
  ENDIF

  'Switch on plane
  IF plane = ISOZPLANE THEN
    ISOIMap a0, b0, c0, ax, ay, tax, tay, ofx, ofy, ipt, plane, inverted, isotexmode, isotexcol
  ELSEIF plane = ISOXPLANE THEN
    ISODMap a0, b0, c0, ax, ay, tax, tay, ofx, ofy, ipt, plane, inverted, isotexmode, isotexcol
  ELSEIF plane = ISOYPLANE THEN
    ISODMap a0, b0, c0, ax, ay, tax, tay, ofx, ofy, ipt, plane, inverted, isotexmode, isotexcol
  ELSEIF plane = ISOFPLANE THEN
    ISODMap a0, b0, c0, ax, ay, tax, tay, ofx, ofy, ipt, plane, inverted, isotexmode, isotexcol
  ENDIF

END SUB

' Draw isometric line
' Input: a1%, b1%, c1%, a2%, b2%, c2%, col%
SUB ISOLine (a1 AS INTEGER, b1 AS INTEGER, c1 AS INTEGER, _
             a2 AS INTEGER, b2 AS INTEGER, c2 AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER px1, px2
  DIM AS INTEGER py1, py2
  
  'Proyect line
  _ISODTrans(a1,b1,c1,px1,py1)
  _ISODTrans(a2,b2,c2,px2,py2)
  
  'Write line
  LINE (px1, py1)-(px2, py2), col
				  
END SUB

' Isometric cursor drawing
SUB ISOSquare(a AS INTEGER, b AS INTEGER, c AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
              plane AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER a1, b1, c1, d1 'Cursor points
  DIM AS INTEGER a2, b2, c2, d2 'Cursor points
  DIM AS INTEGER a3, b3, c3, d3 'Cursor points
  
  'Draw cursor on x plane
  If plane = ISOXPLANE Then
    a1 = a:      a2 = b: a3 = c
    b1 = a:      b2 = b: b3 = c + ay
    c1 = a + ax: c2 = b: c3 = c + ay
    d1 = a + ax: d2 = b: d3 = c
  End if

  'Draw cursor on y plane
  If plane = ISOYPLANE Then
    a1 = a: a2 = b     : a3 = c
    b1 = a: b2 = b:    : b3 = c + ay
    c1 = a: c2 = b + ax: c3 = c + ay
    d1 = a: d2 = b + ax: d3 = c
  End if

  'Draw cursor on z plane
  If plane = ISOZPLANE Then
    a1 = a     : a2 = b     : a3 = c
    b1 = a + ax: b2 = b     : b3 = c
    c1 = a + ax: c2 = b + ay: c3 = c
    d1 = a     : d2 = b + ay: d3 = c
  End if
    
  'Draw cursor
  ISOLine a1, a2, a3, b1, b2, b3, col
  ISOLine b1, b2, b3, c1, c2, c3, col
  ISOLine c1, c2, c3, d1, d2, d3, col
  ISOLine d1, d2, d3, a1, a2, a3, col

End SUB

' Draw isometric circle
SUB ISOCircle (a AS INTEGER, b AS INTEGER, c AS INTEGER, r AS INTEGER, _
               col AS UINTEGER, fill AS INTEGER)

  'Variables
  DIM AS INTEGER px, py
  
  'Proyect circle center
  _ISODTrans(a,b,c,px,py)
  
  'Draw circle
  IF fill = 1 THEN
    CIRCLE (px, py), r, col,,,,F
  ELSE
    CIRCLE (px, py), r, col
  ENDIF
				  
END SUB

'Z-Buffer initialize
SUB ISOZBufferClear
  IF ZBuffLayer = 1 THEN 
    CLEAR zbuffer1(0,0),0,LEN(zbuffer1(0,0))*AX0*AY0
  ELSEIF ZBuffLayer = 2 THEN 
    CLEAR zbuffer2(0,0),0,LEN(zbuffer2(0,0))*AX0*AY0
  ENDIF
END SUB

'Z-Buffer enable
SUB ISOZBufferEnable
  ZBuffEnable = 1
END SUB

'Z-Buffer disable
SUB ISOZBufferDisable
  ZBuffEnable = 0
END SUB

'Z-Buffer set layer
SUB ISOZBufferLayer(layer AS INTEGER)
  ZBuffLayer = layer
END SUB

'Z-Buffer overwrite mode
SUB ISOZBufferOverwrite(mode AS INTEGER)
  ZBuffOvWrite = mode
END SUB

' Determine wether a light source is visible in a plane
FUNCTION ISOLightVisible(index AS INTEGER, a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, plane AS INTEGER) AS INTEGER

  'Variables
  DIM AS INTEGER l1,l2,l3
  DIM AS SINGLE p1,p2,p3
  DIM AS SINGLE prod
 
  'Get normal vector
  _ISONormalVector(plane,p1,p2,p3)

  'Get light vector
  l1 = a0 - isolight(index).x
  l2 = b0 - isolight(index).y
  l3 = c0 - isolight(index).z
  
  'Get scalar product
  prod = l1*p1 + l2*p2 + l3*p3 
  
  'Calculate light factor
  IF prod > 0 THEN
    ISOLightVisible = 0
  ELSE
    ISOLightVisible = 1
  ENDIF

END FUNCTION

' Calculate pixel brightnes according to light sources
FUNCTION ISOLightCalc (a AS INTEGER, b AS INTEGER, c AS INTEGER, plane AS INTEGER) AS RGBf

  'Variables
  DIM AS INTEGER i, used
  DIM AS INTEGER l1,l2,l3
  DIM AS SINGLE p1,p2,p3
  DIM AS SINGLE prod, lm2, factor
  DIM AS RGBf light
 
  'Get normal vector
  _ISONormalVector(plane,p1,p2,p3)

  'Init light factor
  light.r = isolightambr
  light.g = isolightambg
  light.b = isolightambb
  
  'Calculation loop over light sources
  FOR i=0 TO isolightnum - 1
  
    'Do not calculate light source if it is not visible
    IF isolight(i).calc = 1 THEN
    
      'Get light vector
      l1 = a - isolight(i).x
      l2 = b - isolight(i).y
      l3 = c - isolight(i).z
      lm2 = l1*l1 + l2*l2 + l3*l3
  
      'Get scalar product
      prod = l1*p1 + l2*p2 + l3*p3 
  
      'Calculate light factor
      IF lm2 = 0 THEN
        factor = 100
      ELSE
        factor = ABS(Prod) * isolight(i).lum / lm2
      ENDIF
      light.r = light.r + factor * isolight(i).r
      light.g = light.g + factor * isolight(i).g
      light.b = light.b + factor * isolight(i).b
    
    ENDIF
    
  NEXT i
  
  'Return light factor
  ISOLightCalc = light
  
END FUNCTION

'Set ambient light
SUB ISOLightAmbient(l AS SINGLE, r AS INTEGER, g AS INTEGER, b AS INTEGER)
  isolightambr = l * r / 100
  isolightambg = l * g / 100
  isolightambb = l * b / 100
END SUB

'Set light aproximation factor
SUB ISOLightAproxFactor(f AS INTEGER)
  isolightaprox = f
END SUB

'Clear all light sources
SUB ISOLightClearAll
  DIM AS INTEGER i
  FOR i = 0 TO MAXLIGHT - 1
    isolight(i).x = 0
    isolight(i).y = 0
    isolight(i).z = 0
    isolight(i).lum = 0
    isolight(i).r = 0
    isolight(i).g = 0
    isolight(i).b = 0
    isolight(i).calc = 0
  NEXT i
  isolightnum = 0
END SUB

'Set light source
SUB ISOLightSource( x AS INTEGER, y AS INTEGER, z AS INTEGER, lum AS SINGLE, _
                    r AS INTEGER, g AS INTEGER, b AS INTEGER )
  isolight(isolightnum).x = x
  isolight(isolightnum).y = y
  isolight(isolightnum).z = z
  isolight(isolightnum).lum = lum
  isolight(isolightnum).r = r
  isolight(isolightnum).g = g
  isolight(isolightnum).b = b
  isolight(isolightnum).calc = 0
  isolightnum = isolightnum + 1
END SUB

' Init shadow calculation
SUB ISOLMapInit
  numsplane = 0
  numuplane = 0
  CLEAR pixel3d(0,0),0,LEN(pixel3d(0,0))*AX0*AY0
  CLEAR lightmap(0,0,0),0,LEN(lightmap(0,0,0))*AX0*AY0*2
  isolmapupdated = 0
END SUB
    
' Set shadow plane
SUB ISOLMapSetPlane(a0 AS INTEGER, b0 AS INTEGER, c0 AS INTEGER, _
                    ax AS INTEGER, ay AS INTEGER, tax AS INTEGER, tay AS INTEGER, _
                    ofx AS INTEGER, ofy AS INTEGER, ipt AS UINTEGER PTR, _
                    plane AS INTEGER, inverted AS INTEGER, transp AS INTEGER)
  
  'Exit if plane store is not enabled
  IF shadowstore = 0 THEN EXIT SUB
  
  'Store shadow plane
  splane(numsplane).a0       = a0    
  splane(numsplane).b0       = b0    
  splane(numsplane).c0       = c0    
  splane(numsplane).ax       = ax    
  splane(numsplane).ay       = ay    
  splane(numsplane).tax      = tax   
  splane(numsplane).tay      = tay   
  splane(numsplane).ofx      = ofx   
  splane(numsplane).ofy      = ofy   
  splane(numsplane).ipt      = ipt   
  splane(numsplane).plane    = plane 
  splane(numsplane).inverted = inverted
  splane(numsplane).transp   = transp
  
  'Increase shadow plane counter
  numsplane = numsplane + 1

END SUB

'Unify shadow planes
SUB ISOLMapUnifyPlanes(plane AS INTEGER, plane1() AS shadowplane, plane2() AS shadowplane, _
                       numplane1 AS INTEGER, BYREF numplane2 AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,j
  DIM AS INTEGER x,y,z
  DIM AS INTEGER a0,b0,c0
  DIM AS INTEGER unified           
  
  'Unify X planes
  IF plane = ISOXPLANE THEN
    FOR i=0 TO numplane1-1
      IF plane1(i).plane = ISOXPLANE THEN
        unified = 0
        FOR j=0 TO numplane2-1
          IF plane1(i).a0 = plane2(j).a0 _
          AND plane1(i).c0 = plane2(j).c0 _
          AND ( plane1(i).b0 = plane2(j).b0 + plane2(j).ax _
          OR    plane1(i).b0 + plane1(i).ax = plane2(j).b0 ) _
          AND plane1(i).ay = plane2(j).ay _
          AND plane1(i).plane = plane2(j).plane _
          AND plane1(i).transp = 0 _
          AND plane2(j).transp = 0 THEN
            IF plane1(i).b0 = plane2(j).b0 + plane2(j).ax THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
            ELSEIF plane1(i).b0 + plane1(i).ax = plane2(j).b0 THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
              plane2(j).b0 = plane2(j).b0 - plane1(i).ax
            ENDIF
            unified = 1
            EXIT FOR
          ENDIF
        NEXT j
        IF unified = 0 THEN
          plane2(numplane2) = plane1(i)
         numplane2 = numplane2 + 1
        ENDIF
      ENDIF
    NEXT i    
  ENDIF

  'Unify Y planes
  IF plane = ISOYPLANE THEN
    FOR i=0 TO numplane1-1
      IF plane1(i).plane = ISOYPLANE THEN
        unified = 0
        FOR j=0 TO numplane2-1
          IF  plane1(i).b0 = plane2(j).b0 _
          AND plane1(i).c0 = plane2(j).c0 _
          AND ( plane1(i).a0 = plane2(j).a0 + plane2(j).ax _
          OR    plane1(i).a0 + plane1(i).ax = plane2(j).a0 )_
          AND plane1(i).ay = plane2(j).ay _
          AND plane1(i).plane = plane2(j).plane _
          AND plane1(i).transp = 0 _
          AND plane2(j).transp = 0 THEN
            IF plane1(i).a0 = plane2(j).a0 + plane2(j).ax THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
            ELSEIF plane1(i).a0 + plane1(i).ax = plane2(j).a0 THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
              plane2(j).a0 = plane2(j).a0 - plane1(i).ax
            ENDIF
            unified = 1
            EXIT FOR
          ENDIF
        NEXT j
        IF unified = 0 THEN
          plane2(numplane2) = plane1(i)
          numplane2 = numplane2 + 1
        ENDIF
      ENDIF
    NEXT i    
  ENDIF

  'Unify Z planes
  IF plane = ISOZPLANE THEN
    FOR i=0 TO numplane1-1
      IF plane1(i).plane = ISOZPLANE THEN
        unified = 0
        FOR j=0 TO numplane2-1
          IF plane1(i).c0 = plane2(j).c0 _
          AND plane1(i).plane = plane2(j).plane _
          AND plane1(i).transp = 0 _
          AND plane2(j).transp = 0 _
          AND plane1(i).b0 = plane2(j).b0 _
          AND plane1(i).ay = plane2(j).ay _
          AND ( plane1(i).a0 = plane2(j).a0 + plane2(j).ax _
          OR    plane1(i).a0 + plane1(i).ax = plane2(j).a0 ) THEN
            IF plane1(i).a0 = plane2(j).a0 + plane2(j).ax THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
            ELSEIF plane1(i).a0 + plane1(i).ax = plane2(j).a0 THEN
              plane2(j).ax = plane2(j).ax + plane1(i).ax
              plane2(j).a0 = plane2(j).a0 - plane1(i).ax
            ENDIF
            unified = 1
            EXIT FOR
          ENDIF
          IF plane1(i).c0 = plane2(j).c0 _
          AND plane1(i).plane = plane2(j).plane _
          AND plane1(i).transp = 0 _
          AND plane2(j).transp = 0 _
          AND plane1(i).a0 = plane2(j).a0 _
          AND plane1(i).ax = plane2(j).ax _
          AND ( plane1(i).b0 = plane2(j).b0 + plane2(j).ay _
          OR    plane1(i).b0 + plane1(i).ay = plane2(j).b0 ) THEN
            IF plane1(i).b0 = plane2(j).b0 + plane2(j).ay THEN
              plane2(j).ay = plane2(j).ay + plane1(i).ay
            ELSEIF plane1(i).b0 + plane1(i).ay = plane2(j).b0 THEN
              plane2(j).ay = plane2(j).ay + plane1(i).ay
              plane2(j).b0 = plane2(j).b0 - plane1(i).ay
            ENDIF
            unified = 1
            EXIT FOR
          ENDIF
        NEXT j
        IF unified = 0 THEN
          plane2(numplane2) = plane1(i)
          numplane2 = numplane2 + 1
        ENDIF
      ENDIF
    NEXT i    
  ENDIF

  'Copy F planes (they are not unified)
  IF plane = ISOFPLANE THEN
    FOR i=0 TO numplane1-1
      IF plane1(i).plane = ISOFPLANE THEN
        plane2(numplane2) = plane1(i)
        numplane2 = numplane2 + 1
      ENDIF
    NEXT i    
  ENDIF

END SUB

' Unify all planes
SUB ISOLMapUnifyAllPlanes

  'Variables
  DIM AS INTEGER numaplane
  DIM AS shadowplane aplane(MAXPLANE)
  
  'Exit if planes already unified
  IF numuplane <> 0 THEN EXIT SUB
  
  'Init auxiliary planes
  numaplane = 0
  
  'Unify X planes
  ISOLMapUnifyPlanes ISOXPLANE, splane(), uplane(), numsplane, numuplane

  'Unify Y planes
  ISOLMapUnifyPlanes ISOYPLANE, splane(), uplane(), numsplane, numuplane

  'Unify Z planes (two steps)
  ISOLMapUnifyPlanes ISOZPLANE, splane(), aplane(), numsplane, numaplane
  ISOLMapUnifyPlanes ISOZPLANE, aplane(), uplane(), numaplane, numuplane
  
  'Unify F planes
  ISOLMapUnifyPlanes ISOFPLANE, splane(), uplane(), numsplane, numuplane
  
END SUB                  

'Calculate light map
SUB ISOLMapCalculate
  
  'Variables
  DIM AS INTEGER i,j,k
  DIM AS INTEGER shadowed
  DIM AS SINGLE percentage

  'Process columns
  FOR i=0 TO AX0-1
    
    'Process rows
    FOR j=0 TO AY0-1
    
      'Calculate non empty pixels
      IF pixel3d(i,j).plane <> 0 THEN
    
        'Disable shadowed light sources for this pixel
        FOR k=0 TO isolightnum-1
          isolight(k).calc = ISOLightVisible(k,pixel3d(i,j).x,pixel3d(i,j).y,pixel3d(i,j).z,pixel3d(i,j).plane)
          IF isolight(k).calc = 1 THEN
            IF ISOLMapShadowedRay(pixel3d(i,j),k) = 1 THEN 
              isolight(k).calc = 0
            ENDIF
          ENDIF
        NEXT k
          
        'Light calculation
        lightmap(i,j,isolmapbuffer) = _
        ISOLightCalc(pixel3d(i,j).x,pixel3d(i,j).y,pixel3d(i,j).z,pixel3d(i,j).plane)
          
      ENDIF
    
    NEXT j
    
    'Print completion percentage
    IF (i MOD 5) = 0 THEN
      percentage = (100 * i) / AX0
      PrintStrBkg FCol(25),FRow(52),"Calculating light map..." + FORMAT(percentage,"000") + "%",RGB(255,255,255),0,2
    ENDIF
  
  NEXT i
  
  'Set light map updated flag
  isolmapupdated = 1
  
END SUB

'Convert RGB value from float to integer
#MACRO _RGBf2i(f,i)
  f.r = (65534 * f.r) / 100: i.r = INT(f.r): IF i.r > 65534 THEN i.r = 65534
  f.g = (65534 * f.g) / 100: i.g = INT(f.g): IF i.g > 65534 THEN i.g = 65534
  f.b = (65534 * f.b) / 100: i.b = INT(f.b): IF i.b > 65534 THEN i.b = 65534
#ENDMACRO

'Convert RGB value from integer to float
#MACRO _RGBi2f(i,f)
  f.r = 100 * i.r: f.r /= 65534
  f.g = 100 * i.g: f.g /= 65534
  f.b = 100 * i.b: f.b /= 65534
#ENDMACRO

'Export light map
SUB ISOLMapExport(lmap() AS RGBi, BYREF lmaplen AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,j,k
  DIM AS RGBf value
  DIM AS INTEGER count

  'Export light map
  k = 0: count = 0
  FOR i=0 TO AX0-1
  FOR j=0 TO AY0-1
    IF  lightmap(i,j,isolmapbuffer).r = 0 _
    AND lightmap(i,j,isolmapbuffer).g = 0 _
    AND lightmap(i,j,isolmapbuffer).b = 0 THEN
      count = count + 1
      IF count = 65535 THEN
        lmap(k).r = 65535
        lmap(k).g = count
        lmap(k).b = 0
        k = k + 1
        count = 0
      ENDIF
    ELSE
      IF count > 0 THEN
        lmap(k).r = 65535
        lmap(k).g = count
        lmap(k).b = 0
        k = k + 1
        count = 0
      ENDIF
      value = lightmap(i,j,isolmapbuffer)
      _RGBf2i(value,lmap(k))
      k = k + 1
    ENDIF
  NEXT j
  NEXT i
  IF count > 0 THEN
    lmap(k).r = 65535
    lmap(k).g = count
    lmap(k).b = 0
    k = k + 1
  ENDIF
  lmaplen = k

END SUB

'Import light map
SUB ISOLMapImport(lmap() AS RGBi, lmaplen AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,j,k,l
  DIM AS INTEGER count
 
  'Export light map
  i = 0: j = 0: k = 0: count = 0
  WHILE k < lmaplen
    IF lmap(k).r = 65535 THEN
      count = lmap(k).g
      k = k + 1
      FOR l=0 TO count-1
        lightmap(i,j,isolmapbuffer).r = 0
        lightmap(i,j,isolmapbuffer).g = 0
        lightmap(i,j,isolmapbuffer).b = 0
        j = j + 1
        IF j >= AY0 THEN
          j = 0
          i = i + 1
        ENDIF
      NEXT l
    ELSE
      _RGBi2f(lmap(k),lightmap(i,j,isolmapbuffer))
      k = k + 1
      j = j + 1
      IF j >= AY0 THEN
        j = 0
        i = i + 1
      ENDIF
    ENDIF
  WEND

  'Set light map updated flag
  isolmapupdated = 1
  
END SUB

' Enable shadow plane store
SUB ISOLMapStorePlanes(store AS INTEGER)
  shadowstore = store
END SUB
    
' Set light map offset
SUB ISOLMapOffset(x AS INTEGER,y AS INTEGER)
  isolmapoffx = x
  isolmapoffy = y
END SUB
    
'Get light map status
FUNCTION ISOLMapStatus AS INTEGER
  ISOLMapStatus = isolmapupdated
END FUNCTION

'Set use light map flag
SUB ISOLMapUseFlag(use AS INTEGER)
  isolmapuse = use
END SUB

'Light map buffer increase
SUB ISOLMapIncBuffer
  IF isolmapbuffer < MAXLMBUF - 1 THEN
    isolmapbuffer = isolmapbuffer + 1
  ENDIF
END SUB

'Light map buffer decrease
SUB ISOLMapDecBuffer
  IF isolmapbuffer > 0 THEN
    isolmapbuffer = isolmapbuffer - 1
  ENDIF
END SUB

'Calculate if light ray is shadowed
FUNCTION ISOLMapShadowedRay(pix3d AS pixelcoord, light AS INTEGER) AS INTEGER
  
  'Check pixel inside of shadow plane
  #MACRO CheckInside(a,b,c,ax,ay,plane,border,inside)
    inside = 0
    IF plane = ISOXPLANE _
    AND b >= -border AND b < ax+2*border _
    AND c >= -border AND c < ay+2*border THEN
      inside = 1
    ELSEIF plane = ISOYPLANE _
    AND a >= -border AND a < ax+2*border _
    AND c >= -border AND c < ay+2*border THEN
      inside = 1
    ELSEIF plane = ISOZPLANE _
    AND a >= -border AND a < ax+2*border _
    AND b >= -border AND b < ay+2*border THEN
      inside = 1
    ENDIF
  #ENDMACRO

  'Variables
  DIM AS INTEGER i,j,k         'Counter
  DIM AS Mtx m0,m1             'Matrixes
  DIM AS Vec s0,s1             'Vectors
  DIM AS SINGLE lx, ly, lz     'Light source
  DIM AS SINGLE px, py, pz     'Pixel location
  DIM AS SINGLE rx, ry, rz     'Light ray vector
  DIM AS SINGLE u1, u2, u3, u4 'Plane 1 for light ray line
  DIM AS SINGLE v1, v2, v3, v4 'Plane 2 for light ray line
  DIM AS SINGLE w1, w2, w3, w4 'Shadow plane 
  DIM AS SINGLE a, b, c        'Shadow plane coordinates
  DIM AS INTEGER x, y          'Textel coordinates
  DIM AS INTEGER shadowed      'Shadowed ray flag
  DIM AS INTEGER inside        'Inside shadow plane flag
  DIM AS INTEGER checkplane    'Check flag
  DIM AS UINTEGER pxcol        'Pixel color
  DIM AS INTEGER wlight        'W plane equation with light point
  DIM AS INTEGER wpixel        'W plane equation with pixel point
  DIM AS INTEGER frame         'Pixel frame flag
  
  'Light source & pixel location
  lx = isolight(light).x: px = pix3d.x
  ly = isolight(light).y: py = pix3d.y
  lz = isolight(light).z: pz = pix3d.z
  
  'Get light ray definition
  rx = px - lx: ry = py - ly: rz = pz - lz
  
  'Get light ray planes
  IF rx <> 0 THEN
    u1 = (-ry/rx): u2 = 1: u3 = 0: u4 = -u1 * px - u2 * py
    v1 = (-rz/rx): v2 = 0: v3 = 1: v4 = -v1 * px - v3 * pz
  ELSEIF ry <> 0 THEN
    u1 = 1: u2 = (-rx/ry): u3 = 0: u4 = -u1 * px - u2 * py
    v1 = 0: v2 = (-rz/ry): v3 = 1: v4 = -v2 * py - v3 * pz
  ELSEIF rz <> 0 THEN
    u1 = 1: u2 = 0: u3 = (-rx/rz): u4 = -u1 * px - u3 * pz
    v1 = 0: v2 = 1: v3 = (-ry/rz): v4 = -v2 * py - v3 * pz
  ENDIF
  
  'Loop over shadow planes to find if light ray is interrupted
  shadowed = 0
  FOR k=0 TO numuplane-1
    
    'Skip FPLANE for now
    IF uplane(k).plane <> ISOFPLANE THEN
    
      'Get shadow plane definition
      _ISONormalVector(uplane(k).plane,w1,w2,w3)
      w4 = -w1 * uplane(k).a0 - w2 * uplane(k).b0 - w3 * uplane(k).c0
    
      'Get shadow plane signs for light source and pixel
      wlight = SGN(w1*lx+w2*ly+w3*lz+w4)
      wpixel = SGN(w1*px+w2*py+w3*pz+w4)
      
      'Check light source and pixel on same side of plane
      IF wlight <> wpixel THEN       
      
        'Solve equations to get solution point
        m0 = Stm(u1,u2,u3,v1,v2,v3,w1,w2,w3)
        m1 = Inv(m0)
      
        'Continue if solution is found (result is not zero matrix)
        IF Com(m1,Clm())=0 THEN
        
          'Get solution point
          s0 = Stv(-u4,-v4,-w4)
          s1 = Mxv(m1,s0)
          
          'Get coordinates of solution point in shadow plane
          a = s1.x - uplane(k).a0
          b = s1.y - uplane(k).b0
          c = s1.z - uplane(k).c0
          
          'Check pixels at plane borders
          IF wpixel = 0 AND ( a <> px OR b <> py OR c <> px ) THEN
            
            'Check if solution point is contained in shadow plane
            CheckInside(a,b,c,uplane(k).ax,uplane(k).ay,uplane(k).plane,1,inside)
    
            'Shadow pixels that belong to several planes with diff. light visibility
            IF inside = 1 THEN
              IF ISOLightVisible(light,px,py,pz,uplane(k).plane) = 0 THEN
                shadowed = 1
                EXIT FOR
              ENDIF
            ENDIF
            
          'Check normal pixels
          ELSEIF ( a <> px OR b <> py OR c <> px ) THEN
          
            'Check if solution point is contained in shadow plane
            CheckInside(a,b,c,uplane(k).ax,uplane(k).ay,uplane(k).plane,0,inside)
            
            'Check inside pixels only
            IF inside = 1 THEN
            
              'Shadowed pixel if plane has no transparency
              IF uplane(k).transp = 0 THEN
                shadowed = 1
                EXIT FOR
          
              'Check textel if plane has transparency
              ELSEIF uplane(k).transp = 1 THEN
                IF uplane(k).plane = ISOXPLANE THEN
                  x = b: y = c
                ELSEIF  uplane(k).plane = ISOYPLANE THEN
                  x = a: y = c
                ELSEIF  uplane(k).plane = ISOZPLANE THEN
                  x = a: y = b
                ENDIF
                IF uplane(k).plane = ISOZPLANE THEN
                  _ISOITextelGet(x,y,uplane(k).tax,uplane(k).tay,uplane(k).ofx,uplane(k).ofy,uplane(k).ipt,uplane(k).inverted,pxcol)
                ELSE
                  _ISODTextelGet(x,y,uplane(k).tax,uplane(k).tay,uplane(k).ofx,uplane(k).ofy,uplane(k).ipt,uplane(k).inverted,pxcol)
                ENDIF
                IF pxcol <> TRANCOLHI THEN
                  shadowed = 1
                  EXIT FOR
                ENDIF
              ENDIF
              
            ENDIF
          
          ENDIF
          
        ENDIF
      
      ENDIF
    
    ENDIF  
    
  NEXT k
  
  'Set function result
  ISOLMapShadowedRay = shadowed
  
END FUNCTION

'Generate difuse light map
SUB ISOLMapDifuse(difuse AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,j
  
  'Map loop (difuse mode 4)
  IF difuse = 4 THEN
    FOR i=0 TO AX0% - 2
    FOR j=0 TO AY0% - 2
      lightmap(i,j,isolmapbuffer).r = lightmap(i+0,j+0,isolmapbuffer).r _
                                    + lightmap(i+1,j+0,isolmapbuffer).r _
                                    + lightmap(i+1,j+1,isolmapbuffer).r _
                                    + lightmap(i+0,j+1,isolmapbuffer).r
      lightmap(i,j,isolmapbuffer).r = lightmap(i,j,isolmapbuffer).r / 4
      lightmap(i,j,isolmapbuffer).g = lightmap(i+0,j+0,isolmapbuffer).g _
                                    + lightmap(i+1,j+0,isolmapbuffer).g _
                                    + lightmap(i+1,j+1,isolmapbuffer).g _
                                    + lightmap(i+0,j+1,isolmapbuffer).g
      lightmap(i,j,isolmapbuffer).g = lightmap(i,j,isolmapbuffer).g / 4
      lightmap(i,j,isolmapbuffer).b = lightmap(i+0,j+0,isolmapbuffer).b _
                                    + lightmap(i+1,j+0,isolmapbuffer).b _
                                    + lightmap(i+1,j+1,isolmapbuffer).b _
                                    + lightmap(i+0,j+1,isolmapbuffer).b
      lightmap(i,j,isolmapbuffer).b = lightmap(i,j,isolmapbuffer).b / 4
    NEXT j
    NEXT i
  
  'Map loop (difuse mode 5)
  ELSEIF difuse = 5 THEN
    FOR i=1 TO AX0% - 2
    FOR j=1 TO AY0% - 2
      lightmap(i,j,isolmapbuffer).r = lightmap(i+0,j+0,isolmapbuffer).r _
                                    + lightmap(i+1,j+1,isolmapbuffer).r _
                                    + lightmap(i-1,j-1,isolmapbuffer).r _
                                    + lightmap(i-1,j+1,isolmapbuffer).r _
                                    + lightmap(i+1,j-1,isolmapbuffer).r
      lightmap(i,j,isolmapbuffer).r = lightmap(i,j,isolmapbuffer).r / 5
      lightmap(i,j,isolmapbuffer).g = lightmap(i+0,j+0,isolmapbuffer).g _
                                    + lightmap(i+1,j+1,isolmapbuffer).g _
                                    + lightmap(i-1,j-1,isolmapbuffer).g _
                                    + lightmap(i-1,j+1,isolmapbuffer).g _
                                    + lightmap(i+1,j-1,isolmapbuffer).g
      lightmap(i,j,isolmapbuffer).g = lightmap(i,j,isolmapbuffer).g / 5
      lightmap(i,j,isolmapbuffer).b = lightmap(i+0,j+0,isolmapbuffer).b _
                                    + lightmap(i+1,j+1,isolmapbuffer).b _
                                    + lightmap(i-1,j-1,isolmapbuffer).b _
                                    + lightmap(i-1,j+1,isolmapbuffer).b _
                                    + lightmap(i+1,j-1,isolmapbuffer).b
      lightmap(i,j,isolmapbuffer).b = lightmap(i,j,isolmapbuffer).b / 5
    NEXT j
    NEXT i

  'Map loop (difuse mode 9)
  ELSEIF difuse = 9 THEN
    FOR i=1 TO AX0% - 2
    FOR j=1 TO AY0% - 2
      lightmap(i,j,isolmapbuffer).r = lightmap(i+0,j+0,isolmapbuffer).r _
                                    + lightmap(i+0,j+1,isolmapbuffer).r _
                                    + lightmap(i+0,j-1,isolmapbuffer).r _
                                    + lightmap(i+1,j+0,isolmapbuffer).r _
                                    + lightmap(i+1,j+1,isolmapbuffer).r _
                                    + lightmap(i+1,j-1,isolmapbuffer).r _
                                    + lightmap(i-1,j+0,isolmapbuffer).r _
                                    + lightmap(i-1,j+1,isolmapbuffer).r _
                                    + lightmap(i-1,j-1,isolmapbuffer).r
      lightmap(i,j,isolmapbuffer).r = lightmap(i,j,isolmapbuffer).r / 9
      lightmap(i,j,isolmapbuffer).g = lightmap(i+0,j+0,isolmapbuffer).g _
                                    + lightmap(i+0,j+1,isolmapbuffer).g _
                                    + lightmap(i+0,j-1,isolmapbuffer).g _
                                    + lightmap(i+1,j+0,isolmapbuffer).g _
                                    + lightmap(i+1,j+1,isolmapbuffer).g _
                                    + lightmap(i+1,j-1,isolmapbuffer).g _
                                    + lightmap(i-1,j+0,isolmapbuffer).g _
                                    + lightmap(i-1,j+1,isolmapbuffer).g _
                                    + lightmap(i-1,j-1,isolmapbuffer).g
      lightmap(i,j,isolmapbuffer).g = lightmap(i,j,isolmapbuffer).g / 9
      lightmap(i,j,isolmapbuffer).b = lightmap(i+0,j+0,isolmapbuffer).b _
                                    + lightmap(i+0,j+1,isolmapbuffer).b _
                                    + lightmap(i+0,j-1,isolmapbuffer).b _
                                    + lightmap(i+1,j+0,isolmapbuffer).b _
                                    + lightmap(i+1,j+1,isolmapbuffer).b _
                                    + lightmap(i+1,j-1,isolmapbuffer).b _
                                    + lightmap(i-1,j+0,isolmapbuffer).b _
                                    + lightmap(i-1,j+1,isolmapbuffer).b _
                                    + lightmap(i-1,j-1,isolmapbuffer).b
      lightmap(i,j,isolmapbuffer).b = lightmap(i,j,isolmapbuffer).b / 9
    NEXT j
    NEXT i
  ENDIF  

END SUB

'Draw shadow planes
SUB ISOLMapDrawPlanes(unified AS INTEGER)
  
  'Variables
  DIM AS INTEGER i
  DIM AS UINTEGER col
    
  'Disable ZBuffer
  ISOZBufferDisable
    
  'Draw source planes
  IF unified = 0 THEN
    FOR i=0 TO numsplane-1
      IF uplane(i).transp = 0 THEN
        col = RGB(200,200,200)
      ELSEIF uplane(i).transp = 1 THEN
        col = RGB(255,0,0)
      ENDIF
      IF splane(i).plane = ISOZPLANE THEN
        ISOIMap splane(i).a0, splane(i).b0, splane(i).c0, splane(i).ax, splane(i).ay, _
        splane(i).tax, splane(i).tay, splane(i).ofx, splane(i).ofy, splane(i).ipt, _
        splane(i).plane, splane(i).inverted, TEX_BOX, col
      ELSE
        ISODMap splane(i).a0, splane(i).b0, splane(i).c0, splane(i).ax, splane(i).ay, _
        splane(i).tax, splane(i).tay, splane(i).ofx, splane(i).ofy, splane(i).ipt, _
        splane(i).plane, splane(i).inverted, TEX_BOX, col
      ENDIF
    NEXT i
  
  'Draw unified planes
  ELSE
    FOR i=0 TO numuplane-1
      IF uplane(i).transp = 0 THEN
        col = RGB(200,200,200)
      ELSEIF uplane(i).transp = 1 THEN
        col = RGB(255,0,0)
      ENDIF
      IF uplane(i).plane = ISOZPLANE THEN
        ISOIMap uplane(i).a0, uplane(i).b0, uplane(i).c0, uplane(i).ax, uplane(i).ay, _
        uplane(i).tax, uplane(i).tay, uplane(i).ofx, uplane(i).ofy, uplane(i).ipt, _
        uplane(i).plane, uplane(i).inverted, TEX_BOX, col
      ELSE
        ISODMap uplane(i).a0, uplane(i).b0, uplane(i).c0, uplane(i).ax, uplane(i).ay, _
        uplane(i).tax, uplane(i).tay, uplane(i).ofx, uplane(i).ofy, uplane(i).ipt, _
        uplane(i).plane, uplane(i).inverted, TEX_BOX, col
      ENDIF
    NEXT i
  ENDIF
  
  'Enable ZBuffer
  ISOZBufferEnable
  
  'Print number of planes
  PrintStr FCol(25),FRow(2),"Source planes.:" + STR$(numsplane), RGB(255,255,255)
  PrintStr FCol(25),FRow(3),"Unified planes:" + STR$(numuplane), RGB(255,255,255)

END SUB

'*** End of isolib.bas ***


'*** Begin of texlib.bi ***

'Constants
CONST TEXTUREAX = 320
CONST TEXTUREAY = 200
CONST CLIPAXTEX = 64
CONST CLIPAYTEX = 64
CONST TEXTURESZ = TEXTUREAX * TEXTUREAY
CONST MAXTEXTUR = 4000
CONST TEXTURMEM = TEXTURESZ * MAXTEXTUR / 16
CONST NUMCATEG = 12 
CONST BINSEARCH = 1

'Texture memory control records
TYPE TxRecord
  TexDir AS INTEGER
  TexName AS STRING * 15
  Ax AS INTEGER
  Ay AS INTEGER
  Start AS INTEGER
  Length AS INTEGER
  TexPath AS STRING * 255
  Sort AS STRING * 20
END TYPE

'Texture clasifier file
TYPE TexClasf
  txd AS STRING * 1
  texname AS STRING * 15
  cname AS STRING * 12
END TYPE

'Functions
DECLARE SUB TexLoaderConfig(factor AS INTEGER,texpath AS STRING)
DECLARE SUB KillTexMemory
DECLARE SUB TexClasif(commd AS STRING, txd AS INTEGER, texname AS STRING, BYREF cname AS STRING, BYREF counter AS INTEGER)
DECLARE FUNCTION TexMemGetSpace AS STRING
DECLARE FUNCTION GetTexPath(txd AS INTEGER) AS STRING
DECLARE FUNCTION ReadTexture(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR) AS INTEGER
DECLARE FUNCTION ReadTextureOnce(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR, buffer AS INTEGER) AS INTEGER
DECLARE FUNCTION GetTexCat(cat AS INTEGER) AS STRING
DECLARE SUB TexMemGetOccupied(BYREF recused AS INTEGER, BYREF memused AS INTEGER)
DECLARE SUB InsertTexRecord(TxRec0 AS TxRecord)

'*** End of texlib.bi ***


'*** Begin of texlib.bas ***

#INCLUDE "texlib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "vbcompat.bi"

'Texture memory control array
DIM SHARED TxPtr AS LONG = 0
DIM SHARED TxNum AS INTEGER = 0
REDIM SHARED TxRec(MAXTEXTUR) AS TxRecord
REDIM SHARED TextureMem(TEXTURMEM) AS UINTEGER
REDIM SHARED aTxRec(10) AS TxRecord
REDIM SHARED TxAux(MAXTEXTUR) AS TxRecord

'Double image buffer
REDIM SHARED imgpx1(TEXTURESZ) AS UINTEGER
REDIM SHARED imgpx2(TEXTURESZ*4) AS UINTEGER

'Texture loader configuration variables
DIM SHARED txl_factor AS INTEGER
DIM SHARED txl_texpath AS STRING

'Texture classification
REDIM SHARED TxCat(MAXTEXTUR) AS TexClasf 'Texture classification

' Get texture directory path
FUNCTION GetTexPath(txd AS INTEGER) AS STRING
  SELECT CASE txd 
    CASE 1: GetTexPath = "textures\wall\"      'Wall texture
    CASE 2: GetTexPath = "textures\ground\"    'Ground texture
    CASE 3: GetTexPath = "textures\object\"    'Object textures
    CASE 4: GetTexPath = "textures\entity\"    'Entity textures
  END SELECT
END FUNCTION

' Get texture category
FUNCTION GetTexCat(cat AS INTEGER) AS STRING
  SELECT CASE cat
    CASE 00: GetTexCat = "[all]"
    CASE 01: GetTexCat = "WOods"
    CASE 02: GetTexCat = "STone"
    CASE 03: GetTexCat = "MEtal"
    CASE 04: GetTexCat = "DOors"
    CASE 05: GetTexCat = "TRans"
    CASE 06: GetTexCat = "GLass"
    CASE 07: GetTexCat = "ARtis"
    CASE 08: GetTexCat = "DEvic"
    CASE 09: GetTexCat = "ORgan"
    CASE 10: GetTexCat = "WAter"
    CASE 11: GetTexCat = "OTher"
    CASE 99: GetTexCat = "[w/o]"
  END SELECT
END FUNCTION

'Texture classification
SUB TexClasif(commd AS STRING, txd AS INTEGER, texname AS STRING, _
              BYREF cname AS STRING, BYREF counter AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,k, found
  DIM AS INTEGER file
  DIM AS INTEGER count
  STATIC AS INTEGER numclasf
  
  'Switch on command
  SELECT CASE commd
  
    'Read texture classification file
    CASE "READ"
     file = FREEFILE
     OPEN "texclasf.dat" FOR RANDOM as #file Len = SIZEOF(TexClasf)
     k=0
     WHILE NOT EOF(file) AND k < MAXTEXTUR
       GET #file,,TxCat(k)
       k = k + 1
     WEND
     CLOSE #file
     numclasf = k

    'Save texture clasification file
    CASE "WRITE"
      file = FREEFILE
      OPEN "texclasf.dat" FOR RANDOM as #file Len = SIZEOF(TexClasf)
      k=0
      WHILE k < numclasf
        PUT #file,,TxCat(k)
        k = k + 1
      WEND
      CLOSE #file
      
    'Get classification for texture
    CASE "GET"
      found = 0
      FOR i=0 TO numclasf - 1
        IF val(TxCat(i).txd) = txd AND TxCat(i).texname = texname THEN 
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        cname = TxCat(i).cname
      ELSE
        cname = ""
      ENDIF
      
    'Set classification for texture
    CASE "SET"
      found = 0
      FOR i=0 TO numclasf - 1
        IF val(TxCat(i).txd) = txd AND TxCat(i).texname = texname THEN 
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        TxCat(i).cname = cname
      ELSEIF found = 0 AND numclasf < MAXTEXTUR THEN
        i = numclasf
        TxCat(i).txd = STR$(txd)
        TxCat(i).texname = texname
        TxCat(i).cname = cname
        numclasf = numclasf + 1
      ENDIF
    
    'Count items in category
    CASE "COUNT"
      count = 0
      FOR i=0 TO numclasf - 1
        IF VAL(TxCat(i).txd) = txd AND TxCat(i).cname = cname THEN
          count = count + 1 
        ENDIF
      NEXT i
      counter = count
    
  END SELECT

END SUB

' Get texture memory occupied space
FUNCTION TexMemGetSpace AS STRING
  TexMemGetSpace = "Mem:" + FORMAT(TxNum / MAXTEXTUR,"000%") + " " + FORMAT(TxPtr / TEXTURMEM,"000%")
END FUNCTION

' Get texture memory occupied space
SUB TexMemGetOccupied(BYREF recused AS INTEGER, BYREF memused AS INTEGER)
  
  'Vartiables
  DIM AS SINGLE rec, mem
  
  'Return occupied memory
  rec = 100 * TxNum / MAXTEXTUR
  mem = 100 * TxPtr / TEXTURMEM
  recused = INT(rec)
  memused = INT(mem)
  
END SUB

'Texture loader configuration
SUB TexLoaderConfig(factor AS INTEGER, texpath AS STRING)
  txl_factor = factor
  txl_texpath = texpath
END SUB

' Load texture file with cache memory (once oly)
FUNCTION ReadTextureOnce(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR, buffer AS INTEGER) AS INTEGER

  'Variables
  DIM AS INTEGER RetCode
  
  'Check if texture was accessed just before
  IF TexDir = aTxRec(buffer).TexDir AND TexName = aTxRec(buffer).TexName THEN
    ax = aTxRec(buffer).Ax
    ay = aTxRec(buffer).Ay
    ReadTextureOnce = 0
    EXIT FUNCTION
  
  'Texture was not accessed just before: call normal function
  ELSE
    RetCode = ReadTexture( TexDir, TexName, ax, ay, ipt )
    IF RetCode = 0 THEN
      aTxRec(buffer).TexDir = TexDir
      aTxRec(buffer).TexName = TexName
      aTxRec(buffer).Ax = ax
      aTxRec(buffer).Ay = ay
      ReadTextureOnce = 0
    ELSE
      ReadTextureOnce = 1
    ENDIF
  ENDIF

END FUNCTION

' Read texture (pointer version)
FUNCTION ReadTexture(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR) AS INTEGER

  'Variables
  DIM AS INTEGER i,j,found,index
  DIM AS INTEGER low,high,middle
  DIM AS INTEGER tax, tay, tax2
  DIM AS LONG start,length
  DIM AS PReg col0,col1,col2,col3,col4
  DIM AS STRING filename
  DIM AS STRING * 20 value
  DIM AS INTEGER numcol
  DIM AS TxRecord TxRec0
  
  'Search texture in cache memory (Binary search)
  IF BINSEARCH = 1 THEN
    IF txl_texpath = "" THEN 
      value = STR$(TexDir) + TexName
    ELSE
      value = STR$(0) + TexName
    ENDIF
    low = 0
    high = TxNum - 1
    found = 0
    WHILE low <= high
      middle = (low + high) / 2
      IF TxRec(middle).Sort > value THEN
        high = middle - 1
      ELSEIF TxRec(middle).Sort < value THEN
        low = middle + 1
      ELSE
        found = 1
        index = middle
        EXIT WHILE
      ENDIF
    WEND
  
  'Search texture in cache memory (Sequential search)
  ELSE
    found = 0
    FOR i = 0 TO TxNum - 1
      IF ( txl_texpath =  "" AND TxRec(i).TexName = TexName AND TxRec(i).TexDir = TexDir ) _
      OR ( txl_texpath <> "" AND TxRec(i).TexName = TexName AND TxRec(i).texpath = txl_texpath ) THEN
        found = 1
        index = i
        EXIT FOR
      ENDIF
    NEXT i
  ENDIF
  
  'Return image data from cache memory
  IF found = 1 THEN
    
    'Return data
    start = TxRec(index).start
    ipt = @TextureMem(start)
    ax = TxRec(index).ax
    ay = TxRec(index).ay
    
    'Return code
    ReadTexture = 0
   
  'Load new texture file & save to cache memory
  ELSE
    
    'Texture filename
    IF txl_texpath = "" THEN
      filename = GetTexPath(TexDir) + TexName
    ELSE
      filename = txl_texpath + TexName
    ENDIF

    'Load image data (normal images)
    IF ( txl_factor = 0 AND LCASE(RIGHT$(TexName,4)) = ".pcx" ) OR txl_factor = 1 THEN
      PcxLoad filename, tax, tay, imgpx1(), TEXTUREAX, TEXTUREAY, TRANCOL08
    
    'Load image data (double size images)
    ELSEIF ( txl_factor = 0 AND LCASE(RIGHT$(TexName,4)) = ".px2" ) OR txl_factor = 2 THEN
      PcxLoad filename, tax, tay, imgpx2(), TEXTUREAX*2, TEXTUREAY*2, TRANCOL08
      tax2 = tax / 2
      FOR i=0 TO tax - 1 STEP 2
      FOR j=0 TO tay - 1 STEP 2
        numcol = 0
        col1.r=0: col1.g=0: col1.b=0: col2.r=0: col2.g=0: col2.b=0
        col3.r=0: col3.g=0: col3.b=0: col4.r=0: col4.g=0: col4.b=0
        index = (i+0) + tax*(j+0)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col1.r=RGB_R(imgpx2(index)):col1.g=RGB_G(imgpx2(index)):col1.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+1) + tax*(j+0)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col2.r=RGB_R(imgpx2(index)):col2.g=RGB_G(imgpx2(index)):col2.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+1) + tax*(j+1)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col3.r=RGB_R(imgpx2(index)):col3.g=RGB_G(imgpx2(index)):col3.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+0) + tax*(j+1)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col4.r=RGB_R(imgpx2(index)):col4.g=RGB_G(imgpx2(index)):col4.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        IF numcol <> 0 THEN
          col0.r = (col1.r + col2.r + col3.r + col4.r) / numcol
          col0.g = (col1.g + col2.g + col3.g + col4.g) / numcol
          col0.b = (col1.b + col2.b + col3.b + col4.b) / numcol
          imgpx1((i/2)+tax2*(j/2)) = RGB(col0.r,col0.g,col0.b)
        ELSE
          imgpx1((i/2)+tax2*(j/2)) = TRANCOLHI
        ENDIF
      NEXT j
      NEXT i
      tax = tax / 2
      tay = tay / 2
    ENDIF
    
    'Return texture size
    ax = tax
    ay = tay
    
    'Check that there are free records to store the new image
    IF TxNum >= MAXTEXTUR THEN
      ReadTexture = 1
      EXIT FUNCTION
    ENDIF
    index = TxNum
    
    'Check if there is enough memory
    IF TxPtr+(tax*tay) >= TEXTURMEM THEN
      ReadTexture = 1
      EXIT FUNCTION
    ENDIF
    
    'Save image data
    start = TxPtr
    length = tax * tay
    FOR i = 0 TO length - 1
      TextureMem(start + i) = imgpx1(i)
    NEXT i
    
    'Return image pointer
    ipt = @TextureMem(start)
    
    'Save texture record as used
    IF txl_texpath = "" THEN
      TxRec0.TexDir = TexDir
      TxRec0.TexPath = ""
    ELSE
      TxRec0.TexDir = 0
      TxRec0.TexPath = txl_texpath
    ENDIF
    TxRec0.TexName = TexName
    TxRec0.Ax = tax
    TxRec0.Ay = tay
    TxRec0.Start = start
    TxRec0.Length = length
    TxRec0.Sort = STR$(TxRec0.TexDir) + TexName
    
    'Insert texture record
    InsertTexRecord(TxRec0)
    
    'Modify Memory pointer and texture counter
    TxNum = TxNum + 1
    TxPtr = TxPtr + length

    'Return code
    ReadTexture = 0

  ENDIF

END FUNCTION

'Insert texture record with sorting
SUB InsertTexRecord(TxRec0 AS TxRecord)

  'Variables
  DIM AS INTEGER i

  'Insert new record
  IF TxNum = 0 THEN
    TxRec(0) = TxRec0
  ELSE
    i = TxNum - 1
    DO
      IF i < 0 THEN EXIT DO
      IF TxRec(i).Sort <= TxRec0.Sort THEN EXIT DO
      TxRec(i+1) = TxRec(i)
      i = i - 1
    LOOP
    TxRec(i+1) = TxRec0
  ENDIF

END SUB

' Kill Texture memory
SUB KillTexMemory

  'Variables
  DIM AS INTEGER i
  
  'Initialize all records
  FOR i = 0 TO MAXTEXTUR - 1
    TxRec(i).TexDir = 0
    TxRec(i).TexPath = ""
    TxRec(i).TexName = ""
    TxRec(i).Ax = 0
    TxRec(i).Ay = 0
    TxRec(i).Start = 0
    TxRec(i).Length = 0
    TxRec(i).Sort = ""
  NEXT i
  
  'Initialize previous accessed textures
  FOR i=0 TO 9
    aTxRec(i).TexDir = 0
    aTxRec(i).TexPath = ""
    aTxRec(i).TexName = ""
    aTxRec(i).Ax = 0
    aTxRec(i).Ay = 0
    aTxRec(i).Start = 0
    aTxRec(i).Length = 0
    aTxRec(i).Sort = ""
  NEXT i
    
  'Reset texture counter and memory pointer
  TxNum = 0
  TxPtr = 0
  
END SUB

'*** End of texlib.bas ***


'*** Begin of pcxlib.bi ***

'Constants
CONST TRANCOL08 = 247
CONST TRANCOLHI = &HFF00FF

' Palete register
TYPE PReg
  r AS INTEGER
  g AS INTEGER
  b AS INTEGER
END TYPE

'Functions
DECLARE SUB SetPalette(pal() AS PReg)
DECLARE SUB ImgDisplay(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, ipt AS UINTEGER PTR, mirror AS INTEGER)
DECLARE SUB ImgDisplayClip(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, ipt AS UINTEGER PTR, mirror AS INTEGER, sax AS INTEGER, say AS INTEGER)
DECLARE SUB ImgDisplayMod(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, ipt AS UINTEGER PTR, mirror AS INTEGER, modcol AS UINTEGER)
DECLARE SUB PcxDump(filename AS STRING, x AS INTEGER, y AS INTEGER)
DECLARE SUB PcxLoad(filename AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, img() AS UINTEGER, xlim AS INTEGER, ylim AS INTEGER, trans AS INTEGER)
DECLARE SUB SetBackground(fname AS STRING)

'Get RGB components from colors
#define RGB_R(x) ((x) Shr 16 And 255)
#define RGB_G(x) ((x) Shr 8 And 255)
#define RGB_B(x) ((x) And 255)

'*** End of pcxlib.bi ***


'*** Begin of pcxlib.bas ***

#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "vbcompat.bi"

'Image display in High color modes
SUB ImgDisplay(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, ipt AS UINTEGER PTR, mirror AS INTEGER)
  
  'Variables
  DIM AS INTEGER i, j
  DIM AS UINTEGER pxcol
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    pxcol = ipt[(ax * j) + i]
    IF pxcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        PSET (x0 + i, y0 + j), pxcol
      ELSE
        PSET (x0 + ax - 1 - i, y0 + j), pxcol
      ENDIF
    ENDIF
  NEXT j
  NEXT i
  
END SUB

'Image display in High color modes with size limit
SUB ImgDisplayClip(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
                   ipt AS UINTEGER PTR, mirror AS INTEGER, sax AS INTEGER, say AS INTEGER)
  
  'Variables
  DIM AS INTEGER i, j
  DIM AS UINTEGER pxcol
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    pxcol = ipt[(ax * j) + i]
    IF pxcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        IF i < sax AND j < say THEN PSET (x0 + i, y0 + j), pxcol
      ELSE
        IF ax - 1 - i < sax AND j < say THEN PSET (x0 + ax - 1 - i, y0 + j), pxcol
      ENDIF
    ENDIF
  NEXT j
  NEXT i
  
END SUB

'Image display in High color modes with color modulation
SUB ImgDisplayMod(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
                  ipt AS UINTEGER PTR, mirror AS INTEGER, modcol AS UINTEGER)
  
  'Color modulation formula
  #MACRO _Modulate(imgcolor,scrcolor,modcolor,result)
    result = (((modcolor)-(scrcolor))*(imgcolor))/255+(scrcolor)
  #ENDMACRO
  
  'Variables
  DIM AS INTEGER i, j, px, py
  DIM AS UINTEGER imgcol,scrcol
  DIM AS UINTEGER r,g,b
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    imgcol = ipt[(ax * j) + i]
    IF imgcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        px = x0 + i: py = y0 + j
      ELSE
        px = x0 + ax - 1 - i: py = y0 + j
      ENDIF
      scrcol = POINT(px,py)
      _Modulate(RGB_R(imgcol),RGB_R(scrcol),RGB_R(modcol),r)
      _Modulate(RGB_G(imgcol),RGB_G(scrcol),RGB_G(modcol),g)
      _Modulate(RGB_B(imgcol),RGB_B(scrcol),RGB_B(modcol),b)
      PSET (px,py), RGB(r,g,b)
    ENDIF
  NEXT j
  NEXT i
  
END SUB

' Load PCX file into memory buffer (for High color modes)                               
SUB PcxLoad (filename AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, _
             img() AS UINTEGER, xlim AS INTEGER, ylim AS INTEGER, trans AS INTEGER)
                                                                            
  'Variables                                                     
  DIM AS INTEGER file                  'File
  DIM AS INTEGER i                     'Counter
  DIM AS INTEGER asci                  'ASCII value
  DIM AS LONG Size, l, reps, count     'Algorithm variables                    
  DIM char0 AS STRING * 1              'Byte                               
  DIM char1 AS STRING * 1              'Byte
  DIM AS INTEGER xmin, xmax            'Image size
  DIM AS INTEGER ymin, ymax            'Image size
  DIM AS INTEGER px, py                'Pixel Coodinates
  DIM AS PReg pal(256)                 'Palette
  DIM AS LONG index                    'Buffer index
  DIM AS UINTEGER pxcol                 'Pixel color
  
  'Error handling
  ON LOCAL ERROR GOTO errhandler
  
  'Open file             
  file = FREEFILE
  OPEN filename FOR BINARY AS #file
                                                                           
  'Calculate image size
  SEEK #file, 5
  GET #file,,char0:GET #file,,char1:xmin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:xmax=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymax=ASC(char0) + 256 * ASC(char1)
  ax = xmax - xmin + 1
  ay = ymax - ymin + 1
  Size = ax * ay
  
  'Read palette                                                  
  SEEK #file, LOF(file) - 767
  FOR i = 0 TO 255
    GET #file, , char0: pal(i).r = ASC(char0)
    GET #file, , char0: pal(i).g = ASC(char0)
    GET #file, , char0: pal(i).b = ASC(char0)
  NEXT i
  
  'Decoding loop                                                 
  px=0:py=0:index=0
  SEEK #file, 129
  DO WHILE count < Size
    GET #file, , char0
    asci = ASC(char0)
    IF asci >= 192 THEN
      reps = asci AND &H3F
      GET #file, , char0
    ELSE
      reps = 1
    ENDIF
    FOR l = 0 TO reps - 1
      IF trans <> -1 AND ASC(char0) = trans THEN
        pxcol = TRANCOLHI
      ELSE
        pxcol = RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
      ENDIF
      IF px < xlim AND py < ylim THEN
        img(index) = pxcol: index = index + 1
      ENDIF
      px = px + 1
      IF px = ax THEN: px = 0: py = py + 1: ENDIF
    NEXT l
    count = count + reps
  LOOP

  'Check file size limit
  IF ax > xlim OR ay > xlim THEN
    IF ax > xlim THEN ax = xlim
    IF ay > ylim THEN ay = ylim
  ENDIF

  'Close file & exit
  CLOSE #file
  EXIT SUB
  
  'Error handler
  errhandler:
  ax = 0
  ay = 0
  CLOSE #file
  EXIT SUB
  
END SUB

' Load PCX file and dump to screen (for High color modes)                               
SUB PcxDump (filename AS STRING, x AS INTEGER, y AS INTEGER)
                                                                            
  'Variables                                                     
  DIM AS INTEGER file
  DIM AS INTEGER i                 'Counter
  DIM AS INTEGER asci              'ASCII value
  DIM AS LONG Size, l, reps, count 'Algorithm variables                    
  DIM char0 AS STRING * 1          'Byte                               
  DIM char1 AS STRING * 1          'Byte
  DIM AS INTEGER xmin, xmax        'Image size
  DIM AS INTEGER ymin, ymax        'Image size
  DIM AS PReg pal(256)             'Palette
  DIM AS INTEGER ax, ay            'Image size
  DIM AS INTEGER px, py            'Screen coordinates
  DIM AS INTEGER errcode           'Error code
  
  'Error handling
  ON LOCAL ERROR GOTO errhandler

  'Open file             
  file = FREEFILE
  errcode = OPEN( filename FOR BINARY AS #file)
  IF errcode <> 0 THEN
    EXIT SUB
  ENDIF
                                                                           
  'Read header
  'GET #file, , pcxhead
  
  'Calculate image size
  SEEK #file, 5
  GET #file,,char0:GET #file,,char1:xmin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:xmax=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymax=ASC(char0) + 256 * ASC(char1)
  ax = xmax - xmin + 1
  ay = ymax - ymin + 1
  Size = ax * ay
  
  'Read palette                                                  
  SEEK #file, LOF(file) - 767
  FOR i = 0 TO 255
    GET #file, , char0: pal(i).r = ASC(char0)
    GET #file, , char0: pal(i).g = ASC(char0)
    GET #file, , char0: pal(i).b = ASC(char0)
  NEXT i%
                                                                           
  'Decoding loop                                                 
  px = x: py = y
  SEEK #file, 129
  DO WHILE count < Size
    GET #file, , char0
    asci = ASC(char0)
    IF asci >= 192 THEN
      reps = asci AND &H3F
      GET #file, , char0
      FOR l = 0 TO reps - 1
        PSET (px,py), RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
        px = px + 1
        if px = x + ax then 
          px = x
          py = py + 1
        endif
      NEXT l
      count = count + reps
    ELSE
      PSET (px,py), RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
      px = px + 1
      if px = x + ax then 
        px = x
        py = py + 1
      endif
      count = count + 1
    END IF
  LOOP

  'Close file
  CLOSE #file
  EXIT SUB
  
  'Error handler
  errhandler:
  EXIT SUB
  
END SUB

' Set palette
SUB SetPalette (pal() AS PReg)

  ' Variables
  DIM AS INTEGER i

' Palette set loop
  OUT &H3C8, 0
  FOR i = 0 TO 255
     OUT &H3C9, pal(i).r
     OUT &H3C9, pal(i).g
     OUT &H3C9, pal(i).b
  NEXT i

END SUB

'Set background image
SUB SetBackground(fname AS STRING)
  
  'Variables
  REDIM AS UINTEGER img(AX0*AY0)
  DIM AS INTEGER x,y,ax,ay
  
  'Load image file
  PcxLoad fname,ax,ay,img(),AX0,AY0,-1
  IF ax = 0 OR ay = 0 THEN EXIT SUB
  
  'Print loop
  x = 0: y = 0
  DO
    ImgDisplay x,y,ax,ay,@img(0),0
    x = x + ax
    IF x >= AX0 THEN
      x = 0
      y = y + ay
      IF y >= AY0 THEN EXIT DO
    ENDIF
  LOOP

END SUB


'*** End of pcxlib.bas ***


'*** Begin of conlib.bi ***

'Pictogram aliases
CONST PIC_MODE_MAP   = 0
CONST PIC_MODE_CELL  = 1
CONST PIC_MODE_ATTR  = 2
CONST PIC_MODE_OVL   = 3
CONST PIC_MODE_LIGHT = 4
CONST PIC_MODE_LNK   = 5
CONST PIC_MODE_EVT   = 6
CONST PIC_MODE_NAV   = 7
CONST PIC_MODE_SND   = 8

'Get string exit modes
CONST GSEXIT_NORMAL = 1
CONST GSEXIT_UPDOWN = 2

'Constants
CONST MAXPCXFN = 2

'Pictogram 11x15 definition
TYPE pictogram11x15
  row(15) AS SHORT
END TYPE

'PCX font definition
TYPE pcxfdef
  ax AS INTEGER
  ay AS INTEGER
  char(256) AS UINTEGER PTR
  chmin(256) AS INTEGER
  chmax(256) AS INTEGER
  img(200000) AS UINTEGER
END TYPE

' Subroutines
DECLARE SUB InitFonts ()
DECLARE SUB SetFont (font AS INTEGER)
DECLARE SUB PrintChr (x AS INTEGER, y AS INTEGER, char as STRING, col AS UINTEGER)
DECLARE SUB PrintStr (x AS INTEGER, y AS INTEGER, strn as STRING, col AS UINTEGER)
DECLARE SUB PrintStrBkg (x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, cur AS INTEGER, bkg AS INTEGER)
DECLARE SUB PrintStrBCol(x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, bcol AS UINTEGER)
DECLARE FUNCTION FCol(col AS INTEGER) AS INTEGER
DECLARE FUNCTION FRow(row AS INTEGER) AS INTEGER
DECLARE SUB InitPictograms
DECLARE SUB SetPictogramFont (pic AS INTEGER)
DECLARE SUB PrintPictogram (x AS INTEGER, y AS INTEGER, pic AS INTEGER, col AS UINTEGER)
DECLARE FUNCTION PCol(col AS INTEGER) AS INTEGER
DECLARE FUNCTION PRow(row AS INTEGER) AS INTEGER
DECLARE SUB SetKeyb(keyb AS STRING)
DECLARE FUNCTION GetKeyb(waitmode AS INTEGER) AS INTEGER
DECLARE SUB GetStringExitMode(mode AS INTEGER)
DECLARE FUNCTION GetStringBCol(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, bcol AS UINTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
DECLARE FUNCTION GetStringBkg(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, bkg AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
DECLARE FUNCTION GetString(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, bcol AS UINTEGER, bkg AS INTEGER, mode AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
DECLARE SUB InitPcxFonts
DECLARE SUB SetPcxFont (pic AS INTEGER)
DECLARE FUNCTION XCol(col AS INTEGER) AS INTEGER
DECLARE FUNCTION XRow(row AS INTEGER) AS INTEGER
DECLARE SUB PCXFLoad(filename AS STRING, ax AS INTEGER, ay AS INTEGER, tart AS INTEGER, num AS INTEGER, handle AS INTEGER)
DECLARE SUB PCXFSetColor(col AS UINTEGER)
DECLARE SUB PCXFCentered(center AS INTEGER)
DECLARE SUB PCXFPrintChr(px AS INTEGER,py AS INTEGER,char AS INTEGER)
DECLARE SUB PCXFPrintStrMS(px AS INTEGER, py AS INTEGER, strn AS STRING)
DECLARE SUB PCXFPrintStrVS(px AS INTEGER, py AS INTEGER, strn AS STRING)

'*** End of conlib.bi ***


'*** Begin of conlib.bas ***

#INCLUDE "conlib.bi"
#INCLUDE "texlib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"

'Global variables (fonts)
DIM SHARED curfont AS INTEGER 'Current font
DIM SHARED fontax AS INTEGER  'Current font size x 
DIM SHARED fontay AS INTEGER  'Current font size y
DIM SHARED spacex AS INTEGER  'Current font spacing x
DIM SHARED spacey AS INTEGER  'Current font spacing y

'Global variables (pictograms)
DIM SHARED curpic AS INTEGER 'Current font
DIM SHARED picax AS INTEGER  'Current font size x 
DIM SHARED picay AS INTEGER  'Current font size y
DIM SHARED picspx AS INTEGER 'Current font spacing x
DIM SHARED picspy AS INTEGER 'Current font spacing y

'Global variables (PCX fonts)
DIM SHARED curpcx AS INTEGER    'Current font
DIM SHARED pcxax AS INTEGER     'Current font size x 
DIM SHARED pcxay AS INTEGER     'Current font size y
DIM SHARED pcxspx AS INTEGER    'Current font spacing x
DIM SHARED pcxspy AS INTEGER    'Current font spacing y
DIM SHARED pcxvsp AS INTEGER    'Spacing for variable space font
DIM SHARED pcxcenter AS INTEGER 'Center attribute

' ASCII character definition table
DIM SHARED asctable3x5(128) AS LONG
DIM SHARED asctable5x6(128) AS LONG

'Pictogram table
DIM SHARED pict11x15(8) AS pictogram11x15

'Keyboard buffer for manual input
DIM SHARED kbuffptr AS INTEGER = 0
DIM SHARED kbuffer(16) AS STRING * 2

'Get string exit mode
DIM SHARED gsexitmode AS INTEGER = GSEXIT_NORMAL
DIM SHARED gscurpos   AS INTEGER = 0

'PCX fonts
REDIM SHARED pcxfont(MAXPCXFN) AS pcxfdef
DIM SHARED modcol AS UINTEGER = 0

'ASCII table initialization
SUB InitFonts

  '3x5 font definition
  asctable3x5(0) = 0       ' NUL
  asctable3x5(1) = 3960    ' SOH
  asctable3x5(2) = 3960    ' STX
  asctable3x5(3) = 3960    ' ETX
  asctable3x5(4) = 3960    ' EOT
  asctable3x5(5) = 3960    ' ENQ
  asctable3x5(6) = 3960    ' ACK
  asctable3x5(7) = 3960    ' BEL
  asctable3x5(8) = 3960    ' BS
  asctable3x5(9) = 3960    ' TAB
  asctable3x5(10) = 3960   ' LF
  asctable3x5(11) = 3960   ' VT
  asctable3x5(12) = 3960   ' FF
  asctable3x5(13) = 3960   ' CR
  asctable3x5(14) = 3960   ' SO
  asctable3x5(15) = 3960   ' SI
  asctable3x5(16) = 3960   ' DEL
  asctable3x5(17) = 3960   ' XON
  asctable3x5(18) = 3960   ' DC2
  asctable3x5(19) = 3960   ' XOFF
  asctable3x5(20) = 3960   ' DC4
  asctable3x5(21) = 3960   ' NAK
  asctable3x5(22) = 3960   ' SYN
  asctable3x5(23) = 3960   ' ETB
  asctable3x5(24) = 3960   ' CAN
  asctable3x5(25) = 3960   ' EM
  asctable3x5(26) = 3960   ' SUB
  asctable3x5(27) = 3960   ' ESC
  asctable3x5(28) = 3960   ' FS
  asctable3x5(29) = 3960   ' GS
  asctable3x5(30) = 3960   ' RS
  asctable3x5(31) = 3960   ' US
  asctable3x5(32) = 0      ' SP
  asctable3x5(33) = 8338   ' !
  asctable3x5(34) = 45     ' "
  asctable3x5(35) = 24573  ' #
  asctable3x5(36) = 32223  ' $
  asctable3x5(37) = 21157  ' %
  asctable3x5(38) = 27306  ' &
  asctable3x5(39) = 18     ' '
  asctable3x5(40) = 17556  ' (
  asctable3x5(41) = 5265   ' )
  asctable3x5(42) = 2728   ' *
  asctable3x5(43) = 1488   ' +
  asctable3x5(44) = 10240  ' ,
  asctable3x5(45) = 448    ' -
  asctable3x5(46) = 08192  ' .
  asctable3x5(47) = 4772   ' /
  asctable3x5(48) = 11114  ' 0
  asctable3x5(49) = 29850  ' 1
  asctable3x5(50) = 29671  ' 2
  asctable3x5(51) = 31207  ' 3
  asctable3x5(52) = 18925  ' 4
  asctable3x5(53) = 31183  ' 5
  asctable3x5(54) = 31695  ' 6
  asctable3x5(55) = 18727  ' 7
  asctable3x5(56) = 31727  ' 8
  asctable3x5(57) = 31215  ' 9
  asctable3x5(58) = 1040   ' :
  asctable3x5(59) = 9232   ' ;
  asctable3x5(60) = 17492  ' <
  asctable3x5(61) = 3640   ' =
  asctable3x5(62) = 5393   ' >
  asctable3x5(63) = 8359   ' ?
  asctable3x5(64) = 3960   ' @
  asctable3x5(65) = 24431  ' A
  asctable3x5(66) = 15083  ' B
  asctable3x5(67) = 29263  ' C
  asctable3x5(68) = 15211  ' D
  asctable3x5(69) = 29647  ' E
  asctable3x5(70) = 5071   ' F
  asctable3x5(71) = 31311  ' G
  asctable3x5(72) = 23533  ' H
  asctable3x5(73) = 9362   ' I
  asctable3x5(74) = 31524  ' J
  asctable3x5(75) = 23277  ' K
  asctable3x5(76) = 29257  ' L
  asctable3x5(77) = 23551  ' M
  asctable3x5(78) = 23407  ' N
  asctable3x5(79) = 31599  ' O
  asctable3x5(80) = 5103   ' P
  asctable3x5(81) = 18287  ' Q
  asctable3x5(82) = 23279  ' R
  asctable3x5(83) = 31183  ' S
  asctable3x5(84) = 9367   ' T
  asctable3x5(85) = 31597  ' U
  asctable3x5(86) = 11117  ' V
  asctable3x5(87) = 32621  ' W
  asctable3x5(88) = 23213  ' X
  asctable3x5(89) = 9709   ' Y
  asctable3x5(90) = 29351  ' Z
  asctable3x5(91) = 25750  ' [
  asctable3x5(92) = 17553  ' \
  asctable3x5(93) = 26918  ' ]
  asctable3x5(94) = 42     ' ^
  asctable3x5(95) = 28672  ' _
  asctable3x5(96) = 17     ' `
  asctable3x5(97) = 31168  ' a
  asctable3x5(98) = 31689  ' b
  asctable3x5(99) = 29632  ' c
  asctable3x5(100) = 31716 ' d
  asctable3x5(101) = 30656 ' e
  asctable3x5(102) = 9686  ' f
  asctable3x5(103) = 15744 ' g
  asctable3x5(104) = 23497 ' h
  asctable3x5(105) = 9346  ' i
  asctable3x5(106) = 31492 ' j
  asctable3x5(107) = 22344 ' k
  asctable3x5(108) = 25746 ' l
  asctable3x5(109) = 24512 ' m
  asctable3x5(110) = 23488 ' n
  asctable3x5(111) = 31680 ' o
  asctable3x5(112) = 8128  ' p
  asctable3x5(113) = 20416 ' q
  asctable3x5(114) = 5056  ' r
  asctable3x5(115) = 31104 ' s
  asctable3x5(116) = 25786 ' t
  asctable3x5(117) = 31552 ' u
  asctable3x5(118) = 11072 ' v
  asctable3x5(119) = 32576 ' w
  asctable3x5(120) = 21824 ' x
  asctable3x5(121) = 9536  ' y
  asctable3x5(122) = 29496 ' z
  asctable3x5(123) = 25814 ' {
  asctable3x5(124) = 9362  ' |
  asctable3x5(125) = 13715 ' }
  asctable3x5(126) = 408   ' ~
  asctable3x5(127) = 3960  ' 

  '5x6 Font definition
  asctable5x6(000) = 0000000000 ' NUL
  asctable5x6(001) = 0033081312 ' SOH
  asctable5x6(002) = 0033081312 ' STX
  asctable5x6(003) = 0033081312 ' ETX
  asctable5x6(004) = 0033081312 ' EOT
  asctable5x6(005) = 0033081312 ' ENQ
  asctable5x6(006) = 0033081312 ' ACK
  asctable5x6(007) = 0033081312 ' BEL
  asctable5x6(008) = 0033081312 ' BS
  asctable5x6(009) = 0033081312 ' TAB
  asctable5x6(010) = 0033081312 ' LF
  asctable5x6(011) = 0033081312 ' VT
  asctable5x6(012) = 0033081312 ' FF
  asctable5x6(013) = 0033081312 ' CR
  asctable5x6(014) = 0033081312 ' SO
  asctable5x6(015) = 0033081312 ' SI
  asctable5x6(016) = 0033081312 ' DEL
  asctable5x6(017) = 0033081312 ' XON
  asctable5x6(018) = 0033081312 ' DC2
  asctable5x6(019) = 0033081312 ' XOFF
  asctable5x6(020) = 0033081312 ' DC4
  asctable5x6(021) = 0033081312 ' NAK
  asctable5x6(022) = 0033081312 ' SYN
  asctable5x6(023) = 0033081312 ' ETB
  asctable5x6(024) = 0033081312 ' CAN
  asctable5x6(025) = 0033081312 ' EM
  asctable5x6(026) = 0033081312 ' SUB
  asctable5x6(027) = 0033081312 ' ESC
  asctable5x6(028) = 0033081312 ' FS
  asctable5x6(029) = 0033081312 ' GS
  asctable5x6(030) = 0033081312 ' RS
  asctable5x6(031) = 0033081312 ' US
  asctable5x6(032) = 0000000000 ' SP
  asctable5x6(033) = 0134353028 ' !
  asctable5x6(034) = 0000000330 ' "
  asctable5x6(035) = 0368389098 ' #
  asctable5x6(036) = 1062180031 ' $
  asctable5x6(037) = 0572657937 ' %
  asctable5x6(038) = 0748365094 ' &
  asctable5x6(039) = 0000000132 ' '
  asctable5x6(040) = 0272765064 ' (
  asctable5x6(041) = 0071438466 ' )
  asctable5x6(042) = 0921697755 ' *
  asctable5x6(043) = 0139432064 ' +
  asctable5x6(044) = 0138412032 ' ,
  asctable5x6(045) = 0001015808 ' -
  asctable5x6(046) = 0134217728 ' .
  asctable5x6(047) = 0035787024 ' /
  asctable5x6(048) = 0488162862 ' 0
  asctable5x6(049) = 0474091716 ' 1
  asctable5x6(050) = 1008869964 ' 2
  asctable5x6(051) = 0488534574 ' 3
  asctable5x6(052) = 0277850504 ' 4
  asctable5x6(053) = 0520553564 ' 5
  asctable5x6(054) = 0488223806 ' 6
  asctable5x6(055) = 0035791391 ' 7
  asctable5x6(056) = 0488064558 ' 8
  asctable5x6(057) = 0487540270 ' 9
  asctable5x6(058) = 0004194432 ' :
  asctable5x6(059) = 0138412160 ' ;
  asctable5x6(060) = 0136349824 ' <
  asctable5x6(061) = 0032537600 ' =
  asctable5x6(062) = 0143138944 ' >
  asctable5x6(063) = 0134361646 ' ?
  asctable5x6(064) = 1008391726 ' @
  asctable5x6(065) = 0589284927 ' A
  asctable5x6(066) = 0521651759 ' B
  asctable5x6(067) = 0487622190 ' C
  asctable5x6(068) = 0521717295 ' D
  asctable5x6(069) = 1042252863 ' E
  asctable5x6(070) = 0035619903 ' F
  asctable5x6(071) = 1058965055 ' G
  asctable5x6(072) = 0589284913 ' H
  asctable5x6(073) = 0474091662 ' I
  asctable5x6(074) = 1058554384 ' J
  asctable5x6(075) = 0307334313 ' K
  asctable5x6(076) = 1041269793 ' L
  asctable5x6(077) = 0588972027 ' M
  asctable5x6(078) = 0597481139 ' N
  asctable5x6(079) = 1058588223 ' O
  asctable5x6(080) = 0035112495 ' P
  asctable5x6(081) = 0552125998 ' Q
  asctable5x6(082) = 0307742255 ' R
  asctable5x6(083) = 0520554030 ' S
  asctable5x6(084) = 0138547359 ' T
  asctable5x6(085) = 0488162865 ' U
  asctable5x6(086) = 0145041969 ' V
  asctable5x6(087) = 0358274737 ' W
  asctable5x6(088) = 0581046609 ' X
  asctable5x6(089) = 0138547537 ' Y
  asctable5x6(090) = 1041441311 ' Z
  asctable5x6(091) = 0943853724 ' [
  asctable5x6(092) = 0545394753 ' \
  asctable5x6(093) = 0239210631 ' ]
  asctable5x6(094) = 0000017732 ' ^
  asctable5x6(095) = 1040187392 ' _
  asctable5x6(096) = 0000000130 ' `
  asctable5x6(097) = 0211230912 ' a
  asctable5x6(098) = 0244620320 ' b
  asctable5x6(099) = 0470844864 ' c
  asctable5x6(100) = 0479508736 ' d
  asctable5x6(101) = 0471311552 ' e
  asctable5x6(102) = 0069437824 ' f
  asctable5x6(103) = 0210183360 ' g
  asctable5x6(104) = 0311657504 ' h
  asctable5x6(105) = 0138543232 ' i
  asctable5x6(106) = 0104988800 ' j
  asctable5x6(107) = 0439526432 ' k
  asctable5x6(108) = 0237045856 ' l
  asctable5x6(109) = 0727373120 ' m
  asctable5x6(110) = 0311731392 ' n
  asctable5x6(111) = 0488162752 ' o
  asctable5x6(112) = 0034841824 ' p
  asctable5x6(113) = 0277292480 ' q
  asctable5x6(114) = 0034639264 ' r
  asctable5x6(115) = 0243467712 ' s
  asctable5x6(116) = 0203496512 ' t
  asctable5x6(117) = 0211068192 ' u
  asctable5x6(118) = 0206742816 ' v
  asctable5x6(119) = 0358270496 ' w
  asctable5x6(120) = 0308487456 ' x
  asctable5x6(121) = 0104999200 ' y
  asctable5x6(122) = 0505553376 ' z
  asctable5x6(123) = 0136416324 ' {
  asctable5x6(124) = 0138547332 ' |
  asctable5x6(125) = 0143417604 ' }
  asctable5x6(126) = 0000317440 ' ~
  asctable5x6(127) = 0015728064 '

END SUB

' Pictogram table initialization
SUB InitPictograms

  'Mode map
  pict11x15(PIC_MODE_MAP).row(00) = 0000
  pict11x15(PIC_MODE_MAP).row(01) = 0000
  pict11x15(PIC_MODE_MAP).row(02) = 0032
  pict11x15(PIC_MODE_MAP).row(03) = 0080
  pict11x15(PIC_MODE_MAP).row(04) = 0136
  pict11x15(PIC_MODE_MAP).row(05) = 0260
  pict11x15(PIC_MODE_MAP).row(06) = 0514
  pict11x15(PIC_MODE_MAP).row(07) = 1025
  pict11x15(PIC_MODE_MAP).row(08) = 0514
  pict11x15(PIC_MODE_MAP).row(09) = 0260
  pict11x15(PIC_MODE_MAP).row(10) = 0136
  pict11x15(PIC_MODE_MAP).row(11) = 0080
  pict11x15(PIC_MODE_MAP).row(12) = 0032
  pict11x15(PIC_MODE_MAP).row(13) = 0000
  pict11x15(PIC_MODE_MAP).row(14) = 0000
  
  'Mode cells
  pict11x15(PIC_MODE_CELL).row(00) = 0016
  pict11x15(PIC_MODE_CELL).row(01) = 0040
  pict11x15(PIC_MODE_CELL).row(02) = 0068
  pict11x15(PIC_MODE_CELL).row(03) = 0108
  pict11x15(PIC_MODE_CELL).row(04) = 0340
  pict11x15(PIC_MODE_CELL).row(05) = 0724
  pict11x15(PIC_MODE_CELL).row(06) = 1108
  pict11x15(PIC_MODE_CELL).row(07) = 1748
  pict11x15(PIC_MODE_CELL).row(08) = 1370
  pict11x15(PIC_MODE_CELL).row(09) = 1361
  pict11x15(PIC_MODE_CELL).row(10) = 1371
  pict11x15(PIC_MODE_CELL).row(11) = 1365
  pict11x15(PIC_MODE_CELL).row(12) = 0949
  pict11x15(PIC_MODE_CELL).row(13) = 0286
  pict11x15(PIC_MODE_CELL).row(14) = 0004
  
  'Mode attributes
  pict11x15(PIC_MODE_ATTR).row(00) = 0000
  pict11x15(PIC_MODE_ATTR).row(01) = 0066
  pict11x15(PIC_MODE_ATTR).row(02) = 0036
  pict11x15(PIC_MODE_ATTR).row(03) = 0024
  pict11x15(PIC_MODE_ATTR).row(04) = 0024
  pict11x15(PIC_MODE_ATTR).row(05) = 0036
  pict11x15(PIC_MODE_ATTR).row(06) = 0066
  pict11x15(PIC_MODE_ATTR).row(07) = 0000
  pict11x15(PIC_MODE_ATTR).row(08) = 0256
  pict11x15(PIC_MODE_ATTR).row(09) = 0128
  pict11x15(PIC_MODE_ATTR).row(10) = 0064
  pict11x15(PIC_MODE_ATTR).row(11) = 0034
  pict11x15(PIC_MODE_ATTR).row(12) = 0020
  pict11x15(PIC_MODE_ATTR).row(13) = 0008
  pict11x15(PIC_MODE_ATTR).row(14) = 0000
  
  'Mode overlay textures
  pict11x15(PIC_MODE_OVL).row(00) = 0255
  pict11x15(PIC_MODE_OVL).row(01) = 0129
  pict11x15(PIC_MODE_OVL).row(02) = 0129
  pict11x15(PIC_MODE_OVL).row(03) = 0129
  pict11x15(PIC_MODE_OVL).row(04) = 2041
  pict11x15(PIC_MODE_OVL).row(05) = 1033
  pict11x15(PIC_MODE_OVL).row(06) = 1033
  pict11x15(PIC_MODE_OVL).row(07) = 1033
  pict11x15(PIC_MODE_OVL).row(08) = 1033
  pict11x15(PIC_MODE_OVL).row(09) = 1033
  pict11x15(PIC_MODE_OVL).row(10) = 1039
  pict11x15(PIC_MODE_OVL).row(11) = 1032
  pict11x15(PIC_MODE_OVL).row(12) = 1032
  pict11x15(PIC_MODE_OVL).row(13) = 1032
  pict11x15(PIC_MODE_OVL).row(14) = 2040
  
  'Mode lights
  pict11x15(PIC_MODE_LIGHT).row(00) = 0514
  pict11x15(PIC_MODE_LIGHT).row(01) = 0032
  pict11x15(PIC_MODE_LIGHT).row(02) = 1137
  pict11x15(PIC_MODE_LIGHT).row(03) = 0032
  pict11x15(PIC_MODE_LIGHT).row(04) = 0514
  pict11x15(PIC_MODE_LIGHT).row(05) = 0112
  pict11x15(PIC_MODE_LIGHT).row(06) = 0112
  pict11x15(PIC_MODE_LIGHT).row(07) = 0112
  pict11x15(PIC_MODE_LIGHT).row(08) = 0112
  pict11x15(PIC_MODE_LIGHT).row(09) = 0112
  pict11x15(PIC_MODE_LIGHT).row(10) = 0112
  pict11x15(PIC_MODE_LIGHT).row(11) = 0112
  pict11x15(PIC_MODE_LIGHT).row(12) = 1651
  pict11x15(PIC_MODE_LIGHT).row(13) = 2047
  pict11x15(PIC_MODE_LIGHT).row(14) = 1020
  
  'Mode navigation
  pict11x15(PIC_MODE_NAV).row(00) = 0063
  pict11x15(PIC_MODE_NAV).row(01) = 0063
  pict11x15(PIC_MODE_NAV).row(02) = 0063
  pict11x15(PIC_MODE_NAV).row(03) = 0063
  pict11x15(PIC_MODE_NAV).row(04) = 0063
  pict11x15(PIC_MODE_NAV).row(05) = 0018
  pict11x15(PIC_MODE_NAV).row(06) = 0018
  pict11x15(PIC_MODE_NAV).row(07) = 0018
  pict11x15(PIC_MODE_NAV).row(08) = 0018
  pict11x15(PIC_MODE_NAV).row(09) = 0018
  pict11x15(PIC_MODE_NAV).row(10) = 0226
  pict11x15(PIC_MODE_NAV).row(11) = 0914
  pict11x15(PIC_MODE_NAV).row(12) = 1090
  pict11x15(PIC_MODE_NAV).row(13) = 1026
  pict11x15(PIC_MODE_NAV).row(14) = 1020
  
  'Mode sound
  pict11x15(PIC_MODE_SND).row(00) = 0000
  pict11x15(PIC_MODE_SND).row(01) = 0000
  pict11x15(PIC_MODE_SND).row(02) = 0000
  pict11x15(PIC_MODE_SND).row(03) = 1536
  pict11x15(PIC_MODE_SND).row(04) = 1792
  pict11x15(PIC_MODE_SND).row(05) = 2047
  pict11x15(PIC_MODE_SND).row(06) = 2047
  pict11x15(PIC_MODE_SND).row(07) = 2047
  pict11x15(PIC_MODE_SND).row(08) = 1864
  pict11x15(PIC_MODE_SND).row(09) = 1608
  pict11x15(PIC_MODE_SND).row(10) = 0048
  pict11x15(PIC_MODE_SND).row(11) = 0000
  pict11x15(PIC_MODE_SND).row(12) = 0000
  pict11x15(PIC_MODE_SND).row(13) = 0000
  pict11x15(PIC_MODE_SND).row(14) = 0000

  'Mode events
  pict11x15(PIC_MODE_EVT).row(00) = 0112
  pict11x15(PIC_MODE_EVT).row(01) = 0508
  pict11x15(PIC_MODE_EVT).row(02) = 0546 
  pict11x15(PIC_MODE_EVT).row(03) = 1161
  pict11x15(PIC_MODE_EVT).row(04) = 1057
  pict11x15(PIC_MODE_EVT).row(05) = 1317
  pict11x15(PIC_MODE_EVT).row(06) = 1057
  pict11x15(PIC_MODE_EVT).row(07) = 1763
  pict11x15(PIC_MODE_EVT).row(08) = 1025
  pict11x15(PIC_MODE_EVT).row(09) = 1285
  pict11x15(PIC_MODE_EVT).row(10) = 1025
  pict11x15(PIC_MODE_EVT).row(11) = 1161
  pict11x15(PIC_MODE_EVT).row(12) = 0546
  pict11x15(PIC_MODE_EVT).row(13) = 0508
  pict11x15(PIC_MODE_EVT).row(14) = 1022

  'Mode link points
  pict11x15(PIC_MODE_LNK).row(00) = 0000
  pict11x15(PIC_MODE_LNK).row(01) = 0000
  pict11x15(PIC_MODE_LNK).row(02) = 0062
  pict11x15(PIC_MODE_LNK).row(03) = 0065
  pict11x15(PIC_MODE_LNK).row(04) = 0065
  pict11x15(PIC_MODE_LNK).row(05) = 0321
  pict11x15(PIC_MODE_LNK).row(06) = 0833
  pict11x15(PIC_MODE_LNK).row(07) = 2033
  pict11x15(PIC_MODE_LNK).row(08) = 0833
  pict11x15(PIC_MODE_LNK).row(09) = 0321
  pict11x15(PIC_MODE_LNK).row(10) = 0065
  pict11x15(PIC_MODE_LNK).row(11) = 0065
  pict11x15(PIC_MODE_LNK).row(12) = 0062
  pict11x15(PIC_MODE_LNK).row(13) = 0000
  pict11x15(PIC_MODE_LNK).row(14) = 0000

END SUB

' PCX font initialization
SUB InitPcxFonts
  PCXFLoad("Verdana40x31.pcx",40,31,33,94,0)
  PCXFLoad("Verdana24x18.pcx",24,18,33,94,1)
  pcxcenter = 0
END SUB

'Set current font
SUB SetFont(font AS INTEGER)
  curfont = font
  SELECT CASE font
    CASE 1
      fontax = 3
      fontay = 5
      spacex = 1
      spacey = 1
    CASE 2
      fontax = 5
      fontay = 6
      spacex = 1
      spacey = 3
  END SELECT
END SUB

'Set current font
SUB SetPictogramFont(pic AS INTEGER)
  curpic = pic
  SELECT CASE pic
    CASE 1
      picax = 11
      picay = 15
      picspx = 1
      picspy = 3
  END SELECT
END SUB

'Set current PCX font
SUB SetPCXFont(pcx AS INTEGER)
  curpcx = pcx
  SELECT CASE pcx
    CASE 1
      pcxax = 20
      pcxay = 31
      pcxspx = 1
      pcxspy = 1
      pcxvsp = 5
    CASE 2
      pcxax = 12
      pcxay = 19
      pcxspx = 1
      pcxspy = 1
      pcxvsp = 3
  END SELECT
END SUB

' Print character function
SUB PrintChr (x AS INTEGER, y AS INTEGER, char as STRING, col AS UINTEGER)

  'Variables
  DIM AS INTEGER i, j
  DIM AS LONG value, count
  
  'Get char value
  SELECT CASE curfont
    CASE 1
      value = asctable3x5(ASC(MID$(char, 1, 1)))
    CASE 2
      value = asctable5x6(ASC(MID$(char, 1, 1)))
  END SELECT
  
  'Print loop
  count = 1
  FOR j = 0 TO fontay - 1
  FOR i = 0 TO fontax - 1
    IF (value AND count) <> 0 THEN
  	  PSET (x + i, y + j), col
    END IF
    count = count * 2
  NEXT i
  NEXT j

END SUB

' Print string function
SUB PrintStr (x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  
  'Get String length
  length = LEN(strn)
  
  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print string function with background
SUB PrintStrBkg (x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, cur AS INTEGER, bkg AS INTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  REDIM AS BYTE charbkg(16384)
  
  'Get String length
  length = LEN(strn)
  
  'Copy background image
  SCREENSET bkg
  GET (x,y)-STEP(((fontax + spacex)*length-spacex), fontay), charbkg
  SCREENSET cur
  PUT (x,y), charbkg, PSET

  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print string function with background color
SUB PrintStrBCol(x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, bcol AS UINTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  
  'Get String length
  length = LEN(strn)
  
  'Set background color
  LINE (x,y)-STEP(((fontax + spacex)*length)-spacex, fontay),bcol,BF

  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print pictogram
SUB PrintPictogram(x AS INTEGER, y AS INTEGER, pic AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER i, j
  DIM AS USHORT value, count
  
  'Print loop
  FOR j = 0 TO picay - 1
    count = 1
    value = pict11x15(pic).row(j)
    FOR i = 0 TO picax - 1
      IF (value AND count) <> 0 THEN
    	  PSET (x + i, y + j), col
      END IF
      count = count * 2
    NEXT i
  NEXT j

END SUB

'Cursor functions for fonts
FUNCTION FCol(col AS INTEGER) AS INTEGER
  FCol = 2 + col * ( fontax + spacex )
END FUNCTION
FUNCTION FRow(row AS INTEGER) AS INTEGER
  FRow = 2 + row * ( fontay + spacey )
END FUNCTION  

'Cursor functions for pictograms
FUNCTION PCol(col AS INTEGER) AS INTEGER
  PCol = 2 + col * ( picax + picspx )
END FUNCTION
FUNCTION PRow(row AS INTEGER) AS INTEGER
  PRow = 2 + row * ( picay + picspy )
END FUNCTION  

'Cursor functions for PCX fonts
FUNCTION XCol(col AS INTEGER) AS INTEGER
  XCol = 2 + col * ( pcxax + pcxspx )
END FUNCTION
FUNCTION XRow(row AS INTEGER) AS INTEGER
  XRow = 2 + row * ( pcxay + pcxspy )
END FUNCTION  

'Manual input in keyboard buffer
SUB SetKeyb(keyb AS STRING)
  IF kbuffptr < 16 THEN
    kbuffer(kbuffptr) = keyb
    kbuffptr = kbuffptr + 1
  ENDIF
END SUB  

'Get keyboard key
FUNCTION GetKeyb(waitmode AS INTEGER) AS INTEGER

  'Variables
  DIM AS STRING keyb
  DIM AS INTEGER i,code1, code2, code
  DIM AS INTEGER count

  'Get key from manual input buffer
  IF kbuffptr > 0 THEN
    keyb = kbuffer(0)
    FOR i=1 TO kbuffptr-1
      kbuffer(i-1) = kbuffer(i)
    NEXT i
    kbuffptr = kbuffptr - 1
  
  'Get key and wait
  ELSEIF waitmode = 1 THEN
    count = 0
    DO
      SLEEP 10 
      keyb = INKEY$ 
      count = count + 1
    LOOP WHILE keyb = ""
  
  'Get key without waiting
  ELSEIF waitmode = 0 THEN
    keyb = INKEY$
    IF keyb = "" THEN
      GetKeyb = 0
      EXIT FUNCTION
    ENDIF
  ENDIF
  
  'Return key
  code1 = ASC(LEFT$(keyb, 1)): code2 = ASC(RIGHT$(keyb, 1))
  IF code1 = 255 THEN 
    GetKeyb = -code2
  ELSE
    GetKeyb = code1
  END IF	  

END FUNCTION

'Get string exit mode
SUB GetStringExitMode(mode AS INTEGER)
  gsexitmode = mode
END SUB

'Get string from keyboard with echo to the screen
FUNCTION GetStringBCol(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, _
                      bcol AS UINTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
  GetStringBCol = GetString(col,row,length,colr,bcol,0,1,strinit,ecode)
END FUNCTION

'Get string from keyboard with echo to the screen
FUNCTION GetStringBkg(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, _
                      bkg AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
  GetStringBkg = GetString(col,row,length,colr,0,bkg,2,strinit,ecode)
END FUNCTION

'Get string from keyboard with echo to the screen
FUNCTION GetString(col AS INTEGER, row AS INTEGER, length AS INTEGER, _
                   colr AS UINTEGER, bcol AS UINTEGER, bkg AS INTEGER, _
                   mode AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING

  'Variables
  DIM AS INTEGER i,curs,code,char,blink 
  DIM AS INTEGER finish,redraw
  DIM AS STRING str0, str1

  'Loop
  str0 = strinit
  SELECT CASE gsexitmode
    CASE GSEXIT_NORMAL: curs = LEN(str0)
    CASE GSEXIT_UPDOWN: curs = gscurpos
  END SELECT
  IF curs > LEN(str0) THEN curs = LEN(str0)
  redraw = 1
  DO
    
    'Redraw string
    IF redraw = 1 THEN
      IF mode = 1 THEN
        PrintStrBCol FCol(col),FRow(row),SPACE(length),colr,bcol
      ELSEIF mode = 2 THEN
        PrintStrBkg FCol(col),FRow(row),SPACE(length),colr,0,bkg
      ENDIF
      PrintStr FCol(col),FRow(row),str0,colr
      IF curs > LEN(str0)-1 THEN
        char = ASC(" ")
      ELSE
        char = str0[curs]
      ENDIF
      redraw = 1
    ENDIF

    'Cursor blinking loop
    blink = 1
    DO
      IF mode = 1 THEN
        PrintStrBCol FCol(col+curs),FRow(row),SPACE(1),colr,bcol
      ELSEIF mode = 2 THEN
        PrintStrBkg FCol(col+curs),FRow(row),SPACE(1),colr,0,bkg
      ENDIF
      PrintChr FCol(col+curs),FRow(row)+0,CHR$(char),colr
      IF blink = 1 THEN
        PrintChr FCol(col+curs),FRow(row)+0,"_",colr
        PrintChr FCol(col+curs),FRow(row)+1,"_",colr
        blink = 0
      ELSE
        blink = 1
      ENDIF
      code = GetKeyb(0)
      IF code <> 0 THEN EXIT DO
      SLEEP 250
    LOOP

    'Switch on key
    SELECT CASE code
    
      CASE 8 'Return -> delete character
        IF curs > 0 THEN
          str1 = ""
          FOR i=0 TO LEN(str0)-1
            IF i <> curs-1 THEN 
              str1 = str1 + MID$(str0,i+1,1)
            ENDIF
          NEXT i
          str0 = str1
          curs = curs - 1
          redraw = 1
        ENDIF
        
      CASE -83 'Delete -> delete character
        IF curs >= 0 AND curs < LEN(str0) THEN
          str1 = ""
          FOR i=0 TO LEN(str0)-1
            IF i <> curs THEN 
              str1 = str1 + MID$(str0,i+1,1)
            ENDIF
          NEXT i
          IF LEN(str1) < LEN(str0) THEN
            str0 = str1
            redraw = 1
          ENDIF
        ENDIF
        
     CASE -71 'Home -> Move cursor
       curs = 0
       redraw = 1
        
     CASE -79 'End -> Move cursor
       curs = LEN(str0)
       redraw = 1
        
     CASE -75 'Left -> Move cursor
       curs = curs - 1
       IF curs < 0 THEN curs = 0
       gscurpos = curs
       redraw = 1
        
     CASE -77 'Right -> Move cursor
       curs = curs + 1
       IF curs > LEN(str0) THEN curs = LEN(str0)
       gscurpos = curs
       redraw = 1
 
     CASE 13 'Enter -> Finish string input
        GetString = str0
        finish = 1
      
      CASE 27 'Escape -> return empty string
        GetString = ""
        finish = 1
      
      CASE -72 'Cursor up -> exit
        IF gsexitmode = GSEXIT_UPDOWN THEN
          GetString = str0
          finish = 1
        ENDIF
      
      CASE -80 'Cursor down -> exit
        IF gsexitmode = GSEXIT_UPDOWN THEN
          GetString = str0
          finish = 1
        ENDIF
      
      CASE ELSE 'Rest of characters -> copy to string
        IF code >=32 AND code <=126 AND LEN(str0)+1 < length THEN
          str1 = ""
          IF curs <= LEN(str0)-1 THEN
            FOR i=0 TO LEN(str0)-1
              IF i = curs THEN 
                str1 = str1 + CHR$(code)
              ENDIF
              str1 = str1 + MID$(str0,i+1,1)
            NEXT i
          ELSE
            str1 = str0 + CHR$(code)
          ENDIF
          str0 = str1
          curs = curs + 1
          redraw = 1
        ENDIF
    
    END SELECT
    
  LOOP WHILE finish = 0
  
  'Exit code
  ecode = code
  
END FUNCTION

' Load PCX font
SUB PCXFLoad(filename AS STRING, ax AS INTEGER, ay AS INTEGER, _
             start AS INTEGER, num AS INTEGER, handle AS INTEGER)
  'Variables
  DIM AS INTEGER i,j,k,idx,found
  DIM AS INTEGER fax,fay
  DIM AS UINTEGER PTR ipt
    
  'Load font from cache memory
  PcxLoad(filename,fax,fay,pcxfont(handle).img(),ax,ay*num,0)
    
  'Set char map pointers
  FOR k=0 TO 255
    IF k >= start AND k <= start + num THEN
      pcxfont(handle).char(k) = @pcxfont(handle).img((k-start)*ax*ay)
    ELSE
      pcxfont(handle).char(k) = 0 
    ENDIF
  NEXT k    
  
  'Calculate minimum and maximum for chars
  FOR k=0 TO 255
    IF k >= start AND k <= start + num THEN
      pcxfont(handle).chmin(k) = ax
      pcxfont(handle).chmax(k) = 0
      FOR i = 0 TO ax - 1
      FOR j = 0 TO ay - 1
        ipt = pcxfont(handle).char(k)
        IF ipt[(ax * j) + i] <> TRANCOLHI THEN
          IF i < pcxfont(handle).chmin(k) THEN pcxfont(handle).chmin(k) = i
          IF i > pcxfont(handle).chmax(k) THEN pcxfont(handle).chmax(k) = i
        ENDIF
      NEXT j
      NEXT i
    ENDIF
  NEXT k
  
  
  'Set rest of font table fields
  pcxfont(handle).ax = ax
  pcxfont(handle).ay = ay
    
END SUB

'Put PCX font char
SUB PCXFSetColor(col AS UINTEGER)
  modcol = col
END SUB

'Put PCX font char
SUB PCXFCentered(center AS INTEGER)
  pcxcenter = center
END SUB

'Put PCX font char
SUB PCXFPrintChr(px AS INTEGER,py AS INTEGER,char AS INTEGER)
  IF pcxfont(curpcx-1).char(char) <> 0 THEN
    IF modcol <> 0 THEN
      ImgDisplayMod(px,py,pcxfont(curpcx-1).ax,pcxfont(curpcx-1).ay,pcxfont(curpcx-1).char(char),0,modcol)
    ELSE
      ImgDisplay(px,py,pcxfont(curpcx-1).ax,pcxfont(curpcx-1).ay,pcxfont(curpcx-1).char(char),0)
    ENDIF
  ENDIF
END SUB

'Put PCX font string in monospace mode
SUB PCXFPrintStrMS(px AS INTEGER, py AS INTEGER, strn AS STRING)
  
  'Variables
  DIM AS INTEGER i,position
    
  'Calculate position if centered attribute is set
  IF pcxcenter = 1 THEN
    position = (AX0 - (LEN(strn) * (pcxax+pcxspx)))/2
  ELSE
    position = px
  ENDIF
  
  'String loop
  FOR i=0 TO LEN(strn)-1
    PCXFPrintChr(position + i*(pcxax+pcxspx),py,strn[i])
  NEXT i

END SUB

'Put PCX font string in variable space mode
SUB PCXFPrintStrVS(px AS INTEGER, py AS INTEGER, strn AS STRING)
  
  'Variables
  DIM AS INTEGER i,position,length
    
  'Calculate position if centered attribute is set
  IF pcxcenter = 1 THEN
    length = 0
    FOR i=0 TO LEN(strn)-1
      length = length + pcxfont(curpcx-1).chmax(strn[i]) - pcxfont(curpcx-1).chmin(strn[i]) + pcxvsp
    NEXT i
    position = (AX0 - length)/2
  ELSE
    position = px - pcxfont(curpcx-1).chmin(strn[0])
  ENDIF
  
  'String loop
  FOR i=0 TO LEN(strn)-1
    PCXFPrintChr(position - pcxfont(curpcx-1).chmin(strn[i]),py,strn[i])
    IF pcxfont(curpcx-1).char(strn[i]) <> 0 THEN
      position = position + pcxfont(curpcx-1).chmax(strn[i]) - pcxfont(curpcx-1).chmin(strn[i]) + pcxvsp
    ELSE
      position = position + pcxvsp * 2
    ENDIF
  NEXT i
  
END SUB


'*** End of conlib.bas ***


'*** Begin of autlib.bi ***


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
DECLARE SUB AutomSubExecute(waitmode AS INTEGER)

'*** End of autlib.bi ***


'*** Begin of autlib.bas ***

#INCLUDE "autlib.bi"
#INCLUDE "conlib.bi"

'Common variables
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
        ENDIF
      ENDIF
    NEXT i
    
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

'*** End of autlib.bas ***


'*** Begin of matlib.bi ***

'Math constants
CONST PI0 = CAST(SINGLE,3.141592654)  'Pi
CONST P12 = CAST(SINGLE,1.570796327)  'Pi/2
CONST P21 = CAST(SINGLE,6.283185307)  'Pi*2
CONST P32 = CAST(SINGLE,4.712388980)  '3*Pi/2

'Vector type
TYPE Vec
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
END TYPE

'Matrix type
TYPE Mtx
  a AS SINGLE
  b AS SINGLE
  c AS SINGLE
  d AS SINGLE
  e AS SINGLE
  f AS SINGLE
  g AS SINGLE
  h AS SINGLE
  i AS SINGLE
END TYPE

'Functions
DECLARE FUNCTION Clv AS Vec
DECLARE FUNCTION Clm AS Mtx
DECLARE FUNCTION Stv(x AS SINGLE, y AS SINGLE, z AS SINGLE) AS Vec
DECLARE FUNCTION Stm(a AS SINGLE, b AS SINGLE, c AS SINGLE, d AS SINGLE, e AS SINGLE, f AS SINGLE, g AS SINGLE, h AS SINGLE, i AS SINGLE) AS Mtx
DECLARE FUNCTION Cov(a AS Vec, b AS Vec) AS INTEGER
DECLARE FUNCTION Com(a AS Mtx, b AS Mtx) AS INTEGER
DECLARE FUNCTION Adv(a AS Vec, b AS Vec) AS Vec
DECLARE FUNCTION Sbv(a AS Vec, b AS Vec) AS Vec
DECLARE FUNCTION Vpr(a AS Vec, b AS Vec) AS Vec
DECLARE FUNCTION Spr(a AS Vec, b AS Vec) AS SINGLE
DECLARE FUNCTION Scv(v AS Vec,f AS SINGLE) AS Vec
DECLARE FUNCTION Mxv(m AS Mtx,v AS Vec) AS Vec
DECLARE FUNCTION Mov(v AS Vec) AS SINGLE
DECLARE FUNCTION Mo2(v AS Vec) AS SINGLE
DECLARE FUNCTION M2d(v AS Vec) AS SINGLE
DECLARE FUNCTION Uni(v AS Vec) AS Vec
DECLARE FUNCTION Adm(a AS Mtx, b AS Mtx) AS Mtx
DECLARE FUNCTION Sbm(a AS Mtx, b AS Mtx) AS Mtx
DECLARE FUNCTION Scm(m AS Mtx, f AS SINGLE) AS Mtx
DECLARE FUNCTION Mxm(m AS Mtx, n AS Mtx) AS Mtx
DECLARE FUNCTION Trs(m AS Mtx) AS Mtx
DECLARE FUNCTION Inv(m AS Mtx) AS Mtx
DECLARE FUNCTION Rmx(a AS SINGLE) AS Mtx
DECLARE FUNCTION Rmy(a AS SINGLE) AS Mtx
DECLARE FUNCTIOn Rmz(a AS SINGLE) AS Mtx
DECLARE FUNCTION Rox(v AS Vec, a AS SINGLE) AS Vec
DECLARE FUNCTION Roy(v AS Vec, a AS SINGLE) AS Vec
DECLARE FUNCTION Roz(v AS Vec, a AS SINGLE) AS Vec
DECLARE FUNCTION Ang(x AS SINGLE, y AS SINGLE) AS SINGLE
DECLARE FUNCTION Sth(v AS Vec) AS SINGLE
DECLARE FUNCTION Sfi(v AS Vec) AS SINGLE
DECLARE FUNCTION Vsc(rad AS SINGLE, th AS SINGLE, fi AS SINGLE) AS Vec

'*** End of matlib.bi ***


'*** Begin of matlib.bas ***

#INCLUDE "matlib.bi"

'Vector clear
FUNCTION Clv AS Vec
  DIM AS Vec r
  r.x = 0: r.y = 0: r.z = 0
  Clv = r
END FUNCTION

'Matrix clear
FUNCTION Clm AS Mtx
  DIM AS Mtx r
  r.a = 0: r.b = 0: r.c = 0
  r.d = 0: r.e = 0: r.f = 0
  r.g = 0: r.h = 0: r.i = 0
  Clm = r
END FUNCTION

'Set vector components
FUNCTION Stv(x AS SINGLE, y AS SINGLE, z AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = x
  r.y = y
  r.z = z
  Stv = r
END FUNCTION

'Set matrix coomponents
FUNCTION Stm(a AS SINGLE, b AS SINGLE, c AS SINGLE, _
             d AS SINGLE, e AS SINGLE, f AS SINGLE, _
             g AS SINGLE, h AS SINGLE, i AS SINGLE) AS Mtx
  DIM AS Mtx r
  r.a = a: r.b = b: r.c = c
  r.d = d: r.e = e: r.f = f
  r.g = g: r.h = h: r.i = i
  Stm = r
END FUNCTION

'Compare vector equality
FUNCTION Cov(a AS Vec, b AS Vec) AS INTEGER
  IF a.x = b.x AND a.y = b.y AND a.z = b.z THEN
    Cov = 1
  ELSE
    Cov = 0
  ENDIF
END FUNCTION

'Compare matrix equality
FUNCTION Com(a AS Mtx, b AS Mtx) AS INTEGER
  IF  a.a = b.a AND a.b = b.b AND a.c = b.c _
  AND a.d = b.d AND a.e = b.e AND a.f = b.f _
  AND a.g = b.g AND a.h = b.h AND a.i = b.i THEN
    Com = 1
  ELSE
    Com = 0
  ENDIF
END FUNCTION

'Vector addition
FUNCTION Adv(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.x + b.x
  r.y = a.y + b.y
  r.z = a.z + b.z
  Adv = r
END FUNCTION

'Vector substraction
FUNCTION Sbv(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.x - b.x
  r.y = a.y - b.y
  r.z = a.z - b.z
  Sbv = r
END FUNCTION

'Vectorial product
FUNCTION Vpr(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.y*b.z - a.z*b.y
  r.y = a.z*b.x - a.x*b.z
  r.z = a.x*b.y - a.y*b.x
  Vpr = r
END FUNCTION

'Scalar product
FUNCTION Spr(a AS Vec, b AS Vec) AS SINGLE
  Spr = ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z )
END FUNCTION

'Vector scale
FUNCTION Scv(v AS Vec,f AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = v.x * f
  r.y = v.y * f
  r.z = v.z * f
  Scv = r
END FUNCTION

'Matrix x vector multiplication
FUNCTION Mxv(m AS Mtx,v AS Vec) AS Vec
  DIM AS Vec r
  r.x = ( m.a * v.x ) + ( m.b * v.y ) + ( m.c * v.z )
  r.y = ( m.d * v.x ) + ( m.e * v.y ) + ( m.f * v.z )
  r.z = ( m.g * v.x ) + ( m.h * v.y ) + ( m.i * v.z )
  Mxv = r
END FUNCTION

'Vector module
FUNCTION Mov(v AS Vec) AS SINGLE
  Mov = SQR( v.x * v.x + v.y * v.y + v.z * v.z )
END FUNCTION

'Vector square module
FUNCTION Mo2(v AS Vec) AS SINGLE
  Mo2 = v.x * v.x + v.y * v.y + v.z * v.z
END FUNCTION

'Vector 2D module
FUNCTION M2d(v AS Vec) AS SINGLE
  M2d = SQR( v.x * v.x + v.y * v.y )
END FUNCTION

'Scale vector to module 1
FUNCTION Uni(v AS Vec) AS Vec
  DIM AS SINGLE m
  m = Mov(v)
  IF m <> 0 THEN 
    Uni = Scv( v, 1 / m )
  ELSE
    Uni = v
  ENDIF
END FUNCTION

'Matrix addition
FUNCTION Adm(a AS Mtx, b AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a =a.a + b.a: r.b = a.b + b.b: r.c = a.c + b.c
  r.d =a.d + b.d: r.e = a.e + b.e: r.f = a.f + b.f
  r.g =a.g + b.g: r.h = a.h + b.h: r.i = a.i + b.i
  Adm = r
END FUNCTION

'Matrix substraction
FUNCTION Sbm(a AS Mtx, b AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a =a.a - b.a: r.b = a.b - b.b: r.c = a.c - b.c
  r.d =a.d - b.d: r.e = a.e - b.e: r.f = a.f - b.f
  r.g =a.g - b.g: r.h = a.h - b.h: r.i = a.i - b.i
  Sbm = r
END FUNCTION

'Matrix by scalar product
FUNCTION Scm(m AS Mtx, f AS SINGLE) AS Mtx
  DIM AS Mtx r
  r.a =m.a * f: r.b = m.b * f: r.c = m.c * f
  r.d =m.d * f: r.e = m.e * f: r.f = m.f * f
  r.g =m.g * f: r.h = m.h * f: r.i = m.i * f
  Scm = r
END FUNCTION

'Matrix by Matrix product
FUNCTION Mxm(m AS Mtx, n AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a = m.a * n.a + m.b * n.d + m.c * n.g
  r.b = m.a * n.b + m.b * n.e + m.c * n.h
  r.c = m.a * n.c + m.b * n.f + m.c * n.i
  r.d = m.d * n.a + m.e * n.d + m.f * n.g
  r.e = m.d * n.b + m.e * n.e + m.f * n.h
  r.f = m.d * n.c + m.e * n.f + m.f * n.i
  r.g = m.g * n.a + m.h * n.d + m.i * n.g
  r.h = m.g * n.b + m.h * n.e + m.i * n.h
  r.i = m.g * n.c + m.h * n.f + m.i * n.i
  Mxm = r
END FUNCTION

'Matrix transpose
FUNCTION Trs(m AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a = m.a
  r.b = m.d
  r.c = m.g
  r.d = m.b
  r.e = m.e
  r.f = m.h
  r.g = m.c
  r.h = m.f
  r.i = m.i
  Trs = r
END FUNCTION

'Matrix inversion
FUNCTION Inv(m AS Mtx) AS Mtx

  'Variables
  DIM AS SINGLE d 'Matrix det
  DIM AS Mtx r    'Result

  'Matrix det
  d = +m.a * m.e * m.i + m.b * m.f * m.g + m.c * m.d * m.h _
      -m.g * m.e * m.c - m.d * m.b * m.i - m.a * m.h * m.f

  'Unable to calculate inverted matrix
  IF d = 0 THEN
    Inv = Clm()
    EXIT FUNCTION
  ENDIF

  'Transpose matrix
   m = Trs(m)

  'Calculate inverted matrix
  r.a = +(m.e * m.i - m.h * m.f) / d
  r.b = -(m.d * m.i - m.g * m.f) / d
  r.c = +(m.d * m.h - m.g * m.e) / d
  r.d = -(m.b * m.i - m.h * m.c) / d
  r.e = +(m.a * m.i - m.g * m.c) / d
  r.f = -(m.a * m.h - m.g * m.b) / d
  r.g = +(m.b * m.f - m.e * m.c) / d
  r.h = -(m.a * m.f - m.d * m.c) / d
  r.i = +(m.a * m.e - m.d * m.b) / d

  'Return result
  Inv = r

END FUNCTION

'Rotation matrix around x axis
FUNCTION Rmx(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a = 1: m.b = 0      : m.c = 0
  m.d = 0: m.e = +cos(a): m.f = -sin(a)
  m.g = 0: m.h = +sin(a): m.i = +cos(a)
  Rmx = m
END FUNCTION

'Rotation matrix around y axis
FUNCTION Rmy(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a =+sin(a): m.b = 0: m.c = +cos(a)
  m.d =0      : m.e = 1: m.f = 0
  m.g =+cos(a): m.h = 0: m.i = -sin(a)
  Rmy = m
END FUNCTION

'Rotation matrix around z axis
FUNCTIOn Rmz(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a = +cos(a): m.b = -sin(a): m.c = 0
  m.d = +sin(a): m.e = +cos(a): m.f = 0
  m.g = 0      : m.h = 0      : m.i = 1
  RMz = m
END FUNCTION

'Vector rotation around x axis
FUNCTION Rox(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = v.x
  r.y = +cos(a) * v.y - sin(a) * v.z
  r.z = +sin(a) * v.y + cos(a) * v.z
  Rox = r
END FUNCTION

'Vector rotation around y axis
FUNCTION Roy(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = +sin(a) * v.x + cos(a) * v.z
  r.y = v.y
  r.z = +cos(a) * v.x - sin(a) * v.z
  Roy = r
END FUNCTION

'Vector rotation around z axis
FUNCTION Roz(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x =+cos(a) * v.x - sin(a) * v.y
  r.y =+sin(a) * v.x + cos(a) * v.y
  r.z =v.z
  Roz = r
END FUNCTION

'Angle (cos^-1)
FUNCTION Ang(x AS SINGLE, y AS SINGLE) AS SINGLE

    'Variables
    DIM AS SINGLE m      'Module
    DIM AS SINGLE a = 0  'Angle

    'Module
    m = SQR( x * x + y * y )

    'Calculate angle
    IF y > 0 THEN
      a = 000 + acos( +x / m )
    ELSEIF y < 0 THEN
      a = PI0 + acos( -x / m )
    ELSEIF y = 0 AND x > 0 THEN
      a = 000
    ELSEIF y = 0 AND x < 0 THEN
      a = PI0
    ELSEIF y = 0 AND x = 0 THEN
      a = 000
    ENDIF
    
    'Return result
    Ang = a

END FUNCTION

'Spherical coordinates theta angle calculation
FUNCTION Sth(v AS Vec) AS SINGLE
  Sth = Ang( v.x , v.y )
END FUNCTION

'Spherical coordinates fi angle calculation
FUNCTION Sfi(v AS Vec) AS SINGLE
  Sfi = Ang( M2d(v) , v.z )
END FUNCTION

'Compose vector via spherical coordinates
FUNCTION Vsc(rad AS SINGLE, th AS SINGLE, fi AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = rad * sin( fi ) * cos( th )
  r.y = rad * sin( fi ) * sin( th )
  r.z = rad * cos( fi )
  Vsc = r
END FUNCTION

'*** End of matlib.bas ***

