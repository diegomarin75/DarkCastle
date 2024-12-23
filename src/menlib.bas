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
  _SetNode(NODE_MAIN   ,0,"New game"    ,MENU_NEW_GAME    ,NODE_NEW_GAME ,0) 
  _SetNode(NODE_MAIN   ,1,"Save game"   ,MENU_SAVE_GAME   ,NODE_NULL     ,0) 
  _SetNode(NODE_MAIN   ,2,"Load game"   ,MENU_LOAD_GAME   ,NODE_NULL     ,0) 
  _SetNode(NODE_MAIN   ,3,"Options"     ,MENU_OPTIONS     ,NODE_OPTIONS  ,0) 
  _SetNode(NODE_MAIN   ,4,"Credits"     ,MENU_CREDITS     ,NODE_NULL     ,0) 
  _SetNode(NODE_MAIN   ,5,"Exit"        ,MENU_EXIT        ,NODE_NULL     ,1) 
  _SetNode(NODE_OPTIONS,0,"Graphics"    ,MENU_GRAPHICS    ,NODE_NULL     ,0) 
  _SetNode(NODE_OPTIONS,1,"Sound"       ,MENU_SOUND       ,NODE_NULL     ,0)  
  _SetNode(NODE_OPTIONS,2,"Game editor" ,MENU_GAME_EDITOR ,NODE_NULL     ,1) 
  
  'Node titles
  ntitle(NODE_MAIN)     = "Dark Castle v1.0"
  ntitle(NODE_NEW_GAME) = "Choose entry point"
  ntitle(NODE_OPTIONS)  = "Options"
  
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