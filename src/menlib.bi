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
CONST NODE_OPTIONS       =  3

'Menu item codes (can't be zero)
CONST MENU_NEW_GAME      = 1
CONST MENU_SAVE_GAME     = 2
CONST MENU_LOAD_GAME     = 3
CONST MENU_OPTIONS       = 4
CONST MENU_CREDITS       = 5
CONST MENU_EXIT          = 6
CONST MENU_GRAPHICS      = 7
CONST MENU_SOUND         = 8
CONST MENU_GAME_EDITOR   = 9

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
