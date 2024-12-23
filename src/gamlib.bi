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

'Map window macros
#MACRO _MapWSetWindow(win)
  MapWin("SET_WINDOW",win)
#ENDMACRO
#MACRO _MapWSetView()
  MapWin("SET_VIEW",0)
#ENDMACRO
#MACRO _MapWSetViewClear()
  MapWin("SET_VIEW_CLEAR",0)
#ENDMACRO
#MACRO _MapWInitView()
  MapWin("INIT_VIEW",0)
#ENDMACRO
#MACRO _MapWStoreWindow(win)
  MapWin("STORE_WINDOW",win)
#ENDMACRO
#MACRO _MapWRestoreWindow(win)
  MapWin("RESTORE_WINDOW",win)
#ENDMACRO

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
DECLARE SUB MapWin(commd AS STRING, parm AS INTEGER)
DECLARE SUB MapEditor
DECLARE SUB MapExecute(mapname AS STRING)