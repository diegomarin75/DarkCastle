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
CONST MAXLINKM  = 100     'Maximum link points per map
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
#MACRO _LinkListLoad(map)
  LinkPnt("LINK_LIST_LOAD",map,0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkListInit()
  LinkPnt("LINK_LIST_INIT","",0,0,"",0,0,0)
#ENDMACRO
#MACRO _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
  finish=LinkPnt("LINK_LIST_GET",map1,x1,y1,map2,x2,y2,way)
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
DECLARE FUNCTION EntRefreshAnyCmd(index AS INTEGER) AS INTEGER
DECLARE SUB EntRefreshKill
DECLARE SUB EntRefreshDraw
DECLARE SUB EntRefreshSetMapPointer(mnamep AS STRING PTR, rattrp AS roomattr PTR, cmapp AS mapcell PTR)
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
DECLARE SUB JumpConditionSet(mapname AS STRING, x AS INTEGER, y AS INTEGER)
DECLARE SUB JumpConditionGet(BYREF mapname AS STRING, BYREF x AS INTEGER, BYREF y AS INTEGER)
DECLARE SUB JumpConditionClear()
DECLARE FUNCTION JumpConditionCheck() AS INTEGER