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
  EntRefreshSetMapPointer(@filename,@rattr,@cmap(0,0))
  _MapWSetWindow(1)
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
          CASE 1: _MapWSetWindow(2)
          CASE 2: _MapWSetWindow(3)
        END SELECT
        SCREENCOPY 2,1
        refresh = 2
      ELSEIF aedit = EDIT_EVT THEN  
        _MapWSetWindow(1)
        SCREENCOPY 2,1
        refresh = 2
      ENDIF
    
    ENDIF
  
    'Refresh map
    IF Refresh = 2 THEN
      _MapWSetViewClear()
      RoomMap rattr, cmap(), light(), ovtex(), lmapupdate
      _MapWInitView()
      _MapWStoreWindow(1)
    END IF
    
    'Refresh cursor
    IF Refresh = 1 OR refresh = 2 THEN
      _MapWRestoreWindow(1)
      _MapWSetView()
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
      _MapWStoreWindow(2)
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
          _MapWSetView()
          ISOLMapUnifyAllPlanes
          ISOLMapDrawPlanes(1)
          GetKeyb(1)
          refresh = 2
          SCREENSET 1,0
          _MapWInitView()
        
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
  filename = mapname
  SetFont(2)
  MapRead(filename, rattr,cmap(),light(),ovtex(),event(),actio())
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
  EntRefreshSetMapPointer(@filename,@rattr,@cmap(0,0))
  _MapWSetWindow(0)
  
  'Map drawing loop
  refresh = 2
  finish = 0
  mode = 5
  x = 0: y = 0
  entypenr = 0
  DO
    
    'Refresh map
    IF refresh = 2 THEN
      _MapWSetViewClear()
      RoomMap rattr, cmap(), light(), ovtex(), 0
      _MapWInitView()
      _MapWStoreWindow(2)
    END IF
    
    'Clear refresh flag
    refresh=0
    
    'Multikey commands
    IF MULTIKEY(FB.SC_LEFT) AND EntRefreshAnyCmd(entidx) = 0 THEN 
      EntRefreshCommand(entidx,ENCMOVL,1)
    ENDIF
    IF MULTIKEY(FB.SC_RIGHT) AND EntRefreshAnyCmd(entidx) = 0 THEN 
      EntRefreshCommand(entidx,ENCMOVR,1)
    ENDIF
    IF MULTIKEY(FB.SC_UP) AND EntRefreshAnyCmd(entidx) = 0 THEN 
      EntRefreshCommand(entidx,ENCMOVU,1)
    ENDIF
    IF MULTIKEY(FB.SC_DOWN) AND EntRefreshAnyCmd(entidx) = 0 THEN 
      EntRefreshCommand(entidx,ENCMOVD,1)
    ENDIF
    
    'Get keyboard code & refresh objects
    code = GetKeyb(0)
    IF code = 0 THEN AutomSubExecute(1)
    
    'Check map jump condition
    IF JumpConditionCheck() = 1 THEN
      JumpConditionGet(filename,x,y)
      JumpConditionClear()
      MapInit(rattr,cmap(),light(),ovtex(),event(),actio())
      MapRead(filename,rattr,cmap(),light(),ovtex(),event(),actio())
      IF rattr.lmapstore = 1 THEN
        SetDispMode(LIGHT_MODE,MODE_LMAP)
      ELSE
        SetDispMode(LIGHT_MODE,MODE_LDYN)
      ENDIF
      EntRefreshKill
      entidx = EntRefreshEnqueue(x*rattr.cellsize,y*rattr.cellsize, _
      cmap(x,y).floor*rattr.cellsize,etype(0))
      refresh = 2
    
    'Keyboard commands
    ELSE
    
      'Switch on keyboard commands
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
          finish = 1
      
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
      
  LOOP WHILE finish = 0  
  
END SUB

'Refresh daemon
SUB MapRefreshDaemon
  
  'Init secondary z-buffer
  ISOZBufferLayer 2
  ISOZBufferClear
  ISOLMapUseFlag(0)
  
  'Rewrite map
  _MapWRestoreWindow(2)

  'Set clipping margin
  _MapWSetView()
  
  'Animated object refresh
  ObjRefreshMode(1)
  ObjRefreshDraw
  
  'Entity refresh
  EntRefreshDraw

  'Restore clipping
  VIEW SCREEN
  
  'Copy screen
  SCREENCOPY 1,0
  
  'Check map jump condition and exit auomatic refresh
  IF JumpConditionCheck() = 1 THEN AutomSubExit()
  
END SUB

'Map window management
SUB MapWin(commd AS STRING, parm AS INTEGER)

  'Variables
  STATIC AS INTEGER px1,py1,px2,py2

  'Switch on command
  SELECT CASE commd
  
    'Set window
    CASE "SET_WINDOW"
      IF parm = 0 THEN
        px1 = 0
        py1 = 0
        px2 = AX0 - 1
        py2 = AY0 - 1
      ELSEIF parm = 1 THEN
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
    _LinkListLoad(mapname)
    _LinkListInit()
    _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
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
      _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
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

