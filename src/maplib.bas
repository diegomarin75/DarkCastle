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
DIM SHARED mnameptr AS STRING PTR
DIM SHARED rattrptr AS roomattr PTR
DIM SHARED cmapptr AS mapcell PTR

'Light map to store / restore from files
DIM SHARED lmaplen AS INTEGER
REDIM SHARED lmap(AX0*AY0) AS RGBi

'Link point variables
DIM SHARED numlink AS INTEGER
REDIM SHARED linkpoint(MAXLINKP) AS lpfile 
REDIM SHARED linkpoint(MAXLINKP) AS lpfile 
REDIM SHARED linkpmap(MAXLINKM) AS lpfile 

'Map images
DIM SHARED mapptr AS INTEGER = 0
DIM SHARED numimr AS INTEGER = 0
REDIM SHARED mapimr(MAPIMGRE) AS mapimgrec
REDIM SHARED mapimg(MAPIMGSZ) AS UINTEGER

'Defined actions table
REDIM SHARED AS actdefin actdef(DEFACTIO)

'Jump map condition
DIM SHARED jumpm AS STRING
DIM SHARED jumpx AS INTEGER
DIM SHARED jumpy AS INTEGER

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
  
  'Comment
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
        IF disp_light = MODE_LMAP AND ISOLMapStatus() = 0 THEN disp_light = MODE_OFF
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
  _LinkListLoad(mapname)
  _LinkListInit()
  _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
  WHILE finish = 0
    IF way = +1 THEN 'One way exit
      ISOLightAmbient rattr.lightambl+1.5,0,0,100
    ELSEIF way = -1 THEN 'One way entry
      ISOLightAmbient rattr.lightambl+1.5,100,0,0
    ELSEIF way = 0 THEN 'Two way link
      ISOLightAmbient rattr.lightambl+1.5,0,100,0
    ENDIF
    RoomGCell rattr, x1, y1, cmap(), 0
    _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
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

'Entity refresh check pending command
FUNCTION EntRefreshAnyCmd(index AS INTEGER) AS INTEGER
  IF entref(index).cindex > 0 THEN
    EntRefreshAnyCmd = 1
  ELSE
    EntRefreshAnyCmd = 0
  ENDIF
END FUNCTION

'Entity set map pointer
SUB EntRefreshSetMapPointer(mnamep AS STRING PTR, rattrp AS roomattr PTR, cmapp AS mapcell PTR)
  mnameptr = mnamep
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
  DIM AS INTEGER finish
  DIM AS STRING map1,map2
  DIM AS INTEGER x1,y1,x2,y2,way
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
      PrintStrBkg FCol(98),FRow(50), FORMAT(cx,"00") + "," + FORMAT(cy,"00"), RGB(255,255,255),1,1
      ISOSquare cx*rattrptr->cellsize,cy*rattrptr->cellsize,_
                cmapptr[cx*(MAXCELLY+1)+cy].floor*rattrptr->cellsize,_
                rattrptr->cellsize,rattrptr->cellsize,ISOZPLANE,RGB(0,255,0)
      
      'Check map change
      _LinkListLoad(*mnameptr)
      _LinkListInit()
      _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
      WHILE finish = 0
        IF x1 = cx AND y1 = cy AND entref(i).acomm <> "" THEN
          JumpConditionSet(map2,x2,y2)
          EXIT WHILE
        ENDIF
        _LinkListGet(map1,x1,y1,map2,x2,y2,way,finish)
      WEND
      
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
  IF cmap.height > 0 AND cmap.ceiling > 0 AND cmap.height - cmap.floor < 4 THEN
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

'Set map jump condition
SUB JumpConditionSet(mapname AS STRING, x AS INTEGER, y AS INTEGER)
  jumpm = mapname
  jumpx = x
  jumpy = y
END SUB  

'Get map jump condition
SUB JumpConditionGet(BYREF mapname AS STRING, BYREF x AS INTEGER, BYREF y AS INTEGER)
  mapname = jumpm
  x = jumpx
  y = jumpy
END SUB  

'Clear map jump condition
SUB JumpConditionClear()
  jumpm = ""
  jumpx = 0
  jumpy = 0
END SUB  

'Check map jump condition
FUNCTION JumpConditionCheck() AS INTEGER
  IF jumpm <> "" THEN
    JumpConditionCheck = 1
  ELSE
    JumpConditionCheck = 0
  ENDIF
END FUNCTION  

'Link point management
FUNCTION LinkPnt(commd AS STRING, BYREF map1 AS STRING, BYREF x1 AS INTEGER, BYREF y1 AS INTEGER, _
                 BYREF map2 AS STRING, BYREF x2 AS INTEGER, BYREF y2 AS INTEGER, _
                 BYREF way AS INTEGER) AS INTEGER
  
  'Variables
  STATIC AS STRING map
  STATIC AS INTEGER index
  STATIC AS INTEGER numlmap
  DIM AS INTEGER i,j,k,file
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
    CASE "LINK_LIST_LOAD"
      j = 0
      FOR i=0 TO numlink-1
        IF linkpoint(i).delet = 0 THEN
          IF linkpoint(i).map1 = map1 THEN
            linkpmap(j).map1 = linkpoint(i).map1
            linkpmap(j).x1   = linkpoint(i).x1
            linkpmap(j).y1   = linkpoint(i).y1
            linkpmap(j).map2 = linkpoint(i).map2
            linkpmap(j).x2   = linkpoint(i).x2
            linkpmap(j).y2   = linkpoint(i).y2
            linkpmap(j).way  = linkpoint(i).way
            j = j + 1
          ELSEIF linkpoint(i).map2 = map1 THEN
            linkpmap(j).map1 = linkpoint(i).map2
            linkpmap(j).x1   = linkpoint(i).x2
            linkpmap(j).y1   = linkpoint(i).y2
            linkpmap(j).map2 = linkpoint(i).map1
            linkpmap(j).x2   = linkpoint(i).x1
            linkpmap(j).y2   = linkpoint(i).y1
            linkpmap(j).way  = -linkpoint(i).way
            j = j + 1
          ENDIF
        ENDIF
      NEXT i
      numlmap = j
      
    'Get link points for map
    CASE "LINK_LIST_INIT"
      index = 0
    
    'Get link points for map
    CASE "LINK_LIST_GET"
      IF index <= numlmap - 1 THEN
        map1 = linkpmap(index).map1
        x1   = linkpmap(index).x1
        y1   = linkpmap(index).y1
        map2 = linkpmap(index).map2
        x2   = linkpmap(index).x2
        y2   = linkpmap(index).y2
        way  = linkpmap(index).way
        index = index + 1
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

