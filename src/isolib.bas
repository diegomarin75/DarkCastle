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
