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
