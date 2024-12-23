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
