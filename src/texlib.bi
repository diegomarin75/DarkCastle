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
