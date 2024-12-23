#INCLUDE "matlib.bi"

'Vector clear
FUNCTION Clv AS Vec
  DIM AS Vec r
  r.x = 0: r.y = 0: r.z = 0
  Clv = r
END FUNCTION

'Matrix clear
FUNCTION Clm AS Mtx
  DIM AS Mtx r
  r.a = 0: r.b = 0: r.c = 0
  r.d = 0: r.e = 0: r.f = 0
  r.g = 0: r.h = 0: r.i = 0
  Clm = r
END FUNCTION

'Set vector components
FUNCTION Stv(x AS SINGLE, y AS SINGLE, z AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = x
  r.y = y
  r.z = z
  Stv = r
END FUNCTION

'Set matrix coomponents
FUNCTION Stm(a AS SINGLE, b AS SINGLE, c AS SINGLE, _
             d AS SINGLE, e AS SINGLE, f AS SINGLE, _
             g AS SINGLE, h AS SINGLE, i AS SINGLE) AS Mtx
  DIM AS Mtx r
  r.a = a: r.b = b: r.c = c
  r.d = d: r.e = e: r.f = f
  r.g = g: r.h = h: r.i = i
  Stm = r
END FUNCTION

'Compare vector equality
FUNCTION Cov(a AS Vec, b AS Vec) AS INTEGER
  IF a.x = b.x AND a.y = b.y AND a.z = b.z THEN
    Cov = 1
  ELSE
    Cov = 0
  ENDIF
END FUNCTION

'Compare matrix equality
FUNCTION Com(a AS Mtx, b AS Mtx) AS INTEGER
  IF  a.a = b.a AND a.b = b.b AND a.c = b.c _
  AND a.d = b.d AND a.e = b.e AND a.f = b.f _
  AND a.g = b.g AND a.h = b.h AND a.i = b.i THEN
    Com = 1
  ELSE
    Com = 0
  ENDIF
END FUNCTION

'Vector addition
FUNCTION Adv(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.x + b.x
  r.y = a.y + b.y
  r.z = a.z + b.z
  Adv = r
END FUNCTION

'Vector substraction
FUNCTION Sbv(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.x - b.x
  r.y = a.y - b.y
  r.z = a.z - b.z
  Sbv = r
END FUNCTION

'Vectorial product
FUNCTION Vpr(a AS Vec, b AS Vec) AS Vec
  DIM AS Vec r
  r.x = a.y*b.z - a.z*b.y
  r.y = a.z*b.x - a.x*b.z
  r.z = a.x*b.y - a.y*b.x
  Vpr = r
END FUNCTION

'Scalar product
FUNCTION Spr(a AS Vec, b AS Vec) AS SINGLE
  Spr = ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z )
END FUNCTION

'Vector scale
FUNCTION Scv(v AS Vec,f AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = v.x * f
  r.y = v.y * f
  r.z = v.z * f
  Scv = r
END FUNCTION

'Matrix x vector multiplication
FUNCTION Mxv(m AS Mtx,v AS Vec) AS Vec
  DIM AS Vec r
  r.x = ( m.a * v.x ) + ( m.b * v.y ) + ( m.c * v.z )
  r.y = ( m.d * v.x ) + ( m.e * v.y ) + ( m.f * v.z )
  r.z = ( m.g * v.x ) + ( m.h * v.y ) + ( m.i * v.z )
  Mxv = r
END FUNCTION

'Vector module
FUNCTION Mov(v AS Vec) AS SINGLE
  Mov = SQR( v.x * v.x + v.y * v.y + v.z * v.z )
END FUNCTION

'Vector square module
FUNCTION Mo2(v AS Vec) AS SINGLE
  Mo2 = v.x * v.x + v.y * v.y + v.z * v.z
END FUNCTION

'Vector 2D module
FUNCTION M2d(v AS Vec) AS SINGLE
  M2d = SQR( v.x * v.x + v.y * v.y )
END FUNCTION

'Scale vector to module 1
FUNCTION Uni(v AS Vec) AS Vec
  DIM AS SINGLE m
  m = Mov(v)
  IF m <> 0 THEN 
    Uni = Scv( v, 1 / m )
  ELSE
    Uni = v
  ENDIF
END FUNCTION

'Matrix addition
FUNCTION Adm(a AS Mtx, b AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a =a.a + b.a: r.b = a.b + b.b: r.c = a.c + b.c
  r.d =a.d + b.d: r.e = a.e + b.e: r.f = a.f + b.f
  r.g =a.g + b.g: r.h = a.h + b.h: r.i = a.i + b.i
  Adm = r
END FUNCTION

'Matrix substraction
FUNCTION Sbm(a AS Mtx, b AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a =a.a - b.a: r.b = a.b - b.b: r.c = a.c - b.c
  r.d =a.d - b.d: r.e = a.e - b.e: r.f = a.f - b.f
  r.g =a.g - b.g: r.h = a.h - b.h: r.i = a.i - b.i
  Sbm = r
END FUNCTION

'Matrix by scalar product
FUNCTION Scm(m AS Mtx, f AS SINGLE) AS Mtx
  DIM AS Mtx r
  r.a =m.a * f: r.b = m.b * f: r.c = m.c * f
  r.d =m.d * f: r.e = m.e * f: r.f = m.f * f
  r.g =m.g * f: r.h = m.h * f: r.i = m.i * f
  Scm = r
END FUNCTION

'Matrix by Matrix product
FUNCTION Mxm(m AS Mtx, n AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a = m.a * n.a + m.b * n.d + m.c * n.g
  r.b = m.a * n.b + m.b * n.e + m.c * n.h
  r.c = m.a * n.c + m.b * n.f + m.c * n.i
  r.d = m.d * n.a + m.e * n.d + m.f * n.g
  r.e = m.d * n.b + m.e * n.e + m.f * n.h
  r.f = m.d * n.c + m.e * n.f + m.f * n.i
  r.g = m.g * n.a + m.h * n.d + m.i * n.g
  r.h = m.g * n.b + m.h * n.e + m.i * n.h
  r.i = m.g * n.c + m.h * n.f + m.i * n.i
  Mxm = r
END FUNCTION

'Matrix transpose
FUNCTION Trs(m AS Mtx) AS Mtx
  DIM AS Mtx r
  r.a = m.a
  r.b = m.d
  r.c = m.g
  r.d = m.b
  r.e = m.e
  r.f = m.h
  r.g = m.c
  r.h = m.f
  r.i = m.i
  Trs = r
END FUNCTION

'Matrix inversion
FUNCTION Inv(m AS Mtx) AS Mtx

  'Variables
  DIM AS SINGLE d 'Matrix det
  DIM AS Mtx r    'Result

  'Matrix det
  d = +m.a * m.e * m.i + m.b * m.f * m.g + m.c * m.d * m.h _
      -m.g * m.e * m.c - m.d * m.b * m.i - m.a * m.h * m.f

  'Unable to calculate inverted matrix
  IF d = 0 THEN
    Inv = Clm()
    EXIT FUNCTION
  ENDIF

  'Transpose matrix
   m = Trs(m)

  'Calculate inverted matrix
  r.a = +(m.e * m.i - m.h * m.f) / d
  r.b = -(m.d * m.i - m.g * m.f) / d
  r.c = +(m.d * m.h - m.g * m.e) / d
  r.d = -(m.b * m.i - m.h * m.c) / d
  r.e = +(m.a * m.i - m.g * m.c) / d
  r.f = -(m.a * m.h - m.g * m.b) / d
  r.g = +(m.b * m.f - m.e * m.c) / d
  r.h = -(m.a * m.f - m.d * m.c) / d
  r.i = +(m.a * m.e - m.d * m.b) / d

  'Return result
  Inv = r

END FUNCTION

'Rotation matrix around x axis
FUNCTION Rmx(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a = 1: m.b = 0      : m.c = 0
  m.d = 0: m.e = +cos(a): m.f = -sin(a)
  m.g = 0: m.h = +sin(a): m.i = +cos(a)
  Rmx = m
END FUNCTION

'Rotation matrix around y axis
FUNCTION Rmy(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a =+sin(a): m.b = 0: m.c = +cos(a)
  m.d =0      : m.e = 1: m.f = 0
  m.g =+cos(a): m.h = 0: m.i = -sin(a)
  Rmy = m
END FUNCTION

'Rotation matrix around z axis
FUNCTIOn Rmz(a AS SINGLE) AS Mtx
  DIM AS Mtx m
  m.a = +cos(a): m.b = -sin(a): m.c = 0
  m.d = +sin(a): m.e = +cos(a): m.f = 0
  m.g = 0      : m.h = 0      : m.i = 1
  RMz = m
END FUNCTION

'Vector rotation around x axis
FUNCTION Rox(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = v.x
  r.y = +cos(a) * v.y - sin(a) * v.z
  r.z = +sin(a) * v.y + cos(a) * v.z
  Rox = r
END FUNCTION

'Vector rotation around y axis
FUNCTION Roy(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = +sin(a) * v.x + cos(a) * v.z
  r.y = v.y
  r.z = +cos(a) * v.x - sin(a) * v.z
  Roy = r
END FUNCTION

'Vector rotation around z axis
FUNCTION Roz(v AS Vec, a AS SINGLE) AS Vec
  DIM AS Vec r
  r.x =+cos(a) * v.x - sin(a) * v.y
  r.y =+sin(a) * v.x + cos(a) * v.y
  r.z =v.z
  Roz = r
END FUNCTION

'Angle (cos^-1)
FUNCTION Ang(x AS SINGLE, y AS SINGLE) AS SINGLE

    'Variables
    DIM AS SINGLE m      'Module
    DIM AS SINGLE a = 0  'Angle

    'Module
    m = SQR( x * x + y * y )

    'Calculate angle
    IF y > 0 THEN
      a = 000 + acos( +x / m )
    ELSEIF y < 0 THEN
      a = PI0 + acos( -x / m )
    ELSEIF y = 0 AND x > 0 THEN
      a = 000
    ELSEIF y = 0 AND x < 0 THEN
      a = PI0
    ELSEIF y = 0 AND x = 0 THEN
      a = 000
    ENDIF
    
    'Return result
    Ang = a

END FUNCTION

'Spherical coordinates theta angle calculation
FUNCTION Sth(v AS Vec) AS SINGLE
  Sth = Ang( v.x , v.y )
END FUNCTION

'Spherical coordinates fi angle calculation
FUNCTION Sfi(v AS Vec) AS SINGLE
  Sfi = Ang( M2d(v) , v.z )
END FUNCTION

'Compose vector via spherical coordinates
FUNCTION Vsc(rad AS SINGLE, th AS SINGLE, fi AS SINGLE) AS Vec
  DIM AS Vec r
  r.x = rad * sin( fi ) * cos( th )
  r.y = rad * sin( fi ) * sin( th )
  r.z = rad * cos( fi )
  Vsc = r
END FUNCTION