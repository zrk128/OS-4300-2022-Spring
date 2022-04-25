PRINT "image name"
INPUT imgname$
PRINT "csv name"
INPUT csvname$
PRINT "depth"
INPUT z
SCREEN _NEWIMAGE(256, 10, 32)
pal = _LOADIMAGE("pallette.png")
DIM SHARED palt(0 TO 255)
_PUTIMAGE (0, 0), pal

FOR i = 0 TO 255
    palt(i) = POINT(i, 5)
NEXT



SCREEN _NEWIMAGE(32, 32, 32)
bopha = _LOADIMAGE(imgname$)
OPEN csvname$ FOR OUTPUT AS #1
DIM img(0 TO 31, 0 TO 31)

SCREEN _NEWIMAGE(32, 32, 32)
_PUTIMAGE (0, 0), bopha
FOR x = 0 TO 31
    FOR y = 0 TO 31
        img(x, y) = quant%(POINT(x, y))
        WRITE #1, x, y, z, img(x, y)
    NEXT
NEXT
SCREEN _NEWIMAGE(32, 32, 256)
FOR x = 0 TO 31
    FOR y = 0 TO 31
        PSET (x, y), img(x, y)
    NEXT
NEXT

DO
LOOP UNTIL INKEY$ = "q"

FUNCTION quant% (col)
    tcolor = 0
    dif = 9999999999
    FOR i = 0 TO 255
        ndif = ABS(_RED32(palt(i)) - _RED32(col)) + ABS(_GREEN32(palt(i)) - _GREEN32(col)) + ABS(_BLUE32(palt(i)) - _BLUE32(col))
        IF ndif < dif AND RND < .1 THEN dif = ndif: tcolor = i


    NEXT
    quant% = tcolor
END FUNCTION
