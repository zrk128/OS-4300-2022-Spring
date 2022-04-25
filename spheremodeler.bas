CONST wid = 640
CONST hig = 480

SCREEN _NEWIMAGE(wid, hig, 256)
DIM vop(0 TO 31, 0 TO 31, 0 TO 31)
cx = 0
cy = 0
cz = 0
cc = 15
DIM theta AS SINGLE
theta = 0
zoomfactor = 2
showscore = 1
DO

    'get key
    SELECT CASE INKEY$
        'move cursor
        CASE "w": IF cy > -15 THEN cy = cy - 1
        CASE "s": IF cy < 16 THEN cy = cy + 1
        CASE "a": IF cx > -15 THEN cx = cx - 1
        CASE "d": IF cx < 16 THEN cx = cx + 1
        CASE "i": IF cz < 16 THEN cz = cz + 1
        CASE "k": IF cz > -15 THEN cz = cz - 1
        CASE "j": IF cc > 0 THEN cc = cc - 1
        CASE "l": IF cc < 255 THEN cc = cc + 1
        CASE "q": theta = theta - .1
        CASE "e": theta = theta + .1
        CASE "r": GOSUB rendermode
        CASE "f": GOSUB writeout
        CASE "t": GOSUB loadin
        CASE "c": GOSUB clearspace
        CASE CHR$(32): vop(cx + 15, cy + 15, cz + 15) = cc
        CASE CHR$(9): showscore = 0 - showscore
    END SELECT
    IF theta < 0 THEN theta = 6.28318530718
    IF theta > 6.28318530718 THEN theta = 0
    'draw model
    FOR mz = 31 TO 0 STEP -1
        FOR mx = 0 TO 31
            FOR my = 0 TO 31
                drawsph mx - 15, my - 15, mz - 15, vop(mx, my, mz), theta
            NEXT
        NEXT
    NEXT

    'draw cursor
    drawsph cx, cy, cz, cc, theta
    'draw hud
    IF showscore > 0 THEN
        PRINT "x:", cx
        PRINT "y:", cy
        PRINT "z:", cz
        PRINT "color:", cc
        PRINT "theta:", theta
    END IF

    _DISPLAY
    CLS
LOOP

writeout:
INPUT "filename: ", filename$
OPEN filename$ FOR OUTPUT AS #1
PRINT "Creating file..."
FOR fz% = 31 TO 0 STEP -1
    FOR fy% = 0 TO 31
        FOR fx% = 0 TO 31
            IF vop(fx%, fy%, fz%) > 0 THEN
                WRITE #1, fx%, fy%, fz%, vop(fx%, fy%, fz%)
            END IF
        NEXT
    NEXT
NEXT
CLOSE #1
PRINT "Saved ", filename$
_DISPLAY
SLEEP
RETURN

loadin:
GOSUB clearspace
INPUT "filename: ", filename$
OPEN filename$ FOR INPUT AS #2
IF _FILEEXISTS(filename$) THEN
    DO UNTIL EOF(2)
        INPUT #2, lx%, ly%, lz%, lc%
        vop(lx%, ly%, lz%) = lc%
    LOOP
ELSE
    PRINT "File not found..."
END IF
CLOSE #2
RETURN

clearspace:
FOR ccx = 0 TO 31
    FOR ccy = 0 TO 31
        FOR ccz = 0 TO 31
            vop(ccx, ccy, ccz) = 0
        NEXT
    NEXT
NEXT
RETURN

rendermode:
DO
    DO WHILE _MOUSEINPUT
        theta = _MOUSEX / wid * 6.28318530718
    LOOP
    FOR mx = 0 TO 31
        FOR my = 0 TO 31
            FOR mz = 31 TO 0 STEP -1
                drawsph mx - 15, my - 15, mz - 15, vop(mx, my, mz), theta
            NEXT
        NEXT
    NEXT

    _LIMIT 60
    _DISPLAY
    CLS
LOOP UNTIL INKEY$ = "r"
theta = 0
RETURN

SUB drawsph (vx, vy, vz, vc, vt)
    st = ATN(vz / (vx + .00001))
    sr = SQR(vx ^ 2 + vz ^ 2)
    xx = sr * COS(st + vt) * (vx + .00001) / ABS(vx + .00001)
    zz = sr * SIN(st + vt) * (vx + .00001) / ABS(vx + .00001)
    COLOR vc
    vr = (wid + hig / 2) / 32
    az# = (zz + 16) / 16 + 1
    IF vc > 0 THEN
        LINE (INT(wid / 2 + xx * wid / 32 / az#) - vr / az#, INT(hig / 2 + vy * hig / 32 / az#) - vr / az#)-(INT(wid / 2 + xx * wid / 32 / az#) + vr / az#, INT(hig / 2 + vy * hig / 32 / az#) + vr / az#), , BF
    END IF
END SUB


