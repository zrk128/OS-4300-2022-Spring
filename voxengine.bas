CONST wid = 512
CONST hig = 256
CONST zbr = 1
_TITLE "Zachary's Adventure Game"
RANDOMIZE TIMER
SCREEN _NEWIMAGE(wid, hig, 256)
_FULLSCREEN
PRINT "Loading..."
_MOUSEHIDE
DIM SHARED theta
DIM SHARED zbuf(0 TO wid / zbr, 0 TO hig / zbr)
DIM SHARED bgzbuf(0 TO wid / zbr, 0 TO hig / zbr)
DIM SHARED map(-63 TO 64, -63 TO 64, -63 TO 64) AS INTEGER
DIM SHARED world(-8 TO 8, -8 TO 8, -8 TO 8) AS STRING
DIM SHARED px 'player positions
DIM SHARED py
DIM SHARED pz
DIM SHARED pspeed
DIM SHARED bg(wid * hig)

DIM SHARED grass$
DIM SHARED tree$
DIM SHARED cube$
DIM SHARED dirt$
DIM SHARED eye$
DIM SHARED raise$
DIM SHARED column$
DIM SHARED giantlegs$
DIM SHARED gianttorso$
DIM SHARED box$
DIM SHARED splat$
DIM SHARED sky$
DIM SHARED plus$
DIM SHARED stalag$
DIM SHARED b$
DIM SHARED mobmodel$
DIM SHARED groover$
DIM SHARED alfred$
DIM SHARED patrick$


DIM SHARED worldx
DIM SHARED worldy
DIM SHARED worldz
DIM SHARED maxblocks
DIM SHARED blocks
DIM SHARED mobs
DIM SHARED mobx
DIM SHARED moby
DIM SHARED mobz
DIM SHARED health AS INTEGER
DIM SHARED numframe AS INTEGER
DIM SHARED msh
DIM SHARED corozal
DIM SHARED glenkenn

pdir = -1

'load sound
ping = _SNDOPEN("ping.ogg", "sync,vol")
pain = _SNDOPEN("pain.ogg", "sync,vol")
zap = _SNDOPEN("zap.ogg", "sync,vol")
scream1 = _SNDOPEN("scream_1.ogg", "sync,vol")
scream2 = _SNDOPEN("scream_2.ogg", "sync,vol")
shot1 = _SNDOPEN("shot_1.ogg", "sync,vol")
shot2 = _SNDOPEN("shot_2.ogg", "sync,vol")
corozal = _SNDOPEN("Corozal.ogg", "sync,vol")
glenkenn = _SNDOPEN("GlenKenn.ogg", "sync,vol")
msh = _SNDOPEN("MtStHelens.ogg", "sync,vol")
stomp = _SNDOPEN("stomp.ogg", "sync,vol")
splode1 = _SNDOPEN("explode_1.ogg", "sync,vol")
splode2 = _SNDOPEN("explode_3.ogg", "sync,vol")
splode3 = _SNDOPEN("explode_2.ogg", "sync,vol")

'load models
loadmod "pat.csv", patrick$
loadmod "groover.csv", groover$
loadmod "b.csv", b$
loadmod "grass.csv", grass$
loadmod "man.csv", man$
loadmod "splat.csv", splat$
loadmod "cube.csv", cube$
loadmod "test.csv", test$
loadmod "tree.csv", tree$
loadmod "dirt.csv", dirt$
loadmod "eye.csv", eye$
loadmod "raiser.csv", raise$
loadmod "column.csv", column$
loadmod "box.csv", box$
loadmod "giant legs.csv", giantlegs$
loadmod "giant torso.csv", gianttorso$
loadmod "sky.csv", sky$
loadmod "plus.csv", plus$
loadmod "stalag.csv", stalag$
loadmod "alfred.csv", alfred$
'initialize world
FOR x = -8 TO 8
    FOR y = -8 TO 8
        FOR z = -8 TO 8
            IF y = 0 THEN
                world(x, y, z) = "forest"
                IF (x + z) MOD 5 = 1 THEN world(x, y, z) = "hole"
            END IF
            IF y = -2 THEN world(x, y, z) = "firmament"
            IF y = 1 THEN world(x, y, z) = "underground"
        NEXT
    NEXT
NEXT
world(0, 0, 0) = "main"

world(3, 1, 3) = "undergroundz"



start:
IF _SNDPLAYING(corozal) THEN _SNDSTOP corozal
IF _SNDPLAYING(msh) THEN _SNDSTOP msh
IF _SNDPLAYING(glenkenn) THEN _SNDSTOP glenkenn
CLS
DO
    COLOR RND * 255
    IF RND < .3 THEN PAINT (10, 10)
    COLOR RND * 255
    LOCATE RND * 15 + 1, RND * 25 + 1
    PRINT "ZACHARY'S ADVENTURE GAME!"
    LOCATE RND * 15 + 1, RND * 25 + 1
    PRINT " press , to start"
    _LIMIT 10
    _DISPLAY
LOOP UNTIL INKEY$ = ","
maxblocks = 1
worldx = 0
worldy = 0
worldz = 0
px = 0
py = 4
pz = 0
jumpingkey = 0
pyvel = 0
numframe = 0
pspeed = .1
health = 100
playermodel$ = man$
mobmodel$ = eye$
golevel world(worldx, worldy, worldz)
starttime = TIMER
DO
    IF INKEY$ = "r" THEN GOTO start

    numframe = numframe + 1
    '    PRINT numframe / 50
    '   PRINT TIMER - starttime

    'get input and walk
    IF health > 0 THEN
        DO WHILE _MOUSEINPUT
            mx = _ROUND((_MOUSEX - wid / 2) / wid * 16)
            mz = _ROUND((hig / 2 - _MOUSEY) / hig * 16)
            'OBJECT PLACE
            IF _MOUSEBUTTON(1) AND clicking = 0 AND space(mx, _ROUND(py), mz) = 0 AND blocks > 0 THEN
                IF SQR((mx - mobx) ^ 2 + (py - moby) ^ 2 + (mz - mobz) ^ 2) < 2 AND mobs > 0 THEN
                    mobs = mobs - 1: maxblocks = maxblocks + 11
                    _SNDPLAY ping
                    _SNDPLAY splode1
                END IF
                bakeobj mx, _ROUND(py), mz, cube$
                _SNDPLAY shot1
                clicking = 1
                blocks = blocks - 1
            END IF
            IF NOT _MOUSEBUTTON(1) THEN clicking = 0
        LOOP

        IF _KEYDOWN(ASC("w")) THEN walk 0
        IF _KEYDOWN(ASC("d")) THEN walk 1
        IF _KEYDOWN(ASC("s")) THEN walk 2
        IF _KEYDOWN(ASC("a")) THEN walk 3


        'JUMP
        IF space(px, INT(py) + 1, pz) = 1 AND pyvel >= 0 THEN
            pyvel = 0
            py = _ROUND(py)
            IF _KEYDOWN(32) AND jumpingkey = 0 AND health > 0 THEN pyvel = -.2: jumpingkey = 1
            IF NOT _KEYDOWN(32) THEN jumpingkey = 0
        ELSE
            IF space(px, py - 1, pz) = 1 THEN
                pyvel = ABS(pyvel)
            ELSE
                IF pyvel < .5 THEN pyvel = pyvel + .02
            END IF
        END IF


        py = py + pyvel
    END IF

    'CHANGE LEVEL
    IF py > 8 THEN
        py = -8: worldy = worldy + 1
        IF worldy > 8 THEN worldy = 8
        golevel (world(worldx, worldy, worldz))
    END IF
    IF py < -8 THEN
        py = 5: worldy = worldy - 1
        IF worldy = -3 THEN GOTO kyle
        IF worldy < -8 THEN worldy = -8
        golevel (world(worldx, worldy, worldz))
    END IF
    IF px > 8 THEN
        px = -8: worldx = worldx + 1
        IF worldx > 8 THEN worldx = 8
        golevel (world(worldx, worldy, worldz))
    END IF
    IF px < -8 THEN
        px = 8: worldx = worldx - 1
        IF worldx < -8 THEN worldx = -8
        golevel (world(worldx, worldy, worldz))
    END IF
    IF pz > 8 THEN
        pz = -8: worldz = worldz + 1
        IF worldz > 8 THEN worldz = 8
        golevel (world(worldx, worldy, worldz))
    END IF
    IF pz < -8 THEN
        pz = 8: worldz = worldz - 1
        IF worldz < -8 THEN worldz = -8
        golevel (world(worldx, worldy, worldz))
    END IF


    'DRAW
    PUT (0, 0), bg() 'draw level
    '    IF space(px, py, pz) = 1 THEN GOTO dead
    IF mobs > 0 THEN
        drawmod mobx, moby, mobz, mobmodel$
    END IF

    SELECT CASE world(worldx, worldy, worldz)
        CASE "undergroundz":

            drawmod 4 * COS(numframe / 20), -3 + SIN(numframe / 41), 4 * SIN(numframe / 20), eye$

        CASE "underground":
            IF mobs > 0 THEN
                IF numframe MOD 100 = 0 THEN _SNDPLAY zap
                IF numframe MOD 100 < 30 THEN
                    IF drawray(mobx, moby, mobz, px, py, pz) = 1 AND RND < .6 AND health > 0 THEN health = health - 10: _SNDPLAY pain
                END IF
            END IF
        CASE "firmament":
            IF mobs > 0 THEN
                mobx = COS(numframe / 7.1) * SQR(px ^ 2 + pz ^ 2) * .9
                mobz = SIN(numframe / 7) * SQR(px ^ 2 + pz ^ 2) * .9
                moby = 5 - 5 * ABS(COS(numframe / 6))
                IF moby > 4.5 THEN _SNDPLAY stomp: bakeobj _ROUND(mobx), _ROUND(moby), _ROUND(mobz), box$
                drawmod mobx, moby - 1, mobz, b$
                IF SQR((mobx - px) ^ 2 + (mobz - pz) ^ 2) < 1 AND py >= moby AND moby > 4 THEN health = 0
            END IF

        CASE "forest":
            IF mobs > 0 THEN
                mobx = COS(numframe / 7) * SQR(px ^ 2 + pz ^ 2) * .9
                mobz = SIN(numframe / 7) * SQR(px ^ 2 + pz ^ 2) * .9
                moby = 5 - 5 * ABS(COS(numframe / 6))
                IF moby > 4.5 THEN _SNDPLAY stomp
                drawmod mobx, moby - 1, mobz, alfred$
                IF SQR((mobx - px) ^ 2 + (mobz - pz) ^ 2) < 1 AND py >= moby AND moby > 4 THEN health = 0
            END IF
    END SELECT
    IF health = 0 THEN GOSUB dead
    drawmod px, py, pz, playermodel$ 'draw player
    drawmod mx, py, mz, test$
    COLOR 87
    PRINT "you have "; blocks; " squares"
    COLOR 4
    LINE (0, hig * .99)-(health / 100 * wid, hig), , BF
    IF pyvel >= .5 AND space(px, py + pyvel, pz) = 1 THEN health = 0
    IF space(px, py, pz) = 1 THEN health = 0
    _LIMIT 50
    _DISPLAY
    CLS
    clz

LOOP

kyle:
_SNDPLAY scream1
SCREEN _NEWIMAGE(512, 256, 32)
kyle = _LOADIMAGE("kyle.jpg")
begintunnel:
_PUTIMAGE (0, 0), kyle
mx = 256
my = 128

DO
    full = 1
    numframe = numframe + 1
    DO WHILE _MOUSEINPUT
        mx = _MOUSEX
        my = _MOUSEY
    LOOP
    magnitude = 30
    FOR I = 1 TO 512 * 256
        sx = RND * 512
        sy = RND * 256
        PSET (sx, sy), (POINT(sx - (sx - mx) / magnitude, sy - (sy - my) / magnitude))
    NEXT
    FOR I = 1 TO 8
        sx = RND * 512
        sy = RND * 256
        IF ABS(_RED32(POINT(sx, sy)) - _RED32(POINT(0, 0))) > 10 OR ABS(_GREEN32(POINT(sx, sy)) - _GREEN32(POINT(0, 0))) > 10 OR ABS(_BLUE32(POINT(sx, sy)) - _BLUE32(POINT(0, 0))) > 10 THEN
            full = 0
        END IF
    NEXT
    IF full = 1 THEN
        GOTO begintunnel
    END IF
    _LIMIT 45
    _DISPLAY

LOOP UNTIL _MOUSEBUTTON(1)
CLS
PRINT "Congratulations!"
PRINT "You beat the game in", numframe / 50, "seconds."
_DISPLAY
SLEEP
SYSTEM
nextstage:
SCREEN _NEWIMAGE(wid, hig, 256)
CLS
PRINT "You Found it!"
_DISPLAY
SLEEP
SYSTEM

'SUBS and FUNCTIONS
dead:
_PRINTMODE _FILLBACKGROUND
py = INT(py)
_SNDPLAY scream1
deadtime = TIMER
drawmod px, py, pz, splat$
DO
    FOR I = 1 TO 5
        drawsph px + COS(I) * (TIMER - deadtime) * RND * 3 + .5, py - 3 + (2 * (TIMER - deadtime) - 1) ^ 2 * 4, pz + SIN(I) * (TIMER - deadtime) * 3 + .5, 4
    NEXT
    _LIMIT 45
    _DISPLAY

LOOP UNTIL TIMER - deadtime > 1

PRINT "You have died. Press any key to respawn"
_DISPLAY
SLEEP
GOTO start
RETURN

'LEVELS
SUB golevel (levelname$)
    _PRINTMODE _KEEPBACKGROUND
    mobs = 0
    blocks = maxblocks
    theta = RND - .5
    COLOR 15
    CLS
    FOR i = 0 TO wid / zbr
        FOR j = 0 TO hig / zbr
            zbuf(i, j) = 1000
        NEXT
    NEXT

    FOR mapx = -63 TO 64
        FOR mapy = -63 TO 64
            FOR mapz = -63 TO 64
                map(mapx, mapy, mapz) = 0
            NEXT
        NEXT
    NEXT

    SELECT CASE levelname$
        CASE "main":
            IF NOT _SNDPLAYING(corozal) THEN _SNDPLAY corozal
            IF _SNDPLAYING(msh) THEN _SNDSTOP msh
            IF _SNDPLAYING(glenkenn) THEN _SNDSTOP glenkenn
            COLOR 15
            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 54
                NEXT
            NEXT

            PRINT , , , "The Wilderness"
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    IF SIN(bx * bz) < -.9 AND ABS(bx) + ABS(bz) < 8 THEN putobj bx, 5, bz, tree$
                    putobj bx, 6, bz, grass$
                NEXT
            NEXT
        CASE "forest":
            IF NOT _SNDPLAYING(corozal) THEN _SNDPLAY corozal
            IF _SNDPLAYING(msh) THEN _SNDSTOP msh
            IF _SNDPLAYING(glenkenn) THEN _SNDSTOP glenkenn

            mobmodel$ = groover$
            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 54
                NEXT
            NEXT
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    IF SIN(bz ^ 3 - bx ^ 2 + worldx ^ 4 - worldz ^ 5) > -.7 THEN
                        putobj bx, 6, bz, grass$
                        IF COS(bx * bz + worldx + worldz) < -.9 AND ABS(bx) + ABS(bz) < 8 THEN putobj bx, 5, bz, tree$
                    ELSE putobj bx, 6, bz, dirt$
                    END IF
                NEXT
            NEXT
            IF SIN(worldx) = SIN(worldz) THEN mobs = 1
        CASE "hole":
            IF NOT _SNDPLAYING(corozal) THEN _SNDPLAY corozal
            IF _SNDPLAYING(msh) THEN _SNDSTOP msh
            IF _SNDPLAYING(glenkenn) THEN _SNDSTOP glenkenn

            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 54
                NEXT
            NEXT
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    IF SQR(bx ^ 2 + bz ^ 2) > 4 THEN
                        IF SIN(bz ^ 3 - bx ^ 2 + worldx ^ 4 - worldz ^ 5) > -.7 THEN
                            putobj bx, 6, bz, grass$
                            IF COS(bx * bz + worldx + worldz) < -.9 AND ABS(bx) + ABS(bz) < 8 THEN putobj bx, 5, bz, tree$
                        ELSE putobj bx, 6, bz, dirt$
                        END IF
                    END IF
                NEXT
            NEXT
        CASE "underground":
            IF NOT _SNDPLAYING(glenkenn) THEN _SNDPLAY glenkenn
            IF _SNDPLAYING(msh) THEN _SNDSTOP msh
            IF _SNDPLAYING(corozal) THEN _SNDSTOP corozal

            mobmodel$ = eye$
            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 36
                NEXT
            NEXT
            columcount = 0
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    putobj bx, 6, bz, dirt$
                    IF world(worldx, worldy - 1, worldz) <> "hole" THEN putobj bx, -8 + _ROUND(SQR(worldx ^ 2 + worldz ^ 2) / 2 + COS(bx - worldx) + SIN(bz + worldz)), bz, stalag$
                    IF bx / bz < worldx / worldz AND ABS(bz) < 8 AND ABS(bx) < 8 AND SIN(bz - bx) < COS(bz * bx) AND COS(bx + bz + worldx - worldz) < .01 AND SIN(bx * bz) > .9 THEN
                        FOR height = 0 TO (bx ^ 2 + bz ^ 2) MOD 10
                            putobj bx, 5 - height, bz, column$
                        NEXT
                        IF RND < .9 THEN
                            putobj bx, 4 - ((bx ^ 2 + bz ^ 2) MOD 10), bz, box$
                        ELSE
                            putobj bx, 4 - ((bx ^ 2 + bz ^ 2) MOD 10), bz, patrick$
                        END IF
                        columcount = columcount + 1
                    END IF
                NEXT
            NEXT
            IF columcount = 0 AND RND < .3 THEN
                mobs = 1
                FOR i = 1 TO 5
                    putobj _ROUND(4 * COS(i)), 5, _ROUND(4 * SIN(i)), raise$
                    IF RND > .2 THEN
                        drawmod _ROUND(4 * COS(i)), 4, _ROUND(4 * SIN(i)), splat$
                    ELSE
                        putobj _ROUND(4 * COS(i)), 4, _ROUND(4 * SIN(i)), plus$
                    END IF
                NEXT
                putobj 0, 5, 0, giantlegs$
                putobj 0, 4, 0, gianttorso$
                mobx = 0
                moby = 3
                mobz = 0
            END IF
        CASE "undergroundz":
            IF NOT _SNDPLAYING(glenkenn) THEN _SNDPLAY glenkenn
            IF _SNDPLAYING(msh) THEN _SNDSTOP msh
            IF _SNDPLAYING(corozal) THEN _SNDSTOP corozal

            mobs = 0
            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 36
                NEXT
            NEXT
            FOR bz = -4 TO 4
                FOR bx = -4 TO 4
                    putobj bx, 2, bz, dirt$
                    FOR height = 1 TO 2 * SQR(bx ^ 2 + bz ^ 2) - 7 STEP -1
                        putobj bx, height, bz, raise$
                    NEXT
                NEXT
            NEXT
            FOR height = 2 TO 5
                putobj -4, height, -4, column$
                putobj -4, height, 4, column$
                putobj 4, height, -4, column$
                putobj 4, height, 4, column$
            NEXT
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    putobj bx, 6, bz, dirt$
                NEXT
            NEXT
            COLOR 4
            PRINT , , , "The Ziggurat"
        CASE "firmament":
            IF _SNDPLAYING(corozal) THEN _SNDSTOP corozal
            IF NOT _SNDPLAYING(msh) THEN _SNDPLAY msh
            mobmodel$ = groover$
            mobs = RND * 100 MOD 2
            FOR screenx = 0 TO wid
                FOR screeny = 0 TO hig
                    PSET (screenx, screeny), 15
                NEXT
            NEXT
            FOR bz = -8 TO 8
                FOR bx = -8 TO 8
                    putobj bx, 6, bz, sky$
                NEXT
            NEXT
            COLOR 4
            PRINT , , , "The Firmament"

        CASE ELSE:
            COLOR 7
            PRINT , , , "The Void"
            IF py > 0 THEN putobj _ROUND(px), _ROUND(py) + 2, _ROUND(pz), sky$
    END SELECT


    GET (0, 0)-(wid - 1, hig - 1), bg()
    FOR i = 0 TO wid / zbr
        FOR j = 0 TO hig / zbr
            bgzbuf(i, j) = zbuf(i, j)
        NEXT
    NEXT
END SUB

'MOVING
FUNCTION space (sx, sy, sz)
    space = map(_ROUND(sx), _ROUND(sy), _ROUND(sz))
END FUNCTION

FUNCTION drawray (alx1, aly1, alz1, alx2, aly2, alz2)
    lx2 = alx2 + .5
    ly2 = aly2 + .5
    lz2 = alz2 + .5
    magnitude = SQR((lx2 - alx1) ^ 2 + (ly2 - aly1) ^ 2 + (lz2 - alz1) ^ 2) * 8
    lx = alx1 + .5
    ly = aly1 + .5
    lz = alz1 + .5
    DO
        drawsph lx, ly, lz, 56 + RND * 16
        lx = lx + (lx2 - alx1 - .5) / magnitude
        ly = ly + (ly2 - aly1 - .5) / magnitude
        lz = lz + (lz2 - alz1 - .5) / magnitude
    LOOP UNTIL SQR((lx - lx2) ^ 2 + (ly - ly2) ^ 2 + (lz - lz2) ^ 2) < .1 OR SQR((lx - alx1) ^ 2 + (ly - aly1) ^ 2 + (lz - alz1) ^ 2) > 5
    IF SQR((lx - lx2) ^ 2 + (ly - ly2) ^ 2 + (lz - lz2) ^ 2) < 1 THEN drawray = 1 ELSE drawray = 0
END FUNCTION

SUB walk (dir)
    SELECT CASE dir
        CASE 0:
            IF space((px), py, pz + pspeed) <> 1 THEN pz = pz + pspeed
        CASE 1:
            IF space(px + pspeed, py, pz) <> 1 THEN px = px + pspeed
        CASE 2:
            IF space(px, py, pz - pspeed) <> 1 THEN pz = pz - pspeed
        CASE 3:
            IF space(px - pspeed, py, pz) <> 1 THEN px = px - pspeed

    END SELECT
END SUB
'OBJECTS
SUB putobj (ox, oy, oz, mdl$)
    drawmod ox, oy, oz, mdl$
    map(ox, oy, oz) = 1
END SUB
'RENDERING
SUB bakeobj (ox, oy, oz, mdl$)
    CLS
    PUT (0, 0), bg() 'draw level
    putobj ox, oy, oz, mdl$
    FOR i = 0 TO wid / zbr
        FOR j = 0 TO hig / zbr
            bgzbuf(i, j) = zbuf(i, j)
        NEXT
    NEXT
    GET (0, 0)-(wid - 1, hig - 1), bg()
END SUB
SUB clz
    FOR i = 0 TO wid STEP zbr
        FOR j = 0 TO hig STEP zbr
            zbuf(i / zbr, j / zbr) = bgzbuf(i / zbr, j / zbr)
        NEXT
    NEXT
END SUB

SUB drawsquare (x1, y1, x2, y2, z1)
    FOR sqx = x1 - x2 / 2 TO x1 + x2 / 2
        FOR sqy = y1 - y2 / 2 TO y1 + y2 / 2
            asqx = sqx
            IF sqx < 0 THEN asqx = 0
            IF sqx > wid THEN asqx = wid
            asqy = sqy
            IF sqy < 0 THEN asqy = 0
            IF sqy > hig THEN asqy = hig

            IF z1 <= zbuf(asqx / zbr, asqy / zbr) THEN
                PSET (asqx, asqy)
                zbuf(asqx / zbr, asqy / zbr) = z1
            END IF
        NEXT
    NEXT
END SUB

SUB drawsph (vx, vy, vz, vc)
    vt = theta
    st = ATN(vz / (vx + .0001))
    sr = SQR(vx ^ 2 + vz ^ 2)
    xx = sr * COS(st + vt) * (vx + .0001) / ABS(vx + .0001)
    zz = sr * SIN(st + vt) * (vx + .0001) / ABS(vx + .0001)
    COLOR vc
    vr = (wid + hig / 2) / 100
    az = ABS((zz + 16) / 3) + .1
    IF vc > 0 THEN
        drawsquare wid / 2 + xx * wid / 4 / az, hig / 2 + vy * hig / 4 / az, vr / az, vr / az, zz
    END IF

END SUB


SUB drawmod (mx, my, mz, mdl$)
    FOR index = 1 TO LEN(mdl$) - 3 STEP 4
        voxstr$ = MID$(mdl$, index, 4)
        dx = ASC(MID$(voxstr$, 1, 1))
        dy = ASC(MID$(voxstr$, 2, 1))
        dz = ASC(MID$(voxstr$, 3, 1))
        dc = ASC(MID$(voxstr$, 4, 1))
        drawsph mx + dx / 32, my + dy / 32, mz + dz / 32, dc
    NEXT

END SUB


SUB loadmod (mm$, dest$):
    dest$ = ""
    IF _FILEEXISTS(mm$) THEN
        OPEN mm$ FOR INPUT AS #1
        DO UNTIL EOF(1)
            INPUT #1, lx%, ly%, lz%, lc%
            dest$ = dest$ + CHR$(lx%) + CHR$(ly%) + CHR$(lz%) + CHR$(lc%)
        LOOP
        PRINT mm$, "loaded successfully"
        CLOSE #1
    ELSE
        PRINT "File not found..."
    END IF

END FUNCTION
