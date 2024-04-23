'OPTION _EXPLICIT
'$DYNAMIC
'$Debug
'Title:   add_cues.bas
'Date:    8/22/94
'Author:  CHARLES ROBERT HILL
'Description: This program creates a stereo wave file from a mono wave
'file. It does this y modeling the Haas Effect (Precedence Effect), and the
'head shadow.  These are discretely modeled in a data file.  This program
'reads the files and creates the stereo file.

' FILE NUM   FILE
' --------   -----
'       #1   CONFIG.DAT
'       #2   WAVEIN
'       #3   WAVEOUT
'       #4   LOGFILE.DAT

Cls
Screen 11
WIDTH 80, 60

'MISC
Dim LOGFILE        AS STRING
Dim CONFIGFILE     AS STRING
DIM PI             AS SINGLE
DIM NINETY_DEGREES AS SINGLE
DIM CRLF           AS STRING * 2
Dim WAVEDATAOFFSET AS INTEGER

LOGFILE        = "output.log"
CONFIGFILE     = "config.dat"
PI             = 3.14159265 
NINETY_DEGREES = 2 * PI / 4
CRLF           = CHR$(13) + CHR$(10)
WAVEDATAOFFSET = 45 'NOTE: CALCULATE FROM THE WAVE FILE

'SCREEN TO UNIVERSAL COORDINATES
DIM SXL AS INTEGER
DIM SXR AS INTEGER
DIM SYT AS INTEGER
DIM SYB AS INTEGER

DIM uxl AS INTEGER
DIM uxr AS INTEGER
DIM uyb AS INTEGER
DIM uyt AS INTEGER

SXL = 0
SXR = 639
SYT = 0 
SYB = 480

'HEADER OF A WAVE FILE
Type waveHeader
    Riff            As String * 4   '4 BYTES
    RiffLen         As Long         '4 BYTES
    Wave            As String * 4   '4 BYTES
    Fmt             As String * 4   '4 BYTES
    FmtLen          As Long         '4 BYTES
    FmtTag          As Integer      '2 BYTES
    channels        As Integer      '2 BYTES
    SamplesPerSec   As Long         '4 BYTES
    AvgBytesPerSec  As Long         '4 BYTES
    BlockAlign      As Integer      '2 BYTES
    BitsPerSample   As Integer      '2 BYTES
    Dat             As String * 4   '4 BYTES
    DataLen         As Long         '4 BYTES
End Type

Dim WAVE_IN_HEADER As waveHeader
Dim WAVE_OUT_HEADER As waveHeader

'VARIABLES USED TO LOAD DATAFILE
Dim VERSION     AS String
Dim TITLE       AS String
Dim WAVEIN      AS String
Dim WAVEOUT     AS String
Dim C           AS SINGLE
Dim K           AS SINGLE
Dim HR          AS SINGLE
Dim N           AS INTEGER
DIM PP          AS INTEGER

DIM i           AS SINGLE

DIM EAR_LEFT_ANGLE  AS SINGLE
DIM EAR_RIGHT_ANGLE AS SINGLE 

DIM XL, YL AS SINGLE
DIM XR, YR AS SINGLE

DIM DL,DR AS SINGLE

DIM LEFTOFFSET  AS SINGLE
DIM RIGHTOFFSET AS SINGLE

DIM THIS_SEGMENT AS INTEGER
DIM NEXT_SEGMENT AS INTEGER
Dim LAST_SEGMENT AS INTEGER

DIM STOPUTMIN AS SINGLE
DIM STOPUTMAX AS SINGLE

DIM LEFT_C1 AS SINGLE
DIM LEFT_C2 AS SINGLE
DIM LEFT_C3 AS SINGLE

DIM RIGHT_C1 AS SINGLE
DIM RIGHT_C2 AS SINGLE
DIM RIGHT_C3 AS SINGLE

DIM STOPUT               AS SINGLE
DIM LEFT_S_IN_FLOAT      AS SINGLE
DIM RIGHT_S_IN_FLOAT     AS SINGLE
DIM LEFT_S_IN_POS_0      AS INTEGER
DIM LEFT_S_IN_POS_1      AS INTEGER
DIM RIGHT_S_IN_POS_0     AS INTEGER
DIM RIGHT_S_IN_POS_1     AS INTEGER
DIM LEFT_S_IN_VAL_0      AS STRING * 1
DIM LEFT_S_IN_VAL_1      AS STRING * 1
DIM RIGHT_S_IN_VAL_0     AS STRING * 1
DIM RIGHT_S_IN_VAL_1     AS STRING * 1
DIM LEFT_S_IN_VAL_0_INT  AS INTEGER
DIM LEFT_S_IN_VAL_1_INT  AS INTEGER
DIM RIGHT_S_IN_VAL_0_INT AS INTEGER
DIM RIGHT_S_IN_VAL_1_INT AS INTEGER

DIM LEFT_S_IN_VAL_INTERP     AS SINGLE
DIM LEFT_S_IN_VAL_INTER_INT  AS INTEGER
DIM LEFT_S_IN_VAL_INTERP_OUT AS STRING * 1
DIM LEFT_FILE_POSTION        AS INTEGER

DIM RIGHT_S_IN_VAL_INTERP     AS SINGLE
DIM RIGHT_S_IN_VAL_INTER_INT  AS INTEGER
DIM RIGHT_S_IN_VAL_INTERP_OUT AS STRING * 1
DIM RIGHT_FILE_POSTION        AS INTEGER


'CREATE A NEW OUTPUT.LOG
OPEN LOGFILE FOR OUTPUT AS #4
Print #4, DATE$, TIME$

'=============================================================================
' READ CONFIG FILE
'=============================================================================
CALL LOGPRINT(CRLF + "READING CONFIGURATION FROM CONFIG.DAT")

Open CONFIGFILE For Input As #1
Input #1, VERSION
Input #1, TITLE
Input #1, WAVEIN
Input #1, WAVEOUT
Input #1, C
Input #1, K
Input #1, HR
Input #1, N
Input #1, PP

CALL LOGPRINT("    VERSION = " + VERSION)
CALL LOGPRINT("    TITLE   = " + TITLE)
CALL LOGPRINT("    WAVEIN  = " + WAVEIN)
CALL LOGPRINT("    WAVEOUT = " + WAVEOUT)
CALL LOGPRINT("    C (speed of sound)      = " + STR$(C))
CALL LOGPRINT("    K (decay factor)        = " + STR$(K))
CALL LOGPRINT("    HR (head radius)        = " + STR$(HR))
CALL LOGPRINT("    N  (data points)        = " + STR$(N))
CALL LOGPRINT("    PP (ear profile points) = " + STR$(PP))

'GAIN AS A FUNCTION OF EAR TO SOURCE SQUINT ANGLE
ReDim PPTHETA(PP)   AS SINGLE
ReDim PPGAIN(PP)    AS SINGLE

'TIME SERIES
ReDim t(N)          AS SINGLE

'HEAD CENTER AND ANGLE IN DEGREES AND RADIANS
ReDim XHC(N), YHC(N), HA(N), HAR(N) AS SINGLE
ReDim XS(N), YS(N) AS SINGLE 'SOURCE COORDINATE
ReDim SOUTLEFT(N), SOUTRIGHT(N), SAMPLEIN(N) AS SINGLE 'SAMPLE IN INPUT AND OUTPUT WAVE FILES

'READ THE EAR PROFILES
CALL LOGPRINT(CRLF + "EAR PROFILE POINTS")
CALL LOGPRINT("    i    THETA(i)    GAIN(i)")
For i = 1 To PP
    Input #1, PPTHETA(i), PPGAIN(i)
    CALL LOGPRINT("    " + STR$(i) + "     " + STR$(PPTHETA(i)) + "     " + STR$(PPGAIN(i)))
Next i

'TIME
CALL LOGPRINT(CRLF +"READING TIME SERIES")
CALL LOGPRINT("    i    t(i)")
For i = 1 To N
    Input #1, t(i)
    CALL LOGPRINT("    " + STR$(i) + "     " + STR$(t(i)))
Next i

'HEAD
CALL LOGPRINT(CRLF +"READING HEAD CENTER COORDINATE SERIES")
CALL LOGPRINT("    i    XHC(i)    YHC(i)    HA(i)    HAR(I)")
For i = 1 To N
    Input #1, XHC(i), YHC(i), HA(i)
    HAR(i) = HA(i) * PI / 180
    CALL LOGPRINT("    " + STR$(i) + "    " + STR$(XHC(i)) + "    " + STR$(YHC(i)) + "    " + STR$(HA(i)) + "    " + STR$(HAR(i)) )
Next i

'SOURCE
CALL LOGPRINT(CRLF + "READING SOURCE COORDINATE SERIES")
CALL LOGPRINT("    i    XS(i)    YS(i)")
For i = 1 To N
    Input #1, XS(i), YS(i)
    CALL LOGPRINT("    " + STR$(i) + "    " + STR$(XS(i)) + "    " + STR$(YS(i)))
Next i

'CLOSE THE CONFIGURATION FILE
Close #1

'=============================================================================
' READ WAVEINFILE
'=============================================================================

'READ THE HEADER OF THE WAVEIN FILE
Open WAVEIN$ For Binary As #2
Get #2, , WAVE_IN_HEADER

'DISPLAY THE FILE STRUCTURE
CALL LOGPRINT(CRLF + "WAVE_IN_HEADER")
CALL LOGPRINT("    RIFF           = " + WAVE_IN_HEADER.Riff)
CALL LOGPRINT("    RiffLen        = " + STR$(WAVE_IN_HEADER.RiffLen))
CALL LOGPRINT("    Wave           = " + WAVE_IN_HEADER.Wave)
CALL LOGPRINT("    Fmt            = " + WAVE_IN_HEADER.Fmt)
CALL LOGPRINT("    FmtLen         = " + STR$(WAVE_IN_HEADER.FmtLen))
CALL LOGPRINT("    FmtTag         = " + STR$(WAVE_IN_HEADER.FmtTag))
CALL LOGPRINT("    channels       = " + STR$(WAVE_IN_HEADER.channels))
CALL LOGPRINT("    SamplesPerSec  = " + STR$(WAVE_IN_HEADER.SamplesPerSec))
CALL LOGPRINT("    AvgBytesPerSec = " + STR$(WAVE_IN_HEADER.AvgBytesPerSec))
CALL LOGPRINT("    BlockAlign     = " + STR$(WAVE_IN_HEADER.BlockAlign))
CALL LOGPRINT("    BitsPerSample  = " + STR$(WAVE_IN_HEADER.BitsPerSample))
CALL LOGPRINT("    Dat            = " + WAVE_IN_HEADER.Dat)
CALL LOGPRINT("    DataLen        = " + STR$(WAVE_IN_HEADER.DataLen))


'=============================================================================
' CALCULATING THE INTERAURAL DIFFERENCES AT EACH INDEX / TIME
' MAP WAVE_IN THE SAMPLES TO THE WAVE_OUT LEFT AND RIGHT CHANNELS
'=============================================================================
CALL LOGPRINT(CRLF + "CALCULATING IAD'S")

'GO THROUGH THE DATA
For i = 1 To N

    'CALCULATE EAR COORDINATES
    'LEFT EAR IS 90 DEGREES CCW FROM THE NOSE
    EAR_LEFT_ANGLE  =  HAR(i) + NINETY_DEGREES

    'LEFT EAR IS 90 DEGREES CW FROM THE NOSE
    EAR_RIGHT_ANGLE =  HAR(i) - NINETY_DEGREES

    XL = XHC(i) + (HR * Cos(EAR_LEFT_ANGLE))
    YL = YHC(i) + (HR * Sin(EAR_LEFT_ANGLE))

    XR = XHC(i) + (HR * Cos(EAR_RIGHT_ANGLE))
    YR = YHC(i) + (HR * Sin(EAR_RIGHT_ANGLE))

    'CALCULATE DISTANCE TO EACH EAR AT TOLD
    'LEFT DISTANCE
    DL = Sqr((XS(i) - XL) ^ 2 + (YS(i) - YL) ^ 2)
    'RIGHT DISTANCE
    DR = Sqr((XS(i) - XR) ^ 2 + (YS(i) - YR) ^ 2)

    'PUSH SAMPLES IN THE OUTFILE TO THE LEFT BASED ON THE TIME LAG
    'THIS KEEPS THE OUTPUT IN SYNC WITH THE INPUT BY ADVANCING ALL
    'SAMPLE LOCATIONS IN THE WAVE_OUT FILE BY THE AMOUNT OF
    'INITIAL SILENCE
    If i = 1 Then
        'CALCULATE THE SAMPLE OFFSETS
        LEFTOFFSET  = Int(WAVE_IN_HEADER.SamplesPerSec * (t(1) + DL / C))
        RIGHTOFFSET = Int(WAVE_IN_HEADER.SamplesPerSec * (t(1) + DR / C))

        CALL LOGPRINT(CRLF + "    INITIAL CHANNEL OFFSETS")
        CALL LOGPRINT("        LEFTOFFSET  = " + STR$(LEFTOFFSET))
        CALL LOGPRINT("        RIGHTOFFSET = " + STR$(RIGHTOFFSET))
    End If

    CALL LOGPRINT("")
    CALL LOGPRINT("-----------------------------------------------")
    CALL LOGPRINT("    SEGMENT = " + STR$(I))

    CALL LOGPRINT("")
    CALL LOGPRINT("    SOURCE POSITION")
    CALL LOGPRINT("        XS(i) = " + STR$(XS(i)))
    CALL LOGPRINT("        YS(i) = " + STR$(YS(I)))

    CALL LOGPRINT("")
    CALL LOGPRINT("    HEAD POSITION")
    CALL LOGPRINT("        XHC(i) = " + STR$(XHC(i)))
    CALL LOGPRINT("        YHC(i) = " + STR$(YHC(i)))
    CALL LOGPRINT("        HA(i)  = " + STR$(HA(i)))
    CALL LOGPRINT("        HAR(i) = " + STR$(HAR(i)))

    CALL LOGPRINT("")
    CALL LOGPRINT("    EAR POSITIONS AND AZMITH")
    CALL LOGPRINT("        XL = " + STR$(XL))
    CALL LOGPRINT("        YL = " + STR$(YL))
    CALL LOGPRINT("        DL = " + STR$(DL))
    CALL LOGPRINT("")
    CALL LOGPRINT("        XR = " + STR$(XR))
    CALL LOGPRINT("        YR = " + STR$(YR))
    CALL LOGPRINT("        DR = " + STR$(DR))
    CALL LOGPRINT("")
    CALL LOGPRINT("        DISTANCE_CHECK = " + STR$(DISTANCE_CHECK))

    'CALCULATE THE SAMPLE # OF THE SAMPLE IN THE INPUT FILE THAT
    'CORRESPONDS TO T(i)
    SAMPLEIN(i) = Int(WAVE_IN_HEADER.SamplesPerSec * t(i))

    'OUTPUT MAPPING
    'CALCULATE THE SAMPLE FROM THE INPUT AUDIO FILE THAT
    'SHOULD BE ARRIVING AT EACH EAR AT THIS TIME
    DIM LEFT_DELAY AS SINGLE
    DIM RIGHT_DELAY AS SINGLE

    'CALCLATE HOW LONG AGO THE SOUND EMINATED FORM THE SOURCE
    LEFT_DELAY  = DL / C
    RIGHT_DELAY = DR / C

    SOUTLEFT(i)  = WAVE_IN_HEADER.SamplesPerSec * (t(i) + LEFT_DELAY) - LEFTOFFSET
    SOUTRIGHT(i) = WAVE_IN_HEADER.SamplesPerSec * (t(i) + RIGHT_DELAY) - RIGHTOFFSET

    CALL LOGPRINT("")
    CALL LOGPRINT("        SAMPLEIN(i)  = " + STR$(SAMPLEIN(i)))
    CALL LOGPRINT("        SOUTLEFT(i)  = " + STR$(SOUTLEFT(i)))
    CALL LOGPRINT("        SOUTRIGHT(i) = " + STR$(SOUTRIGHT(i)))

Next i



'SECTION 2 = USING THE INPUT TO OUTPUT MAPPING AS THE BASIS
'    FOR A SPLINE, THAT IS USED TO MAP INPUT SAMPLE NUMBER
'    TO OUTPUT SAMPLE NUMBER FOR EACH THE LEFT AND 
'    RIGHT CHANNEL IN THE WAVEOUT FILE

'CREATE OUTPUT FILE
CALL LOGPRINT("")
CALL LOGPRINT("CREATING WAVEOUT FILE: " + WAVEOUT)
Open WAVEOUT$ For Binary As #3

'CREATE THE WAVE FILE HEADER
WAVE_OUT_HEADER.RIFF           = "RIFF"
WAVE_OUT_HEADER.RiffLen        = WAVE_IN_HEADER.RiffLen * 2
WAVE_OUT_HEADER.Wave           = "WAVE"
WAVE_OUT_HEADER.Fmt            = WAVE_IN_HEADER.Fmt
WAVE_OUT_HEADER.FmtLen         = 16
WAVE_OUT_HEADER.FmtTag         = 1
WAVE_OUT_HEADER.channels       = 2
WAVE_OUT_HEADER.BitsPerSample  = 8
WAVE_OUT_HEADER.SamplesPerSec  = WAVE_IN_HEADER.SamplesPerSec
WAVE_OUT_HEADER.AvgBytesPerSec = WAVE_OUT_HEADER.SamplesPerSec * WAVE_OUT_HEADER.channels * WAVE_OUT_HEADER.BitsPerSample / 8
WAVE_OUT_HEADER.BlockAlign     = WAVE_OUT_HEADER.channels * WAVE_OUT_HEADER.BitsPerSample / 8
WAVE_OUT_HEADER.Dat            = WAVE_IN_HEADER.Dat
WAVE_OUT_HEADER.DataLen        = SOUTRIGHT(N) * 2 + 1

' 4         4   ChunkSize        36 + SubChunk2Size, or more precisely:
'                                4 + (8 + SubChunk1Size) + (8 + SubChunk2Size)
'                                This is the size of the rest of the chunk 
'                                following this number.  This is the size of the 
'                                entire file in bytes minus 8 bytes for the
'                                two fields not included in this count:
'                                ChunkID and ChunkSize.

'DISPLAY THE FILE STRUCTURE
CALL LOGPRINT("    RIFF           = " + WAVE_OUT_HEADER.Riff)
CALL LOGPRINT("    RiffLen        = " + STR$(WAVE_OUT_HEADER.RiffLen))
CALL LOGPRINT("    Wave           = " + WAVE_OUT_HEADER.Wave)
CALL LOGPRINT("    Fmt            = " + WAVE_OUT_HEADER.Fmt)
CALL LOGPRINT("    FmtLen         = " + STR$(WAVE_OUT_HEADER.FmtLen))
CALL LOGPRINT("    FmtTag         = " + STR$(WAVE_OUT_HEADER.FmtTag))
CALL LOGPRINT("    channels       = " + STR$(WAVE_OUT_HEADER.channels))
CALL LOGPRINT("    SamplesPerSec  = " + STR$(WAVE_OUT_HEADER.SamplesPerSec))
CALL LOGPRINT("    AvgBytesPerSec = " + STR$(WAVE_OUT_HEADER.AvgBytesPerSec))
CALL LOGPRINT("    BlockAlign     = " + STR$(WAVE_OUT_HEADER.BlockAlign))
CALL LOGPRINT("    BitsPerSample  = " + STR$(WAVE_OUT_HEADER.BitsPerSample))
CALL LOGPRINT("    Dat            = " + WAVE_OUT_HEADER.Dat)
CALL LOGPRINT("    DataLen        = " + STR$(WAVE_OUT_HEADER.DataLen))

Put #3, , WAVE_OUT_HEADER

'LEFT CHANNEL
'INITIALIZE VARIABLES FOR THE LOOP

'CALCULATE SPLINE COEFICIENTS FOR DELAY AND ATTENATION
ReDim LS(N, 4), RS(N, 4) 'SAMPLE SPLINE COEFICIENTS
'ReDim LA(N, 4), RA(N, 4) 'ATTENUATION SPLINE COEFICIENTS
'ReDim LP(N, 4), RP(N, 4) 'EAR PROFILE SPLINE COEFICIENTS

'REDIMENSION MATRIX ARRAYS
ReDim a(4, 4), b(4), C(50, 4) 'MATRIX VARIABLES
ReDim x(N+1), Y(N+1) 'PASSED ARGUMENTS

'STAGE 
'RESAMPLE LEFT CHANNEL
Call SPLINE(N, SOUTLEFT(), SAMPLEIN(), LS())
Call SPLINE(N, SOUTRIGHT(), SAMPLEIN(), RS())
'Call SPLINE2(N, SOUTLEFT(), LATTEN(), LA())

'GO THROUGH EACH OF THE SEGMENTS
'WITHIN THAT SEGMENT ITERATE THROUGH THE SAMPLES IN TH WAVEOUT
'USE THE SPLINE TO FIND THE SAMPLES NEEDED FROM THE WAVEIN

LAST_SEGMENT = N - 1

For i = 1 To LAST_SEGMENT Step 1

    THIS_SEGMENT = i
    NEXT_SEGMENT = i + 1

    CALL LOGPRINT("")
    CALL LOGPRINT("LEFT CHANNEL")
    CALL LOGPRINT("    SEGEMENT     = " + STR$(THIS_SEGMENT) + " to " + STR$(NEXT_SEGMENT))
    CALL LOGPRINT("        OUTPUT RANGE = " + STR$(SOUTLEFT(THIS_SEGMENT)) + "    " + STR$(SOUTLEFT(NEXT_SEGMENT)))
    CALL LOGPRINT("        INPUT_RANGE  = " + STR$(SAMPLEIN(THIS_SEGMENT)) + "    " + STR$(SAMPLEIN(NEXT_SEGMENT)))

    'SET VARAIBLES FOR THE LOOP
    STOPUTMIN = SOUTLEFT(THIS_SEGMENT)
    STOPUTMAX = SOUTLEFT(NEXT_SEGMENT)

    CALL LOGPRINT("")
    CALL LOGPRINT("        SAMPLE RANGE IN OUTPUT = " + STR$(STOPUTMIN) + "    " + STR$(STOPUTMAX))
    CALL LOGPRINT("        SAMPLE RANGE IN INPUT  = " + STR$(SAMPLEIN(THIS_SEGMENT)) + "    " + STR$(SAMPLEIN(NEXT_SEGMENT)))

    'GET THE POLYNOMOIAL CONSTANTS FOR THIS SEGMENT
    LEFT_C1 = LS(THIS_SEGMENT, 1)
    LEFT_C2 = LS(THIS_SEGMENT, 2)
    LEFT_C3 = LS(THIS_SEGMENT, 3)

    RIGHT_C1 = RS(THIS_SEGMENT, 1)
    RIGHT_C2 = RS(THIS_SEGMENT, 2)
    RIGHT_C3 = RS(THIS_SEGMENT, 3)

    'GO THROUGH ALL OUTPUT FILE SAMPLE POSITIONS
    'FIND THE INPUT FILE POSITION
    'GET THE INPUT FILE VALUE
    For STOPUT = INT(STOPUTMIN) To INT(STOPUTMAX) 'LOOP OVER THE RANGE OF POINTS IN THE WAVEOUt

        'GET THE INPUT SAMPLE NUMBER FROM THE SPLINE
        'THAT MAPS OUTPUT SAMPLE POSITIONS TO INPUT
        'SAMPLE POSITIONS
        LEFT_S_IN_FLOAT  = LEFT_C1  * (STOPUT * STOPUT) + LEFT_C2  * STOPUT + LEFT_C3 'LIKELY NOT AN INTEGER
        RIGHT_S_IN_FLOAT = RIGHT_C1 * (STOPUT * STOPUT) + RIGHT_C2 * STOPUT + RIGHT_C3 'LIKELY NOT AN INTEGER

        'GET THE SAMPLE WINDOW
        LEFT_S_IN_POS_0  = INT(LEFT_S_IN_FLOAT)
        LEFT_S_IN_POS_1  = LEFT_S_IN_POS_0 + 1

        RIGHT_S_IN_POS_0 = INT(RIGHT_S_IN_FLOAT)
        RIGHT_S_IN_POS_1 = RIGHT_S_IN_POS_0 + 1

        'GET THE SAMPLES VALUES
        LEFT_S_IN_VAL_0$ = " "
        LEFT_S_IN_VAL_1$ = " "
        Get #2, LEFT_S_IN_POS_0 + 45, LEFT_S_IN_VAL_0$
        Get #2, LEFT_S_IN_POS_1 + 45, LEFT_S_IN_VAL_1$

        RIGHT_S_IN_VAL_0$ = " "
        RIGHT_S_IN_VAL_1$ = " "
        Get #2, RIGHT_S_IN_POS_0 + 45, RIGHT_S_IN_VAL_0$
        Get #2, RIGHT_S_IN_POS_1 + 45, RIGHT_S_IN_VAL_1$

        ' CONVERT THE BYTE VALUE TO AN INTEGER FOR INTERPOLATION
        LEFT_S_IN_VAL_0_INT = ASC(LEFT_S_IN_VAL_0$)
        LEFT_S_IN_VAL_1_INT = ASC(LEFT_S_IN_VAL_1$)

        RIGHT_S_IN_VAL_0_INT = ASC(RIGHT_S_IN_VAL_0$)
        RIGHT_S_IN_VAL_1_INT = ASC(RIGHT_S_IN_VAL_1$)

        'INERPOLATE BEWEEN THE INPUT WINDOW POSITION AND THE 
        LEFT_S_IN_VAL_INTERP      = LINTERP(LEFT_S_IN_POS_0, LEFT_S_IN_POS_1, LEFT_S_IN_VAL_0_INT, LEFT_S_IN_VAL_1_INT, LEFT_S_IN_FLOAT)
        LEFT_S_IN_VAL_INTER_INT   = INT(LEFT_S_IN_VAL_INTERP)
        LEFT_S_IN_VAL_INTERP_OUT$ = CHR$(LEFT_S_IN_VAL_INTER_INT)
        LEFT_FILE_POSTION         = STOPUT * 2 + 45

        RIGHT_S_IN_VAL_INTERP      = LINTERP(RIGHT_S_IN_POS_0, RIGHT_S_IN_POS_1, RIGHT_S_IN_VAL_0_INT, RIGHT_S_IN_VAL_1_INT, RIGHT_S_IN_FLOAT)
        RIGHT_S_IN_VAL_INTER_INT   = INT(RIGHT_S_IN_VAL_INTERP)
        RIGHT_S_IN_VAL_INTERP_OUT$ = CHR$(RIGHT_S_IN_VAL_INTER_INT)
        RIGHT_FILE_POSTION         = STOPUT * 2 + 45 + 1

        CALL LOGPRINT("")
        CALL LOGPRINT("STOPUT               = " + STR$(STOPUT))
        CALL LOGPRINT("")
        CALL LOGPRINT("LEFT_S_IN_FLOAT           = " + STR$(LEFT_S_IN_FLOAT))
        CALL LOGPRINT("LEFT_S_IN_POS_0           = " + STR$(LEFT_S_IN_POS_0))
        CALL LOGPRINT("LEFT_S_IN_POS_1           = " + STR$(LEFT_S_IN_POS_1))
        CALL LOGPRINT("LEFT_S_IN_VAL_0_INT       = " + STR$(LEFT_S_IN_VAL_0_INT))
        CALL LOGPRINT("LEFT_S_IN_VAL_1_INT       = " + STR$(LEFT_S_IN_VAL_1_INT))
        CALL LOGPRINT("LEFT_S_IN_VAL_INTERP      = " + STR$(LEFT_S_IN_VAL_INTERP))
        CALL LOGPRINT("LEFT_S_IN_VAL_INTER_INT   = " + STR$(LEFT_S_IN_VAL_INTER_INT))
        CALL LOGPRINT("LEFT_S_IN_VAL_INTERP_OUT  = " + LEFT_S_IN_VAL_INTERP_OUT$)
        CALL LOGPRINT("LEFT_FILE_POSTION         = " + STR$(LEFT_FILE_POSTION))

        CALL LOGPRINT("")
        CALL LOGPRINT("RIGHT_S_IN_FLOAT           = " + STR$(RIGHT_S_IN_FLOAT))
        CALL LOGPRINT("RIGHT_S_IN_POS_0           = " + STR$(RIGHT_S_IN_POS_0))
        CALL LOGPRINT("RIGHT_S_IN_POS_1           = " + STR$(RIGHT_S_IN_POS_1))
        CALL LOGPRINT("RIGHT_S_IN_VAL_0_INT       = " + STR$(RIGHT_S_IN_VAL_0_INT))
        CALL LOGPRINT("RIGHT_S_IN_VAL_1_INT       = " + STR$(RIGHT_S_IN_VAL_1_INT))
        CALL LOGPRINT("RIGHT_S_IN_VAL_INTERP      = " + STR$(RIGHT_S_IN_VAL_INTERP))
        CALL LOGPRINT("RIGHT_S_IN_VAL_INTER_INT   = " + STR$(RIGHT_S_IN_VAL_INTER_INT))
        CALL LOGPRINT("RIGHT_S_IN_VAL_INTERP_OUT  = " + RIGHT_S_IN_VAL_INTERP_OUT$)
        CALL LOGPRINT("RIGHT_FILE_POSTION         = " + STR$(RIGHT_FILE_POSTION))

        Put #3, LEFT_FILE_POSTION,  LEFT_S_IN_VAL_INTERP_OUT$
        Put #3, RIGHT_FILE_POSTION, RIGHT_S_IN_VAL_INTERP_OUT$
    Next stoput
Next i

'CLOSE OPEN FILES
CALL LOGPRINT("")
CALL LOGPRINT("DONE")
CLOSE #2
CLOSE #3
CLOSE #4


FUNCTION LINTERP(X0 AS DOUBLE, X1 AS DOUBLE, Y0 AS DOUBLE, Y1 AS DOUBLE, X)
    LINTERP = (X - X0) / (X1 - X0) * (Y1 - Y0) + Y0
END FUNCTION


SUB LOGPRINT(LOGLINE as String)
    Print LOGLINE
    Print #4, LOGLINE
END SUB


Sub SPLINE (N, X(), Y(), CS())
    DIM i,j             AS INTEGER 'MATRIX ROW AND COL INDICES
    Dim a(3, 3)         AS SINGLE  'MATRIX TO SOLVE
    DIM b(3)            AS SINGLE  'SOLTUTION COEFICIENTS FOR ONE LINE SEGMENT
    DIM DET             AS SINGLE  'MATRIX DETERMINATE
    ReDim CS(N, 3)      AS SINGLE  'SOLTUTION COEFICIENTS FOR EACH LINE SEGEMENT

    'POINTS TO INTERPOLATE AND THE DERIVATE AT THE LEFT POINT
    DIM X0, Y0
    DIM X1, Y1
    DIM YPRIME0

    CALL LOGPRINT("")
    CALL LOGPRINT("SPLINE")

    'ITEREATE THROUGH EACH SET OF ADJACENT POINTS
    'AND DETERMINE THE 
    For i = 1 To N - 1
        CALL LOGPRINT("    SEGMENT = " + STR$(I))

        'ADJACENT POINTS FOR INTERPOLATION
        X0 = x(i)
        X1 = x(i + 1)
        Y0 = Y(i)
        Y1 = Y(i + 1)

        'DERIVATIVE AT X0 FOR FIRST ITERATION SET TO
        'SLOPE OF LINE SEGMENT TO INTERPOLATE
        If i = 1 Then
            CALL LOGPRINT("        FIRST SEGMENT, USING SLOPE AS YPRIME0")
            YPRIME0 = (Y1 - Y0) / (X1 - X0)
        End If

        CALL LOGPRINT("        POINTS")
        CALL LOGPRINT("            X0, X1 = " + STR$(X0) + ", " + STR$(Y0))
        CALL LOGPRINT("            X0, X1 = " + STR$(X1) + ", " + STR$(Y1))
        CALL LOGPRINT("            YPRIME = " + STR$(I))

        'SETUP THE SIMULTANEOUS EQUATIONS
        'LEFT POINT - CLAMPED AT X0, Y0
        a(1, 1) = X0 ^ 2
        a(2, 1) = X0
        a(3, 1) = 1
        b(1)    = Y0

        'RIGHT POINT - CLAMPED AT X1, Y1
        a(1, 2) = X1 ^ 2
        a(2, 2) = X1
        a(3, 2) = 1
        b(2)    = Y1

        'CLAMP LEFT POINT TO THE DERIVATIVE AT THE
        'RIGHT POINT OF THE LAST POLYNOMIAL, EXCEPT
        'FOR THE FIRST SEGMENT, THEN USE THE SLOPE OF
        'THE SEGMENT
        a(1, 3) = 2 * X0
        a(2, 3) = 1
        a(3, 3) = 0
        b(3)   = YPRIME0

        CALL LOGPRINT("        SEGMENT MATRIX")

        FOR ROW = 1 TO 3
                CALL LOGPRINT("           | " + STR$(A(1, ROW)) + " " + STR$(A(2, ROW)) + " " + STR$(A(3, ROW)) + " |   | " + STR$(Y(ROW)) + " | " )
        NEXT ROW

        'SOLVE FOR THE COEFICIENTS OF THE POLYNOMIAL
        DET = 1
        Call LINEQN(a(), b(), 3, DET)

        'NO SOLUTION
        If DET = 0 Then
            CALL LOGPRINT("INDETERMINATE")
            Stop
        End If

        'COPY THE COEFICIENTS FOR THIS SEGMENT INTO
        'AN ARRAY CONTAINING ALL SEGMENT COEFICIENTS
        CS(i, 1) = b(1)
        CS(i, 2) = b(2)
        CS(i, 3) = b(3)

        CALL LOGPRINT("        SOLUTION")
        CALL LOGPRINT("        " + STR$(b(1)) + " " + STR$(b(2)) + STR$(b(3)))

        'CALUCATE THE DERIVATE BASED ON THE PRESENTLY
        'SOLVED POLYNOMIAL FOR FIRST ORDER
        'CONTINUITY
        CALL LOGPRINT("        NOT FIRST SEGMENT, USING SLOPE FROM PREVIOUS POLYNOMIAL")
        YPRIME = 2 * CS(i, 1) * X1 + CS(i, 2)

        CALL LOGPRINT("")

    Next i
End Sub


Rem $STATIC
Sub LINEQN (a(), Y(), N, DET)
    Dim LSTROW(3)

    ND = 64

    DET = 1.0!
    'Zero an auxilliary array for storing interchanges of rows carried out
    For i = 1 To N
        LSTROW(i) = 0
    Next i

    'MAIN LOOP
    For i = 1 To N
        'find the next pivot
        AMAX = 0
        NROW = i
        NCOL = i
        'Locate the element with the largest absolute value.
        'Scan only rows not yet transformed
        For j = 1 To N
            If LSTROW(j) = 0 Then
                For k = 1 To N
                    If Abs(a(k, j)) > AMAX Then
                        NROW = j
                        NCOL = k
                        AMAX = Abs(a(k, j))
                    End If
                Next k
            End If
        Next j

        'If the largest absolute value is zero,
        'return zero for DET to signal a singular case.
        If AMAX = 0 Then
            DET = 0
            EXIT SUB
        End If

        'Multiply D by the pivot.
        DIAG = a(NCOL, NROW)
        DET = DET * DIAG
        If NROW <> NCOL Then
            'If necessary, interchange rows of $\bfA$ and $\bfY$ so that
            'the element is at the diagonal position of the column
            'it is found.
            'Change the sign of DET
            DET = -DET

            For k = 1 To N
                temp = a(k, NROW)
                a(k, NROW) = a(k, NCOL)
                'Divide all elements of $\bfA$ in the row
                'by the pivot so that the diagonal element
                'is now unity.
                a(k, NCOL) = temp / DIAG
            Next k

            temp = Y(NROW)
            Y(NROW) = Y(NCOL)

            'Divide the corresponding element in $\bfY$ also
            'by the pivot.
            Y(NCOL) = temp / DIAG

            'Update the information in the auxilliary array
            LSTROW(NROW) = LSTROW(NCOL)
        Else
            'normalise the diagonal element to 1
            For k = 1 To N
                a(k, NCOL) = a(k, NCOL) / DIAG
            Next k

            Y(NCOL) = Y(NCOL) / DIAG
        End If

        'Mark the row as transformed in the auxilliary array.
        LSTROW(NCOL) = 1

        'Apply the transformation of (5-13) to the rest of the
        'matrix $\bfA$ and $\bfY$.
        For j = 1 To N
            If j <> NCOL Then
                FCTR = a(NCOL, j)
                For k = 1 To N
                    a(k, j) = a(k, j) - FCTR * a(k, NCOL)
                Next k
                Y(j) = Y(j) - FCTR * Y(NCOL)
            End If
        Next j
        
    Next i
End Sub
