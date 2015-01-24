VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form Form1 
   Caption         =   "PanMan - Plotroboter"
   ClientHeight    =   8715
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9960
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   9.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   Picture         =   "PanMan.frx":0000
   ScaleHeight     =   8715
   ScaleWidth      =   9960
   StartUpPosition =   3  'Windows-Standard
   Begin VB.Timer TimerPlotList 
      Enabled         =   0   'False
      Interval        =   5
      Left            =   1020
      Top             =   5460
   End
   Begin MSComDlg.CommonDialog cd1 
      Left            =   1740
      Top             =   4920
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Oben ausrichten
      Height          =   990
      Left            =   0
      TabIndex        =   3
      Top             =   0
      Width           =   9960
      _ExtentX        =   17568
      _ExtentY        =   1746
      ButtonWidth     =   609
      ButtonHeight    =   1588
      Appearance      =   1
      _Version        =   393216
      Begin VB.CommandButton Command8 
         Caption         =   "Command8"
         Height          =   315
         Left            =   8880
         TabIndex        =   29
         Top             =   420
         Width           =   315
      End
      Begin VB.CommandButton CommandDelShape 
         Caption         =   "DEL"
         Height          =   315
         Left            =   5640
         TabIndex        =   28
         Top             =   600
         Width           =   615
      End
      Begin VB.CommandButton CommandPlotShape 
         Caption         =   "Plot"
         Height          =   315
         Left            =   5640
         TabIndex        =   27
         Top             =   300
         Width           =   615
      End
      Begin VB.CommandButton Command1MakeText 
         Enabled         =   0   'False
         Height          =   315
         Left            =   5280
         MaskColor       =   &H8000000F&
         Picture         =   "PanMan.frx":00F2
         Style           =   1  'Grafisch
         TabIndex        =   26
         Top             =   600
         Width           =   375
      End
      Begin VB.CommandButton CommandMakeBogen 
         Enabled         =   0   'False
         Height          =   315
         Left            =   5280
         MaskColor       =   &H8000000F&
         Picture         =   "PanMan.frx":01F4
         Style           =   1  'Grafisch
         TabIndex        =   25
         Top             =   300
         Width           =   375
      End
      Begin VB.CommandButton CommandMakeKreic 
         Height          =   315
         Left            =   4920
         MaskColor       =   &H8000000F&
         Picture         =   "PanMan.frx":02F6
         Style           =   1  'Grafisch
         TabIndex        =   24
         Top             =   600
         Width           =   375
      End
      Begin VB.CommandButton CommandMakeRechteck 
         Height          =   315
         Left            =   4920
         MaskColor       =   &H8000000F&
         Picture         =   "PanMan.frx":03F8
         Style           =   1  'Grafisch
         TabIndex        =   23
         Top             =   300
         Width           =   375
      End
      Begin VB.CheckBox chkDrawByMove 
         Caption         =   "Draw"
         Height          =   255
         Left            =   3060
         TabIndex        =   21
         ToolTipText     =   "Zeichnen beim Bewegen"
         Top             =   660
         Width           =   855
      End
      Begin VB.TextBox XMouse 
         BackColor       =   &H00C0FFFF&
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   3060
         Locked          =   -1  'True
         TabIndex        =   20
         Text            =   "0"
         Top             =   0
         Width           =   855
      End
      Begin VB.TextBox YMouse 
         BackColor       =   &H00C0FFFF&
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   3060
         Locked          =   -1  'True
         TabIndex        =   19
         Text            =   "0"
         Top             =   300
         Width           =   855
      End
      Begin VB.CommandButton Command7 
         Caption         =   "Load Plotlist"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   915
         Left            =   7560
         Picture         =   "PanMan.frx":04FA
         Style           =   1  'Grafisch
         TabIndex        =   18
         Top             =   0
         Width           =   1155
      End
      Begin VB.CommandButton Command6 
         Caption         =   "Robotics Mode"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   915
         Left            =   6420
         Picture         =   "PanMan.frx":05FC
         Style           =   1  'Grafisch
         TabIndex        =   17
         Top             =   0
         Width           =   1155
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Autounwarp"
         Height          =   240
         Left            =   4980
         TabIndex        =   16
         ToolTipText     =   "Automatisches zurückdrehen, damit sich der PanMan nicht im Kabel verfängt"
         Top             =   0
         Value           =   1  'Aktiviert
         Width           =   1635
      End
      Begin VB.CommandButton pen 
         Caption         =   "Pen 3"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   2
         Left            =   4020
         TabIndex        =   15
         Top             =   600
         Width           =   800
      End
      Begin VB.CommandButton pen 
         Caption         =   "Pen 2"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   1
         Left            =   4020
         TabIndex        =   14
         Top             =   300
         Width           =   800
      End
      Begin VB.CommandButton pen 
         Caption         =   "Pen 1"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   0
         Left            =   4020
         TabIndex        =   13
         Top             =   0
         Width           =   800
      End
      Begin VB.CommandButton Command1 
         Caption         =   "RESET!"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   0
         Style           =   1  'Grafisch
         TabIndex        =   12
         Top             =   -60
         UseMaskColor    =   -1  'True
         Width           =   1215
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Initialisierung"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   0
         Style           =   1  'Grafisch
         TabIndex        =   11
         Top             =   300
         Width           =   1215
      End
      Begin VB.CommandButton Command3 
         Caption         =   "Homepos"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   0
         Style           =   1  'Grafisch
         TabIndex        =   10
         Top             =   600
         Width           =   1215
      End
      Begin VB.TextBox coX 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1260
         TabIndex        =   9
         Text            =   "0"
         ToolTipText     =   "absolute Koordinaten"
         Top             =   0
         Width           =   855
      End
      Begin VB.TextBox coY 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1260
         TabIndex        =   8
         Text            =   "0"
         ToolTipText     =   "absolute Koordinaten"
         Top             =   300
         Width           =   855
      End
      Begin VB.CommandButton Command4 
         Caption         =   "MOVETO"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1260
         Style           =   1  'Grafisch
         TabIndex        =   7
         ToolTipText     =   "absolute Bewegung"
         Top             =   600
         Width           =   855
      End
      Begin VB.CommandButton Command5 
         Caption         =   "MOVE"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   2160
         Style           =   1  'Grafisch
         TabIndex        =   6
         ToolTipText     =   "relative Bewegung"
         Top             =   600
         Width           =   855
      End
      Begin VB.TextBox coYr 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   2160
         TabIndex        =   5
         Text            =   "0"
         ToolTipText     =   "relative Koordinaten"
         Top             =   300
         Width           =   855
      End
      Begin VB.TextBox coXr 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   2160
         TabIndex        =   4
         Text            =   "0"
         ToolTipText     =   "relative Koordinaten"
         Top             =   0
         Width           =   855
      End
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  '2D
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   3255
      Left            =   120
      ScaleHeight     =   3225
      ScaleWidth      =   4665
      TabIndex        =   2
      Top             =   1080
      Width           =   4695
      Begin VB.Line LineMarkShapeY 
         BorderColor     =   &H0000C000&
         Visible         =   0   'False
         X1              =   3240
         X2              =   4080
         Y1              =   1140
         Y2              =   1140
      End
      Begin VB.Line LineMarkShapeX 
         BorderColor     =   &H0000C000&
         Visible         =   0   'False
         X1              =   3660
         X2              =   3660
         Y1              =   780
         Y2              =   1560
      End
      Begin VB.Shape ShapeKreis 
         Height          =   2475
         Left            =   240
         Shape           =   2  'Oval
         Top             =   240
         Visible         =   0   'False
         Width           =   2475
      End
      Begin VB.Shape ShapeRechteck 
         Height          =   1755
         Left            =   600
         Top             =   600
         Visible         =   0   'False
         Width           =   1755
      End
      Begin VB.Label LabelHome 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Homeposition ?!"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000000FF&
         Height          =   195
         Left            =   60
         TabIndex        =   22
         Top             =   0
         Width           =   1140
      End
      Begin VB.Line LinePosX 
         BorderColor     =   &H000000FF&
         X1              =   1380
         X2              =   1380
         Y1              =   960
         Y2              =   1740
      End
      Begin VB.Line LinePosY 
         BorderColor     =   &H000000FF&
         X1              =   960
         X2              =   1800
         Y1              =   1320
         Y2              =   1320
      End
   End
   Begin VB.Timer Timer1 
      Interval        =   20
      Left            =   1020
      Top             =   4920
   End
   Begin MSCommLib.MSComm MSComm1 
      Left            =   2460
      Top             =   4860
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      DTREnable       =   -1  'True
   End
   Begin VB.TextBox Testsend 
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   5820
      MultiLine       =   -1  'True
      TabIndex        =   1
      Top             =   7560
      Width           =   4000
   End
   Begin VB.ListBox l1 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   7500
      Left            =   5820
      TabIndex        =   0
      Top             =   1080
      Width           =   4000
   End
   Begin VB.Shape shadow 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Undurchsichtig
      Height          =   3315
      Left            =   300
      Top             =   1260
      Width           =   4755
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Andreas Tobola
' Project page: http://www.tnotes.de/Penman
' Version 1.1


Private Sub Check1_Click()
'Automatisches zurückdrehen, damit sich der PanMan nicht im Kabel verfängt
If Check1.Value = 0 Then transmit "W0,"
If Check1.Value = 1 Then transmit "W1,"
End Sub

Private Sub Command1_Click()
transmit Chr$(27)
End Sub

Private Sub Command2_Click()
transmit "I"
End Sub

Private Sub Command3_Click()
transmit "H"

setLinePos 0, 0
LabelHome.Visible = True
End Sub

Private Sub Command4_Click()
If chkDrawByMove Then
transmit "D "
Else
transmit "U "
End If
transmit "A M " + Str$(Val(coX - 500)) + "," + Str$(Val(coY - 500)) + ","
End Sub

Private Sub Command5_Click()
If chkDrawByMove Then
transmit "D "
Else
transmit "U "
End If
transmit "U R M " + Str$(Val(coXr)) + "," + Str$(Val(coYr)) + ","
End Sub

Private Sub Command6_Click()
robotics.Show 1
End Sub

'Draw Plotlist
Private Sub Command7_Click()
cd1.Filter = "Plotlist (*.pl)|*.pl|Alle Dateien (*.*)|*.*"
cd1.FileName = App.Path + "\*.pl"
cd1.ShowOpen

    plotliststate = 1
    TimerPlotList.Enabled = True

End Sub

Private Sub CommandDelShape_Click()
shapemode = 0
ShapeKreis.Visible = False
ShapeRechteck.Visible = False

End Sub

Private Sub CommandMakeKreic_Click()
shapemode = 2  'Kreis
ShapeKreis.Visible = True
ShapeRechteck.Visible = False
End Sub

Private Sub CommandMakeRechteck_Click()
shapemode = 1  'Rechteck
ShapeRechteck.Visible = True
ShapeKreis.Visible = False
End Sub

Private Sub CommandPlotShape_Click()
If shapemode = 1 Then
    Picture1.Line (ShapeRechteck.Left, ShapeRechteck.Top)-(ShapeRechteck.Left + ShapeRechteck.Width, ShapeRechteck.Top + ShapeRechteck.Height), 0, B
    drawRechteck ShapeRechteck.Left, ShapeRechteck.Top, ShapeRechteck.Left + ShapeRechteck.Width, ShapeRechteck.Top + ShapeRechteck.Height
End If


shapemode = 0
ShapeKreis.Visible = False
ShapeRechteck.Visible = False
End Sub

Private Sub Form_Load()
'Me.WindowState = 2

plotliststate = 0
penmanReady = False
penmanError = 0

MSComm1.Settings = "9600,n,8,1"
MSComm1.CommPort = Val(InputBox("Enter com port number", "Serial port number"))
MSComm1.PortOpen = True
transmit Chr$(27)
transmit "II,"
'transmit "I,"
transmit "W1,"
transmit "P2,"

PanManMaxX = 1500 + 500
PanManMaxY = 2400 + 500

BlattX = 10000

Picture1.Width = BlattX
Picture1.Height = (BlattX / PanManMaxY) * PanManMaxX
shadow.Width = Picture1.Width
shadow.Height = Picture1.Height

pen(0).FontBold = False
pen(1).FontBold = True
pen(2).FontBold = False

setLinePos 0, 0
LabelHome.Visible = False

End Sub


Private Sub Form_Resize()
l1.Left = Form1.ScaleWidth - 4000
l1.Height = Form1.ScaleHeight - 500 - 1380

Testsend.Left = Form1.ScaleWidth - 4000
Testsend.Top = Form1.ScaleHeight - 550
End Sub



Private Sub l1_Click()
Testsend.Text = Right$(l1.List(l1.ListIndex), Len(l1.List(l1.ListIndex)) - 2)
End Sub

Private Sub MSComm1_OnComm()
'l1.AddItem ("Event: " + Str$(MSComm1.CommEvent))
End Sub

Private Sub pen_Click(Index As Integer)
pen(0).FontBold = False
pen(1).FontBold = False
pen(2).FontBold = False
pen(Index).FontBold = True
transmit "P" + Str$(Index + 1) + ","
End Sub



Private Sub Picture1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim PanManX As Integer
Dim PanManY As Integer
PanManY = Int((X / Picture1.Width) * PanManMaxY) - 500
PanManX = Int((Y / Picture1.Height) * PanManMaxX) - 500

'nur wenn Shapezeichenmodus nicht aktiv !
If shapemode = 0 Then

    'Left Mouse
    'Gehe zur Position
    If Shift = 0 And Button = 1 Then
    transmit "U A M " + Str$(PanManX) + "," + Str$(PanManY) + ","
    setLinePos X, Y
    End If

    'Shift und Left Mouse
    'Zeichne Linie zu Position
    If Shift = 1 And Button = 1 Then
    transmit "D A M " + Str$(PanManX) + "," + Str$(PanManY) + ","
    setLinePos X, Y
    End If

End If

If shapemode > 0 Then
LineMarkShapeX.Visible = True
LineMarkShapeY.Visible = True
Else
LineMarkShapeX.Visible = False
LineMarkShapeY.Visible = False
End If

End Sub

Private Sub Picture1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim PanManX As Integer
Dim PanManY As Integer
YMouse = Str$(Int((X / Picture1.Width) * PanManMaxY))
XMouse = Str$(Int((Y / Picture1.Height) * PanManMaxX))

'Recteckshape Editieren
If shapemode = 1 Then
    If Button = 1 Then
        ShapeRechteck.Top = Y
        ShapeRechteck.Left = X
        setEditMarkPos X, Y
    End If
    If Button = 2 Then
        If X < ShapeRechteck.Left Then ShapeRechteck.Left = X - 100
        If Y < ShapeRechteck.Top Then ShapeRechteck.Top = Y - 100
        ShapeRechteck.Height = Y - ShapeRechteck.Top
        ShapeRechteck.Width = X - ShapeRechteck.Left
        setEditMarkPos X, Y
    End If
End If

'Kreisshape Editieren
If shapemode = 2 Then
    If Button = 1 Then
    ShapeKreis.Top = Y - (ShapeKreis.Height / 2)
    ShapeKreis.Left = X - (ShapeKreis.Width / 2)
    setEditMarkPos X, Y
    End If
    If Button = 2 And Not (X < ShapeKreis.Left) Then
    ShapeKreis.Width = X - ShapeKreis.Left
    ShapeKreis.Height = ShapeKreis.Width
    setEditMarkPos X, ShapeKreis.Top + (ShapeKreis.Height / 2)
    End If
End If


End Sub

Private Sub Picture1_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
LineMarkShapeX.Visible = False
LineMarkShapeY.Visible = False
End Sub

Private Sub Testsend_KeyPress(keyascii As Integer)

If Testsend.Text = Chr$(13) + Chr$(10) Then Testsend.Text = ""

If keyascii = 13 Then
transmit UCase$(Testsend.Text)
Testsend.Text = ""
End If
End Sub


Private Sub Timer1_Timer()
' Es sollen alle verfügbaren Daten abgerufen werden.


' Prüfen, ob Daten vorhanden sind.
'If MSComm1.InBufferCount Then
   ' Daten lesen.
'   l1.AddItem (MSComm1.Input)
'End If
End Sub






'rote Markierungen für die Position von PanMan
Private Sub setLinePos(xPos As Single, yPos As Single)

LinePosX.X1 = xPos
LinePosX.Y1 = 0
LinePosX.X2 = xPos
LinePosX.Y2 = Picture1.Height

LinePosY.X1 = 0
LinePosY.Y1 = yPos
LinePosY.X2 = Picture1.Width
LinePosY.Y2 = yPos

LabelHome.Visible = False

End Sub

'grüne Markierungen für das Editieren von Shapes
Private Sub setEditMarkPos(xPos As Single, yPos As Single)

LineMarkShapeX.X1 = xPos
LineMarkShapeX.Y1 = yPos - 200
LineMarkShapeX.X2 = xPos
LineMarkShapeX.Y2 = yPos + 200

LineMarkShapeY.X1 = xPos - 200
LineMarkShapeY.Y1 = yPos
LineMarkShapeY.X2 = xPos + 200
LineMarkShapeY.Y2 = yPos

LabelHome.Visible = False

End Sub


'Recteck Zeichnen
Private Sub drawRechteck(xp1 As Long, yp1 As Long, xp2 As Long, yp2 As Long)
Dim PanManX1 As Integer
Dim PanManY1 As Integer
Dim PanManX2 As Integer
Dim PanManY2 As Integer

PanManY1 = Int((xp1 / Picture1.Width) * PanManMaxY) - 500
PanManX1 = Int((yp1 / Picture1.Height) * PanManMaxX) - 500
PanManY2 = Int((xp2 / Picture1.Width) * PanManMaxY) - 500
PanManX2 = Int((yp2 / Picture1.Height) * PanManMaxX) - 500

transmit "U A M " + Str$(PanManX1) + "," + Str$(PanManY1) + ","
WaitForRady
transmit "D A M " + Str$(PanManX1) + "," + Str$(PanManY2) + ","
WaitForRady
transmit "M " + Str$(PanManX2) + "," + Str$(PanManY2) + ","
WaitForRady
transmit "M " + Str$(PanManX2) + "," + Str$(PanManY1) + ","
WaitForRady
transmit "M " + Str$(PanManX1) + "," + Str$(PanManY1) + ","


LabelHome.Visible = False

End Sub



Private Sub TimerPlotList_Timer()
Dim plotlistbefehl As String

 Select Case plotliststate

 Case 1
        Open cd1.FileName For Input As #1
        plotliststate = 2
 
 Case 2
    
        Line Input #1, plotlistbefehl
        
        If EOF(1) Then
            plotliststate = 0
            Close #1
            TimerPlotList.Enabled = False
            l1.AddItem ("END OF LIST")
        Else
            
            plotlistbefehl = Trim(plotlistbefehl)
            
            ' Vor dem Stiftwechsel warten
            If UCase(Left(plotlistbefehl, 1)) = "P" Then
                'l1.AddItem ("*** Zwangspause ***")
                'Sleep 3000
            End If
            
            
            If UCase(Left(plotlistbefehl, 1)) = "L" Then
                plotlistbefehl = plotlistbefehl + Chr(13)
            Else
                If Not UCase(Right(plotlistbefehl, 1)) = "," Then
                    plotlistbefehl = plotlistbefehl + ","
                End If
                plotlistbefehl = plotlistbefehl + " "
            End If
                        
            transmit (plotlistbefehl)
            WaitForRady

            
        End If
        
  End Select
    

End Sub


Private Sub errorHandling()

    TimerPlotList.Enabled = False


End Sub




' **** Rückgabewerte
' 33     Ready
' 19     Puffer voll !
' 17     Puffer leer
'36 & 7  Fehler
Private Sub checkPenmanResponse()

        Dim penmenResponse As String

        MSComm1.InputLen = 1
        penmenResponse = 0
        

        Do While MSComm1.InBufferCount > 0

        penmenResponse = MSComm1.Input
        
        Select Case Asc(penmenResponse)
        
        Case 33
            penmanReady = True
            l1.AddItem ("READY")
        
        Case 19
            penmanBufferFull = True
            l1.AddItem ("FULL")
                
        Case 17
            penmanBufferFull = False
            l1.AddItem ("EMPTY")
        
        Case 36
            l1.AddItem ("Error 36  :(")
            penmanError = 36
            errorHandling
            
            
        Case 7
            l1.AddItem ("Error 7  :(")
            penmanError = 7
            errorHandling
            
        
        Case Else
            If Asc(penmenResponse) < 48 Then
                l1.AddItem (Asc(penmenResponse))
                'penmanError = 1
            End If
        
        
        End Select
                
        
        
                
        
        
        Loop


End Sub





Private Sub transmit(StrA As String)

Dim TTimoutInputBufPanman As Double
Dim TeilBefehl As String
Dim l As Integer

penmanReady = False

l1.AddItem ("> " + StrA)
l1.Refresh


l = Len(StrA)
    
    Sleep 100 ' Warte auf eine mögliche die Antwort
    
    For w = 1 To l
                                                                                                                                                                          
           
           
           
           checkPenmanResponse
           TTimoutInputBufPanman = Timer + 20 'Sec
           'Timer > TTimoutInputBufPanman
           
           If penmanError = 0 Then
           
            Do While (penmanBufferFull Or Not MSComm1.CTSHolding)
                 Sleep 20
                 checkPenmanResponse
            Loop
            
           End If
           
           If penmanError = 0 Then
           
            TeilBefehl = Mid$(StrA, w, 1)
            MSComm1.Output = TeilBefehl
           
           End If
           
    Next w


End Sub


'Warte bis PanMan fertig ist (Code 33)
Private Sub WaitForRady()
Dim TTimoutWaitForRady As Double

        If penmanError = 0 Then

           If penmanReady = False Then
          
            checkPenmanResponse
            TTimoutWaitForRady = Timer + 60 'Sec
            ' Timer > TTimoutWaitForRady
            Do Until (penmanReady)
                 checkPenmanResponse
                 Sleep 10
                 If Timer > TTimoutWaitForRady Then Exit Do
            Loop
                       
           End If
        
    End If

End Sub
