VERSION 5.00
Begin VB.Form robotics 
   Caption         =   "Robotics Mode"
   ClientHeight    =   3765
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3180
   LinkTopic       =   "Form2"
   ScaleHeight     =   3765
   ScaleWidth      =   3180
   StartUpPosition =   3  'Windows-Standard
   Begin VB.CheckBox chkPen2 
      Caption         =   "Pen 2 down !"
      Height          =   255
      Left            =   120
      TabIndex        =   14
      Top             =   3300
      Width           =   2775
   End
   Begin VB.CheckBox notdeadband 
      Caption         =   "without deadband"
      Height          =   255
      Left            =   120
      TabIndex        =   13
      Top             =   1440
      Width           =   2655
   End
   Begin VB.CheckBox MotorEnabled 
      Caption         =   "Enable Servo control and hold Position"
      Height          =   315
      Left            =   120
      TabIndex        =   12
      Top             =   1020
      Width           =   3075
   End
   Begin VB.Frame Frame2 
      Caption         =   " Motor rechts"
      Height          =   1275
      Left            =   1680
      TabIndex        =   8
      Top             =   1860
      Width           =   1275
      Begin VB.OptionButton vorR 
         Caption         =   "vor"
         Height          =   195
         Left            =   120
         TabIndex        =   11
         Top             =   300
         Width           =   555
      End
      Begin VB.OptionButton Option2 
         Caption         =   "stop"
         Height          =   195
         Left            =   120
         TabIndex        =   10
         Top             =   600
         Value           =   -1  'True
         Width           =   615
      End
      Begin VB.OptionButton backR 
         Caption         =   "zurück"
         Height          =   255
         Left            =   120
         TabIndex        =   9
         Top             =   900
         Width           =   795
      End
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Left            =   1920
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   540
      Width           =   855
   End
   Begin VB.Timer TimerOutput 
      Interval        =   10
      Left            =   2940
      Top             =   0
   End
   Begin VB.Frame Frame1 
      Caption         =   " Motor links "
      Height          =   1275
      Left            =   120
      TabIndex        =   4
      Top             =   1860
      Width           =   1275
      Begin VB.OptionButton backL 
         Caption         =   "zurück"
         Height          =   255
         Left            =   120
         TabIndex        =   7
         Top             =   900
         Width           =   795
      End
      Begin VB.OptionButton stoL 
         Caption         =   "stop"
         Height          =   195
         Left            =   120
         TabIndex        =   6
         Top             =   600
         Value           =   -1  'True
         Width           =   615
      End
      Begin VB.OptionButton vorL 
         Caption         =   "vor"
         Height          =   195
         Left            =   120
         TabIndex        =   5
         Top             =   300
         Width           =   555
      End
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Recive:"
      Height          =   195
      Left            =   180
      TabIndex        =   3
      Top             =   600
      Width           =   1455
   End
   Begin VB.Label lSensorR 
      Alignment       =   2  'Zentriert
      BackColor       =   &H000080FF&
      BorderStyle     =   1  'Fest Einfach
      Caption         =   "Sensor R"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   1740
      TabIndex        =   2
      Top             =   60
      Width           =   1275
   End
   Begin VB.Label lSensorL 
      Alignment       =   2  'Zentriert
      BackColor       =   &H000080FF&
      BorderStyle     =   1  'Fest Einfach
      Caption         =   "Sensor L"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   120
      TabIndex        =   1
      Top             =   60
      Width           =   1275
   End
End
Attribute VB_Name = "robotics"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()

End Sub

Private Sub Form_Load()
'Enter robotics mode
Form1.MSComm1.Output = "#"
Form1.MSComm1.InputLen = 1
Form1.Timer1.Enabled = False

End Sub

Private Sub Form_Unoad()
Form1.Timer1.Enabled = True
'Exit robotics mode
Form1.MSComm1.Output = Chr$(16)
End Sub

Private Sub TimerOutput_Timer()
Dim byteRM As Byte
Dim nichts As String
byteRM = 0


'BIT 6: Pen Down
If chkPen2 Then byteRM = byteRM Or 64

'BIT 5: Enable Servo control and hold Position with deadband
If MotorEnabled.Value Then byteRM = byteRM Or 32
'BIT 5 und 4: Enable Servo control and hold Position without deadband
If Not notdeadband.Value Then byteRM = byteRM Or 48

'BIT 3: Left Motor forward
If vorL.Value Then byteRM = byteRM Or 8
'BIT 2: Left Motor backward
If backL.Value Then byteRM = byteRM Or 4

'BIT 3: Right Motor backward
If backR.Value Then byteRM = byteRM Or 2
'BIT 2: Right Motor forward
If vorR.Value Then byteRM = byteRM Or 1



Form1.MSComm1.Output = Chr$(byteRM)

Form1.MSComm1.InputLen = 2
Text1.Text = Form1.MSComm1.Input

If Len(Text1.Text) = 2 Then
    If Str$(Val(Mid$(Text1.Text, 2, 1)) And 1) Then
    lSensorR.BackColor = RGB(255, 128, 0)
    Else
    lSensorR.BackColor = RGB(150, 20, 0)
    End If
    If Str$(Val(Mid$(Text1.Text, 2, 1)) And 2) Then
    lSensorL.BackColor = RGB(255, 128, 0)
    Else
    lSensorL.BackColor = RGB(150, 20, 0)
    End If
   
End If

Form1.MSComm1.InputLen = 0
nichts = Form1.MSComm1.Input



End Sub

Private Sub TimerInput_Timer()
'Form1.MSComm1.Input
End Sub
