VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "mswinsck.ocx"
Begin VB.Form frm780SDS 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "IND780 Shared Data Server Test"
   ClientHeight    =   7020
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   12540
   ClipControls    =   0   'False
   Icon            =   "frm780SDS.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   7020
   ScaleWidth      =   12540
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.ListBox List1 
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   12
         Charset         =   204
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3420
      Left            =   120
      TabIndex        =   11
      Top             =   3240
      Width           =   12255
   End
   Begin MSWinsockLib.Winsock Winsock 
      Left            =   100
      Tag             =   "WinSock"
      Top             =   100
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Frame frmWeightData 
      Caption         =   "Weights"
      Height          =   2775
      Left            =   4680
      TabIndex        =   9
      Top             =   240
      Width           =   3135
      Begin VB.Label Weight2 
         Alignment       =   1  'Right Justify
         Caption         =   "00000"
         BeginProperty Font 
            Name            =   "Lucida Console"
            Size            =   24
            Charset         =   204
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   615
         Left            =   480
         TabIndex        =   12
         Top             =   1680
         Width           =   2055
      End
      Begin VB.Label Weight1 
         Alignment       =   1  'Right Justify
         Caption         =   "00000"
         BeginProperty Font 
            Name            =   "Lucida Console"
            Size            =   24
            Charset         =   204
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   615
         Left            =   480
         TabIndex        =   10
         Top             =   840
         Width           =   2055
      End
   End
   Begin VB.Frame frmConnection 
      Caption         =   "Connection Settings"
      Height          =   2775
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4215
      Begin VB.CommandButton cmdDisconnect 
         Caption         =   "Disconnect"
         Height          =   375
         Left            =   1920
         TabIndex        =   8
         Top             =   2040
         Width           =   1215
      End
      Begin VB.CommandButton cmdConnect 
         Caption         =   "Connect"
         Height          =   375
         Left            =   360
         TabIndex        =   7
         Top             =   2040
         Width           =   1215
      End
      Begin VB.TextBox txtPassword 
         Height          =   285
         Left            =   2040
         TabIndex        =   3
         Top             =   1440
         Width           =   1815
      End
      Begin VB.TextBox txtUsername 
         Height          =   285
         Left            =   2040
         TabIndex        =   2
         Top             =   960
         Width           =   1815
      End
      Begin VB.TextBox txtIPAddress 
         Height          =   285
         Left            =   2040
         TabIndex        =   1
         Top             =   480
         Width           =   1815
      End
      Begin VB.Label lblPassword 
         Caption         =   "Password :"
         Height          =   255
         Left            =   360
         TabIndex        =   6
         Top             =   1440
         Width           =   1215
      End
      Begin VB.Label lblUsername 
         Caption         =   "Username :"
         Height          =   255
         Left            =   240
         TabIndex        =   5
         Top             =   960
         Width           =   1215
      End
      Begin VB.Label lblIPAddress 
         Caption         =   "780 IPAddress :"
         Height          =   255
         Left            =   240
         TabIndex        =   4
         Top             =   480
         Width           =   1215
      End
   End
End
Attribute VB_Name = "frm780SDS"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const defaultUsername = "Admin"
Const defaultPort = 1701

Public Sub SendKey(text$, Optional Wait As Boolean = False)
   Dim WshShell As Object
   Set WshShell = CreateObject("wscript.shell")
       WshShell.SendKeys text, Wait
   Set WshShell = Nothing
End Sub


Private Sub Form_Load()
 
  txtIPAddress.text = "111.111.111.111"
  txtUsername.text = defaultUsername
  txtPassword.text = "Admin"
  
  Weight1.Caption = ""
  Weight2.Caption = ""
  
  cmdDisconnect.Enabled = False

End Sub

Private Sub Form_Unload(Cancel As Integer)

  If Winsock.State = sckConnected Then
    CloseSocket
  End If

End Sub

Private Sub cmdConnect_Click()

  Winsock.RemoteHost = txtIPAddress.text
  Winsock.RemotePort = defaultPort
  Winsock.Connect

End Sub

Private Sub cmdDisconnect_Click()
 
  CloseSocket
 
End Sub

Private Sub Winsock_Connect()
 
  cmdDisconnect.Enabled = True
  cmdConnect.Enabled = False
    
'  Wait for 780 to respond with OK
'  Using the Winsock_DataArrival Event Handler
    
End Sub

Private Sub Winsock_DataArrival(ByVal bytesTotal As Long)

  Dim WinSockData$
  Dim ResponseCode$
  Dim sdsCode As String
  Dim ResponseString$
  Dim UnexpectedResponse As Boolean

    Winsock.GetData WinSockData$    ' <10><13><48><48>  First 2 Chars Are <lf> & <cr>
    Debug.Print Len(WinSockData$)
    AddItemToList "{" + WinSockData$ + "}"
    
    If WinSockData$ <> "" Then
        
                    ResponseCode$ = Mid(WinSockData$, 3, 2)      ' Get response code
        Select Case ResponseCode$
        
            Case "00"       ' Were logged in and receiving data!!!
                ' Pase 3rd character
                
                   sdsCode = Mid(WinSockData$, 5, 1)
                If sdsCode = "C" Then           ' Parse out String From CallBack
'                   ResponseString$ = Mid$(WinSockData$, 17, 12)
'                   ResponseString$ = Trim$(ResponseString$)
                    ParseOutWinSockDataString (WinSockData$)
'                   Weight1.Caption = ResponseString$
                End If
                
                If sdsCode = "R" Then           ' Parse Out String From Read
                    ' Parse out weight data
                    ResponseString$ = Mid$(WinSockData$, 10, 8)
                    ResponseString$ = Trim$(ResponseString$)
'                   Weight1.Caption = ResponseString$
                End If
                
                If sdsCode = "B" Then           ' Parse Out String From Read
                    ' Parse out weight data
'                    ResponseString$ = Mid$(WinSockData$, 10, 8)
'                    ResponseString$ = Trim$(ResponseString$)
'                    Weight1.Caption = ResponseString$
                End If
                
            Case "53"           ' Ready for user
                                ' Send User [usename]
                                
                   AddItemToList "User " & txtUsername.text
                Winsock.SendData "User " & txtUsername.text & vbCrLf
                
            Case "51"           ' Enter password
                                ' Send Pass [password]
                
                If txtPassword.text = "" Then
                    MsgBox "Please enter a password and try connecting again.", vbOKOnly, "WARNING"
                    CloseSocket
                Else
                       AddItemToList "pass " & txtPassword.text
                    Winsock.SendData "pass " & txtPassword.text & vbCrLf
                End If
                
            Case "12"           ' Access Ok
                                ' Now we can send our call back command
                    
                    AddItemToList "callback wt0101"
                 Winsock.SendData "callback wt0101" & vbCrLf
                 Winsock.SendData "callback wt0201" & vbCrLf
            
            Case "83"           ' Command not recognized
                
                Dim result As VbMsgBoxResult
                result = MsgBox("Command not recognized." & vbCrLf & "Do you want to logon again?", vbYesNo, "WARNING")
                If result = vbYes Then
                       AddItemToList "User " & txtUsername.text
                    Winsock.SendData "User " & txtUsername.text & vbCrLf
                Else
                    CloseSocket
                End If
                 
            Case "93"            ' No Access
                
                MsgBox "Invalid Password", vbOKOnly, "WARNING"
                CloseSocket
            
            Case Else
                
                UnexpectedResponse = True
                
        End Select
            
    End If      ' WinSockData$ was Null
    
    If UnexpectedResponse = True Then
        MsgBox "Received unexpected response from 780" & vbCrLf & WinSockData$, vbOKOnly, "ERROR"
        CloseSocket
    End If
        


End Sub

Private Sub ParseOutWinSockDataString(S$)

 Dim p1%, p2%, p3%, p4%
 Dim w1$, w2$, w3$, w4$

 Dim StartPos%, FindString$, FoundString%

'   Here are Some Examples
' 00C030~wt0101=       14540~wt0201=       6.478~
' 00C033~wt0101=       14860~wt0201=       6.487~
' 00C069~wt0101=       18420~wt0201=       6.391~
' 00C053~wt0101=       18440~wt0201=       6.241~
' 00C051~wt0101=       17760~wt0201=       6.316~
' 00C055~wt0201=       6.222~
' 00C057~wt0101=       18460~

'   Look For WT0101 or WT0*01 or WT0201 or WT0301 or WT0401

  FindString$ = "wt0101="
        
        StartPos% = 1
        FoundString% = -1
  While FoundString% <> 0
        FoundString% = InStr(StartPos%, S$, FindString$)
     If FoundString% <> 0 Then
       Weight1.Caption = Trim$(Mid$(S$, FoundString% + 7, 12))
       StartPos% = FoundString% + 8
     End If
  Wend
     
'     p1% = InStr(1, S$, "wt0101=")
'  If p1% <> 0 Then
'     w1$ = Mid$(S$, p1% + 7, 12)
'     Weight1.Caption = Trim$(w1$)
'  End If
     
     p2% = InStr(1, S$, "wt0201=")
  If p2% <> 0 Then
     w2$ = Mid$(S$, p2 + 7, 12)
     Weight2.Caption = Trim$(w2$)
  End If

End Sub

Private Sub CloseSocket()

  If Winsock.State = sckConnected Then
     Winsock.Close
     cmdConnect.Enabled = True
     cmdDisconnect.Enabled = False
     Weight1.Caption = "CLOSED"
  End If

End Sub

Private Sub AddItemToList(Verbage$)

  List1.AddItem Verbage$
  SendKey "^{PgDn}", True

End Sub
