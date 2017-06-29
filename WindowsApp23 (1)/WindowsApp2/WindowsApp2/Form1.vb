
Public Class Form1
    Dim Powers As New List(Of Double)
    Dim externalResistance As New List(Of Double)
    Dim graphics As Graphics
    Dim good As Boolean = True
    Private Sub fillLists(ByRef howManySteps As Integer)


        Dim Rw, Ez, Rext, dR, P As Double
        Powers.Clear()
        externalResistance.Clear()
        howManySteps = TrackBar1.Value
        Try
            Ez = Double.Parse(TextBox1.Text)
            Rw = Double.Parse(TextBox2.Text)
        Catch
            MsgBox("Podałeś nieprawidłową wartość w jednym z pól.", MsgBoxStyle.OkCancel, "Uwaga")
            good = False

        End Try
        If (Ez = 0 Or Rw = 0) Then
            MsgBox("Jedno z pól ma wartość 0, tak nie może być! Popraw to!", MsgBoxStyle.OkCancel, "Uwaga")
            good = False
        End If
        Rext = 0.1 * Rw
        dR = 1.9 * Rw / howManySteps
        For i As Integer = 0 To howManySteps
            externalResistance.Add(Rext)
            P = (Ez / (Rw + Rext)) ^ 2 * Rext
            Powers.Add(P)
            Rext = Rext + dR
        Next
    End Sub

    Private Sub printListBox()
        ListBox1.Items.Clear()
        For i As Integer = 0 To Powers.Count - 1 Step 1
            ListBox1.Items.Add("P " & Math.Round(Powers.Item(i), 3) & "   R " & externalResistance.Item(i))
        Next
    End Sub

    Private Sub drawDiagram(ByVal howMany)
        graphics.Clear(BackColor)
        Dim p As New Pen(Color.Black, 2)
        Dim Px1, Px2, Py1, Py2, p1, w1, p2, w2, position As Integer
        Dim endDiagram, scala As Double
        Px1 = 190
        Px2 = 700
        Py1 = 100
        Py2 = 550
        scala = (Py2 - Py1 - 5) / (Powers.Max - Powers.Min)
        graphics.DrawLine(p, Px1, Py1, Px2, Py1)
        graphics.DrawLine(p, Px1, Py1, Px1, Py2)
        graphics.DrawLine(p, Px1, Py2, Px2, Py2)
        graphics.DrawLine(p, Px2, Py1, Px2, Py2)
        Dim elementWidth As Double = (Px2 - Px1) / howMany
        p.Color = Color.Blue
        endDiagram = Px1
        For i As Integer = 0 To howMany - 1
            p1 = Math.Round(endDiagram)
            w1 = Py2 - Math.Round(Powers.Item(i) * scala) + position
            endDiagram = endDiagram + elementWidth
            p2 = Math.Round(endDiagram)
            w2 = Py2 - Math.Round(Powers.Item(i + 1) * scala) + position
            graphics.DrawLine(p, p1, w1, p2, w2)
            If (w2 < Py1 + 3) Then
                i = 0
                endDiagram = Px1
                graphics.Clear(BackColor)
                p.Color = Color.Black
                graphics.DrawLine(p, Px1, Py1, Px2, Py1)
                graphics.DrawLine(p, Px1, Py1, Px1, Py2)
                graphics.DrawLine(p, Px1, Py2, Px2, Py2)
                graphics.DrawLine(p, Px2, Py1, Px2, Py2)
                p.Color = Color.Blue
                position = position + 1
            End If
            If (w2 > Py2) Then
                i = 0
                endDiagram = Px1
                graphics.Clear(BackColor)
                p.Color = Color.Black
                graphics.DrawLine(p, Px1, Py1, Px2, Py1)
                graphics.DrawLine(p, Px1, Py1, Px1, Py2)
                graphics.DrawLine(p, Px1, Py2, Px2, Py2)
                graphics.DrawLine(p, Px2, Py1, Px2, Py2)
                p.Color = Color.Blue
                position = position - 1
            End If
        Next
        resistanceScale(Px1, Px2, Py2)
        powerScale(Py2, Py1, Px1)
    End Sub

    Private Sub resistanceScale(x0, xk, y)
        Dim a As Integer = 0
        Dim p As New Pen(Color.Black, 2)
        Dim font1 As New Font(Me.Font, FontStyle.Bold)
        Dim b1 As New SolidBrush(Color.Black)
        Dim pointLeftTop As PointF

        graphics.DrawLine(p, x0, y + 5, xk, y + 5)
        For i As Integer = x0 To xk Step Math.Round((xk - x0) / 22)
            graphics.DrawLine(p, i, y + 5, i, y + 15)
            If (a Mod 11 = 0) Then
                graphics.DrawLine(p, i, y + 5, i, y + 25)
                pointLeftTop.X = i - 10
                pointLeftTop.Y = y + 25

                graphics.DrawString(Math.Round(externalResistance.Item(Math.Round((a * (externalResistance.Count - 1)) / 22)), 2), font1, b1, pointLeftTop)
            End If
            a = a + 1
        Next


    End Sub
    Private Function pValue(a As Integer) As String
        If ((Powers.Min * (22 - a) / 22 + Powers.Max * a / 22) > 10) Then
            Dim i As Integer = 0
            Dim d As Double = (Powers.Min * (22 - a) / 22 + Powers.Max * a / 22)
            While (d > 10)
                d = d / 10
                i = i + 1
            End While
            Return Math.Round(d, 1) & "E" & i
        ElseIf ((Powers.Min * (22 - a) / 22 + Powers.Max * a / 22) < 1) Then
            Dim i As Integer = 0
            Dim d As Double = (Powers.Min * (22 - a) / 22 + Powers.Max * a / 22)
            While (d < 1)
                d = d * 10
                i = i - 1

            End While
            Return Math.Round(d, 1) & "E" & i
        Else
            Return Math.Round((Powers.Min * (22 - a) / 22 + Powers.Max * a / 22))
        End If
    End Function
    Private Sub powerScale(y2, y1, x)
        Dim a As Integer = 0
        Dim p As New Pen(Color.Black, 2)
        Dim font As New Font(Me.Font, FontStyle.Bold)
        Dim brush As New SolidBrush(Color.Red)
        Dim point As PointF

        graphics.DrawLine(p, x - 3, y1, x - 3, y2)
        For i As Integer = y2 To y1 Step Math.Round((y1 - y2) / 22)
            graphics.DrawLine(p, x - 3, i, x - 8, i)
            If (a Mod 11 = 0) Then
                graphics.DrawLine(p, x - 3, i, x - 15, i)
                point.X = x - 45
                point.Y = i

                graphics.DrawString(pValue(a), font, brush, point)
            End If
            a = a + 1
        Next
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim q As Integer
        fillLists(q)
        If (good) Then
            printListBox()
            drawDiagram(q)
            showMaximum()
        Else
            good = True
        End If
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        graphics = Me.CreateGraphics()
    End Sub
    Private Function findMax() As Double
        Dim maximum As Double
        maximum = Powers.First
        For Each p As Double In Powers
            If (maximum < p) Then
                maximum = p
            End If
        Next
        Return maximum
    End Function

    Private Sub Label3_Click(sender As Object, e As EventArgs) Handles Label3.Click

    End Sub
    Private Sub showMaximum()
        Label3.Text = "Największa moc występuje dla rezystancji równej " & Math.Round(externalResistance.Item(Powers.LastIndexOf(findMax)), 3) & " i wynosi ona " & Math.Round(findMax(), 3)
    End Sub

    Private Sub TrackBar1_Scroll(sender As Object, e As EventArgs) Handles TrackBar1.Scroll
        Label5.Text = "Ilość kroków " & TrackBar1.Value

    End Sub

    Private Sub Label5_Click(sender As Object, e As EventArgs) Handles Label5.Click

    End Sub
End Class
