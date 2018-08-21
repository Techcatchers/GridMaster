Class MainWindow
    Public num As Integer
    Private Sub btnOut_Click(sender As Object, e As RoutedEventArgs) Handles btnOut.Click

        If txtTry.Text = "0" Then
            Dim Randvar As Random = New Random()
            num = Randvar.Next(200, 700)
            Console.WriteLine(num)
        End If

        Dim Rand1 As Integer
        Dim Rand2 As Integer
        Dim Rand3 As Integer

        Rand1 = CStr(num).Substring(0, 1)
        Rand2 = CStr(num).Substring(1, 1)
        Rand3 = CStr(num).Substring(2)

        '   Protection Levels to show Random Only between 2 to 7

        If Rand1 = Rand2 Then
            Rand2 = Rand2 + 1
        ElseIf Rand2 = Rand3 Then
            Rand3 = Rand3 + 1
        End If

        If Rand2 > 7 Then
            Rand2 = Rand2 - 2
        ElseIf Rand2 < 2 Then
            Rand2 = Rand2 + 2
        End If

        If Rand3 > 7 Then
            Rand3 = Rand3 - 2
        ElseIf Rand3 < 2 Then
            Rand3 = Rand3 + 2
        End If

        If Rand1 = Rand2 Then
            Rand2 = Rand2 - 1
        ElseIf Rand2 = Rand3 Then
            Rand3 = Rand3 - 1
        End If

        '    Protection levels Ends

        'MsgBox(Rand1 & Rand2)

        Dim StoOut As String

        Dim Row2 As Integer
        Row2 = Rand1 - 1

        Dim Row2C2 As Integer
        Row2C2 = Rand2 - 1

        Dim Row2C3 As Integer
        Row2C3 = Rand3 - 1

        Dim Row3 As Integer
        Row3 = Rand1 + 2

        Dim Row3C2 As Integer
        Row3C2 = Rand2 + 2

        Dim Row3C3 As Integer
        Row3C3 = Rand3 + 2

        Dim Points As Integer

        'txtInput.MaxLength = 3 in textbox selection change
        StoOut = txtInput.Text

        If IsNumeric(StoOut) = False Then
            MsgBox("Please Enter Numbers Only",, "Error")
            txtInput.Text = ""
            Exit Sub
        End If

        '   Level Controller Below

        If lblLevel.Content = "Level #1" Then
            StoOut = txtInput.Text & "00"
        ElseIf lblLevel.Content = "Level #2" Then
            StoOut = txtInput.Text & "0"
        End If

        '    Level Controller Ends

        Try
            Dim Digit(2) As Integer
            Digit(0) = StoOut.Substring(0, 1)
            Digit(1) = StoOut.Substring(1, 1)
            Digit(2) = StoOut.Substring(2)

            If Digit(0) = Digit(1) Or (Digit(1) = Digit(2) And lblLevel.Content = "Level #3") Then
                MsgBox("2 Sequential Digits can't be the same Digit.")
                Exit Sub

            End If

            ' Level Play for lvl() 1 Digits below 

            If lblLevel.Content = "Level #1" Then

                For Each D As Integer In Digit

                    If D = Rand1 Then
                        'MsgBox("Execute" & D)
                        Points += 2

                        If D = Row2 Then
                            'MsgBox(Row2C2)
                            Points += 1

                            'ElseIf D = Row3 Then
                            '    Points -= 1
                        End If

                        If D = Row3 Then
                            Points -= 1
                        End If

                    ElseIf D = Row2 Then
                        Points += 1
                        If D = Row3 Then
                            'MsgBox(Row3C3)
                            Points -= 1
                        End If

                    ElseIf D = Row3 Then
                        Points -= 1
                    Else
                        Points += 0
                    End If
                Next

            End If
            '  Lvl play for 1 Digit Ends

            ' Level Play for lvl() 2 Digits below 

            If lblLevel.Content = "Level #2" Then

                For Each D As Integer In Digit

                    If D = Rand1 Or D = Rand2 Then
                        'MsgBox("Execute" & D)
                        Points += 2

                        If D = Row2 Or D = Row2C2 Then
                            'MsgBox(Row2C2)
                            Points += 1

                            'ElseIf D = Row3 Or D = Row3C2 Then
                            '    Points -= 1
                        End If

                        If D = Row3 Or D = Row3C2 Then
                            Points -= 1
                        End If

                    ElseIf D = Row2 Or D = Row2C2 Then
                        Points += 1
                        If D = Row3 Or D = Row3C2 Then
                            'MsgBox(Row3C3)
                            Points -= 1
                        End If

                    ElseIf D = Row3 Or D = Row3C2 Then
                        Points -= 1
                    Else
                        Points += 0
                    End If
                Next
            End If


            '  Lvl play for 2 Digit Ends

            ' Level Play for lvl() 3 Digits below 

            If lblLevel.Content = "Level #3" Then

                For Each D As Integer In Digit

                    If D = Rand1 Or D = Rand2 Or D = Rand3 Then
                        'MsgBox("Execute" & D)
                        Points += 2

                        If D = Row2 Or D = Row2C2 Or D = Row2C3 Then
                            'MsgBox(Row2C2)
                            Points += 1

                            'ElseIf D = Row3 Or D = Row3C2 Or D = Row3C3 Then
                            '    Points -= 1
                        End If

                        If D = Row3 Or D = Row3C2 Or D = Row3C3 Then
                            Points -= 1
                        End If

                    ElseIf D = Row2 Or D = Row2C2 Or D = Row2C3 Then
                        Points += 1
                        If D = Row3 Or D = Row3C2 Or D = Row3C3 Then
                            'MsgBox(Row3C3)
                            Points -= 1
                        End If

                    ElseIf D = Row3 Or D = Row3C2 Or D = Row3C3 Then
                        Points -= 1
                    Else
                        Points += 0
                    End If
                Next
            End If


            '  Lvl play for 3 Digit Ends

            StoOut = Points

            txtOutput.Text = StoOut
            txtInput.Text = ""

            ' Change Number of Tries

            If lblLevel.Content = "Level #1" And StoOut = 2 Then
                MsgBox("You Guessed it Right.")
                txtTry.Text = "0"
            ElseIf lblLevel.Content = "Level #2" And StoOut >= 4 And (Digit(0) Or Digit(1)) = (Rand1 Or Rand2) Then
                MsgBox("You Guessed it Right.")
                txtTry.Text = "0"
            ElseIf lblLevel.Content = "Level #3" And StoOut = 7 And (Digit(0) Or Digit(1) Or Digit(2)) = (Rand1 Or Rand2 Or Rand3) Then
                MsgBox("You Guessed it Right.")
                txtTry.Text = "0"
            Else
                If txtTry.Text = "0" Then
                    txtTry.Text = "1"
                ElseIf txtTry.Text = "1" Then
                    txtTry.Text = "2"
                ElseIf txtTry.Text = "2" Then
                    txtTry.Text = "3"
                ElseIf txtTry.Text = "3" Then
                    txtTry.Text = "0"
                End If
            End If

            ' Change Number of Tries ends

            'Adds to Total Below

            Dim TotalP As Integer = txtTotal.Text
            TotalP += CInt(StoOut)
            txtTotal.Text = CStr(TotalP)



            If txtTry.Text = "0" And lblLevel.Content = "Level #1" Then
                MsgBox("Solution: " & Rand1)
            ElseIf txtTry.Text = "0" And lblLevel.Content = "Level #2" Then
                MsgBox("Solution: " & Rand1 & Rand2)
            ElseIf txtTry.Text = "0" And lblLevel.Content = "Level #3" Then
                MsgBox("Solution: " & Rand1 & Rand2 & Rand3)
            End If

            '   Show Solution ABove

            'Helper Grid below

            Gr1.Text = CStr(Digit(0))
            Dim Grid1 = Gr1.Text
            Gr4.Text = CStr(Grid1 - 1)
            Gr7.Text = CStr(Grid1 + 2)


            Gr2.Text = CStr(Digit(1))
            If lblLevel.Content = "Level #2" Or lblLevel.Content = "Level #3" Then
                Gr2.Visibility = Visibility.Visible
                Gr5.Visibility = Visibility.Visible
                Gr8.Visibility = Visibility.Visible
            Else
                Gr2.Visibility = Visibility.Hidden
                Gr5.Visibility = Visibility.Hidden
                Gr8.Visibility = Visibility.Hidden
            End If
            Dim Grid2 = Gr2.Text
            Gr5.Text = CStr(Grid2 - 1)
            Gr8.Text = CStr(Grid2 + 2)


            Gr3.Text = CStr(Digit(2))
            If lblLevel.Content <> "Level #3" Then
                Gr3.Visibility = Visibility.Hidden
                Gr6.Visibility = Visibility.Hidden
                Gr9.Visibility = Visibility.Hidden
            Else
                Gr3.Visibility = Visibility.Visible
                Gr6.Visibility = Visibility.Visible
                Gr9.Visibility = Visibility.Visible
            End If
            Dim Grid3 As Integer = Gr3.Text
            Gr6.Text = CStr(Grid3 - 1)
            Gr9.Text = CStr(Grid3 + 2)

            ' Helper Grid Ends

            '   Level Changer Below

            If TotalP > 10 And TotalP < 20 And txtTry.Text = "0" Then
                lblLevel.Content = "Level #2"
                lblDigit.Content = "Enter 2 Digits"
            ElseIf TotalP > 20 And txtTry.Text = "0" Then
                lblLevel.Content = "Level #3"
                lblDigit.Content = "Enter 3 Digits"
            End If

            '   Level Changer Ends
            Dim Round As Integer = txtRound.Text
            If txtTry.Text = "0" Then
                Round += 1
                txtRound.Text = Round
            End If

        Catch ex As Exception
            MsgBox("Please " & lblDigit.Content & " Minimum.",, "Error")
        End Try

    End Sub

    Private Sub txtInput_TextChanged(sender As Object, e As TextChangedEventArgs) Handles txtInput.TextChanged
        If lblLevel.Content = "Level #1" Then
            txtInput.MaxLength = 1
        ElseIf lblLevel.Content = "Level #2" Then
            txtInput.MaxLength = 2
        Else
            txtInput.MaxLength = 3
        End If
    End Sub
End Class
