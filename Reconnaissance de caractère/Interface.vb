
Public Class Form1
    Dim outilcomparaison As New CompImage
    Dim dll As New DimensionnementImage.GestionImage
    Dim imagesource As Color(,)
    Dim imagetest As Color(,)
    Dim image1 As Color(,)
    Dim image2 As Color(,)
    Dim imageR As Color(,)
    Dim Dct As New Xml.XmlDocument
    Dim bibli As New Bibliothèque
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click 'imagesource
        Dim boiteParcourir As OpenFileDialog
        boiteParcourir = New OpenFileDialog
        If boiteParcourir.ShowDialog(Me) = DialogResult.OK Then
            PictureBox1.Image = Image.FromFile(boiteParcourir.FileName)
            imagesource = dll.chargerimage(boiteParcourir.FileName)
        End If
       

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)  'imagetest
        
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click 'Comparaison des deux images
        Dim imageredimensionnée As Color(,)

        imageredimensionnée = dll.agrandissementaupproche(image1.GetLength(0), image1.GetLength(1), image2)
        TextBox1.Text = outilcomparaison.Comparerpixel(image1, imageredimensionnée)
        noirblanctextbox.Text = outilcomparaison.comparertableauxnoirblanc(image1, imageredimensionnée)

    End Sub


    
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
     
        bibli.Enregistrer(outilcomparaison.Centrer(outilcomparaison.GetPixel(imagesource)), LettreTexteBox.Text)
       
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        bibli.Créer_fichier_xml()
        
    End Sub

    Private Sub Comparertexte_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Comparertexte.Click
        If (TextBox5.Text <> "" And TextBox6.Text <> "") Then
            ValeurComparaison.Text = bibli.Trouver(outilcomparaison.Centrer(outilcomparaison.GetPixel(imagetest)))
        Else : ValeurComparaison.Text = bibli.Trouver(outilcomparaison.Get_Pixel(dll.agrandissementaupproche(TextBox6.Text, TextBox5.Text, outilcomparaison.Cadrer_Image_color(outilcomparaison.Get_Image(imagetest)))))
        End If


    End Sub

    Private Sub Button2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim boiteParcourir As OpenFileDialog
        boiteParcourir = New OpenFileDialog
        If boiteParcourir.ShowDialog(Me) = DialogResult.OK Then
            PictureBox2.Image = Image.FromFile(boiteParcourir.FileName)
            imagetest = dll.chargerimage(boiteParcourir.FileName)
        End If
       
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Dim boiteParcourir As OpenFileDialog
        boiteParcourir = New OpenFileDialog
        If boiteParcourir.ShowDialog(Me) = DialogResult.OK Then
            PictureBox3.Image = Image.FromFile(boiteParcourir.FileName)
            image1 = dll.chargerimage(boiteParcourir.FileName)
        End If
    End Sub

   
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        Dim boiteParcourir As OpenFileDialog
        boiteParcourir = New OpenFileDialog
        If boiteParcourir.ShowDialog(Me) = DialogResult.OK Then
            PictureBox4.Image = Image.FromFile(boiteParcourir.FileName)
            image2 = dll.chargerimage(boiteParcourir.FileName)
        End If
    End Sub



 
    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Dim boiteParcourir As OpenFileDialog
        boiteParcourir = New OpenFileDialog
        If boiteParcourir.ShowDialog(Me) = DialogResult.OK Then
            PictureBox5.Image = Image.FromFile(boiteParcourir.FileName)
            imageR = dll.chargerimage(boiteParcourir.FileName)
        End If
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        If (TextBox3.Text <> "" And TextBox4.Text <> "") Then
            bibli.Enregistrer(outilcomparaison.Get_Pixel(dll.agrandissementaupproche(TextBox3.Text, TextBox4.Text, outilcomparaison.Cadrer_Image_color(outilcomparaison.Get_Image(imageR)))), TextBox2.Text)
        Else : bibli.Enregistrer(outilcomparaison.Get_Pixel(outilcomparaison.Cadrer_Image_color(outilcomparaison.Get_Image(imageR))), TextBox2.Text)
        End If


    End Sub
End Class