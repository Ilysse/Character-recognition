Public Class CompImage




    'On compare pixel par pixel les deux images.
    Public Function Comparerpixel(ByVal image1(,) As Color, ByVal image2(,) As Color)
        Dim pixelcom As Integer = 0
        Dim pixeltot As Integer = 0



        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image2.GetLength(1) - 1
                If image1(i, j) = image2(i, j) Then
                    pixelcom = pixelcom + 1
                End If
                pixeltot = pixeltot + 1
            Next
        Next
        Return (pixelcom / pixeltot) * 100 & "%"
    End Function



    'On remplit des tableaux boolean représentant la couleur (rouge+bleu+vert) d'une pixel puis on compare ces tableaux.
    Public Function comparertableauxcouleur(ByVal image1(,) As Color, ByVal image2(,) As Color, ByVal pas As Integer, ByVal référence As Double)
        Dim pixelcom As Integer = 0
        Dim pixeltot As Integer = 0
        Dim pixel_imagesource(image1.GetLength(0), image1.GetLength(1)) As Boolean
        Dim pixel_imageredimensionnée(image2.GetLength(0), image2.GetLength(1)) As Boolean

        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image1.GetLength(1) - 1
                If pixel_imageredimensionnée(i, j) = pixel_imagesource(i, j) Then
                    pixelcom = pixelcom + 1
                End If
                pixeltot = pixeltot + 1
            Next
        Next
        Return (pixelcom / pixeltot) * 100 & "%"

    End Function

    'On remplit des tableaux boolean représentant la couleur noir ou blanche d'une pixel puis on compare ces tableaux.
    Public Function comparertableauxnoirblanc(ByVal image1(,) As Color, ByVal image2(,) As Color)
        Dim pixelcom As Integer = 0
        Dim pixeltot As Integer = 0
        Dim pixel_imagesource(image1.GetLength(0), image1.GetLength(1)) As Boolean
        Dim pixel_imageredimensionnée(image2.GetLength(0), image2.GetLength(1)) As Boolean


        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image1.GetLength(1) - 1
                If image1(i, j).ToArgb = Color.White.ToArgb Then
                    pixel_imagesource(i, j) = False
                Else : pixel_imagesource(i, j) = True
                    If image2(i, j).ToArgb = Color.White.ToArgb Then
                        pixel_imageredimensionnée(i, j) = False
                    Else : pixel_imageredimensionnée(i, j) = True
                    End If
                End If
            Next
        Next

        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image1.GetLength(1) - 1
                If pixel_imageredimensionnée(i, j) = pixel_imagesource(i, j) Then
                    pixelcom = pixelcom + 1
                End If
                pixeltot = pixeltot + 1
            Next
        Next
        Return (pixelcom / pixeltot) * 100 & "%"

    End Function
    Public Function GetPixel(ByVal image1(,) As Color) As Boolean(,)
        Dim pixel_imagesource(image1.GetLength(0) - 1, image1.GetLength(1) - 1) As Boolean
        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image1.GetLength(1) - 1
                If image1(i, image1.GetLength(1) - 1 - j).ToArgb = Color.White.ToArgb Then
                    pixel_imagesource(i, j) = False
                Else : pixel_imagesource(i, j) = True
                End If
            Next
        Next
        Return pixel_imagesource
    End Function
    Public Function Get_Image(ByVal image(,) As Color)
        Dim imageretour(image.GetLength(0) - 1, image.GetLength(1) - 1) As Color
        For i As Integer = 0 To image.GetLength(0) - 1
            For j As Integer = 0 To image.GetLength(1) - 1
                imageretour(i, j) = image(i, image.GetLength(1) - j - 1)
            Next
        Next
        Return imageretour
    End Function
    Public Function Get_Pixel(ByVal image(,) As Color)
        Dim pixel_imagesource(image.GetLength(0) - 1, image.GetLength(1) - 1) As Boolean
        For i As Integer = 0 To image.GetLength(0) - 1
            For j As Integer = 0 To image.GetLength(1) - 1
                If image(i, j).ToArgb = Color.White.ToArgb Then
                    pixel_imagesource(i, j) = False
                Else : pixel_imagesource(i, j) = True
                End If
            Next
        Next
        Return pixel_imagesource
    End Function
    Public Function Comparer(ByVal image1 As Boolean(,), ByVal image2 As Boolean(,)) As Integer
        Dim pixelcom, pixeltot As Integer
        pixelcom = 0
        pixeltot = 0
        For i As Integer = 0 To image1.GetLength(0) - 1
            For j As Integer = 0 To image1.GetLength(1) - 1
                If image1(i, j) = image2(i, j) Then
                    pixelcom = pixelcom + 1
                End If
                pixeltot = pixeltot + 1
            Next
        Next
        Return (pixelcom / pixeltot) * 100

    End Function
    Public Sub Initialisation(ByVal Image As Boolean(,))
        For i = 0 To Image.GetLength(0) - 1
            For j = 0 To Image.GetLength(1) - 1
                Image(i, j) = False
            Next
        Next
    End Sub
    Public Function Get_Barycentre(ByVal image As Boolean(,))
        Dim barycentre(1) As Integer
        Dim SommeAbscisse As Integer = 0
        Dim SommeOrdonnée As Integer = 0
        Dim x As Integer = 0
        Dim y As Integer = 0
        For i = 0 To image.GetLength(0) - 1
            For j = 0 To image.GetLength(1) - 1
                If image(i, j) = True Then
                    x = x + i
                    y = y + j
                    SommeAbscisse = SommeAbscisse + 1
                    SommeOrdonnée = SommeOrdonnée + 1
                End If
            Next
        Next
        barycentre(0) = x / SommeAbscisse
        barycentre(1) = y / SommeOrdonnée
        Return barycentre
    End Function
    Public Function Get_bords(ByRef image As Boolean(,))
        Dim Centre(1, 2) As Integer
        Dim x_max, y_max, x_min, y_min As Integer
        x_max = 0
        x_min = image.GetLength(0) - 1
        y_min = image.GetLength(1) - 1
        y_max = 0
        For i = 0 To image.GetLength(0) - 1
            For j = 0 To image.GetLength(1) - 1
                If image(i, j) = True Then
                    If x_max < i Then
                        x_max = i
                    End If
                    If y_max < j Then
                        y_max = j
                    End If
                    If x_min > i Then
                        x_min = i
                    End If
                    If y_min > j Then
                        y_min = j
                    End If
                End If
            Next
        Next
        Centre(0, 0) = (x_max + x_min) / 2
        Centre(1, 0) = (y_max + y_min) / 2
        Centre(0, 1) = x_max
        Centre(1, 1) = y_max
        Centre(0, 2) = x_min
        Centre(1, 2) = y_min
        Return Centre
    End Function
    Public Function Centrer(ByRef image As Boolean(,))
        Dim Centre(1, 2) As Integer
        Dim x_max, y_max, x_min, y_min, x_centre, y_centre As Integer
        Dim ImageCentrer(image.GetLength(0) - 1, image.GetLength(1) - 1) As Boolean
        Centre = Get_bords(image)
        x_centre = Int(image.GetLength(0) / 2) - Centre(0, 0)
        y_centre = Int(image.GetLength(1) / 2) - Centre(1, 0)
        x_max = Centre(0, 1)
        y_max = Centre(1, 1)
        x_min = Centre(0, 2)
        y_min = Centre(1, 2)
        If ((x_centre > 1 Or x_centre < -1) And (y_centre > 1 Or y_centre < -1)) Then
            For i = x_min To x_max
                For j = y_min To y_max
                    ImageCentrer(i + x_centre, j + y_centre) = image(i, j)
                Next
            Next
            For i = 0 To x_min - 1 + x_centre
                For j = 0 To y_min - 1 + y_centre

                    ImageCentrer(i, j) = False

                Next
            Next
            For i = x_max + x_centre + 1 To image.GetLength(0) - 1
                For j = y_max + y_centre To image.GetLength(1) - 1
                    ImageCentrer(i, j) = False

                Next
            Next
        Else : ImageCentrer = image
        End If
        Return ImageCentrer
    End Function
    Public Function Cadrer_Image_Boolean(ByVal image As Boolean(,))
        Dim bords(1, 2) As Integer
        Dim x_max, y_max, x_min, y_min, x_bords, y_bords As Integer
        bords = Get_bords(image)
        x_max = bords(0, 1)
        y_max = bords(1, 1)
        x_min = bords(0, 2)
        y_min = bords(1, 2)
        x_bords = (x_max + x_min) / 2
        y_bords = (y_min + y_max) / 2
        Dim Imagebords(x_max - x_min, y_max - y_min) As Boolean
        If ((x_bords > 1 Or x_bords < -1) And (y_bords > 1 Or y_bords < -1)) Then
            For i = 0 To x_max - x_min
                For j = 0 To y_max - y_min
                    Imagebords(i, j) = image(i + x_min, j + y_min)
                Next
            Next
        End If
        Return Imagebords
    End Function
    Public Function Cadrer_Image_color(ByVal image As Color(,))
        Dim bords(1, 2) As Integer
        Dim x_max, y_max, x_min, y_min, x_bords, y_bords As Integer
        bords = Get_bords(Get_Pixel(image))
        x_max = bords(0, 1)
        y_max = bords(1, 1)
        x_min = bords(0, 2)
        y_min = bords(1, 2)
        x_bords = (x_max + x_min) / 2
        y_bords = (y_min + y_max) / 2
        Dim Imagebords(x_max - x_min, y_max - y_min) As Color
        If ((x_bords > 1 Or x_bords < -1) And (y_bords > 1 Or y_bords < -1)) Then
            For i = 0 To x_max - x_min
                For j = 0 To y_max - y_min
                    Imagebords(i, j) = image(i + x_min, j + y_min)
                Next
            Next
        End If
        Return Imagebords
    End Function
End Class
