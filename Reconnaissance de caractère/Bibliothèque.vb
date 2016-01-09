Public Class Bibliothèque
    Dim DocumentXML As New Xml.XmlDocument
   
    Dim AttributCaractereTampon As Xml.XmlAttribute = Nothing

    Dim Int As New DimensionnementImage.GestionImage
    Dim Comp As New CompImage
    Dim caractere(3) As String
    Dim NombreCaractere As Integer
    Dim NombreCaractereMemoire As Integer

   
    Public Function GetNbCaractere()
        Return NombreCaractereMemoire
    End Function
    Public Function Charger()
        Dim TablauImage(0) As Object

        DocumentXML.Load(".\BibliothequeImage.XML")
        Dim NoeudResolution As Xml.XmlNode = DocumentXML.ChildNodes.Item(0)

        For Each TailleResolution As Xml.XmlNode In NoeudResolution.ChildNodes

            Dim NouvelleResolution As Integer = TailleResolution.Attributes("Taille").Value
            Dim ImageTampon(NouvelleResolution - 1, NouvelleResolution - 1) As Boolean

            For Each modeles As Xml.XmlNode In TailleResolution.ChildNodes
                For ligne = 0 To NouvelleResolution - 1
                    Dim Pixels As String = modeles.ChildNodes.Item(ligne).Attributes("pixels").Value
                    For colone = 0 To NouvelleResolution - 1
                        If Mid(Pixels, colone + 1, 1) = "1" Then
                            ImageTampon(colone, ligne) = True
                        ElseIf Mid(Pixels, colone + 1, 1) = "0" Then
                            ImageTampon(colone, ligne) = False
                        End If
                    Next
                Next
                NombreCaractere = NombreCaractere + 1
                ReDim Preserve caractere(NombreCaractere - 1)
                caractere(NombreCaractere - 1) = modeles.Attributes("caractère").Value
                ReDim Preserve TablauImage(NombreCaractere - 1)
                TablauImage(NombreCaractere - 1) = ImageTampon

                ReDim ImageTampon(NouvelleResolution - 1, NouvelleResolution - 1)
                NombreCaractereMemoire = NombreCaractere
            Next
        Next
        NombreCaractere = 0
        Return TablauImage

    End Function

    Public Sub Enregistrer(ByVal image(,) As Boolean, ByVal texte As String)
        Dim NouvelleResolution As Boolean = False
        Dim NouvelleImage As Boolean = False

        Dim AttributCaractereEnregistrer As Xml.XmlAttribute = Nothing
        DocumentXML.Load(".\BibliothequeImage.XML") 'On charge le fichier xml

        Dim Images As Xml.XmlElement = DocumentXML.CreateElement("Images")

        For Each NoeudRésolution1 As Xml.XmlNode In DocumentXML.ChildNodes.Item(0).ChildNodes
            If NoeudRésolution1.Attributes("Taille").Value = image.GetLength(0) Then
                NouvelleResolution = False
                AttributCaractereEnregistrer = DocumentXML.CreateAttribute("caractère")
                AttributCaractereEnregistrer.InnerText = texte
                Images.SetAttributeNode(AttributCaractereEnregistrer)
                For i = 0 To image.GetLength(1) - 1
                    Dim ligne As Xml.XmlElement = DocumentXML.CreateElement("Ligne")
                    Dim AttributLigne As Xml.XmlAttribute = DocumentXML.CreateAttribute("pixels")
                    Dim pixel As String = ""
                    For j = 0 To image.GetLength(0) - 1
                        If (image(j, i) = True) Then
                            pixel += "1"
                        Else
                            pixel += "0"
                        End If
                    Next
                    DocumentXML.Save(Application.StartupPath & ".\BibliothequeImage.XML")
                    AttributLigne.InnerText = pixel
                    ligne.SetAttributeNode(AttributLigne)
                    Images.AppendChild(ligne)
                Next
                NouvelleImage = True
                NoeudRésolution1.AppendChild(Images)
                AttributCaractereTampon = AttributCaractereEnregistrer
                MsgBox("Enregistrement réussi")
            Else : NouvelleResolution = True
            End If
        Next
        If (NouvelleResolution = True And NouvelleImage = False) Then
            Créer_Bibliothèque_Nouvelle_Résolution(image.GetLength(0))
            Enregistrer(image, texte)
        End If


        DocumentXML.Save(Application.StartupPath & ".\BibliothequeImage.XML") 'On sauvegarde le fichier xml


    End Sub
    Public Sub Créer_Bibliothèque_Nouvelle_Résolution(ByVal resolution_x As Integer)
        Dim NoeudRésolution As Xml.XmlElement
        Dim AttributRésolution As Xml.XmlAttribute
        DocumentXML.Load(".\BibliothequeImage.XML")

        NoeudRésolution = DocumentXML.CreateElement("Résolution")
        DocumentXML.DocumentElement.AppendChild(NoeudRésolution)
        AttributRésolution = DocumentXML.CreateAttribute("Taille")
        AttributRésolution.InnerText = resolution_x
        NoeudRésolution.SetAttributeNode(AttributRésolution)
        DocumentXML.Save(".\BibliothequeImage.XML")

    End Sub

    Public Function Créer_fichier_xml()
        Dim NoeudRésolution1 As Xml.XmlElement
        Dim NoeudRésolution2 As Xml.XmlElement
        Dim NoeudRésolution3 As Xml.XmlElement
        Dim AttributResolution As Xml.XmlAttribute
        DocumentXML.LoadXml("<Bibliotheque></Bibliotheque>")

        NoeudRésolution1 = DocumentXML.CreateElement("Résolution")
        DocumentXML.DocumentElement.AppendChild(NoeudRésolution1)
        AttributResolution = DocumentXML.CreateAttribute("Taille")
        AttributResolution.InnerText = 28
        NoeudRésolution1.SetAttributeNode(AttributResolution)


        NoeudRésolution2 = DocumentXML.CreateElement("Résolution")
        DocumentXML.DocumentElement.AppendChild(NoeudRésolution2)
        AttributResolution = DocumentXML.CreateAttribute("Taille")
        AttributResolution.InnerText = 15
        NoeudRésolution2.SetAttributeNode(AttributResolution)


        NoeudRésolution3 = DocumentXML.CreateElement("Résolution")
        DocumentXML.DocumentElement.AppendChild(NoeudRésolution3)
        AttributResolution = DocumentXML.CreateAttribute("Taille")
        AttributResolution.InnerText = 10
        NoeudRésolution3.SetAttributeNode(AttributResolution)




        DocumentXML.Save(Application.StartupPath & ".\BibliothequeImage.XML")
        Return Nothing
    End Function
    Public Function Trouver(ByVal image1 As Boolean(,))
        Dim ImageFromBibliotheque() As Object = Charger()
        Dim image As Boolean(,)
        Dim TauxCorrespondance As Integer
        Dim k As Integer
        TauxCorrespondance = 0

        For i = 0 To NombreCaractereMemoire - 1

            image = ImageFromBibliotheque(i)
            If image.GetLength(0) = image1.GetLength(0) Then
                If image.GetLength(1) = image1.GetLength(1) Then
                    If TauxCorrespondance < Comp.Comparer(image1, image) Then
                        TauxCorrespondance = Comp.Comparer(image1, image)
                        k = i
                    End If
                End If
            End If
        Next
        Return caractere(k)
    End Function
End Class
