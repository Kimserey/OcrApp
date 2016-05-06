namespace OrcApp.Core

open System
open System.IO
open Tesseract
open System.Drawing
open ImageMagick
open FredsImageMagickScripts

module OCR =
    
    type Image = Image of string
    type TextResult = TextResult of string 
    type Confidence = Confidence of float32

    let imageToByte (img: Bitmap) =
        use stream = new MemoryStream()
        img.Save(stream, System.Drawing.Imaging.ImageFormat.Png)
        stream.ToArray()

    let getText imgStream =
        
        // Use ImageMagick to process image before OCR
        // 
        use img = new MagickImage(new Bitmap(Bitmap.FromStream imgStream))
        let cleaner = TextCleanerScript()
        cleaner.FilterOffset <- new Percentage(15.)
        cleaner.Trim <- true
        let cleaned = cleaner.Execute(img).ToBitmap()

        // Use processed image with Tesseract OCR to get text
        //
        use engine = new TesseractEngine("tessdata", "eng")
        use img    = PixConverter.ToPix cleaned
        use page   = engine.Process img

        // Result
        Image (Convert.ToBase64String (imageToByte cleaned)), TextResult (page.GetText()), Confidence (page.GetMeanConfidence())

    let getTextNoCleaning imgStream =
        let original = new Bitmap(Bitmap.FromStream imgStream)
        use engine = new TesseractEngine("tessdata", "eng")
        use img    = PixConverter.ToPix original
        use page   = engine.Process img

        // Result
        Image (Convert.ToBase64String (imageToByte original)),
        TextResult (page.GetText()),
        Confidence (page.GetMeanConfidence())