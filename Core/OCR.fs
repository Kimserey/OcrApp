namespace OrcApp.Core

open System
open System.IO
open Tesseract
open System.Drawing
open ImageMagick
open FredsImageMagickScripts

module OCR =
    
    type TextResult = TextResult of string 
    type Confidence = Confidence of float32

    let getText imgStream =
        
        // Use ImageMagick to process image before OCR
        // 
        use img = new MagickImage(new Bitmap(Bitmap.FromStream imgStream))
        let cleaner = TextCleanerScript()
        cleaner.FilterOffset <- new Percentage(9.1)
        cleaner.Trim <- true

        // Use processed image with Tesseract OCR to get text
        //
        use engine = new TesseractEngine("tessdata", "eng")
        use img    = PixConverter.ToPix (cleaner.Execute(img).ToBitmap())
        use page   = engine.Process img

        // Result
        TextResult (page.GetText()), Confidence (page.GetMeanConfidence())