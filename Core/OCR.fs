namespace OrcApp.Core

open System
open System.IO
open Tesseract
open System.Drawing

module OCR =
    
    type TextResult = TextResult of string 
    type Confidence = Confidence of float32

    let getText imgStream =
        use engine = new TesseractEngine("tessdata", "eng")
        use img    = PixConverter.ToPix (new Bitmap(Bitmap.FromStream imgStream))
        use page   = engine.Process img
        TextResult (page.GetText()), Confidence (page.GetMeanConfidence())