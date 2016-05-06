namespace OrcApp.Host

open System
open System.IO
open System.Drawing
open OrcApp.Core
open WebSharper
open WebSharper.JavaScript
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.Owin
open global.Owin
open Microsoft.Owin.Hosting
open Microsoft.Owin.StaticFiles
open Microsoft.Owin.FileSystems
open Cropbox
open WebSharper.Resources

module Resources =

    module General =
        type Style() =
           inherit BaseResource("style.css")

    [<assembly:System.Web.UI.WebResource("style.css", "text/css"); 
      assembly:Require(typeof<General.Style>)>]
    do()

module Remoting =
    [<Rpc>]
    let getTextNoCleaning (imageBase64String: string): Async<string * string list * float> = 
        use memory = new MemoryStream(Convert.FromBase64String imageBase64String)
        let OCR.Image imageBase64, OCR.TextResult txt, OCR.Confidence confidence = OCR.getTextNoCleaning memory
        async.Return (imageBase64, txt.Split '\n' |> Array.toList, float confidence)

    [<Rpc>]
    let getText (imageBase64String: string): Async<string * string list * float> = 
        use memory = new MemoryStream(Convert.FromBase64String imageBase64String)
        let OCR.Image imageBase64, OCR.TextResult txt, OCR.Confidence confidence = OCR.getText memory
        async.Return (imageBase64, txt.Split '\n' |> Array.toList, float confidence)

[<JavaScript>]
module Client =
    open Bootstrap

    let ocrResult = Var.Create ("", [], 0.)
    let ocrNoCleaningResult = Var.Create ("", [], 0.)
    let isLoading = Var.Create false

    
    let display txtView isLoadingView =
        divAttr 
            [ attr.``class`` "well" ]
            [ (txtView, isLoadingView)
              ||> View.Map2 (fun (imgBase64, txt, confidence) isLoading ->imgBase64, txt, confidence, isLoading)
              |> Doc.BindView(fun (imgBase64: string, txt: string list, confidence: float, isLoading: bool) -> 
                    if isLoading then
                        iAttr [ attr.``class`` "fa fa-refresh fa-spin fa-3x fa-fw margin-bottom" ] [] :> Doc
                    else
                        [ strong [ Doc.TextNode "Scanned image:" ] 
                          div [ imgAttr [ attr.src ("data:image/jpeg;base64," + imgBase64) ] [] ]
                          br []
                          p [ yield strong [ Doc.TextNode "Detected text:" ] :> Doc
                              yield! txt |> List.map (fun t -> div [ Doc.TextNode t ] :> Doc) ]
                          br []
                          strong [ Doc.TextNode ("Confidence:" + string confidence) ] ]
                        |> Seq.cast
                        |> Doc.Concat) ]

    let handle (img: Image) =
        async {
            Var.Set isLoading true
            let! res1 = Remoting.getText img.ContentBase64
            Var.Set ocrResult res1
            let! res2 = Remoting.getTextNoCleaning img.ContentBase64
            Var.Set ocrNoCleaningResult res2
            Var.Set isLoading false
        } |> Async.Start

    let main() =
        [ Header.Create HeaderType.H1 "Read text from image in WebSharper/F#"
          |> Header.AddSubtext "Using ImageMagick and Tesseract-OCR"
          |> Header.Render

          Hyperlink.Create(HyperlinkAction.Href "https://twitter.com/Kimserey_Lam", "-> Follow me on twitter @Kimserey_Lam <-")
          |> Hyperlink.Render :> Doc
          br [] :> Doc
          Hyperlink.Create(HyperlinkAction.Href "https://github.com/Kimserey/OcrApp", "-> Source code available here <-")
          |> Hyperlink.Render :> Doc

          GridRow.Create [ GridColumn.Create([ Header.Create HeaderType.H3 "Scan image"
                                               |> Header.Render
                                               Cropbox.cropper handle ], [ GridColumnSize.ColMd4; GridColumnSize.ColSm6 ])

                           GridColumn.Create([ Header.Create HeaderType.H3 "With Textcleaner"
                                               |> Header.Render
                                               display ocrResult.View isLoading.View ], [ GridColumnSize.ColMd4; GridColumnSize.ColSm6 ]) 

                           GridColumn.Create([ Header.Create HeaderType.H3 "Without Textcleaner"
                                               |> Header.Render
                                               display ocrNoCleaningResult.View isLoading.View ], [ GridColumnSize.ColMd4; GridColumnSize.ColSm6 ])]
          |> GridRow.AddCustomStyle "margin-top: 50px;"
          |> GridRow.Render :> Doc ]
        |> Doc.Concat

module Site =
    module Main =
        type Page = { Body: Doc list }
        let template = 
            Content
                .Template<Page>(__SOURCE_DIRECTORY__ + "/index.html")
                .With("body", fun x -> x.Body)

        let site = Application.SinglePage (fun _ -> 
            Content.WithTemplate template { Body = [ divAttr [ attr.style "padding:15px;" ] [ client <@ Client.main() @> ] ] })

    [<EntryPoint>]
    let main args =
        let rootDirectory, url =
            match args with
            | [| rootDirectory; url |] -> rootDirectory, url
            | [| url |] -> "../..", url
            | [| |] -> "../..", "http://localhost:9900/"
            | _ -> eprintfn "Usage: Host ROOT_DIRECTORY URL"; exit 1
        use server = WebApp.Start(url, fun appB ->
            appB.UseStaticFiles(
                    StaticFileOptions(
                        FileSystem = PhysicalFileSystem(rootDirectory)))
                .UseSitelet(rootDirectory, Main.site)
            |> ignore)
        stdout.WriteLine("Serving {0}", url)
        stdin.ReadLine() |> ignore
        0