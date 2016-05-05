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
    let getText (imageBase64String: string): Async<string list * float> = 
        use memory = new MemoryStream(Convert.FromBase64String imageBase64String)
        let OCR.TextResult txt, OCR.Confidence confidence = OCR.getText memory
        async.Return (txt.Split '\n' |> Array.toList, float confidence)

[<JavaScript>]
module Client =
    open Bootstrap

    let ocrResult = Var.Create ([], 0.)
    let isLoading = Var.Create false
    
    let display txtView isLoadingView =
        (txtView, isLoadingView)
        ||> View.Map2 (fun (txt, confidence) isLoading -> txt, confidence, isLoading)
        |> Doc.BindView(fun (txt: string list, confidence: float, isLoading: bool) -> 
            if isLoading then
                iAttr [ attr.``class`` "fa fa-refresh fa-spin fa-3x fa-fw margin-bottom" ] [] :> Doc
            else
                txt 
                |> List.map (fun t -> div [ Doc.TextNode t ] :> Doc)
                |> Doc.Concat)

    let handle (img: Image) =
        async {
            Var.Set isLoading true
            let! res = Remoting.getText img.ContentBase64
            Var.Set ocrResult res
            Var.Set isLoading false
        } |> Async.Start

    let main() =
        [ Header.Create HeaderType.H1 "Read text from image in WebSharper/F#"
          |> Header.AddSubtext "Using ImageMagick and Tesseract-OCR"
          |> Header.Render

          Hyperlink.Create(HyperlinkAction.Href "https://twitter.com/Kimserey_Lam", "Follow me on twitter @Kimserey_Lam")
          |> Hyperlink.Render :> Doc

          br [] :> Doc

          Hyperlink.Create(HyperlinkAction.Href "https://twitter.com/Kimserey_Lam", "Source code available here.")
          |> Hyperlink.Render :> Doc

          GridRow.Create [ GridColumn.Create([ Cropbox.cropper handle ], [ GridColumnSize.ColMd6 ])
                           GridColumn.Create([ display ocrResult.View isLoading.View ], [ GridColumnSize.ColMd6 ]) ]
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