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

[<JavaScript; AutoOpen>]
module Domain =
    type Image = {
        ContentType: string
        ContentBase64: string
    } with
        member x.Src = sprintf "data:%s;base64,%s" x.ContentType x.ContentBase64

module Remoting =
    [<Rpc>]
    let getText (img: Image): Async<string> = 
        use memory = new MemoryStream(Convert.FromBase64String img.ContentBase64)
        let OCR.TextResult txt, OCR.Confidence confidence = OCR.getText memory
        async.Return txt

[<JavaScript>]
module Client =
    
    let initCropbox() =
        Cropbox.init
        <| Identifiers.Default
        <| Options.Default
        <| fun values -> 
            async {
                let! txt = Remoting.getText({ ContentType = values.[0]; ContentBase64 = values.[1] })
                Console.Log txt
            }
            |> Async.Start

    let main() =
        divAttr [ attr.style "width: 400px; height: 400px; margin: 20px auto;" 
                  on.afterRender (fun _ -> initCropbox()) ] 
                [ divAttr [ attr.``class`` "imageBox" ]
                          [ divAttr [ attr.``class`` "zoom-btn" ] 
                                    [ divAttr [ attr.id Identifiers.Default.zoomIn
                                                attr.``class`` "btn btn-primary btn-block"  ] 
                                              [ iAttr [ attr.``class`` "fa fa-search-plus"  ] [] ]
                                      divAttr [ attr.id Identifiers.Default.zoomOut
                                                attr.``class`` "btn btn-primary btn-block"  ] 
                                              [ iAttr [ attr.``class`` "fa fa-search-minus" ] [] ] ]
                            divAttr [ attr.``class`` "thumbBox" ] []
                            divAttr [ attr.``class`` "spinner"
                                      attr.style "display: none" ] 
                                    [ text "Loading..." ] ]
                  divAttr [ attr.style "width: 60%; margin: auto;" ]
                          [ inputAttr [ attr.``type`` "file"
                                        attr.style "margin: 5px 0;"
                                        attr.id Identifiers.Default.file ] []
                            divAttr [ attr.id Identifiers.Default.crop
                                      attr.``class`` "btn btn-primary btn-block" ] [ text "Upload picture" ]
                            pAttr   [ attr.style "text-align:center; margin: 10px 0;" ] [ text "or use default" ] ] ]

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