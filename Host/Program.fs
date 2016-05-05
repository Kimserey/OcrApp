open OrcApp.Core.OCR
open WebSharper
open WebSharper.JavaScript
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

module Remoting =
    [<Rpc>]
    let sayHello() = 
        async.Return "Hello!"

[<JavaScript>]
module Client =
    let main() =
        View.Const ()
        |> View.MapAsync Remoting.sayHello
        |> Doc.BindView text

module Server =
    let site =
        Application.SinglePage (fun _-> 
            Content.Page [ client <@ Client.main() @> ])

do Warp.RunAndWaitForInput Server.site |> ignore