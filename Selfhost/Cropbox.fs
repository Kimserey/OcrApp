namespace OrcApp.Host

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

/// Wraps cropbox.js functionalities to be able to be used with WebSharper.
/// cropbox is a plugin to crop avatar.
/// https://github.com/hongkhanh/cropbox/tree/master/javascript
[<JavaScript>]
module Cropbox =

    type Image = {
        ContentType: string
        ContentBase64: string
    } 

    type Identifiers = {
        [<Name "file">]
        File: string
        [<Name "crop">]
        Crop: string
        [<Name "zoomIn">]
        ZoomInButton: string
        [<Name "zoomOut">]
        ZoomOutButton: string
    } with
        static member Default = { File = "file"; Crop = "btnCrop"; ZoomInButton = "btnZoomIn"; ZoomOutButton = "btnZoomOut" }

    type Options = {
        [<Name "imageBox">]
        ImageBox: string
        [<Name "thumbBox">]
        ThumbBox: string
        [<Name "spinner">]
        Spinner: string
        [<Name "imgSrc">]
        ImageSource: string
    } with
        static member Default = { ImageBox = ".imageBox"; ThumbBox = ".thumbBox"; Spinner = ".spinner"; ImageSource = "avatar.png" }

    [<Direct("""var cropper;
                document.querySelector('#' + $identifiers.file).addEventListener('change', function () {
                    var reader = new FileReader();
                    reader.onload = function (e) {
                        $opt.imgSrc = e.target.result;
                        cropper = new cropbox($opt);
                    }
                    reader.readAsDataURL(this.files[0]);
                    this.files = [];
                })
                document.querySelector('#' + $identifiers.crop).addEventListener('click', function(){
                    var img = cropper.getDataURL();
                    var arr = img.match(/^data:([a-z//]+);base64,([A-Za-z0-9+/=]+)/);
                    $onCrop(arr.slice(1, 3));
                })
                document.querySelector('#' + $identifiers.zoomIn).addEventListener('click', function(){
                    cropper.zoomIn();
                })
                document.querySelector('#' + $identifiers.zoomOut).addEventListener('click', function(){
                    cropper.zoomOut();
                })""")>]
    let init (identifiers: Identifiers) (opt: Options) (onCrop: string[] -> unit) = X<unit>

    let cropper handle =
        divAttr [ attr.``class`` "cropper"
                  on.afterRender (fun _ -> 
                    init Identifiers.Default 
                         Options.Default 
                         (fun values -> 
                            let img = { ContentType = values.[0]; ContentBase64 = values.[1] }
                            handle img)
                  ) ] 
                [ divAttr [ attr.style "width: 100%; margin: auto; margin-bottom: 10px;" ]
                          [ inputAttr [ attr.``type`` "file"
                                        attr.style "margin: 5px 0;"
                                        attr.id Identifiers.Default.File ] []
                            divAttr [ attr.id Identifiers.Default.Crop
                                      attr.``class`` "btn btn-primary btn-block" ] [ text "SCAN TEXT" ] ]
                  divAttr [ attr.``class`` "imageBox" ]
                          [ divAttr [ attr.``class`` "zoom-btn" ] 
                                    [ divAttr [ attr.id Identifiers.Default.ZoomInButton
                                                attr.``class`` "btn btn-primary btn-block"  ] 
                                              [ iAttr [ attr.``class`` "fa fa-search-plus"  ] [] ]
                                      divAttr [ attr.id Identifiers.Default.ZoomOutButton
                                                attr.``class`` "btn btn-primary btn-block"  ] 
                                              [ iAttr [ attr.``class`` "fa fa-search-minus" ] [] ] ]
                            divAttr [ attr.``class`` "thumbBox" ] []
                            divAttr [ attr.``class`` "spinner"
                                      attr.style "display: none" ] 
                                    [ text "Loading..." ] ] ]