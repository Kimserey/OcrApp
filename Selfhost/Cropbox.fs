namespace OrcApp.Host

open WebSharper
open WebSharper.JavaScript

/// Wraps cropbox.js functionalities to be able to be used with WebSharper.
/// cropbox is a plugin to crop avatar.
/// https://github.com/hongkhanh/cropbox/tree/master/javascript
[<JavaScript>]
module Cropbox =

    type Identifiers = {
        file: string
        crop: string
        zoomIn: string
        zoomOut: string
    } with
        static member Default = { file = "file"; crop = "btnCrop"; zoomIn = "btnZoomIn"; zoomOut = "btnZoomOut" }

    type Options = {
        imageBox: string
        thumbBox: string
        spinner: string
        imgSrc: string
    } with
        static member Default = { imageBox = ".imageBox"; thumbBox = ".thumbBox"; spinner = ".spinner"; imgSrc = "avatar.png" }

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