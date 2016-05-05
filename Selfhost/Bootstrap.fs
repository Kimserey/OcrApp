namespace OrcApp.Host

open WebSharper
open WebSharper.Forms
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<AutoOpen; JavaScript>]
module Utils =
    
    let (/+) (a: string) (b: string) = a.ToLower() + "/" + b.ToLower()

    module Option =

        let getOrElse x v =
            match v with
            | Some v -> v
            | None -> x

    module Async =

        let map f m =
            async {
                let! x = m
                return f x
            }

    module List =

        let rec insertAt i v l =
            match i, l with
            | 0, xs -> v::xs
            | i, x::xs -> x::insertAt (i - 1) v xs
            | i, [] -> [v]

        let rec removeAt i l =
            match i, l with
            | 0, x::xs -> xs
            | i, x::xs -> x::removeAt (i - 1) xs
            | i, [] -> []

    module Dom =

        module Element =
            
            let getId (x: Dom.Element) =
                x.GetAttribute "id"

[<JavaScript; AutoOpen>]
module WebSharperExtensions =
    
    type View 
        with
            ///Used to create custom error messages for forms and be able to filter the custom messages for display
            static member CustomErrorId = 5000

            static member ThroughCustomId(view: View<Result<'T>>) : View<Result<'T>> =
                view |> View.Map (fun x -> match x with Result.Success _ -> x | Failure msgs -> Failure (msgs |> List.filter (fun m -> m.Id = View.CustomErrorId)))

            static member ErrorMessages(view: View<Result<'T>>, rv: Var<'U>): View<string list> =
                View.Through(view, rv) |> View.Map (fun x -> match x with Result.Success _ -> [] | Result.Failure err -> err |> List.map(fun e -> e.Text))
    
    ///Contains helpers to create Result.Failure to be used with WebSharper.Forms
    module Result =
        
        module Failure =
            ///Creates a Result.Failure with the error message specified and with a global custom error Id defined in View.CustomErrorId.
            let create x =
                Result.Failure [ ErrorMessage.Create(View.CustomErrorId, "Oops, an error occured. Please try again..." + (if x <> "" then " Error: " + x else "")) ]
            
            ///Creates a Result.Failure with a generic error message with a global custom error Id defined in View.CustomErrorId.
            let generic() =
                create ""

            ///Creates a Result.Failure with a generic authentication error message with a global custom error Id defined in View.CustomErrorId.
            let auth() =
                create "Not enough permissions to perform this action."
                
            ///Creates a Result.Failure with a generic user not found error message with a global custom error Id defined in View.CustomErrorId.
            let userNotFound() =
                create "User was not found."
        
    ///Extensions to WebSharper attr type.
    type attr
    with
        static member role x = Attr.Create "role" "presentation"
        static member dataToggle x = Attr.Create "data-toggle" x
        static member dataTarget x = Attr.Create "data-target" x
        static member ariaControls x = Attr.Create "aria-controls" x
        static member ariaLabel x = Attr.Create "aria-label" x
        static member ariaHidden x = Attr.Create "aria-hidden" x
        static member ariaExpanded x = Attr.Create "aria-expanded" x


    module Form =
        let YieldVarStringOption x =
             Form.YieldVar x |> Form.Map (fun i -> if i = "" then None else Some i)

    module JQuery =

        let OfId id =
            JQuery.Of("#" + id)

[<JavaScript>]
module Bootstrap =

    type Header = {
        Text: string
        Subtext: string option
        HeaderType: HeaderType 
        Icon: string option
    } with
        static member Create headerType text =
            { Text = text
              HeaderType = headerType
              Icon = None
              Subtext = None }
        
        member x.WithIcon(icon) = 
            { x with Icon = Some icon }
        
        static member AddIcon icon (x: Header) = 
            x.WithIcon icon

        static member Render x =
            let elements() =
                [ (match x.Icon with Some icon -> iAttr [ attr.``class`` ("fa " + icon); attr.style ("margin-right:.5em") ] [] :> Doc | _ -> Doc.Empty); text x.Text ]

            [ match x.HeaderType with
              | H1 -> h1 
              | H2 -> h2
              | H3 -> h3
              | H4 -> h4
              | H5 -> h5
              | H6 -> h6
              <| elements() :> Doc

              x.Subtext 
              |> Option.map (fun txt -> 
                  pAttr [ attr.``class`` "text-muted" ] 
                        [ text txt ] :> Doc)
              |> Option.getOrElse Doc.Empty ]
            |> Doc.Concat
        
        member x.Render() = 
            Header.Render x

        static member AddSubtext txt x =
            { x with Subtext = Some txt }
    
    and HeaderType = H1 | H2 | H3 | H4 | H5 | H6
    
    type Label = {
        Text: string
        LabelStyle: LabelStyle
    } with
        static member Create(text) =
            { Text = text
              LabelStyle = LabelStyle.Default }

        static member Create(text, style) =
            { Text = text
              LabelStyle = style }

        static member Render x =
            spanAttr [ attr.style "margin: 3px 3px 3px 0px; display: inline-block;" 
                       attr.``class`` ("label "  + x.LabelStyle.CssClass()) ] [ text x.Text ]

        member x.Render() =
            spanAttr [ attr.style "margin: 3px 3px 3px 0px; display: inline-block;" 
                       attr.``class`` ("label "  + x.LabelStyle.CssClass()) ] [ text x.Text ]

    and LabelStyle =
        | Default
        | Primary
        | Info
        | Success
        | Warning
        | Danger
        with
            member x.CssClass() =
                match x with 
                | Default -> "label-default"
                | Primary -> "label-primary"
                | Info    -> "label-info"
                | Warning -> "label-warning"
                | Danger  -> "label-danger"
                | Success -> "label-link"

    /// Build a Bootstrap grid row with columns
    type GridRow = {
        Columns: GridColumn list
    } with
        static member Empty =
            { Columns = [] }

        static member Create(columns) =
            { Columns = columns }
        
        static member Render x =
            divAttr [ attr.``class`` "row" ] (x.Columns |> List.map GridColumn.Render |> Seq.cast)
        
        member x.Render() =
            GridRow.Render x
        
        static member AddColumn content size (x: GridRow) = 
            { x with Columns = x.Columns @ [ GridColumn.Create(content, size) ] }

    and GridColumn = {
        Content: Doc list
        GridSizes: GridColumnSize list
    } with
        static member Create(content, gridSizes) =
            { Content = content
              GridSizes = gridSizes }

        static member Render x =
            divAttr [ attr.``class`` (x.GridSizes 
                                      |> List.map GridColumnSize.CssClass 
                                      |> String.concat " ") ] x.Content

        member x.Render() = 
            GridColumn.Render x
    
    and GridColumnSize =
        | ColXs12
        | ColSm3
        | ColSm4
        | ColSm5
        | ColSm6
        | ColSm7
        | ColSm8
        | ColSm12
        | ColMd2
        | ColMd3
        | ColMd4
        | ColMd6
        | ColMd8
        | ColMd9
        | ColMd12
        with
            static member CssClass x =
                match x with
                | ColXs12 -> "col-xs-12"
                | ColSm3  -> "col-sm-3"
                | ColSm4  -> "col-sm-4"
                | ColSm5  -> "col-sm-5"
                | ColSm6  -> "col-sm-6"
                | ColSm7  -> "col-sm-7"
                | ColSm8  -> "col-sm-8"
                | ColSm12 -> "col-sm-12"
                | ColMd2  -> "col-md-2"
                | ColMd3  -> "col-md-3"
                | ColMd4  -> "col-md-4"
                | ColMd6  -> "col-md-6"
                | ColMd8  -> "col-md-8"
                | ColMd9  -> "col-md-9"
                | ColMd12 -> "col-md-12"
            member x.CssClass() = GridColumnSize.CssClass x

    type Container = {
        Content: Doc list
        ContainerType: ContainerType
    } with
        static member Create() = 
            { Content = []; ContainerType = ContainerType.Normal }

        static member Create(containerType) = 
            { Content = []; ContainerType = containerType }
        
        static member Create(content, containerType) = 
            { Content = content; ContainerType = containerType }
        
        static member AddContent content (x: Container) = 
            { x with Content = content }

        static member Render x =
            divAttr [ attr.``class`` (match x.ContainerType with 
                                      | Normal -> "container" 
                                      | Fluid -> "container-fluid") ] x.Content
        
        member x.Render() = Container.Render x
        
        member x.AddRow row = 
            { x with Content = x.Content @ [ GridRow.Render row :> Doc ] }

    and ContainerType =
        | Normal
        | Fluid
        
    type Hyperlink = {
        Action: HyperlinkAction
        Content: HyperlinkContent
        AriaLabel: string option
        Role: string option
        DataToggle: string option
        Id: string option
        CssClass: string list
    } with
        static member Create action =
            { Action = action
              Content = Content Doc.Empty
              AriaLabel = None
              Role = None
              DataToggle = None
              Id = None
              CssClass = [] }

        static member Create(action, content) =
            Hyperlink.Create action
            |> Hyperlink.AddContent content

        static member Create(action, content) =
            Hyperlink.Create action
            |> Hyperlink.AddTextContent content

        static member Render x =
            aAttr [ yield! (match x.Action with 
                            | Href href -> [ attr.href href ] 
                            | Action action -> [ attr.href "#"
                                                 on.click(fun _ ev ->
                                                       ev.PreventDefault()
                                                       action()) ]
                            | Mailto (email, subject) -> [ attr.href (sprintf "mailto:%s?subject=%s" email subject) ])
                    if x.CssClass <> [] then yield attr.``class`` (x.CssClass |> String.concat " ")
                    yield! x.AriaLabel  |> Option.map (fun label -> [ attr.ariaLabel label ]) |> Option.getOrElse []
                    yield! x.Role       |> Option.map (fun role ->  [ attr.role role ])       |> Option.getOrElse []
                    yield! x.DataToggle |> Option.map (fun tog ->   [ attr.dataToggle tog ])  |> Option.getOrElse []
                    yield! x.Id         |> Option.map (fun i ->     [ attr.id i ])            |> Option.getOrElse [] ] 
                  [ (match x.Content with
                     | Content doc -> doc
                     | Text txt -> text txt) ]

        member x.Render() = 
            Hyperlink.Render x
        
        member x.WithAriaLabel label = 
            { x with AriaLabel = Some label }
        
        member x.WithRole role = 
            { x with Role = Some role }
        
        member x.WithDataToggle toggle = 
            { x with DataToggle = Some toggle }
        
        static member AddId id (x: Hyperlink) = 
            { x with Id = Some id }
        
        static member AddContent content (x: Hyperlink) = 
            { x with Content = Content content }
        
        static member AddTextContent content (x: Hyperlink) = 
            { x with Content = Text content }
        
        static member AddClasses cls x = 
            { x with CssClass = x.CssClass @ cls |> List.distinct }
    
    and HyperlinkAction =
        | Href of string
        | Mailto of email:string * subject:string
        | Action of (unit -> unit)
    
    and HyperlinkContent =
        | Content of Doc
        | Text of string

    type ButtonGroup = ButtonGroup of Button list
        with
            static member Render (ButtonGroup btns: ButtonGroup) =
                divAttr [ attr.``class`` "btn-group" ]
                        (btns |> List.map Button.Render)

    and Button = { 
        Title: string
        Icon: string option
        Action: unit -> unit
        ButtonStyle: ButtonStyle
        CssClass: string option
        ExtraAttrs: Attr list
    } with
        static member Empty =
            { Title = ""
              Action = ignore
              ButtonStyle = ButtonStyle.Default
              CssClass = None
              ExtraAttrs = []
              Icon = None }

        static member Create title = 
            { Title = title
              Action = ignore
              ButtonStyle = ButtonStyle.Default
              CssClass = None
              ExtraAttrs = []
              Icon = None }
        
        static member Create(title, style, action) = 
            { Title = title
              Action = action
              ButtonStyle = style
              CssClass = None
              ExtraAttrs = []
              Icon = None }

        static member AddIcon icon (x:Button) =
            { x with Icon = Some icon }
        
        static member SetAction action (x: Button) = 
            { x with Action = action }
        
        static member SetStyle style x = 
            { x with ButtonStyle = style }
        
        static member SetCssClass cls (x: Button) = 
            { x with CssClass = Some cls }
        
        static member SetExtraAttrs attrs x = 
            { x with ExtraAttrs = attrs }
        
        static member Render x =  
            buttonAttr [ yield attr.``class`` (sprintf "btn %s %s" (x.ButtonStyle.CssClass()) (match x.CssClass with Some cls -> cls | None -> "")) 
                         yield attr.style "margin-bottom: .5em;"
                         yield on.click (fun _ ev -> 
                            ev.PreventDefault()
                            x.Action())
                         yield! x.ExtraAttrs ] 
                       [ x.Icon
                         |> Option.map (fun icon -> iAttr [ attr.``class`` ("fa " + icon) ] [] :> Doc)
                         |> Option.getOrElse Doc.Empty
                         text x.Title ] :> Doc
        
        static member RenderDoc x = 
            Button.Render x 
        
        member x.Render() = 
            Button.Render x
        
        member x.WithCssClass cls = 
            { x with CssClass = Some cls }
        
        member x.WithExtraAttrs attrs = 
            { x with ExtraAttrs = attrs }
    
    and ButtonStyle =
        | Default
        | Success
        | Primary
        | Info
        | Warning
        | Danger
        | Link
        with
            member x.CssClass() =
                match x with 
                | Default -> "btn-default"
                | Success -> "btn-success"
                | Primary -> "btn-primary"
                | Info    -> "btn-info"
                | Warning -> "btn-warning"
                | Danger  -> "btn-danger"
                | Link    -> "btn-link"

    type BreadcrumbBar = {
        Breadcrumbs: Breadcrumb list
    } with
        static member Create breadcrumbs = 
            { Breadcrumbs = breadcrumbs }
        
        static member Render x = 
            olAttr [ attr.``class`` "breadcrumb" ] (x.Breadcrumbs |> List.map Breadcrumb.Render |> Seq.cast)
    
        member x.Render() = 
            BreadcrumbBar.Render x
    
    and Breadcrumb = {
        Title: string
        Subtitle: string option
        RelativeUrl: string
        IsActive: bool
        Decoration: BreadCrumbDecoration option
    } with
        static member Create(title, relativeUrl) = 
            { Title = title
              RelativeUrl = relativeUrl
              IsActive = false
              Decoration = None
              Subtitle = None }

        static member CreateActive(title) = 
            { Title = title
              RelativeUrl = ""
              IsActive = true 
              Decoration = None
              Subtitle = None }

        static member Render x =
            let currentElement =
                Doc.Concat (match x.Decoration with
                            | Some (Image src) -> [ imgAttr [ attr.src src
                                                              attr.``class`` "img-circle img-responsive"
                                                              attr.style "width: 25px; height: 25px; margin-right: .5em;display: inline;" ] []
                                                    x.Subtitle
                                                    |> Option.map (fun sub -> 
                                                            [ text x.Title
                                                              smallAttr [ attr.style "padding-left:.5em" ] [ text (sprintf "(%s)" sub) ] :> Doc]
                                                            |> Doc.Concat)
                                                    |> Option.getOrElse (text x.Title) ]

                            | Some (Icon icon) -> [ iAttr [ attr.``class`` ("fa " + icon)
                                                            attr.style "margin-right: .5em;" ] []
                                                    x.Subtitle
                                                    |> Option.map (fun sub -> 
                                                            [ text x.Title
                                                              smallAttr [ attr.style "padding-left:.5em" ] [ text (sprintf "(%s)" sub) ] :> Doc]
                                                            |> Doc.Concat)
                                                    |> Option.getOrElse (text x.Title) ]

                            | None -> [ text x.Title ])

            match x.IsActive with
            | true  -> liAttr [ attr.``class`` "active" ] [ currentElement ]
            | false -> li [ Hyperlink.Create(Href x.RelativeUrl, currentElement).Render() ]

        member x.Render() = 
            Breadcrumb.Render x
        
        member x.WithDecoration decoration = 
            { x with Decoration = Some decoration }
        
        static member AddSubtitle sub (x: Breadcrumb) =
            { x with Subtitle = Some sub }

        static member AddDecoration decoration (x: Breadcrumb) =
            { x with Decoration = Some decoration }

    and BreadCrumbDecoration =
        | Image of src: string
        | Icon of string

    type DescriptionList = {
        Descriptions: DescriptionTerm list
        Orientation: DescriptionOrientation
        CssClass: string
    } with
        static member Create(descriptions, orientation) = 
            { Descriptions = descriptions
              Orientation = orientation
              CssClass = "" }
        
        static member Empty =
            { Descriptions = []
              Orientation = DescriptionOrientation.Horizontal
              CssClass = "" } 

        static member AddDescriptionTerm descriptionTerm (x: DescriptionList) =
            { x with Descriptions = x.Descriptions @ [ descriptionTerm ] }

        static member SetOrientation orientation (x: DescriptionList) =
            { x with Orientation = orientation }

        static member SetCssClass cls (x: DescriptionList) =
            { x with CssClass = cls }

        static member Render x =
            dlAttr [ attr.``class`` (sprintf "%s %s" x.CssClass (match x.Orientation with Horizontal -> "dl-horizontal" | Vertical -> "")) ] (List.map DescriptionTerm.Render x.Descriptions)
        
        member x.Render() = 
            DescriptionList.Render x
        
        member x.WithCssClass cls = 
            { x with CssClass = cls }
    
    and DescriptionTerm = {
        Term: string
        Description: Description
    } with
        static member Create(term, description) =
            { Term = term
              Description = Description.Doc description }
        
        static member Create(term, description) =
            { Term = term
              Description = Description.Text description }
        
        static member Create term =
            { Term = term
              Description = Description.Empty }

        static member AddDescription description (x: DescriptionTerm) =
            { x with Description = description }
        
        static member Render x =
            [ dt [ text x.Term ]
              dd [ (match x.Description with
                   | Doc doc  -> doc
                   | Text txt -> text txt
                   | Empty -> Doc.Empty) ] ]
            |> Seq.cast
            |> Doc.Concat
        
        member x.Render() = 
            DescriptionTerm.Render x
    
    and DescriptionOrientation =
        | Horizontal
        | Vertical

    and Description =
        | Doc of Doc
        | Text of string
        | Empty

    type NavTabs = {
        Tabs: NavTab list
        NavTabType: NavTabType
        IsJustified: bool
    } with
        static member Create(tabs) =
            { Tabs = tabs
              NavTabType = NavTabType.Normal
              IsJustified = false }
        
        static member Create(tabs, navTabType) =
            { Tabs = tabs
              NavTabType = navTabType
              IsJustified = false }
        
        /// Renders the menu of the tabs.
        static member RenderNav x =
            ulAttr [ attr.``class`` ("nav "
                                     + (if x.IsJustified then "nav-justified " else "")
                                     + (match x.NavTabType with 
                                        | Normal -> "nav-tabs" 
                                        | Pill Horizontal -> "nav-pills"
                                        | Pill Vertical -> "nav-pills nav-stacked")) ]
                   (x.Tabs |> List.map NavTab.RenderNavItem |> Seq.cast)
        
        member x.RenderNav() = 
            NavTabs.RenderNav x
        
        /// Renders the content link to the tabs.
        static member RenderContent x =
            divAttr [ attr.``class`` "tab-content" ] (x.Tabs |> List.map NavTab.RenderContent |> Seq.cast)
        
        member x.RenderContent() = 
            NavTabs.RenderContent x
        
        member x.Justified() = 
            { x with IsJustified = true }

    and NavTab = {
        Id: string
        Title: string
        Content: Doc
        NavTabState: NavTabState 
    } with
        static member Create(id, title) =
            { Id = id
              Title = title
              Content = Doc.Empty
              NavTabState = NavTabState.Normal }
        
        static member Create(id, title, state) =
            { Id = id
              Title = title
              Content = Doc.Empty
              NavTabState = state }
        
        static member Create(id, title, content) =
            { Id = id
              Title = title
              Content = content
              NavTabState = NavTabState.Normal }
        
        static member Create(id, title, content, state) =
            { Id = id
              Title = title
              Content = content
              NavTabState = state }

        static member AddContent content (x: NavTab) =
            { x with  Content = content }

        static member SetState state (x: NavTab) =
            { x with NavTabState = state }
        
        static member RenderNavItem x =
            liAttr [ attr.role "presentation"
                     attr.``class`` (match x.NavTabState with
                                     | NavTabState.Normal -> ""
                                     | NavTabState.Active -> "active"
                                     | NavTabState.Disabled -> "disabled") ] 
                   [ (match x.NavTabState with
                      | NavTabState.Disabled -> Hyperlink.Create(Href "#", x.Title)
                      | _ ->  Hyperlink.Create(Href <| "#" + x.Id, x.Title)
                                       .WithRole("tab")
                                       .WithDataToggle("tab")
                      ).Render() ]

        member x.RenderNavItem() = 
            NavTab.RenderNavItem x
        
        static member RenderContent x =
            divAttr [ attr.role "tabpanel"
                      attr.id x.Id
                      attr.``class`` (match x.NavTabState with NavTabState.Active -> "tab-pane fade in active" | _ -> "tab-content tab-pane fade") ]
                    [ x.Content ]
        
        member x.WithContent doc = 
            { x with Content = doc }
        
        member x.RenderContent() = 
            NavTab.RenderContent x
    
    and NavTabState =
        | Normal
        | Active
        | Disabled
    
    and NavTabType =
        | Normal
        | Pill of PillStack
    
    and PillStack =
        | Horizontal
        | Vertical
        
    type Pagination = {
        Links: PaginationListItem list
    } with 
        ///Creates a pagination nav based on the page count and sets the current page reactive variable each time a page is selected.
        static member Create(currentPageRv: Var<int>, pageCount) = 
            { Links = [ let cur = currentPageRv.Value
                        
                        yield (if cur - 1 < 1 then  
                                    PaginationListItem.Create (Hyperlink.Create(HyperlinkAction.Href "#", "«"), PaginationListItemState.Disabled)
                               else                               
                                    PaginationListItem.Create (Hyperlink.Create(Action (fun() -> Var.Set currentPageRv (cur - 1)), "«")))
                        
                        let (leftBoundary, rightBoundary) =
                            let n = 6
                            let left = cur - 3
                            let right = cur + 3

                            if pageCount - (n + 1) < 1 then 
                                (1, pageCount)
                            else
                                match (left < 1, pageCount < right) with
                                | false, false -> left, right
                                | true, false  -> 1, (n + 1)
                                | false , true -> pageCount - n, pageCount
                                | _ -> failwith "This scenario should not be possible - Left pagination inferior to one and right pagination superior to page count"

                        yield! [leftBoundary..rightBoundary] |> List.map (fun i -> 
                                let item = PaginationListItem.Create <| Hyperlink.Create(Action(fun() -> Var.Set currentPageRv i), string i)
                                if cur <> i then item
                                else { item with PaginationListItemState = PaginationListItemState.Active })

                        yield  (if currentPageRv.Value + 1 > pageCount then  
                                    PaginationListItem.Create (Hyperlink.Create(HyperlinkAction.Href "#", "»"), PaginationListItemState.Disabled)
                                else                                        
                                    PaginationListItem.Create (Hyperlink.Create(Action (fun() -> Var.Set currentPageRv (cur + 1)), "»"))) ] }
        static member Render x =
            nav [ ulAttr [ attr.``class`` "pagination" ] (x.Links |> List.map PaginationListItem.Render |> Seq.cast) ]

        member x.Render() = 
            Pagination.Render x
    
    and PaginationListItem = {
        Link: Hyperlink
        PaginationListItemState: PaginationListItemState
    } with
        static member Create link =
            { Link = link
              PaginationListItemState = PaginationListItemState.Normal }
    
        static member Create(link, state) = 
            { Link = link
              PaginationListItemState = state }
    
        static member Render x =
            liAttr [ attr.``class`` (match x.PaginationListItemState with
                                     | Normal -> ""
                                     | Active -> "active"
                                     | Disabled -> "disabled") ]
                   [ x.Link.Render() ]
    
        member x.Render() = 
            PaginationListItem.Render x
    
    and PaginationListItemState =
        | Normal
        | Active
        | Disabled

    type Alert = {
        Style: AlertStyle
        Body: Doc
    } with
        static member Stub style = { Style = style; Body = Doc.Empty } 
        static member SetBody body x = { x with  Body = body }
        static member Render x =
            divAttr [ attr.``class`` ("alert " + (x.Style |> AlertStyle.cssCls))
                      attr.role "alert" ]
                    [ x.Body ]
    and AlertStyle =
    | Danger
    | Warning
    | Success
    with
        static member cssCls x =
            match x with
            | Danger -> "alert-danger"
            | Warning -> "alert-warning"
            | Success -> "alert-success"

    type FormElement = {
        FormGroups: FormGroup list
        Buttons: Button list
        FormDisplay: FormDisplay
    } with
        static member Create display =
            { FormGroups = []
              Buttons = []
              FormDisplay = display }
        
        static member Create(formGroups) =
            { FormGroups = formGroups
              Buttons = []
              FormDisplay = Normal }

        static member Create(formGroups, buttons) =
            { FormGroups = formGroups
              Buttons = buttons
              FormDisplay = Normal }
        
        static member Create(formDisplay, formGroups, buttons) =
            { FormGroups = formGroups
              Buttons = buttons
              FormDisplay = formDisplay }
        
        static member Render x =
            formAttr [ attr.``class`` (match x.FormDisplay with Normal -> "" | Horizontal -> "form-horizontal" | Inline -> "form-inline") ]
                     [ fieldset [ yield! x.FormGroups |> List.map (fun fg -> fg.Render x.FormDisplay) |> Seq.cast
                                  yield! x.Buttons |> List.map Button.Render |> Seq.cast ] ]
        
        member x.Render() = 
            FormElement.Render x
        
        member x.WithButtons btns = 
            { x with Buttons = btns }
        
        member x.WithDisplay display = 
            { x with FormDisplay = display }

        static member AppendFormGroup fg (x: FormElement) =
            { x with FormGroups = x.FormGroups @ [ fg ]  }

        static member AddButtons btns (x: FormElement) =
            { x with Buttons = btns  }
   
    and FormGroup = {
        Id: string
        Title: FormTitle
        Input: FormInput
    } with
        static member Create(id) =
            { Id = id
              Title = FormTitle.SrOnly ""
              Input = FormInput.Custom Doc.Empty }
        
        static member Create(id, title, input) =
            { Id = id
              Title = Text title
              Input = input }
        
        static member Create(id, title, input) =
            { Id = id
              Title = title
              Input = input }
        
        static member CreateTextInput(id, title, rv, err, placeholder, ?attrs, ?helpTxt) =
            { Id = id
              Title = title
              Input = FormInput.Text(rv, err, placeholder, defaultArg attrs [], helpTxt) }
        
        static member CreatePasswordInput(id, title, rv, err, placeholder, ?attrs) =
            { Id = id
              Title = title
              Input = FormInput.Password(rv, err, placeholder, defaultArg attrs []) }
        
        member x.WithTitle title = 
            { x with Title = title }
        
        member x.WithTitle title = 
            { x with Title = FormTitle.Text title }
        
        member x.WithInput input = 
            { x with Input = input }

        static member AddTitleTxt title (x: FormGroup) =
            { x with Title = FormTitle.Text title }

        static member AddInput input (x: FormGroup) = 
            { x with Input = input }
        
        static member Render(formDisplay, x) =
            let _label cls =
                match x.Title with
                | FormTitle.Text txt   -> labelAttr [ attr.``for`` x.Id; attr.``class`` cls ] [ text txt ]
                | SrOnly txt -> labelAttr [ attr.``for`` x.Id; attr.``class`` (cls + " sr-only") ] [ text txt ]

            let container elts =
                match formDisplay with
                | Horizontal -> 
                       match x.Input with 
                       | Checkbox _ -> [ divAttr [ attr.``class`` "col-sm-offset-2 col-sm-10" ] elts :> Doc ]
                       | _ -> [ _label "col-sm-2 control-label"; divAttr [ attr.``class`` "col-sm-10" ] elts :> Doc ]
                | Normal | Inline   ->
                       match x.Input with 
                       | Checkbox _ -> elts
                       | _ -> [ _label "" ] @ elts
            
            match x.Input with
            | FormInput.Text (rv, errors, placeholder, attrs, helpTxt) -> 
                divAttr [ attr.classDyn (errors |> View.Map (fun e -> if e <> [] then "form-group has-error" else "form-group")) ]
                        (container [ Doc.Input [ yield attr.``class`` "form-control"
                                                 yield attr.placeholder placeholder 
                                                 yield! attrs ] rv :> Doc
                                     (match helpTxt with Some h -> spanAttr [ attr.``class`` "help-block" ] [ text h ] :> Doc | None -> Doc.Empty)
                                     errors 
                                     |> View.Map (List.map(fun e -> spanAttr [ attr.``class`` "help-block" ] [ text e ] :> Doc) >> Doc.Concat) 
                                     |> Doc.EmbedView ])
                
            | Password (rv, errors, placeholder, attrs) ->
                divAttr [ attr.classDyn (errors |> View.Map (function e -> if e <> [] then "form-group has-error" else "form-group")) ]
                        (container[ Doc.PasswordBox [ yield attr.``class`` "form-control"
                                                      yield attr.placeholder placeholder
                                                      yield! attrs ] rv :> Doc
                                    errors 
                                    |> View.Map (List.map(fun e -> spanAttr [ attr.``class`` "help-block" ] [ text e ] :> Doc) >> Doc.Concat) 
                                    |> Doc.EmbedView ])

            | Checkbox (rv, attrs) ->
                divAttr [ attr.``class`` "form-group" ]
                        (container [ divAttr [ yield attr.``class`` "checkbox" 
                                               yield! attrs ]
                                             [ label [ Doc.CheckBox [] rv
                                                       (match x.Title with FormTitle.Text txt -> text txt | _ -> Doc.Empty) ] ] ])
                
            | Static txt -> 
                divAttr [ attr.``class`` "form-group" ]
                        (container [ pAttr [ attr.``class`` "form-control-static" ] [ text txt ] ])
                
            | Custom doc -> 
                divAttr [ attr.``class`` "form-group" ]
                        (container [ doc ])

        member x.Render display = 
            FormGroup.Render(display, x)

        /// Displays custom error messages registered with the customErrorId
        static member ShowErrors view =
            FormGroup.Create("errors")
                     .WithTitle(SrOnly "Errors after submit")
                     .WithInput(FormInput.Custom (Doc.ShowErrors (View.ThroughCustomId view) (function [] -> Doc.Empty | errors -> Alert.Stub Warning 
                                                                                                                                   |> Alert.SetBody (errors |> List.map(fun err -> p [ text err.Text ] :> Doc) |> Doc.Concat) 
                                                                                                                                   |> Alert.Render :> Doc)))
        /// Displays success message when submit is successful
        static member ShowSuccess message view =
            FormGroup.Create("success")
                     .WithTitle(SrOnly "Success message after submit")
                     .WithInput(FormInput.Custom (Doc.ShowSuccess view (function _ -> Alert.Stub Success 
                                                                                      |> Alert.SetBody (text message) 
                                                                                      |> Alert.Render :> Doc)))
    and FormInput =
        | Text of ref: IRef<string> 
                  * inlineErrors: View<string list> 
                  * placeholder: string 
                  * extraAttrs: List<Attr>
                  * helpText: string option
        | Password of ref: Var<string> 
                      * inlineErrors: View<string list> 
                      * placeholder: string 
                      * extraAttrs: List<Attr>
        | Checkbox of ref: Var<bool> * extraAttrs: List<Attr>
        | Static of string
        | Custom of Doc
    
    and FormTitle =
        | Text of string
        /// Creates a title visible by screen reader only
        | SrOnly of string
    
    and FormDisplay =
        | Normal
        | Horizontal
        | Inline

    type Table = {
        Headers: string list option
        Body: TableBody
        Style: TableStyle list
    } with
        static member Empty = 
            { Headers = None
              Body = TableBody.Empty
              Style = [] }

        static member AddHeaders headers x =
            { x with Headers = Some headers }

        static member OnAfterRenderBody action (x: Table) =
            { x with Body = { x.Body with OnAfterRenderAction = Some action } }

        static member AddRow row (x: Table) =
            { x with Body = x.Body |> TableBody.AddRow row }

        static member AddRowDoc rowData (x: Table) =
            { x with Body = x.Body |> TableBody.AddRowDoc rowData }

        static member AddRowText rowData (x: Table) =
            { x with Body = x.Body |> TableBody.AddRowText rowData }

        static member AddRowTextWithStatus rowData status (x: Table) =
            { x with Body = x.Body |> TableBody.AddRowTextWithStatus rowData status }
        
        static member AddStyle style (x:Table) =
            { x with Style = style }

        static member Render (x: Table) =
            divAttr 
                [ attr.``class`` "table-responsive" ]
                [ tableAttr [ attr.``class`` ([ "table" ] @ (List.map TableStyle.ToCssClass x.Style) |> String.concat " ") ]
                            [ yield! x.Headers 
                                     |> Option.map (fun hs -> [ thead  [ tr (hs |> List.map (fun h -> td [ text h ] :> Doc)) ] :> Doc ]) 
                                     |> Option.getOrElse []
                              yield x.Body 
                                    |> TableBody.Render :> Doc ] :> Doc ]
    and TableBody = 
        { Rows: Row list
          OnAfterRenderAction: Option<Dom.Element -> unit> }
          with
            static member Empty =
                { Rows = []
                  OnAfterRenderAction = None }

            static member AddRow row (x: TableBody) =
                { x with Rows = x.Rows @ [ row ] }

            static member AddRowDoc rowData (x: TableBody) =
                { x with Rows = x.Rows @ [ { Status = Normal; Data = rowData } ] }
            
            static member AddRowText rowData (x: TableBody) =
                x |> TableBody.AddRowDoc (rowData |> List.map (fun i -> text i))

            static member AddRowTextWithStatus rowData status x =
                { x with Rows = x.Rows @ [ { Status = status;  Data = rowData |> List.map (fun i -> text i) } ] }

            static member Render (x: TableBody) =
                tbodyAttr (x.OnAfterRenderAction 
                           |> Option.map (fun action -> [ on.afterRender action ])
                           |> Option.getOrElse [])
                          (x.Rows 
                           |> List.map (fun r -> 
                                trAttr [ attr.``class`` (string r.Status) ] 
                                       (r.Data |> List.map (fun d -> td [ d ] :> Doc)) :> Doc))

    and TableStyle =
    | Bordered
    | Striped
    | Hover
        with
            static member ToCssClass =
                function
                | Bordered -> "table-bordered"
                | Striped  -> "table-striped"
                | Hover    -> "table-hover"

    and Row = {
        Status: RowStatus
        Data: Doc list
    } with
        static member Empty =
            { Status = RowStatus.Normal
              Data = [] }

        static member AddData data (x: Row) =
            { x with Data = data }

        static member SetStatus status (x: Row) =
            { x with Status = status }

    and RowStatus =
        | Normal
        | Active
        | Success
        | Warning
        | Danger
        | Info
        with 
            override x.ToString() = 
                match x with 
                | Normal -> "" 
                | Active -> "active" 
                | Success -> "success" 
                | Warning -> "warning" 
                | Danger -> "danger" 
                | Info -> "info" 

    /// Provides functionalities to make editable panels.
    type Panel = {
        Title: string option
        Content: Doc
        Icon: string
        Style: PanelStyle
    }
    with
        static member Empty = 
            { Title = None
              Content = Doc.Empty
              Icon = ""
              Style = Default }

        static member AddTitle title (x: Panel) = 
            { x with Title = Some title }

        static member AddContent content (x:Panel) = 
            { x with Content = content }
        
        static member AddIcon icon (x: Panel) = 
            { x with Icon = icon }
        
        static member AddStyle style (x: Panel) = 
            { x with Style = style }

        static member Render (x: Panel) =
            divAttr [ attr.``class`` ("panel " + string x.Style) ]
                    [ x.Title
                      |> Option.map 
                            (fun title ->  
                                divAttr [ attr.``class`` "panel-heading" ]
                                        [ h3Attr [ attr.``class`` "panel-title" ]
                                                 [ iAttr [ attr.``class`` ("fa " + x.Icon) ] []
                                                   spanAttr [ attr.style "margin-left:.5em;display: inline;" ] [ text title ] ] ] :> Doc)
                      |> Option.getOrElse Doc.Empty
                      
                      divAttr [ attr.``class`` "panel-body" ] 
                              [ x.Content ] ]

    and PanelStyle =
        | Default
        | Primary
        | Info
        | Danger
        | Success
        | Warning
    with
        override x.ToString() = match x with
                                | Default -> "panel-default"
                                | Primary -> "panel-primary"
                                | Info    -> "panel-info"
                                | Danger  -> "panel-danger"
                                | Success -> "panel-success"
                                | Warning -> "panel-warning"