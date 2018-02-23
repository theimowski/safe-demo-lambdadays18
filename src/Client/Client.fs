module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Fable.Core.JsInterop

open Shared

open Fulma
open Fulma.Layouts
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Components
open Fulma.BulmaClasses

open Fulma.BulmaClasses.Bulma
open Fulma.BulmaClasses.Bulma.Properties
open Fulma.Extra.FontAwesome
open Fulma.Elements
open Fulma.Elements


type Model = 
  { Comment : string 
    Name    : string
    Score   : Score option
    Loading : bool
    Results : VotingResults option }

type Msg =
| SetComment of string
| SetName    of string
| SetScore   of Score
| Submit
| GotResults of Result<VotingResults, exn>


module Server = 

  open Shared
  open Fable.Remoting.Client
  
  /// A proxy you can use to talk to server directly
  let api : IVotingProtocol = 
    Proxy.createWithBuilder<IVotingProtocol> Route.builder
    

let init () = 
  let model = 
    { Comment = "" 
      Name    = ""
      Score   = None
      Loading = false
      Results = None }
  let cmd =
    Cmd.none
  model, cmd

let mkVote (model : Model) : Vote =
  { Comment = model.Comment
    Name    = model.Name
    Score   = defaultArg model.Score Good }

let update msg (model : Model) =
  let model' =
    match msg with
    | SetComment comment -> { model with Comment = comment }
    | SetName    name    -> { model with Name    = name }
    | SetScore   score   -> { model with Score   = Some score }
    | Submit             -> { model with Loading = true }
    | GotResults (Ok r)  -> { model with Loading = false
                                         Results = Some r }
    | GotResults _       -> { model with Loading = false }

  let cmd =
    match msg with
    | Submit ->
      Cmd.ofAsync
        Server.api.vote
        (mkVote model')
        (Ok >> GotResults)
        (Error >> GotResults)
    | _ -> Cmd.none

  model', cmd

let show = function
| Some x -> string x
| None -> "Loading..."

let navBrand =
  Navbar.Brand.div [ ] 
    [ Navbar.Item.a 
        [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
          Navbar.Item.IsActive true ] 
        [ img [ Src "https://safe-stack.github.io/images/safe_top.png"
                Alt "Logo" ] ] 
      Navbar.burger [ ] 
        [ span [ ] [ ]
          span [ ] [ ]
          span [ ] [ ] ] ]

let navMenu =
  Navbar.menu [ ]
    [ Navbar.End.div [ ] 
        [ Navbar.Item.a [ ] 
            [ str "Home" ] 
          Navbar.Item.a [ ]
            [ str "Examples" ]
          Navbar.Item.a [ ]
            [ str "Documentation" ]
          Navbar.Item.div [ ]
            [ Button.a 
                [ Button.Color IsWhite
                  Button.IsOutlined
                  Button.Size IsSmall
                  Button.Props [ Href "https://github.com/SAFE-Stack/SAFE-template" ] ] 
                [ Icon.faIcon [ ] 
                    [ Fa.icon Fa.I.Github; Fa.fw ]
                  span [ ] [ str "View Source" ] ] ] ] ]

let onChange action =
  OnChange (fun e-> action !!e.target?value)

let field input =
  Field.div [ ]
    [ Field.body [ ]
        [ input ] ]

let scoreIcon = function
| Good -> Fa.I.SmileO
| SoSo -> Fa.I.MehO
| Poor -> Fa.I.FrownO

let scoreColor (model : Model) (score : Score) =
  match model.Score with
  | None -> IsWhite
  | Some s when s <> score -> IsWhite
  | _ ->
    match score with
    | Good -> IsSuccess
    | SoSo -> IsWarning
    | Poor -> IsDanger

let scores (model : Model) dispatch =
  let item score =
    Level.item
      [ ]
      [ Button.a
          [ Button.Color (scoreColor model score)
            Button.OnClick (fun _ -> dispatch (SetScore score)) ]
          [ Icon.faIcon
              [ ]
              [ Fa.icon (scoreIcon score)
                Fa.fa2x ] ] ]

  Level.level [ Level.Level.IsMobile ]
    [ item Good 
      item SoSo 
      item Poor ]

let comment (model : Model) dispatch =
  Textarea.textarea
    [ Textarea.Placeholder "Comment"
      Textarea.DefaultValue model.Comment
      Textarea.Props [ onChange (SetComment >> dispatch) ] ]
    [ ]

let name (model : Model) dispatch =
  Input.text
    [ Input.Placeholder "Name"
      Input.DefaultValue model.Name
      Input.Props [ onChange (SetName >> dispatch) ] ]

let submit (model : Model) dispatch =
  Button.a
    [ Button.IsFullwidth
      Button.Color IsPrimary
      Button.OnClick (fun _ -> dispatch Submit)
      Button.IsLoading model.Loading ]
    [ str "Submit" ]


let formBox model dispatch =
  Box.box' [ ]
    [ field (scores  model dispatch)
      field (comment model dispatch)
      field (name    model dispatch)
      field (submit  model dispatch) ]

let resultsBox (results : VotingResults) =
  let item score =
    let count = defaultArg (Map.tryFind score results.Scores) 0
    Level.item
      [ ]
      [ div
          [ ]
          [ Icon.faIcon
              [ ]
              [ Fa.icon (scoreIcon score)
                Fa.fa2x ]
            h2 [ ] [ str (string count)] ] ]

  
  Box.box' [ ]
    [ Level.level [ Level.Level.IsMobile ]
        [ item Good 
          item SoSo 
          item Poor ]
      Content.content [ Content.Size IsSmall ]
        [ ul []
            [ for (name,comment) in results.Comments ->
                li [ ]
                  [ i [ ] [ str (sprintf "\"%s\"" comment) ]
                    str (sprintf " - %s" name) ] ] ] ]


let containerBox model dispatch =
  match model.Results with
  | Some results -> resultsBox results
  | None -> formBox model dispatch

let imgSrc = "https://crossweb.pl/upload/gallery/cycles/11255/300x300/lambda_days.png"

let view model dispatch =
  Hero.hero [ Hero.Color IsPrimary; Hero.IsFullHeight ] 
    [ Hero.head [ ] 
        [ Navbar.navbar [ ]
            [ Container.container [ ]
                [ navBrand
                  navMenu ] ] ]
      
      Hero.body [ ] 
        [ Container.container [ Container.CustomClass Alignment.HasTextCentered ]
            [ Column.column 
                [ Column.Width (Column.All, Column.Is6)
                  Column.Offset (Column.All, Column.Is3) ]
                [ Level.level []
                    [ Level.item [ ]
                        [ Image.image [ Image.Is64x64 ]
                            [ img [ Src imgSrc ] ] ] ]
                  h1 [ ClassName "title" ] 
                    [ str "SAFE Demo" ]
                  div [ ClassName "subtitle" ]
                    [ str "Score my talk @LambdaDays '18" ]
                  containerBox model dispatch ] ] ] ]

  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
