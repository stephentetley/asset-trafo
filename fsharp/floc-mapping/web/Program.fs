// Currently, this is just the sample code from the Giraffe GitHub pages

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive


open SLSqlite.Core  // must be before Giraffe
open FlocMapping.TranslateFloc


open Giraffe


open FlocMapping.Web.Model
open FlocMapping.Web.View

let pathToDb () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\floc_mapping_active.sqlite")

let getConnParams () : SqliteConnParams = 
    let dbActive = pathToDb () |> Path.GetFullPath
    sqliteConnParamsVersion3 dbActive


let runDb (action : SqliteDb<'a>) : Result<'a, ErrMsg> = 
    let conn = getConnParams () 
    runSqliteDb conn action

let aibReferenceToS4Floc_ (sai : string) : string list = 
    match runDb (aibReferenceToS4Floc sai) with
    | Error msg -> [msg]
    | Ok flocs -> List.map (fun x -> x.ToString()) flocs


let saiHandler : HttpHandler = 
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let! model = ctx.BindFormAsync<SaiModel>()
            let sai = model.SaiCode
            let flocs = aibReferenceToS4Floc_ sai
            return! htmlView (resultsPage sai flocs) next ctx
        }

let webApp =
    choose [
        GET >=> 
            choose [
                route "/"       >=> htmlView saiInputPage
            ]
        POST >=>
            choose [
                route "/results" >=> saiHandler
            ]
    ]

let configureApp (app : IApplicationBuilder) =
    // Add Giraffe to the ASP.NET Core pipeline
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    // Add Giraffe dependencies
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    WebHostBuilder()
        .UseKestrel()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .Build()
        .Run()
    0