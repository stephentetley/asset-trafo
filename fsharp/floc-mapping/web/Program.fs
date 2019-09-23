// Currently, this is just the sample code from the Giraffe GitHub pages

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive


open SLSqlite.Core  // must be before Giraffe
open FlocMapping.AibBasis
open FlocMapping.S4Basis
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

let s4FlocName_ (floc : Floc) : string = 
    match runDb (getS4FlocName floc) with
    | Error msg -> ""
    | Ok (Some ans) -> ans
    | Ok None -> ""

let aibReferenceToS4Floc_ (sai : string) : (string * string) list = 
    match runDb (aibReferenceToS4Floc sai) with
    | Error msg -> [(msg, "")]
    | Ok [] -> [("Not found", "")]
    | Ok flocs -> List.map (fun x -> x.ToString(), s4FlocName_ x) flocs

let aibCommonName_ (sai : string) : string = 
    match runDb (getAibCommonName sai) with
    | Error msg -> ""
    | Ok (Some ans) -> ans
    | Ok None -> ""


let saiHandler : HttpHandler = 
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let! model = ctx.BindFormAsync<SaiModel>()
            let sai = model.SaiCode
            let commonName = aibCommonName_ sai
            let flocs = aibReferenceToS4Floc_ sai
            return! htmlView (resultsPage sai commonName flocs) next ctx
        }

let webApp =
    choose [
        GET >=> 
            choose [
                route "/" >=> htmlView saiInputPage
            ]
        POST >=>
            choose [
                route "/results" >=> saiHandler
            ]
    ]

let configureApp (app : IApplicationBuilder) =
    // Add Giraffe to the ASP.NET Core pipeline
    app.UseStaticFiles () |> ignore
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    // Add Giraffe dependencies
    services.AddGiraffe() |> ignore

// For deployment, use GetCurrentDirectory()...
// let contentRoot = Directory.GetCurrentDirectory()
let contentRoot = Path.Combine(__SOURCE_DIRECTORY__, "")
let webRoot = Path.Combine(contentRoot, "webroot") 

[<EntryPoint>]
let main _ =
    printfn "contentRoot = %s" contentRoot
    printfn "webRoot = %s" webRoot
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseWebRoot(webRoot)
       
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .Build()
        .Run()
    0