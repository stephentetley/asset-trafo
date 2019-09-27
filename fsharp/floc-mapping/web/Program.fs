// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

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
open FlocMapping.Web.Base
open FlocMapping.Web.DataAccess


open Giraffe


open FlocMapping.Web.Model
open FlocMapping.Web.View


let aibCommonName_ (sai : string) : string = 
    match runDb (getAibCommonName sai) with
    | Error msg -> ""
    | Ok (Some ans) -> ans
    | Ok None -> ""



let resultsHandler : HttpHandler = 
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let! model = ctx.BindFormAsync<ReferencesForm>()
            let sai = model.SingleReference.Trim()
            if not <| isAibCode sai then
                return! htmlView (internalErrorPage <| sprintf "Invalid code %s" sai)  next ctx
            else
                let commonName = aibCommonName_ sai
                match flocMapping sai |> runDb with
                | Ok flocs -> return! htmlView (resultsPage sai commonName flocs) next ctx
                | Error msg -> return! htmlView (internalErrorPage msg) next ctx
        }



let webApp =
    choose [
        GET >=> 
            route "/" >=> htmlView saiInputPage
            
        POST >=>
            route "/results" >=> warbler (fun _ -> resultsHandler)
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
let contentRoot = Path.Combine( __SOURCE_DIRECTORY__ , "")
let webRoot = Path.Combine(contentRoot, "webroot") 

// Note - Chrome appears to cache the stylesheet, we may have to 
// delete the cache to see any changes.

[<EntryPoint>]
let main _ =
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