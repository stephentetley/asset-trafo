// Currently, this is just the sample code from the Giraffe GitHub pages

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe


open FlocMapping.Web.Model
open FlocMapping.Web.View


let saiHandler : HttpHandler = 
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let! model = ctx.BindFormAsync<SaiModel>()
            let sai = model.SaiCode
            printfn "Sai: %s" sai
            return! htmlView (resultsPage sai) next ctx
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