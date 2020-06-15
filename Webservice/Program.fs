module Webservice.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Authentication.AzureAD.UI
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Http
open Microsoft.IdentityModel.Logging
open FSharp.Data
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe.Serialization
open System.Text.Json
open Newtonsoft.Json
open Newtonsoft.Json.Converters


// ---------------------------------
// Models
// ---------------------------------

type Message =
    {
        Text : string
    }

let fileUrl = "https://4hkknq.ch.files.1drv.com/y4mXyKEaokarDRTasKnN956qseS7s-sStex1tjW6koC1mc02UeT2pUlkIxeEwfovIThE4aa1iZxu7FqfEJYODBwCHI8cYrDywwZw56CoI6xGTAKxfGMbnXVeKY0-BiJqczo8garIh6RC-N7XjzBU70KR8JTKj1LgvJjbT94ucHl5S2jWPtY0r6vP_szc02FpZw3ekOEX4dlWv70Lpboc7jodw"

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "Webservice" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial () =
        h1 [] [ encodedText "Webservice" ]

    let index (model : Message) =
        [
            partial()
            p [] [ encodedText model.Text ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let mustBeLoggedIn : HttpHandler =
    requiresAuthentication (challenge AzureADDefaults.AuthenticationScheme) 



type jsonProperty = {
    property: string
    value: int
    player: string option 
}

let private playerName (player:TerraformingMars.Player) = 
    match player with
    | TerraformingMars.Player.Green -> "Murray"
    | TerraformingMars.Player.Red -> "Sam"
    | TerraformingMars.Player.Yellow -> "Ross"
    | _-> "Not Playing"

let jsonConvert (x:Output.ScoreBot.Property) =
    {
        property= x.name
        player= Option.map playerName x.player
        value = x.value
    }


let jsonHandler : HttpHandler  =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task { 
            let! file = Http.AsyncRequestString(fileUrl) 
                        |> Async.StartAsTask
            let state = TerraformingMars.interpret <| TTSJson.load file
            let properties = state 
                                |> Output.ScoreBot.getProperties 
                                |> List.choose (Option.map jsonConvert)
            return! json properties next ctx
        } 

let webApp =
    choose [
       //mustBeLoggedIn >=>
        GET >=>
            choose [
                route "/scorebot" >=> warbler (fun _-> jsonHandler)
                route "/" >=> warbler (fun _-> FriendlyView.handler)
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let config =
    (new ConfigurationBuilder())
       .AddJsonFile("appsettings.json",false,true)
       .AddEnvironmentVariables() 
       .Build()

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
       .UseHttpsRedirection()
       .UseCors(configureCors)
       .UseStaticFiles()
       //.UseAuthentication()
       .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    IdentityModelEventSource.ShowPII <- true
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

    let customSettings = JsonSerializerSettings();
    customSettings.Converters.Add(JSON.OptionConverter())
    customSettings.Converters.Add(DiscriminatedUnionConverter())

    services.AddSingleton<IJsonSerializer>( NewtonsoftJsonSerializer(customSettings)) |> ignore
    // Optionally use `FSharp.SystemTextJson` (requires `FSharp.SystemTextJson` package reference)
    let cookieOptions (options:CookiePolicyOptions) = 
        options.CheckConsentNeeded      <- (fun context -> true) 
        options.MinimumSameSitePolicy   <- SameSiteMode.None 
    services.Configure<CookiePolicyOptions>(cookieOptions) |> ignore

    (*services.AddAuthentication AzureADDefaults.AuthenticationScheme) 
        .AddAzureAD(fun options->
                config.Bind("AzureAD",options)
                printf "%O" options
            )
        |> ignore
    let  configreOpenID (options:OpenIdConnectOptions) = 
        //options.Authority <- options.Authority+"/2.0/"
        options.TokenValidationParameters.ValidateIssuer <- false
    services.Configure<OpenIdConnectOptions>(AzureADDefaults.OpenIdScheme, configreOpenID)
    |> ignore
    *)




let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Error)
           .AddConsole()
           .AddDebug() |> ignore


[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0