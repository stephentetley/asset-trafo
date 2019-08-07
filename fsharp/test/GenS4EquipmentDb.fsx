// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\System.Data.SQLite.Core\1.0.111\lib\netstandard2.0"
#r "System.Data.SQLite.dll"
open System.Data.SQLite

// A hack to get over Dll loading error due to the native dll `SQLite.Interop.dll`
[<Literal>] 
let SQLiteInterop = @"C:\Users\stephen\.nuget\packages\System.Data.SQLite.Core\1.0.111\runtimes\win-x64\native\netstandard2.0"
Environment.SetEnvironmentVariable("PATH", 
    Environment.GetEnvironmentVariable("PATH") + ";" + SQLiteInterop
    )




// ********** DATA SETUP **********
type S4EquipmentTable = 
    CsvProvider<Sample = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
                , PreferOptionals = true >

type S4EquipmentRow = S4EquipmentTable.Row



let getEquipmentRows(cvsPath : string) : S4EquipmentRow list = 
    let table = S4EquipmentTable.Load(uri = cvsPath) in Seq.toList table.Rows


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output\", relativePath)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\data\", "assets_db_template.sqlite")

type SQLiteConnParams = 
    { PathToDB : string 
      SQLiteVersion : string }


let paramsConnString (config:SQLiteConnParams) : string = 
    sprintf "Data Source=%s;Version=%s;" config.PathToDB config.SQLiteVersion

let sqliteConnParamsVersion3 (pathToDB:string) : SQLiteConnParams = 
    { PathToDB = pathToDB; SQLiteVersion = "3" }

let emptyIfNull (source : string) : string = 
    match source with
    | null | "NULL" -> ""
    | _ -> source

let emptyIfNone (source : string option) : string = 
    match source with
    | Some str -> emptyIfNull str
    | None -> ""


let makeInsert (row : S4EquipmentRow) : string option = 
    match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
    | Some(num), true -> 
        let line1 = "INSERT INTO s4_equipment (s4_ref, pli_code, s4_name, category, obj_type, obj_class, s4_floc) "
        let line2 = 
            sprintf "VALUES(%i, '%s', '%s', '%s', '%s', '%s', '%s');" 
                    num 
                    (emptyIfNull row.``AI2 AIB Reference``)
                    (emptyIfNone row.``Equipment Description``)
                    (emptyIfNull row.Category)
                    (emptyIfNone row.``Object Type``)
                    (emptyIfNone row.Class)
                    (emptyIfNull row.``L6_Floc Code``)
        String.concat "\n" [line1; line2] |> Some
    | _,_ -> None

let insertRow (conn : SQLiteConnection) (row : S4EquipmentRow) = 
    match makeInsert row with
    | Some statement -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        cmd.ExecuteNonQuery () |> ignore
        ()
    | None -> ()

let main () = 
    let equipmentRows = getEquipmentRows @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputFile "assets.sqlite" |> Path.GetFullPath
    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    let conn = paramsConnString connParams
    try 
        let dbconn = new SQLiteConnection(conn)
        dbconn.Open()
        //let statement = "INSERT INTO s4_equipment (s4_ref, name) VALUES(1000, 'TODO');"
        //let cmd : SQLiteCommand = new SQLiteCommand(statement, dbconn)
        //let ans = cmd.ExecuteNonQuery ()
        let trans = dbconn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
        try 
            List.iter (insertRow dbconn) equipmentRows
            trans.Commit ()
        with 
        | ex -> trans.Rollback ()
        dbconn.Close()
    with
    | err -> printfn "%s" err.Message

    
