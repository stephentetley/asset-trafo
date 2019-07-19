// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO



#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190712\lib\netstandard2.0"
#r "SLFormat"


#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190719a\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons



// ********** DATA SETUP **********


type EquipmentTable = 
    CsvProvider< @"G:\work\Projects\asset_sync\equipment_migration_s1.csv">

type EquipmentRow = EquipmentTable.Row

type Equipment = 
    { AibDescription : string
      ObjectType : string
      ObjectClass : string
    }

let conv1 (row : EquipmentRow) : Equipment option = 
    match row.``Object Type``, row.Class with
    | null,_ | _, null | "", _ -> None
    | _ -> 
        Some { AibDescription = row.``Asset Type Description``
               ObjectType = row.``Object Type``
               ObjectClass = row.Class }


let equipmentFact (item : Equipment) : Predicate = 
    predicate "equip" 
        [ stringTerm item.AibDescription
        ; stringTerm item.ObjectType 
        ; stringTerm item.ObjectClass
        ]

let docNames () : Equipment list = 
    let table = new EquipmentTable ()
    table.Rows
        |> Seq.map conv1 
        |> Seq.choose id
        |> Seq.sort
        |> Seq.distinct
        |> Seq.toList

let main () = 
    let outPath = @"G:\work\Projects\asset_sync\output\equipment.lp"
    let docs = docNames ()
    runFactWriter 160 outPath 
        <| mapMz (tellPredicate << equipmentFact) docs 



