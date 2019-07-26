// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module PlantMapping =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    open AssetTrafo.Aib.HKey
    open AssetTrafo.AspFacts.Common

    // ********** DATA SETUP **********

    [<Literal>]
    let PlantTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         Category(string),CommonName(string)"    

    [<Literal>]
    let PlantTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,PLANT ITEM,COMMON NAME/WITH/SEPARATORS"
     

    type PlantTable = 
        CsvProvider< Schema = PlantTableSchema
                   , Sample = PlantTableSample
                   , HasHeaders = true >

    type PlantRow = PlantTable.Row

    let getRows (cvsPath : string) : PlantRow list = 
        let table = PlantTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList

    let commonNameGetInstallation (commonName : string) : string = 
        let elts = commonName.Split([| '/' |])
        String.concat "/" [ elts.[0]; elts.[1]]

    /// This uses the hkey to identify whether there is a Process Group
    /// in the common name 
    let commonNameGetProcessGroup (commonName : string) (hkey : string) : string option = 
        try 
            match hkeyProcessGroup hkey with
            | Some fragment -> 
                if isAnon fragment then
                    Some ""
                else
                    let elts = commonName.Split([| '/' |])
                    elts.[2] |> Some
            | None -> None
        with
        | exn -> printfn "Failed for: %s" commonName; raise exn


    let commonNameGetProcess (commonName : string) (hkey : string) : string option = 
        let elts = commonName.Split([| '/' |])
        try 
            match hkeyProcessGroup hkey with
            | Some fragment -> 
                if isAnon fragment then
                    elts.[2] |> Some
                else
                    elts.[3] |> Some
            | None -> None
        with
        | exn -> 
            if elts.Length <> 2 then
                printfn "Unhandled for: %s" commonName            
            None

    let processGroupFact (row : PlantRow) : Predicate option = 
        match row.Category with
        | "PROCESS GROUP" -> 
            match commonNameGetProcessGroup row.CommonName row.HKey with 
            | Some pgName ->
                predicate "processGroup" 
                            [ stringTerm row.Reference
                            ; stringTerm row.AssetType
                            ; stringTerm (commonNameGetInstallation row.CommonName)
                            ; stringTerm pgName
                            ] |> Some
            | _ -> None
        | _ -> None

    let generateProcessGroupFacts (rows : PlantRow list) 
                                     (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates processGroupFact rows
                   

    let processFact (row : PlantRow) : Predicate option = 
        match row.Category with
        | "PROCESS" -> 
            match commonNameGetProcessGroup row.CommonName row.HKey, 
                               commonNameGetProcess row.CommonName row.HKey with 
            | Some pgName, Some pName -> 
                predicate "process" 
                            [ stringTerm row.Reference
                            ; stringTerm row.AssetType
                            ; stringTerm (commonNameGetInstallation row.CommonName)
                            ; stringTerm pgName
                            ; stringTerm pName
                            ] |> Some
            | _ -> None
        | _ -> None

    let generateProcessFacts (rows : PlantRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
            <| generatePredicates processFact rows