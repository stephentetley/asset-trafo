﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module AssetTypes =

    open FSharp.Data

    open FactX


    // ********** DATA SETUP **********


    [<Literal>]
    let TypeCatTableSchema = 
        "SuperCategory(string),AssetType(string),\
         AssetTypeDescription(string),AssetTypeCode(string),\
         Category(string option),Group(string option),\
         SubGroup(string option)"

    [<Literal>]
    let TypeCatTableSample = 
         "SUPER CAT,NAME,DESCRIPTION,ABC123,CAT,GROUP,SUBGROUP"
     

    type TypeCatTable = 
        CsvProvider< Schema = TypeCatTableSchema
                   , Sample = TypeCatTableSample
                   , HasHeaders = true >

    type TypeCatRow = TypeCatTable.Row

    let getRows (cvsPath : string) : TypeCatRow list = 
        let table = TypeCatTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList


    let baseAssetType (row : TypeCatRow) : Predicate option = 
        match row.SuperCategory, row.Category with
        | _, None | _, Some "NULL" -> None
        | "BASE ASSET TYPE", Some category ->
            predicate "base_asset_type" 
                        [ stringTerm row.AssetType
                        ; stringTerm category
                        ]
                |> Some
        | _ -> None




    let equipmentCategory (row : TypeCatRow) : Predicate option =
        match row.SuperCategory, row.Category with
        | _, None | _, Some "NULL" -> None
        | "EQUIPMENT CATEGORY", Some category ->
            predicate "equipment_category" 
                        [ stringTerm row.AssetType
                        ; stringTerm category
                        ]
                |> Some
        | _ -> None

    let equipmentGroup (row : TypeCatRow) : Predicate option =
        match row.SuperCategory, row.Group with
        | _, None | _, Some "NULL" -> None
        | "EQUIPMENT CATEGORY", Some group ->
            predicate "equipment_group" 
                        [ stringTerm row.AssetType
                        ; stringTerm group
                        ]
                |> Some
        | _ -> None


    // Don't make SubGroup - it is not telling us anything we don't know
    let equipmentSubGroup (row : TypeCatRow) : Predicate option =
        match row.SuperCategory, row.SubGroup with
        | _, None | _, Some "NULL" -> None
        | "EQUIPMENT CATEGORY", Some subgroup ->
            predicate "equipment_subgroup" 
                        [ stringTerm row.AssetType
                        ; stringTerm subgroup
                        ]
                |> Some
        | _ -> None

