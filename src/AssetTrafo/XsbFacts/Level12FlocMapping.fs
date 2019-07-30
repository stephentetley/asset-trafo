// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.XsbFacts


module Level12FlocMapping =

    open FSharp.Interop.Excel

    open FactX
    open FactX.FactWriter

    open AssetTrafo.Base.FactsCommon

    // ********** DATA SETUP **********
    type SiteMappingTable = 
        ExcelFile< @"G:\work\Projects\asset_sync\Lvl1_2FlocMapping.xlsx",
                    SheetName = "Mapping!",
                    ForceString = true >
    
    type SiteMappingRow = SiteMappingTable.Row


    let getSiteMappingRows (xlsxPath : string) : SiteMappingRow list = 
        let isBlank (row : SiteMappingRow) = 
            try 
                // First column / cell should be an int
                match row.AI2_InstallationReference with
                | null | "" -> true
                | _ -> false
            with
            | _ -> true
        (new SiteMappingTable(filename = xlsxPath)).Data
            |> Seq.filter (not << isBlank)
            |> Seq.toList


    let checkInput (input : string) : string option = 
        match input with
        | null | "NULL" -> None
        | _ -> input.Trim() |> Some


    // ************************************************************************
    // Aib Common Name
    
    //let private aibCommonName (row : SiteMappingRow) : Predicate option = 
    //    let make1 = 
    //        fun code name -> predicate "aib_common_name" [ stringTerm code; stringTerm name ]
    //    match (checkInput row.AI2_InstallationReference, 
    //            checkInput row.AI2_InstallationCommonName) with
    //    | Some code, Some name -> make1 code name |> Some
    //    | _, _ -> None


    

    //let generateCommonNameFacts (mappingRows : SiteMappingRow list)
    //                            (outputFile : string) : unit =  
    //    generatePredicates aibCommonName mappingRows
    //        |> writeFactsWithHeaderComment outputFile
            



    // ************************************************************************
    // Description Lookups (code to description) (S4)

    let aibInstS4SiteMapping (row : SiteMappingRow) : Predicate option = 
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.``S/4 Hana Floc Description``, 
                checkInput row.``S/4 Hana Floc Lvl1_Code``) with
        | Some saicode, Some name, Some s4floc1 -> 
            predicate "aib_inst_floc1_s4_name" 
                        [ quotedAtom saicode
                        ; quotedAtom name
                        ; quotedAtom s4floc1 ] |> Some
        | _, _, _-> None


    let aibInstS4Level12Mapping (row : SiteMappingRow) : Predicate option = 
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.``S/4 Hana Floc Lvl1_Code``, 
                checkInput row.``S/4 Hana Floc Lvl2_Code``) with
        | Some saicode, Some floc1, Some floc2 -> 
            predicate "aib_inst_floc1_floc2" 
                        [ quotedAtom saicode
                        ; quotedAtom floc1
                        ; quotedAtom floc2 ] |> Some
        | _, _, _-> None

    let generateLevel12Mappings (mappingRows : SiteMappingRow list)
                                (outputFile : string) : unit =  
        writeFactsWithHeaderComment outputFile
            <| factWriter { 
                    do! generatePredicates aibInstS4SiteMapping mappingRows
                    do! generatePredicates aibInstS4Level12Mapping mappingRows
                    return ()
                }


    // ************************************************************************
    // Aib Common Name
    
    //let private aibInstallationType (row : SiteMappingRow) : Predicate option = 
    //    let make1 = 
    //        fun code typ -> 
    //            predicate "aib_installation_type" [ quotedAtom code; quotedAtom typ ]
    //    match (checkInput row.AI2_InstallationReference, 
    //            checkInput row.AI2_InstallationAssetType) with
    //    | Some code, Some typ -> make1 code typ |> Some
    //    | _, _ -> None


    

    //let generateAibInstallationType (mappingRows : SiteMappingRow list)
    //                        (outputFile : string) : unit =  
    //    generatePredicates aibInstallationType mappingRows
    //        |> writeFactsWithHeaderComment outputFile

