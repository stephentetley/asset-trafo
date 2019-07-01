// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module StructureRelationships =

    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes

    /// Assets are built as a rose tree
    type Node = 
        { AssetReference : string       // aka SAI number
          AssetName : string
          AssetType : string
          Category : string
          SuperCategory : string
          Attribs : Attributes
          Kids : Node list
        }

    type TopLevelExport = 
        CsvProvider< Schema = @"AssetReference(string), AssetName(string), AssetType(string), Category(string), SuperCategory(string)"
                   , Sample = "SAI01234567,name,type,category,supercategory "
                   , HasHeaders = true >

    type TopLevelRow = TopLevelExport.Row

    let makeTopNode (row : TopLevelRow) : Node = 
        { AssetReference = row.AssetReference
          AssetName = row.AssetName
          AssetType = row.AssetType
          Category = row.Category
          SuperCategory = row.SuperCategory
          Attribs = Attributes.Empty
          Kids = []
        }

    let readTopLevelExport (path:string) : TopLevelRow option = 
        let table = TopLevelExport.Load(uri = path)
        table.Rows |> Seq.tryHead 




    type RelationshipsExport = 
        CsvProvider< Sample = @"..\data\ald_structure_relationships.csv"
                   , HasHeaders = true >

    type RelationshipRow = RelationshipsExport.Row

    
    let makeNode1 (row : RelationshipRow) (kids : Node list) : Node = 
        { AssetReference = row.ChildReference
          AssetName = row.ChildName
          AssetType = row.ChildTypeName
          Category = row.ChildTypeCategory
          SuperCategory = row.ChildTypeSuperCategory
          Attribs = Attributes.Empty
          Kids = kids
        }

    let readRelationshipsExport (path:string) : RelationshipRow list = 
        RelationshipsExport.Load(uri = path).Rows 
            |> Seq.toList




    let findChildRows (reference : string) 
                      (rows : RelationshipRow list) : RelationshipRow list = 
        rows |> List.filter (fun row -> reference = row.ParentReference)


    let relationshipsToTree (topNode : Node) 
                            (input : RelationshipRow list) : Node = 
        let rec work (reference : string) 
                     (cont : Node list -> Node) = 
            let childRows = findChildRows reference input
            workList childRows cont
        and workList (kids : RelationshipRow list) 
                     (cont : Node list -> Node) = 
            match kids with
            | [] -> cont []
            | row1 :: rest -> 
                work row1.ChildReference (fun kids ->
                let node1 = makeNode1 row1 kids 
                workList rest (fun nodes -> 
                cont (node1 :: nodes)))
        work topNode.AssetReference (fun kids -> { topNode with Kids = kids })


    let loadStructure (topLevelExportPath : string) 
                      (structureRelationshipsExportPath : string) : Node option = 
        match readTopLevelExport topLevelExportPath with
        | None -> None
        | Some topLevel -> 
            let top = makeTopNode topLevel 
            let relations = readRelationshipsExport structureRelationshipsExportPath
            relationshipsToTree top relations |> Some
                
            

    //let toJsonValue (input:Node) : JsonValue =
    //    let rec work asset cont = 
    //        workList asset.Kids (fun kids -> 
    //        let jsArr = JsonValue.Array (List.toArray kids)
               
    //        cont (JsonValue.Record 
    //                [| ("assetReference",   JsonValue.String asset.AssetReference)
    //                 ; ("name",             JsonValue.String asset.NodeName)
    //                 ; ("type",             JsonValue.String asset.NodeType)
    //                 ; ("attributes",       asset.Attribs.ToJsonValue ())
    //                 ; ("kids",             jsArr)
    //                |]))
    //    and workList xs cont =
    //        match xs with
    //        | [] -> cont []
    //        | kid1 :: kids -> 
    //            work kid1 ( fun v1 -> 
    //            workList kids ( fun vs -> 
    //            cont (v1::vs)))               
    //    work input id

    //let aibXlsxToJson (inputPath:string) (outputPath:string) : unit = 
    //    match readAibFlat inputPath |> aibToNode |> Option.map toJsonValue with
    //    | None -> printfn "Read error"
    //    | Some json -> 
    //        use sw = new StreamWriter (path = outputPath)
    //        json.WriteTo(sw, JsonSaveOptions.None)