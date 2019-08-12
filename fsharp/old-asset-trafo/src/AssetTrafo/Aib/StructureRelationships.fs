// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module StructureRelationships =

    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes

    let deriveNodeType (category : string) 
                       (superCategory : string) : Result<string, string> = 
        if superCategory = "EQPTCAT" then
            Ok "Equipment"
        else
            match category with
            | "INSTALLATION" -> Ok "Installation"
            | "SITE" -> Ok "Site"
            | "PROCESSGROUP" -> Ok "ProcessGroup"
            | "PROCESS" -> Ok "Process"
            | "PLANT" -> Ok "Plant"
            | "PLANTITEM" -> Ok "PlantItem"
            | _ -> Error <| sprintf "Unknown(%s)" category


    /// Assets are built as a rose tree
    type AibGenericNode = 
        { AssetReference : string       // aka SAI number
          NodeType : string
          AssetName : string
          AssetType : string
          Category : string
          SuperCategory : string
          Attributes : Attributes
          Kids : AibGenericNode list
        }

    type TopLevelExport = 
        CsvProvider< Schema = @"AssetReference(string), AssetName(string), AssetType(string), Category(string), SuperCategory(string)"
                   , Sample = "SAI01234567,name,type,category,supercategory "
                   , HasHeaders = true >

    type TopLevelRow = TopLevelExport.Row

    let makeTopNode (row : TopLevelRow) : AibGenericNode = 
        let nodeType = 
            match deriveNodeType row.Category row.SuperCategory with
            | Error msg -> sprintf "<<<%s>>>" msg
            | Ok ans -> ans

        { AssetReference = row.AssetReference
          NodeType = nodeType
          AssetName = row.AssetName
          AssetType = row.AssetType
          Category = row.Category
          SuperCategory = row.SuperCategory
          Attributes = Attributes.Empty
          Kids = []
        }

    let readTopLevelExport (path:string) : TopLevelRow option = 
        let table = TopLevelExport.Load(uri = path)
        table.Rows |> Seq.tryHead 




    type RelationshipsExport = 
        CsvProvider< Sample = @"..\data\ald_structure_relationships.csv"
                   , HasHeaders = true >

    type RelationshipRow = RelationshipsExport.Row

    
    let makeNode1 (row : RelationshipRow) 
                  (kids : AibGenericNode list) : AibGenericNode =
        let nodeType = 
             match deriveNodeType row.ChildTypeCategory row.ChildTypeSuperCategory with
             | Error msg -> sprintf "<<<%s>>>" msg
             | Ok ans -> ans
        { AssetReference = row.ChildReference
          NodeType = nodeType
          AssetName = row.ChildName
          AssetType = row.ChildTypeName
          Category = row.ChildTypeCategory
          SuperCategory = row.ChildTypeSuperCategory
          Attributes = Attributes.Empty
          Kids = kids
        }

    let readRelationshipsExport (path:string) : RelationshipRow list = 
        RelationshipsExport.Load(uri = path).Rows 
            |> Seq.toList




    let findChildRows (reference : string) 
                      (rows : RelationshipRow list) : RelationshipRow list = 
        rows |> List.filter (fun row -> reference = row.ParentReference)


    let relationshipsToTree (topNode : AibGenericNode) 
                            (input : RelationshipRow list) : AibGenericNode = 
        let rec work (reference : string) 
                     (cont : AibGenericNode list -> AibGenericNode) = 
            let childRows = findChildRows reference input
            workList childRows cont
        and workList (kids : RelationshipRow list) 
                     (cont : AibGenericNode list -> AibGenericNode) = 
            match kids with
            | [] -> cont []
            | row1 :: rest -> 
                work row1.ChildReference (fun kids ->
                let node1 = makeNode1 row1 kids 
                workList rest (fun nodes -> 
                cont (node1 :: nodes)))
        work topNode.AssetReference (fun kids -> { topNode with Kids = kids })


    let loadStructure (topLevelExportPath : string) 
                      (structureRelationshipsExportPath : string) : AibGenericNode option = 
        match readTopLevelExport topLevelExportPath with
        | None -> None
        | Some topLevel -> 
            let top = makeTopNode topLevel 
            let relations = readRelationshipsExport structureRelationshipsExportPath
            relationshipsToTree top relations |> Some
                
            

    let toJsonValue (input : AibGenericNode) : JsonValue =
        let rec work asset cont = 
            workList asset.Kids (fun kids -> 
            let jsArr = JsonValue.Array (List.toArray kids)
               
            cont (JsonValue.Record 
                    [| ("assetReference",   JsonValue.String asset.AssetReference)
                     ; ("nodeType",         JsonValue.String asset.NodeType)
                     ; ("assetName",        JsonValue.String asset.AssetName)
                     ; ("assetType",        JsonValue.String asset.AssetType)
                     ; ("category",         JsonValue.String asset.Category)
                     ; ("superCategory",    JsonValue.String asset.SuperCategory)
                     ; ("attributes",       asset.Attributes.ToJsonValue ())
                     ; ("kids",             jsArr)
                    |]))
        and workList xs cont =
            match xs with
            | [] -> cont []
            | kid1 :: kids -> 
                work kid1 ( fun v1 -> 
                workList kids ( fun vs -> 
                cont (v1::vs)))               
        work input id

    let aibGenericNodeToJson (node : AibGenericNode) (outputPath:string) : unit = 
        let json = toJsonValue node
        use sw = new StreamWriter (path = outputPath)
        json.WriteTo(sw, JsonSaveOptions.None)