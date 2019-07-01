// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Flat =

    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes

    type SuperCat = 
        | EQUIPMENT
        | FLOC

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

    let readTopLevelExport (path:string) : TopLevelRow option = 
        let table = TopLevelExport.Load(uri = path)
        table.Rows |> Seq.tryHead

    let makeTopNode (row : TopLevelRow)  : Node = 
        { AssetReference = row.AssetReference
          AssetName = row.AssetName
          AssetType = row.AssetType
          Category = row.Category
          SuperCategory = row.SuperCategory
          Attribs = Attributes.Empty
          Kids = []
        }


    type RelationshipsExport = 
        CsvProvider< Sample = @"..\data\ald_structure_relationships.csv"
                   , HasHeaders = true >

    type RelationshipRow = RelationshipsExport.Row

    let readRelationshipsExport (path:string) : RelationshipRow list = 
        let table = RelationshipsExport.Load(uri = path)

        table.Rows |> Seq.toList


    let makeNode1 (row : RelationshipRow) (kids : Node list) : Node = 
        { AssetReference = row.ChildReference
          AssetName = row.ChildName
          AssetType = row.ChildTypeName
          Category = row.ChildTypeCategory
          SuperCategory = row.ChildTypeSuperCategory
          Attribs = Attributes.Empty
          Kids = kids
        }


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


    //let commonName1 (fullName: string) : string = 
    //    let arr = fullName.Split('/')
    //    let len = arr.Length
    //    if len = 2 then 
    //        fullName
    //    else
    //        arr.[len - 1]

    //let isDirectPrefix (parentName:string) (childName:string) : bool =
    //    if childName.StartsWith(parentName) then
    //        let arr1 = parentName.Split('/')
    //        let arr2 = childName.Split('/')
    //        arr1.Length = arr2.Length - 1
    //    else 
    //        false

    //let findChildRows (parentName:string) (rows:AibRow list) : AibRow list = 
    //    rows |> List.filter (fun row -> isDirectPrefix parentName row.``Common Name``)


    //let readBool (value:string) : bool = 
    //    match value with 
    //    | null | "FALSE" -> false
    //    | "TRUE" -> true
    //    | _ -> false

    //let hkeyToType (hkey:string) : string = 
    //    let cleanKey = match hkey with | null -> "" | _ -> hkey
    //    if cleanKey = "TODO" then 
    //        "Undefined"
    //    else
    //        match cleanKey.Length with
    //        | 1 -> "BusinessUnit"
    //        | 4 -> "System"
    //        | 8 -> "Function"
    //        | 13 -> "Installation"
    //        | 18 -> "SubInstallation"
    //        | 20 -> "ProcessGroup"
    //        | 24 -> "Process"
    //        | 31 -> "PlantAssembly"
    //        | 36 -> "PlantItem"
    //        | _ -> "Undentified"

    //let findType (elementName:string) (hkey:string) : string = 
    //    if elementName.StartsWith("EQUIPMENT: ") then
    //        "Equipment"
    //    else
    //        hkeyToType hkey


    //let makeAttributes (row:AibRow) : Attributes = 
    //    let assetType = findType (commonName1 row.``Common Name``) row.``Hierarchy Key``
    //    let hkey = if assetType = "Equipment" then "" else row.``Hierarchy Key``

    //    Attributes.Empty 
    //        |> add "gridRef"         (AttrString row.``Loc.Ref.``)
    //        |> add "installedFrom"   (AttrString row.``Installed From``)
    //        |> add "hkey"            (AttrString hkey)
    //        |> add "assetStatus"     (AttrString row.AssetStatus)
    //        |> add "inAide"          (AttrBool <| readBool row.``Asset in AIDE ?``)
    //        |> add "manufacturer"    (AttrString row.Manufacturer)
    //        |> add "model"           (AttrString row.Model)

    //let makeNode (row:AibRow) (kids:Node list) : Node = 
    //    let assetType = findType (commonName1 row.``Common Name``) row.``Hierarchy Key``

    //    { AssetReference = row.Reference 
    //      Attribs = makeAttributes row
    //      NodeName = commonName1 row.``Common Name``
    //      NodeType = assetType
    //      Kids = kids
    //    }



    //let aibToNode (input:AibRow list) : Node option = 
    //    let rec work (rows:AibRow list) (parentName:string) (cont : Node list -> Node) = 
    //        let childRows = findChildRows parentName rows
    //        workList childRows rows cont
    //    and workList (kids:AibRow List) (rows:AibRow list) (cont : Node list -> Node) = 
    //        match kids with
    //        | [] -> cont []
    //        | k1 :: rest -> 
    //            work rows (k1.``Common Name``) (fun ys ->
    //            let node1 = makeNode k1 ys 
    //            workList rest rows (fun nodes -> 
    //            cont (node1 :: nodes)))
    //    match input with 
    //    | [] -> None
    //    | x :: xs -> 
    //        work xs (x.``Common Name``) (makeNode x) |> Some


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