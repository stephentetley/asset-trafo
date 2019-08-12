// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =

    open SLSqlite.Utils
    open SLSqlite.SqliteDb

    open AssetSync.ChangesReport.ImportSchema


   
    let insertStatement (tableName : string) (columnNames : string []) (values : string []) : string =
        let columns = String.concat ", " columnNames
        let vals = String.concat ", " values
        sprintf "INSERT INTO %s (%s)\nVALUES (%s);" tableName columns vals
        

    let attributeChangeInsert (row : AttributeChangeRow) : string option =
        let columns =
            [| "attibute_change_id"
             ; "change_request_id"
             ; "request_status"
            |]
        insertStatement "attribute_change"
                        columns
                        [| row.AideAssetAttributeValueId    |> fmtBigInt
                         ; row.ChangeRequestId              |> fmtBigInt
                         ; row.RequestStatus                |> fmtText
                        |] |> Some

    let insertAttributeChangeRow (row : AttributeChangeRow) : SqliteDb<unit> = 
        match attributeChangeInsert row with
        | Some statement -> 
            printfn "%s" statement
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAttributeChangeExport csvPath)
            let rows = table.Rows |> Seq.toList
            return! withTransaction <| forMz rows insertAttributeChangeRow
        }
