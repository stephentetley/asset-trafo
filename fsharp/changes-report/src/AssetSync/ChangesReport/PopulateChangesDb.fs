// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =


    open AssetSync.ChangesReport.ImportSchema

    // >>>
    //For sl-sqlite

    let escapeQuotes (source: string) : string = 
        source.Replace("'", "''")
 
    let fmtBigInt (i : int64) : string = 
        i.ToString()
     
    let fmtInt (i : int) : string = 
        i.ToString()
       
    // <<<
   
    let insertStatement (tableName : string) (columnNames : string []) (values : string []) : string =
        let columns = String.concat ", " columnNames
        let vals = String.concat ", " values
        sprintf "INSERT INTO %s (%s)\n%s;" tableName columns vals
        
    let attributeChangeInsert (row : AttributeChangeRow) : string =
        let columns =
            [| "attibute_change_id"
             ; "change_request_id"
             ; "request_status"
            |]
        insertStatement "attribute_change"
                        columns
                        [| row.AideAssetAttributeValueId    |> fmtBigInt
                         ; row.ChangeRequestId              |> fmtBigInt
                         ; row.RequestStatus                |> escapeQuotes
                        |]