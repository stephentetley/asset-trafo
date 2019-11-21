// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Uxl

module UxlChangeFile =

    open System

    open SheetDoc.SheetDoc
    open SheetDoc.Internal.Syntax       // shouldn't need to do this...

    type RowDoc = SheetDoc.Internal.Syntax.RowDoc

    
    let xBool (value : bool) : CellDoc = 
        if value then text "X" else blankCell

    let uint32Value (i : uint32) : ValueDoc = int64Value (int64 i)

    let dateText (dt : DateTime) : CellDoc = 
        text <| dt.ToString(format = "dd-MM-yyyy")


    /// This is the data for whole file not a single row).
    /// It generates a ragged sheet.
    type ChangeRequestDetails = 
        { Description : string
          TypeOfChangeRequest : string
          DraftMode : bool
          FlFunctionLocation : string list
          EqEquipment : string list
          ProcessRequester : string
        }

    let mmopChangeRequestDetailsHeaders () : RowDoc = 
        row [ text "Description (Long)"
            ; text "Priority"
            ; text "Due Date"
            ; text "Reason"
            ; text "Type of Change Request"
            ; text "Change Request Group"
            ; text "Draft Mode"
            ; text "MBOM-Material"
            ; text "MBOM-Plant"
            ; text "MBOM-Usage"
            ; text "MBOM-Alternative"
            ; text "FL-Functional Location"
            ; text "EQ-Equipment"
            ; text "Process Requester"
            ]

    let private mmopChangeRequestDetailsRow1 (source : ChangeRequestDetails) : RowDoc = 
        let fl1, eq1 = 
            match List.tryHead source.FlFunctionLocation, List.tryHead source.EqEquipment with
            | Some a, _ -> text a, blankCell
            | None, Some b -> blankCell, text b
            | _, _ -> blankCell, blankCell
        row [ text source.Description
            ; blankCell
            ; blankCell
            ; blankCell
            ; text source.TypeOfChangeRequest
            ; blankCell
            ; xBool source.DraftMode
            ; blankCell
            ; blankCell
            ; blankCell
            ; blankCell
            ; fl1
            ; eq1
            ; text source.ProcessRequester
            ]

    let private mmopChangeRequestDetailsFLRows (source : ChangeRequestDetails) : RowDoc list = 
        let make1 = 
            fun ss -> row [ yield! List.replicate 11 blankCell 
                          ; yield text ss
                          ; yield! List.replicate 2 blankCell
                          ]
        List.map make1 source.FlFunctionLocation

    let private mmopChangeRequestDetailsEQRows (source : ChangeRequestDetails) : RowDoc list = 
        let make1 = 
            fun ss -> row [ yield! List.replicate 12 blankCell 
                          ; yield text ss
                          ; yield blankCell
                          ]
        List.map make1 source.EqEquipment


    let mmopChangeRequestDetails (source : ChangeRequestDetails) : SheetDoc =
        let rows = 
            let r0 = mmopChangeRequestDetailsHeaders ()
            let r1 = mmopChangeRequestDetailsRow1 source
            let xs = mmopChangeRequestDetailsFLRows source
            let ys = mmopChangeRequestDetailsEQRows source
            r0 :: r1 :: xs @ ys
        sheet "Change Request Details" rows

    // ************************************************************************


    type FunctionalLocationData = 
        { FunctionalLocation : string
          MaskedFuncLoc : string
          Description : string
          FuncLocCat : uint32
          StrIndicator : string
          ObjectType : string
          StartUpDate : DateTime
          SupFunctLoc : string
        }

    
    let private mmopFunctionalLocationDataHeaders () : RowDoc = 
        row [ text "Functional Location"
            ; text "Masked Func Loc"
            ; text "Active Inactive Stat"
            ; text "Description (medium)"
            ; text "FunctLocCat."
            ; text "StrIndicator"
            ; text "Object type"
            ; text "AuthorizGroup"
            ; text "Gross Weight"
            ; text "Unit of Weight"
            ; text "Size/dimens."
            ; text "Inventory no."
            ; text "Start-up date"
            ; text "AcquistnValue"
            ; text "Currency"
            ; text "Acquisition Date"
            ; text "Manufacturer"
            ; text "Model number"
            ; text "ManufPartNo."
            ; text "ManufSerialNo."
            ; text "ManufCountry"
            ; text "ConstructYear"
            ; text "ConstructMth"
            ; text "MaintPlant"
            ; text "Location"
            ; text "Room"
            ; text "Plant section"
            ; text "Work center"
            ; text "ABC indic."
            ; text "Sort field"
            ; text "Company Code"
            ; text "Buisness Area"
            ; text "Asset"
            ; text "Sub-number"
            ; text "Cost Center"
            ; text "CO Area"
            ; text "WBS Element - Ext.Form"
            ; text "StandgOrder"
            ; text "SettlementOrder"
            ; text "Planning plant"
            ; text "Planner group"
            ; text "Main WorkCtr"
            ; text "Plnt WorkCenter"
            ; text "Catalog profile"
            ; text "Position"
            ; text "Construction type Ma"
            ; text "SupFunctLoc."
            ; text "EquipInstall."
            ; text "Single inst."
            ; text "Status Profile"
            ; text "User Status"
            ; text "Status of an object"
            ; text "Status without stsno"
            ; text "Begin guarantee(C)"
            ; text "Warranty end(C)"
            ; text "Master Warranty(C)"
            ; text "InheritWarranty(C)"
            ; text "Pass on warrnty(C)"
            ; text "Begin guarantee(V)"
            ; text "Warranty end(V)"
            ; text "Master Warranty(V)"
            ; text "InheritWarranty(V)"
            ; text "Pass on warr(V)"
            ; text "Sales Org."
            ; text "Distr. Channel"
            ; text "Division"
            ; text "Sales Office"
            ; text "Sales Group"
            ]

    

    let private mmopFunctionalLocationDataRow (funcLocData : FunctionalLocationData) : RowDoc =  
        row [ yield text funcLocData.FunctionalLocation
            ; yield text funcLocData.MaskedFuncLoc
            ; yield blankCell
            ; yield text funcLocData.Description
            ; yield cell <| uint32Value funcLocData.FuncLocCat
            ; yield text funcLocData.StrIndicator
            ; yield text funcLocData.ObjectType
            ; yield! List.replicate 5 blankCell
            ; yield dateText funcLocData.StartUpDate
            ; yield! List.replicate 33 blankCell
            ; yield text funcLocData.SupFunctLoc
            ]

    let mmopFunctionalLocationData (source : FunctionalLocationData list) : SheetDoc =
        let r0 = mmopFunctionalLocationDataHeaders ()
        let xs = List.map mmopFunctionalLocationDataRow source
        sheet "Functional Location Data" (r0 :: xs)

    // ************************************************************************



    //type EquipmentData = 
    //    { Equipment : string
    //      DeletionInd : bool
    //      ActiveInactiveStat : string
    //      EquipCategory : string
    //      Description : string
    //      ValidFrom : DateTime option         
    //      ObjectType : string
    //      Manufacturer : string
    //      ModelNumber : string
    //    }


    ///// ClassType is always "003"
    //type FunctionalLocationClassification = 
    //    { FunctionalLocation : string 
    //      DeletionInd : bool
    //      Class : string
    //      Status : string
    //      Characteristics : string
    //      CharValue : string
    //    }
    

    ///// ClassType is always "002"
    //type EquipmentClassification = 
    //    { Equipment : string 
    //      DeletionInd : bool
    //      Class : string
    //      Status : string
    //      Characteristics : string
    //      CharValue : string        
    //    }
    


