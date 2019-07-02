// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Syntax =
    
    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.CompilerMonad
    open AssetTrafo.Base.Attributes
    open AssetTrafo.Base.JsonReader
    
    let assertNodeTypeField  (typeName : string) : RecordReader<unit> = 
        jsonRecord { 
            let! ans = readField "nodeType" readString
            if ans = typeName then
                return ()
            else
                return! readError "type test failed"
        }

    let readStringField  (fieldName : string) : RecordReader<string> = 
        readField fieldName readString

    let readKidsField  (kidReader : JsonReader<'kid>) : RecordReader<'kid list> = 
        (Array.toList <<| (readField "kids" <| readArray kidReader)) 
            <|> mreturn []


    



    type AibEquipment =
        { Reference : string
          EquipmentType : string
          Category : string
          Attributes : Attributes
        }
        static member ReadJson () : JsonReader<AibEquipment> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "Equipment"
                           let! uid         = readStringField "assetReference"
                           let! assetType   = readStringField "assetType"
                           let! category    = readStringField "category"
                           let! attrs = readField "attributes" <| Attributes.ReadJson ()
                           return { Reference = uid
                                    EquipmentType = assetType
                                    Category = category
                                    Attributes = attrs }
                        }


    type AibPlantItem =
        { Reference : string
          PlantItemName : string
          PlantItemType : string
          Attributes : Attributes
          Kids : AibEquipment list
        }
        static member ReadJson () : JsonReader<AibPlantItem> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "PlantItem"
                           let! uid         = readStringField "assetReference"
                           let! assetName   = readStringField "assetName"
                           let! assetType   = readStringField "assetType"
                           let! attrs       = readField "attributes" <| Attributes.ReadJson ()
                           let! kids        = readKidsField (AibEquipment.ReadJson ())
                           return { Reference = uid
                                    PlantItemName = assetName
                                    PlantItemType = assetType
                                    Attributes = attrs
                                    Kids = kids }
                        }

    type AibPlantKid = 
        | AibPlantKidPlantItem of AibPlantItem
        | AibPlantKidEquipment of AibEquipment

        static member ReadJson () : JsonReader<AibPlantKid> = 
            choice [ AibPlantItem.ReadJson ()       |>> AibPlantKidPlantItem
                   ; AibEquipment.ReadJson ()       |>> AibPlantKidEquipment
                   ]


    type AibPlant = 
        { Reference : string
          PlantName : string
          PlantType : string
          Attributes : Attributes
          Kids : AibPlantKid list
        }
        static member ReadJson () : JsonReader<AibPlant> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "Plant"
                           let! uid         = readStringField "assetReference"
                           let! assetName   = readStringField "assetName"
                           let! assetType   = readStringField "assetType"
                           let! attrs       = readField "attributes" <| Attributes.ReadJson ()
                           let! kids        = readKidsField (AibPlantKid.ReadJson ())
                           return { Reference = uid
                                    PlantName = assetName
                                    PlantType = assetType
                                    Attributes = attrs
                                    Kids = kids }
                        }

    
    type AibProcessKid =
        | AibProcessKidPlant of AibPlant
        | AibProcessKidPlantItem of AibPlantItem

        static member ReadJson () : JsonReader<AibProcessKid> = 
            choice [ AibPlant.ReadJson ()   |>> AibProcessKidPlant
                   ; AibPlantItem.ReadJson ()       |>> AibProcessKidPlantItem
                   ]

    type AibProcess =
        { Reference : string
          ProcessName : string
          Attributes : Attributes
          Kids : AibProcessKid list
        }
        static member ReadJson () : JsonReader<AibProcess> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "Process"
                           let! uid         = readStringField "assetReference"
                           let! assetName   = readStringField "assetName"
                           let! attrs       = readField "attributes" <| Attributes.ReadJson ()
                           let! kids        = readKidsField (AibProcessKid.ReadJson ())
                           return { Reference = uid
                                    ProcessName = assetName
                                    Attributes = attrs
                                    Kids = kids }
                        }

    type AibProcessGroupKid =
        | AibProcessGroupKidProcess of AibProcess
        static member ReadJson () : JsonReader<AibProcessGroupKid> = 
            choice [ AibProcess.ReadJson ()         |>> AibProcessGroupKidProcess ]
                   
                   

    type AibProcessGroup = 
        { Reference : string
          ProcessGroupName : string
          Attributes : Attributes
          Kids : AibProcessGroupKid list
        }
        static member ReadJson () : JsonReader<AibProcessGroup> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "ProcessGroup"
                           let! uid         = readStringField "assetReference"
                           let! assetName   = readStringField "assetName"
                           let! attrs       = readField "attributes" <| Attributes.ReadJson ()
                           let! kids        = readKidsField (AibProcessGroupKid.ReadJson ())
                           return { Reference = uid
                                    ProcessGroupName = assetName
                                    Attributes = attrs
                                    Kids = kids }
                        }

    type AibInstallationKid =
        | AibInstallationKidProcessGroup of AibProcessGroup  
        | AibInstallationKidProcess of AibProcess
        static member ReadJson () : JsonReader<AibInstallationKid> = 
            choice [ AibProcessGroup.ReadJson ()    |>> AibInstallationKidProcessGroup
                   ; AibProcess.ReadJson ()         |>> AibInstallationKidProcess ]


    type AibInstallation = 
        { Reference : string
          InstallationName : string
          Attributes : Attributes
          Kids : AibInstallationKid list
        }
        
        static member ReadJson () : JsonReader<AibInstallation> = 
            readRecord 
                <| jsonRecord { 
                           do! assertNodeTypeField "Installation"
                           let! uid         = readStringField "assetReference"
                           let! assetName   = readStringField "assetName"
                           let! attrs       = readField "attributes" <| Attributes.ReadJson ()
                           let! kids        = readKidsField (AibInstallationKid.ReadJson ())
                           return { Reference = uid
                                    InstallationName = assetName
                                    Attributes = attrs
                                    Kids = kids }
                        }


    let readAibInstallationJson (inpath : string) : Result<AibInstallation, ErrMsg> = 
        try
            use source = new StreamReader(path = inpath)
            JsonValue.Load(source) |> runJsonReader (AibInstallation.ReadJson ()) 
        with
        | _ -> Error "Could not read input"

        
