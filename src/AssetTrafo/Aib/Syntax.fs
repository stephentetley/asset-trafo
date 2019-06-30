// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Syntax =
    
    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes
    open AssetTrafo.Base.JsonReader
    

    let private genericRecord (typeName: string) 
                              (kidReader : JsonReader<'kid>)
                              (constructor : string -> string -> Attributes -> 'kid list -> 'ans) : RecordReader<'ans> = 
        jsonRecord { 
            do! assertTypeField "type" typeName
            let! uid = readField "assetReference" readString
            let! name = readField "name" readString
            let! attrs = readField "attributes" <| Attributes.ReadJson ()
            let! kids = (Array.toList <<| (readField "kids" <| readArray kidReader)) <|> mreturn []
            return (constructor uid name attrs kids)
        }

    

    type AibUnknown =
        { Uid : string
          Name : string
          Attributes : Attributes
        }
        static member ReadJson () : JsonReader<AibUnknown> = 
            readRecord 
                <| jsonRecord { 
                        do! assertTypeField "type" "Unknown"
                        let! uid = readField "assetReference" readString
                        let! name = readField "name" readString
                        let! attrs = readField "attributes" <| Attributes.ReadJson ()
                        return { Uid = uid
                                 Name = name
                                 Attributes = attrs
                                }
                        }

    type AibEquipment =
        { Uid : string
          Name : string
          Attributes : Attributes
        }
        static member ReadJson () : JsonReader<AibEquipment> = 
            readRecord 
                <| genericRecord "Equipment" readNull 
                        (fun uid name attrs _ -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs })


    type AibPlantItem =
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibEquipment list
        }
        static member ReadJson () : JsonReader<AibPlantItem> = 
            readRecord 
                <| genericRecord "PlantItem" (AibEquipment.ReadJson()) 
                        (fun uid name attrs kids -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs 
                              Kids = kids })

    type AibPlantAssemblyKid = 
        | AibPlantAssemblyKidPlantItem of AibPlantItem
        | AibPlantAssemblyKidEquipment of AibEquipment
        | AibPlantAssemblyKidUnknown of AibUnknown
        static member ReadJson () : JsonReader<AibPlantAssemblyKid> = 
            choice [ AibPlantItem.ReadJson ()       |>> AibPlantAssemblyKidPlantItem
                   ; AibEquipment.ReadJson ()       |>> AibPlantAssemblyKidEquipment
                   ; AibUnknown.ReadJson ()         |>> AibPlantAssemblyKidUnknown
                   ]

    type AibPlantAssembly = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibPlantAssemblyKid list
        }
        static member ReadJson () : JsonReader<AibPlantAssembly> = 
            readRecord 
                <| genericRecord "PlantAssembly" (AibPlantAssemblyKid.ReadJson()) 
                        (fun uid name attrs kids -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs 
                              Kids = kids })

    
    type AibProcessKid =
        | AibProcessKidPlantAssembly of AibPlantAssembly
        | AibProcessKidPlantItem of AibPlantItem
        static member ReadJson () : JsonReader<AibProcessKid> = 
            choice [ AibPlantAssembly.ReadJson ()   |>> AibProcessKidPlantAssembly
                   ; AibPlantItem.ReadJson ()       |>> AibProcessKidPlantItem
                   ]

    type AibProcess =
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibProcessKid list
        }
        static member ReadJson () : JsonReader<AibProcess> = 
            readRecord 
                <| genericRecord "Process" (AibProcessKid.ReadJson()) 
                        (fun uid name attrs kids -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs 
                              Kids = kids })

    type AibProcessGroupKid =
        | AibProcessGroupKidProcess of AibProcess
        static member ReadJson () : JsonReader<AibProcessGroupKid> = 
            choice [ AibProcess.ReadJson ()         |>> AibProcessGroupKidProcess ]
                   
                   

    type AibProcessGroup = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibProcessGroupKid list
        }
        static member ReadJson () : JsonReader<AibProcessGroup> = 
            readRecord 
                <| genericRecord "ProcessGroup" (AibProcessGroupKid.ReadJson()) 
                        (fun uid name attrs kids -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs 
                              Kids = kids })

    type AibInstallationKid =
        | AibInstallationKidProcessGroup of AibProcessGroup  
        | AibInstallationKidProcess of AibProcess
        static member ReadJson () : JsonReader<AibInstallationKid> = 
            choice [ AibProcessGroup.ReadJson ()    |>> AibInstallationKidProcessGroup
                   ; AibProcess.ReadJson ()         |>> AibInstallationKidProcess ]

    type AibInstallation = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibInstallationKid list
        }
        static member ReadJson () : JsonReader<AibInstallation> = 
            readRecord 
                <| genericRecord "Installation" (AibInstallationKid.ReadJson()) 
                        (fun uid name attrs kids -> 
                            { Uid = uid
                              Name = name
                              Attributes = attrs 
                              Kids = kids })

    let readAibInstallationJson (inpath : string) : Result<AibInstallation, ErrMsg> = 
        try
            use source = new StreamReader(path = inpath)
            JsonValue.Load(source) |> runJsonReader (AibInstallation.ReadJson ()) 
        with
        | _ -> Error "Could not read input"

        
