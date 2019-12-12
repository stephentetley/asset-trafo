// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher



module Datatypes =
    
    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Parser
    open AssetPatch.Base.FuncLocPath
    
    /// Note - the Change types are very 'wide', they can modify 
    /// administrative / metadata fields
    
    type ValuaEquiChange = 
        { Equi : uint32
          CharId : string
          ValueCount : int
          Updates : AssocList<string, string>
        }

    type ValuaFlocChange = 
        { Floc : FuncLocPath
          CharId : string
          ValueCount : int
          Updates : AssocList<string, string>
        }

    type EquiChange = 
        { Equi : uint32
          Updates : AssocList<string, string>
        }

    type Characteristic = 
        { ClassType : IntegerString
          CharacteristicID : string
          CharacteristicValue : string
          ValueCount : int
        }

    // From the information we have in the download files we cannot
    // say what are the characteristics held by a class.
    // So we do not try to synthesize this information.
    type Class = 
        { Name : string
          ClassType : uint32
          ClassInt : uint32
        }

    // Flat or recursive??
    // Try flat for now...

    type Floc = 
        { Path : FuncLocPath 
          Description : string
          ObjectType : string
          Classes : Class list
          Characteristics : Characteristic list
        }

        member x.Level 
            with get() : int = x.Path.Level


    type Equi = 
        { Number : IntegerString 
          Description : string
          ObjectType : string
          Classes : Class list
          Characteristics : Characteristic list
        }



    // Characteristics

    let assocsToCharacteristic (attributes : AssocList<string, string>) : CompilerMonad<Characteristic> = 
        match AssocList.tryFind3 "CLASSTYPE" "CHARID" "TEXTBEZ" attributes with
        | Some (ctype, cid, cvalue) -> 
            mreturn { 
                ClassType = IntegerString.OfString ctype
                CharacteristicID = cid
                CharacteristicValue = cvalue
                ValueCount = Option.defaultValue 1 (AssocList.tryFindInt "VALCNT" attributes)
            }
         | None -> throwError "Could not find required fields for a VALUA* element"

    let assocsToFlocCharacteristic (attributes : AssocList<string, string>) : CompilerMonad<FuncLocPath * Characteristic> = 
        compile {
            let! floc = liftOption (AssocList.tryFind "FUNCLOC" attributes) |>> FuncLocPath.Create
            let! chars = assocsToCharacteristic attributes
            return (floc, chars)
        }

    let assocsToEquiCharacteristic (attributes : AssocList<string, string>) : CompilerMonad<IntegerString * Characteristic> = 
        compile {
            let! equi = liftOption (AssocList.tryFind "EQUI" attributes) |>> IntegerString.OfString
            let! char = assocsToCharacteristic attributes
            return (equi, char)
        }

    // Classes 
    
    let assocsToClass (attributes : AssocList<string, string>) : CompilerMonad<Class> = 
        match AssocList.tryFind3 "CLASS" "CLASSTYPE" "CLINT" attributes with
        | Some (cname, ctype, clint) -> 
            mreturn { 
                Name = cname   
                ClassType = try uint32 clint with | _ -> 0u
                ClassInt = try uint32 clint with | _ -> 0u
            }
         | None -> throwError "Could not find required fields for a CLASS* element"

    let assocsToFlocClass (attributes : AssocList<string, string>) : CompilerMonad<FuncLocPath * Class> = 
        compile {
            let! floc = liftOption (AssocList.tryFind "FUNCLOC" attributes) |>> FuncLocPath.Create
            let! clazz = assocsToClass attributes
            return (floc, clazz)
        }


    let assocsToEquiClass (attributes : AssocList<string, string>) : CompilerMonad<IntegerString * Class> = 
        compile {
            let! equi = liftOption (AssocList.tryFind "EQUI" attributes) |>> IntegerString.OfString
            let! clazz = assocsToClass attributes
            return (equi, clazz)
        }

    // Floc

    let assocsToFloc (attributes : AssocList<string, string>) : CompilerMonad<Floc> = 
        match AssocList.tryFind3 "FUNCLOC" "TXTMI" "EQART" attributes with
        | Some (funcloc, desc, otype) -> 
            mreturn { 
                Path = FuncLocPath.Create funcloc 
                Description = desc
                ObjectType = otype
                Classes = [] 
                Characteristics = []
            }
         | None -> throwError "Could not find required fields for a FUNCLOC"


    // Populate flocs...
    let populateFloc (floc : Floc)     
                        (classes : (FuncLocPath * Class) list) 
                        (characteristics : (FuncLocPath * Characteristic) list) : Floc = 
        let addCharacteristic (path, ch) (floc : Floc) = 
            if path = floc.Path then 
                { floc with Characteristics = ch :: floc.Characteristics }
            else floc
        let addClass (path, cl) (floc : Floc) = 
            if path = floc.Path then 
                { floc with Classes = cl :: floc.Classes }
            else floc
        List.foldBack addCharacteristic characteristics floc 
            |> List.foldBack addClass classes



    // File reading

    let readValuaFlocFile (inputPath : string) : CompilerMonad<(FuncLocPath * Characteristic) list> = 
        compile {
            let! changes = liftResult <| readChangeFile inputPath
            do! assertBool (changes.Header.EntityType = ValuaFloc) "Not a VALUAFLOC file"
            return! mapM assocsToFlocCharacteristic changes.RowAssocs
        }

    let readClassFlocFile (inputPath : string) : CompilerMonad<(FuncLocPath * Class) list> = 
        compile {
            let! changes = liftResult <| readChangeFile inputPath
            do! assertBool (changes.Header.EntityType = ClassFloc) "Not a CLASSFLOC file"
            return! mapM assocsToFlocClass changes.RowAssocs
        }


    let readFuncLocFile (inputPath : string) : CompilerMonad<Floc list> = 
        compile {
            let! changes = liftResult <| readChangeFile inputPath
            do! assertBool (changes.Header.EntityType = FuncLoc) "Not a FUNCLOC file"
            return! mapM assocsToFloc changes.RowAssocs
        }

