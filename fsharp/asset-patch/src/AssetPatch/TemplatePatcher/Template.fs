// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Template =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CommonTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.EquiIndexing
    


    type EnvProperties = 
        { StartupDate : DateTime
          ObjectStatus : string
          StructureIndicator : string
          CompanyCode : uint32
          MaintenancePlant : uint32
          ControllingArea : uint32
          Currency : string
        }


    let defaultEnvProperties () : EnvProperties = 
        { StartupDate = DateTime.Now
          ObjectStatus = "UCON"
          StructureIndicator = "YW-GS"
          CompanyCode = 2100u
          MaintenancePlant = 2100u          
          ControllingArea = 1000u
          Currency = "GBP"
        }

    // FuncLocPath should not be directly visible to client code

    type TemplateEnv = 
        { CurrentFloc : FuncLocPath
          Properties : EnvProperties
          EquipmentIndices : EquiMap
        }

    /// State + Reader + Error
    type Template<'a> = 
        | Template of (TemplateEnv -> Result<'a option, ErrMsg>)

    let inline private apply1 (ma : Template<'a>) 
                              (env : TemplateEnv) : Result<'a option, ErrMsg> = 
        let (Template fn) = ma in fn env

    let mreturn (x:'a) : Template<'a> = 
        Template <| fun _ -> Ok (Some x)

    let blank () : Template<'a> = 
        Template <| fun _ -> Ok (None)

    let inline private bindM (ma : Template<'a>) 
                             (fn : 'a -> Template<'b>) : Template<'b> =
        Template <| fun env -> 
            match apply1 ma env with
            | Error msg -> Error msg 
            | Ok (Some a) -> apply1 (fn a) env
            | Ok None -> Ok None
         
    let inline private delayM (fn : unit -> Template<'a>) : Template<'a> = 
        bindM (mreturn ()) fn 
    
    type TemplateBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (template : TemplateBuilder) = new TemplateBuilder()


    let runTemplate (env : TemplateEnv) (code : Template<'a>) : Result<'a, ErrMsg> = 
        match apply1 code env with
        | Ok (Some(a)) -> Ok a
        | Ok None -> Error "Empty result"
        | Error msg -> Error msg

    
        
    let private ( |>> ) (ma : Template<'a>) (fn : 'a -> 'b) : Template<'b> = 
        Template <| fun env -> 
            match apply1 ma env with 
            | Ok None -> Ok None
            | Ok (Some(a)) -> Ok (Some(fn a))
            | Error msg -> Error msg
    
    let private unlistM (source: Template<'x> list) : Template<'x list> = 
        Template <| fun env -> 
            let rec work xs fk sk = 
                match xs with 
                | [] -> sk []
                | x :: rest -> 
                    match apply1 x env with
                    | Error msg -> fk msg
                    | Ok a -> 
                        work rest fk (fun vs -> 
                        match a with 
                        | Some v -> sk (v :: vs)
                        | None -> sk vs)
            work source (fun msg -> Error msg) (fun xs -> Ok(Some(xs)))



    let rootFloc (floc : FuncLocPath) (ma : Template<'a>) : Template<'a> = 
        Template <| fun env -> 
            apply1 ma { env with CurrentFloc = floc } 

    let private asks () : Template<TemplateEnv> = 
        Template <| fun env -> Ok (Some env)
        
    let asksFloc () : Template<FuncLocPath> = 
        Template <| fun env -> Ok (Some(env.CurrentFloc))

    let asksFuncLocProperties () : Template<FuncLocProperties> = 
        Template <| fun env -> 
            let props : FuncLocProperties = 
                { StartupDate = env.Properties.StartupDate
                  StructureIndicator = env.Properties.StructureIndicator
                  MaintenancePlant = env.Properties.MaintenancePlant
                  ObjectStatus = env.Properties.ObjectStatus
                  ControllingArea = env.Properties.ControllingArea
                  CompanyCode = env.Properties.CompanyCode
                  Currency = env.Properties.Currency
                }
            Ok (Some(props))

    type EnvTransformer = EnvProperties -> EnvProperties

    let local (modify : EnvTransformer) (ma : Template<'a>) : Template<'a> = 
        Template <| fun env -> 
            let props = env.Properties
            apply1 ma { env with Properties = modify props }

    let locals (modifications : EnvTransformer list) (ma : Template<'a>) : Template<'a> = 
        let trafo = List.foldBack (fun f acc -> acc >> f) modifications id
        Template <| fun env -> 
            let props = env.Properties
            apply1 ma { env with Properties = trafo props } 

            
    let startupDate (date : DateTime) : EnvTransformer = 
        fun env -> { env with StartupDate = date }


    let internal extendFloc (levelCode  : string) (ma : Template<'a>) : Template<'a> = 
        Template <| fun env -> 
            let floc = env.CurrentFloc
            apply1 ma { env with CurrentFloc = extend levelCode floc } 
    

    type Characteristic = Template<S4Characteristic>
    
    let _characteristic (name : string) (value : ValuaValue) : Characteristic = 
        mreturn { 
            Name = name
            Value = value
        }


    let optional (ma : Characteristic) : Characteristic = 
        Template <| fun env -> 
            match apply1 ma env with            
            | Error msg -> Error msg
            | Ok None -> Ok (None)
            | Ok (Some(c1)) -> 
                match c1.Value with 
                | NullValue ->  Ok None
                | _ -> Ok (Some(c1))

    let applyOptional (fn : 'a -> Characteristic)  (value : Option<'a>) : Characteristic = 
        match value with 
        | None -> blank ()
        | Some a -> fn a


    type Class = Template<S4Class>

    let _class (name : string) (values : Characteristic list) : Class = 
        template {
            let! vs = unlistM values
            return { 
                ClassName = name
                Characteristics = vs 
            }
        }
        
        

    type Equipment = Template<S4Equipment>
    
    type EquipmentAttribute = Template<S4Equipment -> S4Equipment>

    let private setAttribute (e1 : Equipment) (attrib : EquipmentAttribute) : Equipment = 
        Template <| fun env -> 
            match apply1 e1 env with
            | Ok (Some a) -> 
                match apply1 attrib env with
                | Ok (Some f) -> Ok (Some(f a))
                | Ok None -> Ok None
                | Error msg -> Error msg
            | Ok None -> Ok None
            | Error msg -> Error msg

    let private setAttributes (e1 : Equipment) (attribs : EquipmentAttribute list) : Equipment = 
        List.fold setAttribute e1 attribs
    


    let private getEquipmentIndex (description : string) : Template<uint32> = 
        Template <| fun env -> 
            let floc = env.CurrentFloc
            let indices = env.EquipmentIndices
            match tryFindEquiNum description floc indices with
            | Some num -> Ok (Some num)
            | None -> Error (sprintf "No equipment found for %s '%s'" (floc.ToString()) description) 
    

    let _equipment (description : string) 
                    (category : string) 
                    (objectType : string)
                    (classes : Class list) 
                    (subordinateEquipment : Equipment list) 
                    (attributes : EquipmentAttribute list) : Equipment = 
        let equip1 = 
            template {
                let! level = asksFloc () |>> fun x -> x.Level
                let! cs = unlistM classes
                let! es = unlistM subordinateEquipment
                let! equinum = getEquipmentIndex description
                return {
                    EquipmentId = equinum
                    Description = description
                    Category = category
                    ObjectType = objectType
                    Manufacturer = None
                    Model = None
                    SerialNumber = None
                    ConstructionYear = None
                    ConstructionMonth = None
                    Classes = cs 
                    SuboridnateEquipment = es 
                }
            } 
        setAttributes equip1 attributes


    let internal equipmentAttribute (update : S4Equipment -> S4Equipment) : EquipmentAttribute =
        Template <| fun env -> Ok (Some update)
            
    

    type Component = Template<S4Component>


    let _component (token : string) (description : string) (objectType : string)
                   (classes : Class list) (equipment : Equipment list) : Component = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs 
                    Equipment = es 
                }
            }

    type Item = Template<S4Item>
    
    let _item (token : string) (description : string) (objectType : string)
                (classes : Class list) 
                (components : Component list) (equipment : Equipment list) : Item = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM components
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs 
                    Components = xs
                    Equipment = es 
                }
            }

    type Assembly = Template<S4Assembly>
    
    let _assembly (token : string) (description : string) (objectType : string)
                    (classes : Class list) 
                    (items : Item list) (equipment : Equipment list) : Assembly = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM items
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs
                    Items = xs
                    Equipment = es 
                }
            }

    type System = Template<S4System>
    
    let _system (token : string) (description : string) (objectType : string)
                (classes : Class list) 
                (assemblies : Assembly list) (equipment : Equipment list) : System = 
        extendFloc token
            <| template {
                let! floc =  asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM assemblies
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs
                    Assemblies = xs
                    Equipment = es 
                }
            }

    type Process = Template<S4Process>
    
    let _process (token : string) (description : string) (objectType : string)
                    (classes : Class list) 
                    (systems : System list) : Process = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM systems
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs 
                    Systems = xs 
                }
            }

    type ProcessGroup = Template<S4ProcessGroup>
    
    let _processGroup (token : string) (description : string) (objectType : string)    
                        (classes : Class list) 
                        (processes : Process list) : ProcessGroup = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()               
                let! cs = unlistM classes
                let! xs = unlistM processes
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs 
                    Processes = xs 
                }
            }

    type Function = Template<S4Function>
    
    let _function (token : string) (description : string) (objectType : string)
                  (classes : Class list) 
                  (processGroups : ProcessGroup list) : Function = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM processGroups
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classes = cs 
                    ProcessGroups = xs 
                }
            }

    type Site = Template<S4Site>
    
    let _site (siteCode : string) (description : string) 
                (classes : Class list) 
                (functions : Function list) : Site = 
        rootFloc (FuncLocPath.Create siteCode)
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! cs = unlistM classes
                let! xs = unlistM functions
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description 
                    ObjectType = "SITE"
                    Classes = cs 
                    Functions = xs 
                }
            }

    type Class1<'a> = 'a -> Class
    
    type Component1<'a> = 'a -> Component
     
    type Item1<'a> = 'a -> Item 
     
    type Assembly1<'a> = 'a -> Assembly 
     
    type System1<'a> = 'a -> System 

    type Process1<'a> = 'a -> Process 

    type ProcessGroup1<'a> = 'a -> ProcessGroup 
     
    type Function1<'a> = 'a -> Function 
     
    type Site1<'a> = 'a -> Site

