// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Template =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    
    type Level = int


    type TemplateState = 
        { EquiIndices : Map<Level, int>
          FlocIndices : Map<Level, int>
        }

    let internal templateStateZero : TemplateState = 
        { EquiIndices = Map.empty
          FlocIndices = Map.empty 
        }

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

    type TemplateEnv = FuncLocPath * EnvProperties

    /// State + Reader + Error
    type Template<'a> = 
        | Template of (TemplateEnv -> TemplateState -> Result<'a option * TemplateState, ErrMsg>)

    let inline private apply1 (ma : Template<'a>) 
                              (env : TemplateEnv) 
                              (st : TemplateState) : Result<'a option * TemplateState, ErrMsg> = 
        let (Template fn) = ma in fn env st

    let mreturn (x:'a) : Template<'a> = 
        Template <| fun _ st -> Ok (Some x, st)

    let blank () : Template<'a> = 
        Template <| fun _ st -> Ok (None, st)

    let inline private bindM (ma : Template<'a>) 
                             (fn : 'a -> Template<'b>) : Template<'b> =
        Template <| fun env st -> 
            match apply1 ma env st with
            | Error msg -> Error msg 
            | Ok (Some a, st1) -> apply1 (fn a) env st1
            | Ok (None, st1) -> Ok (None, st1)
         
    let inline private delayM (fn : unit -> Template<'a>) : Template<'a> = 
        bindM (mreturn ()) fn 
    
    type TemplateBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (template : TemplateBuilder) = new TemplateBuilder()


    let runTemplate (env : TemplateEnv) (st : TemplateState) (code : Template<'a>) : Result<'a * TemplateState, ErrMsg> = 
        match apply1 code env st with
        | Ok (Some(a), st) -> Ok (a, st)
        | Ok (None, _) -> Error "Empty result"
        | Error msg -> Error msg

    
        
    let private ( |>> ) (ma : Template<'a>) (fn : 'a -> 'b) : Template<'b> = 
        Template <| fun env st -> 
            match apply1 ma env st with 
            | Ok(None, st1) -> Ok(None, st1)
            | Ok(Some(a), st1) -> Ok(Some(fn a), st1)
            | Error msg -> Error msg
    
    let private unlistM (source: Template<'x> list) : Template<'x list> = 
        Template <| fun env stzero -> 
            let rec work xs st fk sk = 
                match xs with 
                | [] -> sk st []
                | x :: rest -> 
                    match apply1 x env st with
                    | Error msg -> fk msg
                    | Ok (a, st1) -> 
                        work rest st1 fk (fun st2 vs -> 
                        match a with 
                        | Some v -> sk st2 (v :: vs)
                        | None -> sk st2 vs)
            work source stzero (fun msg -> Error msg) (fun st xs -> Ok(Some(xs), st))



    let rootFloc (floc : FuncLocPath) (ma : Template<'a>) : Template<'a> = 
        Template <| fun (_, tenv) st -> 
            apply1 ma (floc, tenv) st
        
    let asksFloc () : Template<FuncLocPath> = 
        Template <| fun (floc, _) st -> Ok (Some(floc), st)

    let asksFuncLocProperties () : Template<FuncLocProperties> = 
        Template <| fun (_, tenv) st -> 
            let props : FuncLocProperties = 
                { StartupDate = tenv.StartupDate
                  StructureIndicator = tenv.StructureIndicator
                  MaintenancePlant = tenv.MaintenancePlant
                  ObjectStatus = tenv.ObjectStatus
                  ControllingArea = tenv.ControllingArea
                  CompanyCode = tenv.CompanyCode
                  Currency = tenv.Currency
                }
            Ok (Some(props), st)

    type EnvTransformer = EnvProperties -> EnvProperties

    let local (modify : EnvTransformer) (ma : Template<'a>) : Template<'a> = 
        Template <| fun (floc, tenv) st -> 
            apply1 ma (floc, modify tenv) st

    let locals (modifications : EnvTransformer list) (ma : Template<'a>) : Template<'a> = 
        let trafo = List.foldBack (fun f acc -> acc >> f) modifications id
        Template <| fun (floc, tenv) st -> 
            apply1 ma (floc, trafo tenv) st

            
    let startupDate (date : DateTime) : EnvTransformer = 
        fun env -> { env with StartupDate = date }


    let internal extendFloc (levelCode  : string) (ma : Template<'a>) : Template<'a> = 
        Template <| fun (floc, tenv) st -> 
            apply1 ma (floc |> extend levelCode, tenv) st
    

    type Characteristic = Template<S4Characteristic>
    
    let _characteristic (name : string) (value : string) : Characteristic = 
        mreturn { 
            Name = name
            Value = value
        }


    let optional (ma : Characteristic) : Characteristic = 
        Template <| fun env st -> 
            match apply1 ma env st with            
            | Error msg -> Error msg
            | Ok(None, st1) -> Ok (None, st1)
            | Ok (Some(c1), st1) -> 
                match c1.Value with 
                | null | "" ->  Ok(None, st1)
                | _ -> Ok (Some(c1), st1)

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
        Template <| fun env st -> 
            match apply1 e1 env st with
            | Ok (Some a, st1) -> 
                match apply1 attrib env st1 with
                | Ok (Some f, st2) -> Ok (Some(f a), st2)
                | Ok (None, st2) -> Ok (None, st2)
                | Error msg -> Error msg
            | Ok (None, st1) -> Ok (None, st1)
            | Error msg -> Error msg

    let private setAttributes (e1 : Equipment) (attribs : EquipmentAttribute list) : Equipment = 
        List.fold setAttribute e1 attribs
    



    let private newEquiInterimId (level : int) : Template<string> = 
        let makeName x = sprintf "L%iE%03i" level x
        Template <| fun _ st -> 
            let equiIndices = st.EquiIndices 
            match Map.tryFind level equiIndices with
            | None -> Ok (Some(makeName 1), {st with EquiIndices = Map.add level 2 equiIndices })
            | Some(i) -> Ok (Some (makeName i), {st with EquiIndices = Map.add level (i+1) equiIndices})

    let private newFlocInterimId (level : int) : Template<string> = 
        let makeName x = sprintf "L%iF%03i" level x
        Template <| fun _ st -> 
            let flocIndices = st.FlocIndices
            match Map.tryFind level flocIndices with
            | None -> Ok (Some(makeName 1), {st with FlocIndices = Map.add level 2 flocIndices })
            | Some(i) -> Ok (Some (makeName i), {st with FlocIndices = Map.add level (i+1) flocIndices})



    let _equipment (description : string) 
                    (category : string) 
                    (objectType : string)
                    (classes : Class list) 
                    (subordinateEquipment : Equipment list) 
                    (attributes : EquipmentAttribute list) : Equipment = 
        let equip1 = 
            template {
                let! level = asksFloc () |>> fun x -> x.Level
                let! interimId = newEquiInterimId level
                let! cs = unlistM classes
                let! es = unlistM subordinateEquipment
                return {
                    InterimId = interimId
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
        Template <| fun env st -> Ok (Some update, st)
            
    

    type Component = Template<S4Component>


    let _component (token : string) (description : string) (objectType : string)
                   (classes : Class list) (equipment : Equipment list) : Component = 
        extendFloc token
            <| template {
                let! floc = asksFloc ()
                let! props = asksFuncLocProperties ()
                let! interimId = newFlocInterimId 8
                let! cs = unlistM classes
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 7
                let! cs = unlistM classes
                let! xs = unlistM components
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 6
                let! cs = unlistM classes
                let! xs = unlistM items
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 5
                let! cs = unlistM classes
                let! xs = unlistM assemblies
                let! es = unlistM equipment
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 4
                let! cs = unlistM classes
                let! xs = unlistM systems
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 3
                let! cs = unlistM classes
                let! xs = unlistM processes
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 2
                let! cs = unlistM classes
                let! xs = unlistM processGroups
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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
                let! interimId = newFlocInterimId 1
                let! cs = unlistM classes
                let! xs = unlistM functions
                return { 
                    FuncLoc = floc
                    InterimId = interimId
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

