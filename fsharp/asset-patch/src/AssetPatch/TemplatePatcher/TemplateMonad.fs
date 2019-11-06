// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Hierarchy =
    
    open System

    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher
    
    /// *** With monadic templates (Reader) we can have a lot more
    /// *** flexibility for inheriting attributes


    type Env = 
        { StartupDate : DateTime
          MaintenancePlant : uint32
        }
    
    let defaultEnv () = 
        { StartupDate = DateTime.Now
          MaintenancePlant = 2100u
        }

    /// Reader + Error
    type Template<'a> = 
        | Template of (Env -> Result<'a, ErrMsg>)

    let inline private apply1 (ma : Template<'a>) 
                              (env : Env) : Result<'a, ErrMsg> = 
        let (Template fn) = ma in fn env

    let mreturn (x:'a) : Template<'a> = 
        Template <| fun _ -> Ok x

    let inline private bindM (ma : Template<'a>) 
                             (fn : 'a -> Template<'b>) : Template<'b> =
        Template <| fun env -> 
            match apply1 ma env with
            | Error msg -> Error msg 
            | Ok a -> apply1 (fn a) env
         
    let inline private delayM (fn : unit -> Template<'a>) : Template<'a> = 
        bindM (mreturn ()) fn 
    
    type TemplateBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (template : TemplateBuilder) = new TemplateBuilder()


    let runTemplate (code : Template<'x>) : Result<'x, ErrMsg> = 
        apply1 code (defaultEnv ())
    
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
                        sk (a :: vs))
            work source (fun msg -> Error msg) (fun xs -> Ok xs)


    

    type Characteristic = Template<Hierarchy.Characteristic>
    
    let _characteristic (name : string) (value : string) : Characteristic = 
        mreturn { 
            Name = name
            Value = value
        }

    type Class = Template<Hierarchy.Class>

    let _class (name : string) (number : uint32) (values : Characteristic list) : Class = 
        template {
            let! vs = unlistM values
            return { 
                ClassName = name
                ClassInt = number
                Characteritics = vs 
            }
        }
        
        


    //let _equipment (description : string) (objectType : string)
    //                (classes : Class list) 
    //                (subordinateEquipment : Equipment list) : Equipment = 
    //    // EquipmentId is filled in by a compiler pass
    //    { EquipmentId = None
    //      Description = description
    //      ObjectType = objectType
    //      Manufacturer = None
    //      Model = None
    //      Classes = classes 
    //      SuboridnateEquipment = subordinateEquipment }

    
    //let _component (token : string) (description : string) (objectType : string)
    //               (classes : Class list) (equipment : Equipment list) : Component = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Equipment = equipment }

    //let _item (token : string) (description : string) (objectType : string)
    //            (classes : Class list) 
    //            (components : Component list) (equipment : Equipment list) : Item = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Components = components
    //      Equipment = equipment }

    //let _assembly (token : string) (description : string) (objectType : string)
    //                (classes : Class list) 
    //                (items : Item list) (equipment : Equipment list) : Assembly = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Items = items
    //      Equipment = equipment }

    //let _system (token : string) (description : string) (objectType : string)
    //            (classes : Class list) 
    //            (assemblies : Assembly list) (equipment : Equipment list) : System = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Assemblies = assemblies
    //      Equipment = equipment }

    //let _process (token : string) (description : string) (objectType : string)
    //                (classes : Class list) 
    //                (systems : System list) : Process = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Systems = systems }

    //let _processGroup (token : string) (description : string) (objectType : string)    
    //                    (classes : Class list) 
    //                    (processes : Process list) : ProcessGroup = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Processes = processes }

    //let _function (token : string) (description : string) (objectType : string)
    //              (classes : Class list) 
    //              (processGroups : ProcessGroup list) : Function = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      ProcessGroups = processGroups }

    //let _site (token : string) (description : string) (objectType : string)
    //            (classes : Class list) 
    //            (functions : Function list) : Site = 
    //    { FuncLocSegment = _segment token description objectType
    //      Classes = classes 
    //      Functions = functions }

