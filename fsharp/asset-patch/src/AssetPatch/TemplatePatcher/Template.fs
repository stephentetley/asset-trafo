﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Template =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.EntityTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    
    /// *** With monadic templates (Reader) we can have a lot more
    /// *** flexibility for inheriting attributes


    type Env = CompilerMonad.TemplateEnv

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


    let runTemplate (env : Env) (code : Template<'a>) : Result<'a, ErrMsg> = 
        apply1 code env
        
        
    
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


    let private _segment (token : string) 
                         (description : string) 
                         (objectType : string) : Template<FuncLocSegment> = 
        mreturn { 
            Name = token
            Description = description
            ObjectType = objectType 
        }

    type Characteristic = Template<S4Characteristic>
    
    let _characteristic (name : string) (value : string) : Characteristic = 
        mreturn { 
            Name = name
            Value = value
        }

    type Class = Template<S4Class>

    let _class (name : string) (number : uint32) (values : Characteristic list) : Class = 
        template {
            let! vs = unlistM values
            return { 
                ClassName = name
                ClassInt = number
                Characteritics = vs 
            }
        }
        
        

    type Equipment = Template<S4Equipment>
    
    let _equipment (description : string) (objectType : string)
                    (classes : Class list) 
                    (subordinateEquipment : Equipment list) : Equipment = 
        // EquipmentId is filled in by a compiler pass
        template {
            let! cs = unlistM classes
            let! es = unlistM subordinateEquipment
            return { 
                EquipmentId = None
                Description = description
                ObjectType = objectType
                Manufacturer = None
                Model = None
                Classes = cs 
                SuboridnateEquipment = es 
            }
          }

    type Component = Template<S4Component>
    
    let _component (token : string) (description : string) (objectType : string)
                   (classes : Class list) (equipment : Equipment list) : Component = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! es = unlistM equipment
            return { 
                FuncLocSegment = floc
                Classes = cs 
                Equipment = es 
            }
        }

    type Item = Template<S4Item>
    
    let _item (token : string) (description : string) (objectType : string)
                (classes : Class list) 
                (components : Component list) (equipment : Equipment list) : Item = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM components
            let! es = unlistM equipment
            return { 
                FuncLocSegment = floc
                Classes = cs 
                Components = xs
                Equipment = es 
            }
        }

    type Assembly = Template<S4Assembly>
    
    let _assembly (token : string) (description : string) (objectType : string)
                    (classes : Class list) 
                    (items : Item list) (equipment : Equipment list) : Assembly = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM items
            let! es = unlistM equipment
            return { 
                FuncLocSegment = floc
                Classes = cs
                Items = xs
                Equipment = es 
            }
        }

    type System = Template<S4System>
    
    let _system (token : string) (description : string) (objectType : string)
                (classes : Class list) 
                (assemblies : Assembly list) (equipment : Equipment list) : System = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM assemblies
            let! es = unlistM equipment
            return { 
                FuncLocSegment = floc
                Classes = cs
                Assemblies = xs
                Equipment = es 
            }
        }

    type Process = Template<S4Process>
    
    let _process (token : string) (description : string) (objectType : string)
                    (classes : Class list) 
                    (systems : System list) : Process = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM systems
            return { 
                FuncLocSegment = floc
                Classes = cs 
                Systems = xs 
            }
        }

    type ProcessGroup = Template<S4ProcessGroup>
    
    let _processGroup (token : string) (description : string) (objectType : string)    
                        (classes : Class list) 
                        (processes : Process list) : ProcessGroup = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM processes
            return { 
                FuncLocSegment = floc
                Classes = cs 
                Processes = xs 
            }
        }

    type Function = Template<S4Function>
    
    let _function (token : string) (description : string) (objectType : string)
                  (classes : Class list) 
                  (processGroups : ProcessGroup list) : Function = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM processGroups
            return { 
                FuncLocSegment = floc
                Classes = cs 
                ProcessGroups = xs 
            }
        }

    type Site = Template<S4Site>
    
    let _site (token : string) (description : string) (objectType : string)
                (classes : Class list) 
                (functions : Function list) : Site = 
        template {
            let! floc = _segment token description objectType
            let! cs = unlistM classes
            let! xs = unlistM functions
            return { 
                FuncLocSegment = floc
                Classes = cs 
                Functions = xs 
            }
        }

